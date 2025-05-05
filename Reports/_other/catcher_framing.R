library(tidyverse)
library(RSQLite)

db <- dbConnect(SQLite(), "C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

shadow_zone_inner_lr <- .708 - 3/12
shadow_zone_inner_top <- 3.5 - 3/12
shadow_zone_inner_bot <- 1.5 + 3/12
shadow_zone_outer_lr <- .708 + (3/12*1.5)
shadow_zone_outer_top <- 3.5 + (3/12*1.5)
shadow_zone_outer_bot <- 1.5 - (3/12*1.5)


catch <- dbGetQuery(db, 'select GameID, `Top.Bottom`, PitchCall, PlateLocSide,PlateLocHeight from pitch_data') %>%
  group_by(GameID, `Top.Bottom`) %>%
  summarise(Pitches = n(),
            Pitches_within_shadow = sum(between(PlateLocSide, -shadow_zone_outer_lr, shadow_zone_outer_lr) &
                                          between(PlateLocHeight, shadow_zone_outer_bot,shadow_zone_outer_top), na.rm = T),
            Pitches_called_within_shadow = sum(PitchCall %in% c("BallCalled", "StrikeCalled") & 
                                                 between(PlateLocSide, -shadow_zone_outer_lr, shadow_zone_outer_lr) &
                                                 between(PlateLocHeight, shadow_zone_outer_bot,shadow_zone_outer_top), na.rm = T),
            Called = sum(PitchCall %in% c("BallCalled", "StrikeCalled")),
            Swings = sum(!PitchCall %in% c("BallCalled", "StrikeCalled"))) %>%
  ungroup()%>%
  group_by(Top.Bottom) %>%
  summarise(
    G = n_distinct(GameID),
    across(c(Pitches,Pitches_within_shadow,Pitches_called_within_shadow,Called,Swings), ~ mean(.,na.rm = T))
  )
run_values_table <- dbGetQuery(db, "select * from run_values_long")

# plotly::ggplotly(
#   ggplot(catchers %>% filter(Catcher == 'Blake Grant-Parks'), aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall))+
#     geom_rect(aes(xmin = -shadow_zone_outer_lr, xmax = shadow_zone_outer_lr, ymin = 1.5- 3/12, ymax = 3.5+ 3/12), fill = 'lightgrey', color = 'red')+
#     geom_rect(aes(xmin = -shadow_zone_inner_lr, xmax = shadow_zone_inner_lr, ymin = 1.5+ 3/12, ymax = 3.5- 3/12), fill = 'white', color = 'red')+
#     xlim(-2,2)+
#     ylim(0,5)+
#     geom_point(alpha = .25)+
#     scale_color_manual(values = c('BallCalled' = 'blue','StrikeCalled' = 'red'))+
#     geom_rect(aes(xmin = -.708, xmax = .708, ymin = 1.5, ymax = 3.5), fill = NA, color = 'black')+
#     geom_segment(data = tonybaseball::home_plate_p_pov, aes(x=x,y=y,xend=xend,yend=yend), inherit.aes = F)
# )

catchers <- dbGetQuery(db, 'select SEASON, Date, Catcher, CatcherId, CatcherTeam, Balls, Strikes, PitchCall, PlayResult, in_zone, run_value run_value_actual, GameID, PlateLocSide, PlateLocHeight
                       from pitch_data
                       where Catcher <> "" and Catcher is not null
                       AND PitchCall in ("StrikeCalled", "BallCalled")
                       and in_zone is not null
                       and SEASON >= 2022') %>%
mutate(
  xPitchCall = case_when(
    PitchCall == 'StrikeCalled' & in_zone == 1 ~ 'StrikeCalled',
    PitchCall == 'StrikeCalled' & in_zone == 0 ~ 'BallCalled',
    PitchCall == 'BallCalled' & in_zone == 1 ~ 'StrikeCalled',
    PitchCall == 'BallCalled' & in_zone == 0 ~ 'BallCalled',
  ),
  NetStrikes = case_when(
    PitchCall == xPitchCall ~ 0,
    PitchCall == 'StrikeCalled' & xPitchCall == 'BallCalled' ~ +1,
    PitchCall == 'BallCalled' & xPitchCall == 'StrikeCalled' ~ -1,
    
  )
) %>%
  relocate(xPitchCall, .after = PlayResult) %>%
  mutate(Count_start = paste0(Balls,"-",Strikes), .after = Strikes) %>%
  mutate(
    Count_end = case_when(
      PitchCall == 'BallCalled' ~ paste0(Balls+1,"-",Strikes),
      PitchCall == 'StrikeCalled' ~ paste0(Balls,"-",Strikes+1)
    ),xCount_end = case_when(
      xPitchCall == 'BallCalled' ~ paste0(Balls+1,"-",Strikes),
      xPitchCall == 'StrikeCalled' ~ paste0(Balls,"-",Strikes+1)
    ), .after = PitchCall)%>%
  relocate(xPitchCall, xCount_end, .after = PlayResult)%>%
  mutate(xPitchCall = gsub('StrikeCalled|StrikeSwinging', 'Strike', xPitchCall)) %>%
  left_join(run_values_table, by = c('Count_start' = 'Count', 'xPitchCall' = 'PlayResult'))  %>%
  rename(x_run_value = run_value) %>%
  mutate(
    # x_run_value = ifelse(Count_end==xCount_end,0,x_run_value),
    rv_net = run_value_actual - x_run_value
    # rv_net = case_when(xPitchCall == 'BallCalled' & PitchCall =='StrikeCalled' ~ -.125, 
    #                    xPitchCall == 'StrikeCalled' & PitchCall =='BallCalled' ~ .125,
    #                    T ~ 0)
  )

stats_catching_league <- catchers %>%
  group_by(SEASON) %>%
  summarise(G = n_distinct(GameID),
            Pitches = n(),
            Correct_Calls = sum(NetStrikes == 0),
            Strikes_Lost = sum(NetStrikes==-1),
            Strikes_Gained = sum(NetStrikes==1),
            Net_Strikes = sum(NetStrikes),
            RV_net = sum(rv_net, na.rm = T)
  ) %>%
  mutate(RV_per_75 = round(RV_net / Pitches * 75,6),
         Net_K_per_75 = round(Net_Strikes / Pitches * 75,6),
         Framing_plus =  RV_per_75 / RV_per_75 * 100)



stats_catching_player <- catchers %>%
  group_by(Catcher, CatcherId, SEASON, CatcherTeam) %>%
  summarise(G = n_distinct(GameID),
            Pitches = n(),
            Correct_Calls = sum(NetStrikes == 0),
            Strikes_Lost = sum(NetStrikes==-1),
            Strikes_Gained = sum(NetStrikes==1),
            Net_Strikes = sum(NetStrikes),
            RV_net = sum(rv_net, na.rm = T)
  ) %>%
  ungroup()%>%
  mutate(RV_per_75 = round(RV_net / Pitches * 75,6),
         Net_K_per_75 = round(Net_Strikes / Pitches * 75,6),
         Framing_plus = RV_per_75 / stats_catching_league$RV_per_75[match(SEASON, stats_catching_league$SEASON)] * 100,
         
         qual = ifelse(Pitches / 24 > 6, 1, 0)
  ) %>%
  arrange(desc(qual), RV_net)

sd_RV_per_75 <- sd(stats_catching_player$RV_per_75)
mean_RV_per_75 <- mean(stats_catching_player$RV_per_75)
  
stats_catching_player$Framing_plus <- with(stats_catching_player,
                                        - (RV_per_75 - mean_RV_per_75) /
                                          sd(RV_per_75, na.rm = TRUE)  * 10 + 100,)

stats_c_p_g <- catchers %>%
  group_by(Catcher, CatcherId, CatcherTeam, Date, SEASON, GameID) %>%
  summarise(G = n_distinct(GameID),
            Pitches = n(),
            Correct_Calls = sum(NetStrikes == 0),
            Strikes_Lost = sum(NetStrikes==-1),
            Strikes_Gained = sum(NetStrikes==1),
            Net_Strikes = sum(NetStrikes),
            RV_net = sum(rv_net, na.rm = T)
  )  %>%
  ungroup()%>%
  mutate(RV_per_75 = round(RV_net / Pitches * 75,6),
         Net_K_per_75 = round(Net_Strikes / Pitches * 75,6),
         Framing_plus = RV_per_75 / stats_catching_league$RV_per_75[match(SEASON, stats_catching_league$SEASON)] * 100,
         # Framing_plus =- (RV_per_75 - mean(RV_per_75, na.rm = TRUE)) /
         #   0.564367  * 10 + 100,
         # qual = ifelse(Pitches / 24 > 6, 1, 0)
  ) %>% 
  filter(Catcher == 'Felix Aberouette')%>% 
  select(-GameID, SEASON) %>%
  filter(Date == '2024-06-22')
# arrange(desc(qual), RV_net)

stats_c_p_g$Framing_plus <- with(stats_c_p_g,  - (RV_per_75 - mean_RV_per_75) / sd_RV_per_75  * 10 + 100,)




ggplot(catchers %>% filter(Catcher == 'Nick Oddo'), aes(x = PlateLocSide, y = PlateLocHeight, color = rv_net))+
  # geom_rect(aes(xmin = -shadow_zone_outer_lr, xmax = shadow_zone_outer_lr, ymin = 1.5- 3/12, ymax = 3.5+ 3/12), fill = 'lightgrey', color = 'red')+
  # geom_rect(aes(xmin = -shadow_zone_inner_lr, xmax = shadow_zone_inner_lr, ymin = 1.5+ 3/12, ymax = 3.5- 3/12), fill = 'white', color = 'red')+
  xlim(-2,2)+
  ylim(0,5)+
  geom_point(alpha = .5)+
  # scale_color_manual(values = c('BallCalled' = 'blue','StrikeCalled' = 'red'))+
  geom_rect(aes(xmin = -.708, xmax = .708, ymin = 1.5, ymax = 3.5), fill = NA, color = 'black')+
  geom_segment(data = tonybaseball::home_plate_p_pov, aes(x=x,y=y,xend=xend,yend=yend), inherit.aes = F)


ggplot(catchers %>% filter(Catcher == 'Nick Oddo') %>% filter(rv_net != 0) %>%
         arrange(-rv_net), 
       aes(x = -PlateLocSide, y = PlateLocHeight, color = rv_net))+
  xlim(-2,2)+
  ylim(0,5)+
  theme_void()+
  geom_point(alpha = .5, size = 5)+
  scale_color_gradient2(low = ("#c91f26"), mid = "grey80", high = ("#365ea6"), midpoint = 0) +
  geom_rect(aes(xmin = -.708, xmax = .708, ymin = 1.5, ymax = 3.5), fill = NA, color = 'black')+
  geom_segment(data = tonybaseball::home_plate_c_pov, aes(x=x,y=y,xend=xend,yend=yend), inherit.aes = F)


stats_catching_player_career <- catchers %>%
  group_by(Catcher, CatcherId) %>%
  summarise(G = n_distinct(GameID),
            Pitches = n(),
            Correct_Calls = sum(NetStrikes == 0),
            Strikes_Lost = sum(NetStrikes==-1),
            Strikes_Gained = sum(NetStrikes==1),
            Net_Strikes = sum(NetStrikes),
            RV_net = sum(rv_net, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(RV_per_75 = round(RV_net / Pitches * 75,6),
         Net_K_per_75 = round(Net_Strikes / Pitches * 75,6),
         Framing_plus = RV_per_75 / stats_catching_league$RV_per_75[stats_catching_league$SEASON==2024] * 100,
         Framing_plus =- (RV_per_75 - mean(RV_per_75, na.rm = TRUE)) /
           sd(RV_per_75, na.rm = TRUE)  * 10 + 100
  ) 

stats_catching_team <- catchers %>%
  group_by(CatcherTeam, SEASON) %>%
  summarise(G = n_distinct(GameID),
            Pitches = n(),
            Correct_Calls = sum(NetStrikes == 0),
            Strikes_Lost = sum(NetStrikes==-1),
            Strikes_Gained = sum(NetStrikes==1),
            Net_Strikes = sum(NetStrikes),
            RV_net = sum(rv_net, na.rm = T)
  ) %>%
  ungroup()%>%
  mutate(RV_per_75 = round(RV_net / Pitches * 75,6),
         Net_K_per_75 = round(Net_Strikes / Pitches * 75,6),
         Framing_plus = RV_per_75 / stats_catching_league$RV_per_75[match(SEASON, stats_catching_league$SEASON)] * 100,
         Framing_plus =- (RV_per_75 - mean(RV_per_75, na.rm = TRUE)) /
           sd(RV_per_75, na.rm = TRUE)  * 10 + 100
  ) 


dbWriteTable(db, name = 'stats_catching_player', value = stats_catching_player, overwrite = T)
dbWriteTable(db, name = 'stats_catching_player_career', value = stats_catching_player_career, overwrite = T)
dbWriteTable(db, name = 'stats_catching_team', value = stats_catching_team, overwrite = T)
dbWriteTable(db, name = 'stats_catching_league', value = stats_catching_league, overwrite = T)


with(stats_catching_player, quantile(RV_per_75[qual == 1], probs = 1 - c(.25, .5, .75, .9)))
with(stats_catching_player, quantile(RV_per_75[qual >= 0], probs = 1 - c(.25, .5, .75, .9)))
with(stats_catching_player, quantile(Framing_plus[qual >=0], probs = c(.1, .25, .5, .75, .9)))
with(stats_catching_player, quantile(Net_K_per_75[qual >=0], probs = c(.1, .25, .5, .75, .9)))
