library(sp)


db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")
{
hitter_ids<-100106

# hitter_ids <- sort(dbGetQuery(db, 'select distinct BatterId from pitch_data where SEASON = 2024')$BatterId)

nitro_hull_master <- data.frame()

nitro_stats <- data.frame()

for(i in seq_along(hitter_ids)){
  
  hitter_id<-hitter_ids[i]
  # hitter_id=100126
  
  hitter_data <- dbGetQuery(db, glue::glue(
    "select SEASON, Batter, BatterId, BatterSide, 
        PitchCall,PlayResult,TaggedHitType,
        PlateLocSide, PlateLocHeight,
        ExitSpeed, Angle, hardhit,
        xBA, xSLG, xWOBA, xWOBACON,
        bbe, in_zone, swing, whiff, total_bases, is_hit, is_pa,
        GameID
        from pitch_data
        where 
        --SEASON = 2024
        /*bbe = 1
        and PlateLocHeight is not null
        and TaggedHitType is not null
        and ExitSpeed is not null */
        --and
        BatterId = {hitter_id}" )
  )
  
  
  hitter_bbe <- hitter_data %>% 
    filter(bbe==1,
           !is.na(ExitSpeed),
           TaggedHitType != "Bunt",
           !is.na(PlateLocHeight),
           !is.na(PlateLocSide))
  
  threshold <- if(nrow(hitter_bbe)>200){.95}else{.9}
  
  nitro_mph <- quantile(hitter_bbe$ExitSpeed, na.rm = T, threshold)
  
  top_ev_percentile <- hitter_bbe %>% filter(ExitSpeed >= nitro_mph)
  
  hull_indices <- chull(top_ev_percentile$PlateLocSide, top_ev_percentile$PlateLocHeight)
  
  data_hull <- top_ev_percentile[hull_indices, ] %>%
    bind_rows(top_ev_percentile[hull_indices[1], ])
  
  nitro_df <- data_hull %>% select(Batter, BatterId, PlateLocSide, PlateLocHeight)
  
  nitro_hull_master <- rbind(nitro_hull_master, nitro_df)
  
  # Pull out the hull polygon coordinates
  poly_x <- data_hull$PlateLocSide
  poly_y <- data_hull$PlateLocHeight
  
  hitter_data <- hitter_data %>%
    mutate(in_nitro_zone = sp::point.in.polygon(PlateLocSide, PlateLocHeight, poly_x, poly_y) > 0) 
  
  hitter_nitro <-
    hitter_data %>%
    group_by(Batter, BatterId) %>%
    summarise(#Team = paste(unique(\Batter)),
      G_tm = n_distinct(GameID),
      PA = sum(is_pa, na.rm = T),
      P = n(),
      BBE = sum(bbe, na.rm = T),
      P_nitro = sum(in_nitro_zone==T, na.rm = T),
      P_o_nitro = sum(in_nitro_zone==F, na.rm = T),
      swing_nitro = sum(swing[in_nitro_zone==T], na.rm = T),
      o_swing_nitro = sum(swing[in_nitro_zone==F], na.rm = T),
      BBE_nitro = sum(bbe[in_nitro_zone==T], na.rm = T),
      InPlay_nitro = sum(PitchCall=='InPlay' & in_nitro_zone==T, na.rm = T),
      BA_nitro = sum(is_hit[in_nitro_zone==T & bbe==1], na.rm = T) / BBE_nitro,
      SLG_nitro = sum(total_bases[in_nitro_zone==T & bbe==1], na.rm = T) / BBE_nitro,
      # wOBA_nitro = sum(woba[in_nitro_zone==T], na.rm = T) / BBE_nitro,,
      xBA_nitro = sum(xBA[in_nitro_zone==T & bbe==1], na.rm = T) / BBE_nitro,
      xSLG_nitro = sum(xSLG[in_nitro_zone==T & bbe==1], na.rm = T) / BBE_nitro,
      xwOBA_nitro = sum(xwOBA[in_nitro_zone==T & bbe==1], na.rm = T) / BBE_nitro,
      xwOBACON = sum(xwOBA, na.rm = T) / BBE,
      xwOBA = sum(xwOBA, na.rm = T) / PA,
      EV_nitro = mean(ExitSpeed[in_nitro_zone==T & bbe ==1], na.rm = T),
      LA_nitro = mean(Angle[in_nitro_zone==T & bbe ==1], na.rm = T),
      .groups = "drop"
    ) %>%
    mutate(Nitro_Sw_pct = swing_nitro / P_nitro,
           O_Nitro_Sw_pct = o_swing_nitro / P_o_nitro)
  
  nitro_stats <- rbind(nitro_stats, hitter_nitro)
  
  cat(paste(hitter_id,i,"of",length(hitter_ids)), sep = '\n')
}

stats_24 <- dbGetQuery(db, 'select player_id_fl, G G_fl,  wOBA from stats_hitting_player where season = 2024')

nitro_stats_2 <- nitro_stats  %>%
  select(Batter,BatterId,G_tm, P, P_nitro, P_o_nitro, BBE_nitro, xwOBA, swing_nitro, o_swing_nitro, Nitro_Sw_pct, O_Nitro_Sw_pct)%>%
  left_join(stats_24, by = c('BatterId' = 'player_id_fl')) %>%
  mutate(bb_nitro = BBE_nitro /swing_nitro) %>%
  filter(G_tm > G_fl / 2,
         G_fl > 1
         )


model <- lm(O_Nitro_Sw_pct ~ wOBA, data = nitro_stats_2)
summary_model <- summary(model)
r2 <- summary_model$r.squared
r <- sqrt(r2) * sign(coef(model)[2])
p <- summary_model$coefficients[2,4]

ggplot(data = nitro_stats_2, aes(x = wOBA, y = O_Nitro_Sw_pct)) +
  geom_point() +
  geom_smooth(method = "lm") +
  annotate("text", x = 0.3, y = max(nitro_stats_2$O_Nitro_Sw_pct),
           label = paste0("R = ", round(r, 2), ", p = ", format.pval(p, digits = 2, eps = .001)),
           hjust = 0)

model <- lm(Nitro_Sw_pct ~ wOBA, data = nitro_stats_2)
summary_model <- summary(model)
r2 <- summary_model$r.squared
r <- sqrt(r2) * sign(coef(model)[2])
p <- summary_model$coefficients[2,4]

ggplot(data = nitro_stats_2, aes(x = wOBA, y = Nitro_Sw_pct)) +
  geom_point() +
  geom_smooth(method = "lm") +
  annotate("text", x = 0.3, y = max(nitro_stats_2$Nitro_Sw_pct),
           label = paste0("R = ", round(r, 2), ", p = ", format.pval(p, digits = 2, eps = .001)),
           hjust = 0)
# Check if each point falls inside the convex hull
# nitro_data <- hitter_bbe %>%
#   mutate(in_nitro_zone = sp::point.in.polygon(PlateLocSide, PlateLocHeight, poly_x, poly_y) > 0) %>%
#   filter(in_nitro_zone==T)


slg_top_95 <- round(sum(top_ev_percentile$total_bases, na.rm = T) / nrow(top_ev_percentile),3)
ba_top_95 <- round(sum(top_ev_percentile$total_bases > 0, na.rm = T) / nrow(top_ev_percentile),3)
xwoba_95  <- round(sum(top_ev_percentile$xwOBA, na.rm = T) / nrow(top_ev_percentile),3)

hitter<-hitter_data$Batter[1]

height <- dbGetQuery(db, glue::glue("select * from rosters_ where Name = '{hitter}'"))$HEIGHT[1]
height_inches = tonybaseball::convert_to_inches(height)
# height_inches <- 75
k_zone_top = height_inches * .535 / 12
k_zone_bot = height_inches * .27 / 12

ggplot() +
  geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = k_zone_bot-.125, ymax = k_zone_top+.125), alpha = 0, size = .75, color = "red", linetype = 'dotted') +
  geom_rect(aes(xmin = -0.708, xmax = 0.708, ymin = k_zone_bot, ymax = k_zone_top), alpha = 0, size = 1, color = "black") +
  geom_segment(data = tonybaseball::home_plate_c_pov, aes(x=x,y=y,xend=xend,yend=yend), size = 1)+
  geom_polygon(data = data_hull, aes(-PlateLocSide, PlateLocHeight), fill = '#ff8e8e', color = "darkgrey", linewidth = 1, alpha = .1) +
  # geom_point(data = data_bbe_out, aes(x=-PlateLocSide, y = PlateLocHeight), size = 3, color = 'grey', alpha = .3)+
  # geom_point(data = top_ev_percentile, aes(x=-PlateLocSide, y = PlateLocHeight, color = PlayResult), size = 4, alpha = .5)+
  geom_point(data = top_ev_percentile, aes(x=-PlateLocSide, y = PlateLocHeight, fill = xwOBA), size = 6, alpha = .75, color = 'black', pch = 21)+
  scale_fill_gradient(low = 'lightgrey', high = 'red')+
  annotate(geom = 'text', x = 0, y = 4.2, label = paste("Nitro EV:",round(nitro_mph,1),"+ mph",
                                                        "\nNitro BA:", ba_top_95,
                                                        "\nNitro SLG:", slg_top_95,
                                                        "\nNitro xwOBA:", xwoba_95))+
  annotate(geom = 'text', x = 0, y = -.2, label = paste("Seasons:", min(hitter_bbe$SEASON),"-",max(hitter_bbe$SEASON)))+
  xlim(-2,2)+
  ylim(-.5,4.5)+
  coord_fixed()+
  theme_void()+
  labs(title = paste("Nitro Zone -", hitter),
       subtitle = glue::glue("Top {(1-threshold)*100}% Exit Velo Zone"))+
  theme(plot.title = element_text(hjust = .5, face = 'bold'),
        plot.subtitle = element_text(hjust = .5))

}
