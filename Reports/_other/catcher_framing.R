db <- dbConnect(SQLite(), "C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

catchers <- dbGetQuery(db, 'select SEASON, Catcher, CatcherId, CatcherTeam, PitchCall, PlayResult, in_zone, run_value, GameID
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
      
    ),
    rv_net = case_when(
      NetStrikes == 0 ~ 0,
      NetStrikes == -1 & run_value < 0 ~ -run_value,
      T ~ run_value
    )
  )

# catcher_sum <- catchers %>%
#   group_by(Catcher, CatcherId, CatcherTeam, SEASON) %>%
#   summarise(Pitches = n(),
#             G = n_distinct(GameID),
#             Correct_Calls = sum(NetStrikes == 0),
#             Strikes_Lost = sum(NetStrikes==-1),
#             Strikes_Gained = sum(NetStrikes==1),
#             Net_Strikes = sum(NetStrikes),
#             rv_net = sum(run_value[NetStrikes %in% c(1,-1)], na.rm = T)
#             ) %>%
#   mutate(RV_per_135 = rv_net / 135,
#          RV_per_G = rv_net / G)


dbGetQuery(db, 'select Date, Catcher, `Top.Bottom`, GameID, PlateLocHeight from pitch_data') %>%
  group_by(`Top.Bottom`) %>%
  summarise(pitches = n(),
            pitches_read = sum(!is.na(PlateLocHeight)),
            games = n_distinct(GameID)) %>%
  mutate(p_per_half = pitches/games)

dbGetQuery(db, "select GameID from pitch_data ") %>%
  # group_by(GameID) %>%
  summarise(n = n(),
            gm = n_distinct(GameID) 
  ) %>%
  mutate(t = n/gm/2)
  
stats_catching_league <- catchers %>%
  group_by(SEASON) %>%
  summarise(G = n_distinct(GameID),
            Pitches = n(),
            Correct_Calls = sum(NetStrikes == 0),
            Strikes_Lost = sum(NetStrikes==-1),
            Strikes_Gained = sum(NetStrikes==1),
            Net_Strikes = sum(NetStrikes),
            RV_net = sum(run_value[NetStrikes %in% c(1,-1)], na.rm = T)
  ) %>%
  mutate(RV_per_130 = round(RV_net / Pitches * 130,6),
         Net_K_per_130 = round(Net_Strikes / Pitches * 130,6),
         Framing_plus =  RV_per_130 / RV_per_130 * 100)


stats_catching_player <- catchers %>%
  group_by(Catcher, CatcherId, SEASON, CatcherTeam) %>%
  summarise(G = n_distinct(GameID),
            Pitches = n(),
            Correct_Calls = sum(NetStrikes == 0),
            Strikes_Lost = sum(NetStrikes==-1),
            Strikes_Gained = sum(NetStrikes==1),
            Net_Strikes = sum(NetStrikes),
            RV_net = sum(run_value[NetStrikes %in% c(1,-1)], na.rm = T)
  ) %>%
  ungroup()%>%
  mutate(RV_per_130 = round(RV_net / Pitches * 130,6),
         Net_K_per_130 = round(Net_Strikes / Pitches * 130,6),
         Framing_plus = RV_per_130 / stats_catching_league$RV_per_130[match(SEASON, stats_catching_league$SEASON)] * 100,
         Framing_plus =- (RV_per_130 - mean(RV_per_130, na.rm = TRUE)) /
         sd(RV_per_130, na.rm = TRUE)  * 10 + 100
         ) 
sd(stats_catching_player$RV_per_130)


stats_catching_player_career <- catchers %>%
  group_by(Catcher, CatcherId) %>%
  summarise(G = n_distinct(GameID),
            Pitches = n(),
            Correct_Calls = sum(NetStrikes == 0),
            Strikes_Lost = sum(NetStrikes==-1),
            Strikes_Gained = sum(NetStrikes==1),
            Net_Strikes = sum(NetStrikes),
            RV_net = sum(run_value[NetStrikes %in% c(1,-1)], na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(RV_per_130 = round(RV_net / Pitches * 130,6),
         Net_K_per_130 = round(Net_Strikes / Pitches * 130,6),
         Framing_plus = RV_per_130 / stats_catching_league$RV_per_130[stats_catching_league$SEASON==2024] * 100,
         Framing_plus =- (RV_per_130 - mean(RV_per_130, na.rm = TRUE)) /
           sd(RV_per_130, na.rm = TRUE)  * 10 + 100
  ) 

stats_catching_team <- catchers %>%
  group_by(CatcherTeam, SEASON) %>%
  summarise(G = n_distinct(GameID),
            Pitches = n(),
            Correct_Calls = sum(NetStrikes == 0),
            Strikes_Lost = sum(NetStrikes==-1),
            Strikes_Gained = sum(NetStrikes==1),
            Net_Strikes = sum(NetStrikes),
            RV_net = sum(run_value[NetStrikes %in% c(1,-1)], na.rm = T)
  ) %>%
  ungroup()%>%
  mutate(RV_per_130 = round(RV_net / Pitches * 130,6),
         Net_K_per_130 = round(Net_Strikes / Pitches * 130,6),
         Framing_plus = RV_per_130 / stats_catching_league$RV_per_130[match(SEASON, stats_catching_league$SEASON)] * 100,
         Framing_plus =- (RV_per_130 - mean(RV_per_130, na.rm = TRUE)) /
           sd(RV_per_130, na.rm = TRUE)  * 10 + 100
  ) 


dbWriteTable(db, name = 'stats_catching_player', value = stats_catching_player, overwrite = T)
dbWriteTable(db, name = 'stats_catching_player_career', value = stats_catching_player_career, overwrite = T)
dbWriteTable(db, name = 'stats_catching_team', value = stats_catching_team, overwrite = T)
dbWriteTable(db, name = 'stats_catching_league', value = stats_catching_league, overwrite = T)
