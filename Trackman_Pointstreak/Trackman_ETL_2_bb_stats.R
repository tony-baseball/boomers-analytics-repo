{
  library(dplyr)
  library(slackr)
  library(emojifont)
  library(RSQLite)
  
  slackr_setup(channel = "#general", username = "slackr", icon_emoji = '',
               incoming_webhook_url = 'https://hooks.slack.com/services/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', 
               token = 'xoxb-7165085228646-7165145332742-bkkhpCh1WqnyS8PxZdlYBkbH', echo = FALSE)
  
  
  
  db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")
}

all_data <-  dbGetQuery(db, 'SELECT * FROM pitch_data') %>%
  mutate(
    pull = case_when(
      BatterSide == 'Right' & between(Direction, -50,-15) & bbe == 1 ~ 1,
      BatterSide == 'Right' & !between(Direction, -50,-15) & bbe == 1 ~ 0,
      BatterSide == 'Left' & between(Direction, 15,50) & bbe == 1 ~ 1,
      BatterSide == 'Left' & !between(Direction, 15,50) & bbe == 1 ~ 0,
      bbe == 1 ~ 0,
      bbe == 0 ~ 0,
      is.na(bbe) ~ NA ),
    oppo = case_when(
      BatterSide == 'Left' & between(Direction, -50,-15) & bbe == 1 ~ 1,
      BatterSide == 'Left' & !between(Direction, -50,-15) & bbe == 1 ~ 0,
      BatterSide == 'Right' & between(Direction, 15,50) & bbe == 1 ~ 1,
      BatterSide == 'Right' & !between(Direction, 15,50) & bbe == 1 ~ 0,
      bbe == 1 ~ 0,
      bbe == 0 ~ 0,
      is.na(bbe) ~ NA ),
    center = case_when(
      between(Direction, -15,15) & bbe == 1 ~ 1,
      !between(Direction, -15,15) & bbe == 1 ~ 0,
      bbe == 1 ~ 0,
      bbe == 0 ~ 0,
      is.na(bbe) ~ NA
    )
  )

weights <- dbGetQuery(db, 'select * from weights') %>%
  rbind(dbGetQuery(db, 'select * from weights where SEASON = 2023'))%>%
  rbind(dbGetQuery(db, 'select * from weights where SEASON = 2023'))%>%
  mutate(season = c(2023,2024,2021,2022)) %>%
  arrange(season)

{ 
  
  
  # HITTING - PLAYER, TEAM, SEASON ----
  ## hitter data ----
  hitter_bb <- all_data %>%
    filter(Batter !='') %>%
    group_by(Batter, BatterId, SEASON) %>%
    dplyr::summarise(
      BatterTeam = paste(unique(BatterTeam), collapse = ", "),
      # savant Batting
      G = n_distinct(GameID),
      P = n(),
      PA = sum(is_pa, na.rm = T),
      AB = sum(is_ab, na.rm = T),
      BIP = sum(PitchCall == "InPlay", na.rm = T),
      BBE = sum(bbe[TaggedHitType!='Bunt'],na.rm = T),
      # nBBE = sum(PitchCall == 'InPlay' & is.na(ExitSpeed)),
      H = sum(is_hit, na.rm = T),
      X1B = sum(PlayResult=='Single', na.rm = T),
      X2B = sum(PlayResult=='Double', na.rm = T),
      X3B = sum(PlayResult=='Triple', na.rm = T),
      HR = sum(PlayResult=='HomeRun', na.rm = T),
      SO = sum(grepl('Strikeout',PlayResult), na.rm = T) ,
      BB = sum(PlayResult=='Walk', na.rm = T),
      HBP = sum(PlayResult=='HitByPitch', na.rm = T),
      SAC = sum(grepl('Sac',PlayResult), na.rm = T),
      xBA_sum = sum(xBA, na.rm = T),
      # xBA = xBA_sum / (BBE+K),
      # xBACON = xBA_sum / BBE,
      xSLG_sum = sum(xSLG, na.rm = T),
      xwoba_sum = sum(xwOBA, na.rm = T),
      xwobacon_sum = sum(xwOBACON[bbe == 1], na.rm = T),
      # xSLG = xSLG_sum / (BBE+K),
      ExitVelo = mean(ExitSpeed[bbe == 1], na.rm = T),
      ExitVelo_max = max(ExitSpeed[bbe == 1], na.rm = T),
      Dist_avg = mean(Distance[bbe==1], na.rm = T),
      Dist_max = max(Distance[bbe==1], na.rm = T),
      HardHit = sum(hardhit == 1 & bbe == 1,na.rm = T),
      Barrels = sum(barrel == 1 & bbe == 1,na.rm = T),
      Sweetspot = sum(sweetspot == "SweetSpot" & bbe == 1,na.rm = T),
      LA = mean(Angle[bbe == 1], na.rm = T),
      Angle_Hard = mean(Angle[hardhit == 1 & bbe == 1], na.rm = T),
      Angle_Sweet = mean(Angle[sweetspot == "SweetSpot" & bbe == 1], na.rm = T),
      RunValue = sum(run_value, na.rm = T),
      # wOBA_val = sum(woba_value, na.rm = T),
      # TB = sum(total_bases, na.rm = T),
      # batted ball profile
      BU = sum(HitType == "Bunt" & PitchCall == 'InPlay',na.rm = T),
      GB = sum(HitType == "GroundBall" & PitchCall == 'InPlay',na.rm = T),
      FB = sum(HitType == "FlyBall" & PitchCall == 'InPlay',na.rm = T),
      LD = sum(HitType == "LineDrive" & PitchCall == 'InPlay',na.rm = T),
      PU = sum(HitType == "PopUp" & PitchCall == 'InPlay',na.rm = T),
      Pull = sum(pull, na.rm = T),
      Center = sum(center, na.rm = T),
      Oppo = sum(oppo, na.rm = T),
      FlyLinePull = sum(pull==1 & between(Angle,10,40), na.rm = T),
      # discipline
      Swings = sum(swing, na.rm =T),
      Whiffs = sum(whiff, na.rm =T),
      Zone = sum(in_zone, na.rm = T),
      oZone = sum(in_zone == 0, na.rm =T),
      ZoneSw = sum(swing == 1 & in_zone == 1,na.rm = T),
      ZoneCon = sum(contact == 1 & in_zone == 1,na.rm = T),
      ZoneWhiff = sum(whiff == 1 & in_zone == 1,na.rm = T),
      Chase = sum(in_zone == 0 & swing ==1, na.rm =T),
      ChaseCon = sum(contact == 1 & in_zone == 0,na.rm = T),
      ChaseWhiff = sum(whiff == 1 & in_zone == 0,na.rm = T),
      `1PSw` = sum(PitchofPA == 1 & swing ==1, na.rm =T),
    ) %>%
    ungroup() %>%
    mutate(AB = PA - BB - SO - HBP) %>%
    mutate(xBA = xBA_sum / AB,
           xBACON = xBA_sum / BBE,
    )%>%
    mutate(TB = (HR*4)+(X3B*3)+(X2B*2)+(X1B),
           SO_pct = SO / PA,
           BB_pct = BB / PA,
           BA = H / AB,
           OBP = (BB+HBP+H)/PA,
           SLG = TB / AB,
           OPS = OBP + SLG,
           wOBA = ((BB * weights$wBB[match(SEASON,weights$season)]) + (HBP * weights$wHBP[match(SEASON,weights$season)]) +
                     (X1B * weights$w1B[match(SEASON,weights$season)]) + (X2B * weights$w2B[match(SEASON,weights$season)]) + 
                     (X3B * weights$w3B[match(SEASON,weights$season)]) + (HR * weights$wHR[match(SEASON,weights$season)]) ) / PA,
           xwOBA = xwoba_sum / PA,
           wOBACON = ((X1B * weights$w1B[match(SEASON,weights$season)]) +  (X2B * weights$w2B[match(SEASON,weights$season)]) +
                        (X3B * weights$w3B[match(SEASON,weights$season)]) + (HR * weights$wHR[match(SEASON,weights$season)]) ) / BIP,
           xWOBACON = xwobacon_sum / BIP,
           BABIP = (H - HR) / (AB - HR - SO),
           xBA = xBA_sum / (BBE + SO),
           xSLG = xSLG_sum / (BBE + SO),
           xBACON = xBA_sum / BBE,
    )
  
  ## hitter career ----
  hitter_bb_career <- all_data %>%
    filter(Batter !='') %>%
    group_by(Batter, BatterId) %>%
    dplyr::summarise(
      BatterTeam = paste(unique(BatterTeam), collapse = ", "),
      # savant Batting
      G = n_distinct(GameID),
      P = n(),
      PA = sum(is_pa, na.rm = T),
      AB = sum(is_ab, na.rm = T),
      BIP = sum(PitchCall == "InPlay", na.rm = T),
      BBE = sum(bbe[TaggedHitType!='Bunt'],na.rm = T),
      # nBBE = sum(PitchCall == 'InPlay' & is.na(ExitSpeed)),
      H = sum(is_hit, na.rm = T),
      X1B = sum(PlayResult=='Single', na.rm = T),
      X2B = sum(PlayResult=='Double', na.rm = T),
      X3B = sum(PlayResult=='Triple', na.rm = T),
      HR = sum(PlayResult=='HomeRun', na.rm = T),
      SO = sum(grepl('Strikeout',PlayResult), na.rm = T) ,
      BB = sum(PlayResult=='Walk', na.rm = T),
      HBP = sum(PlayResult=='HitByPitch', na.rm = T),
      SAC = sum(grepl('Sac',PlayResult), na.rm = T),
      xBA_sum = sum(xBA, na.rm = T),
      # xBA = xBA_sum / (BBE+K),
      # xBACON = xBA_sum / BBE,
      xSLG_sum = sum(xSLG, na.rm = T),
      xwoba_sum = sum(xwOBA, na.rm = T),
      xwobacon_sum = sum(xwOBACON[bbe == 1], na.rm = T),
      # xSLG = xSLG_sum / (BBE+K),
      ExitVelo = mean(ExitSpeed[bbe == 1], na.rm = T),
      ExitVelo_max = max(ExitSpeed[bbe == 1], na.rm = T),
      Dist_avg = mean(Distance[bbe==1], na.rm = T),
      Dist_max = max(Distance[bbe==1], na.rm = T),
      HardHit = sum(hardhit == 1 & bbe == 1,na.rm = T),
      Barrels = sum(barrel == 1 & bbe == 1,na.rm = T),
      Sweetspot = sum(sweetspot == "SweetSpot" & bbe == 1,na.rm = T),
      LA = mean(Angle[bbe == 1], na.rm = T),
      Angle_Hard = mean(Angle[hardhit == 1 & bbe == 1], na.rm = T),
      Angle_Sweet = mean(Angle[sweetspot == "SweetSpot" & bbe == 1], na.rm = T),
      RunValue = sum(run_value, na.rm = T),
      # wOBA_val = sum(woba_value, na.rm = T),
      # TB = sum(total_bases, na.rm = T),
      # batted ball profile
      BU = sum(HitType == "Bunt" & PitchCall == 'InPlay',na.rm = T),
      GB = sum(HitType == "GroundBall" & PitchCall == 'InPlay',na.rm = T),
      FB = sum(HitType == "FlyBall" & PitchCall == 'InPlay',na.rm = T),
      LD = sum(HitType == "LineDrive" & PitchCall == 'InPlay',na.rm = T),
      PU = sum(HitType == "PopUp" & PitchCall == 'InPlay',na.rm = T),
      Pull = sum(pull, na.rm = T),
      Center = sum(center, na.rm = T),
      Oppo = sum(oppo, na.rm = T),
      FlyLinePull = sum(pull==1 & between(Angle,10,40), na.rm = T),
      Swings = sum(swing, na.rm =T),
      Whiffs = sum(whiff, na.rm =T),
      Zone = sum(in_zone, na.rm = T),
      oZone = sum(in_zone == 0, na.rm =T),
      ZoneSw = sum(swing == 1 & in_zone == 1,na.rm = T),
      ZoneCon = sum(contact == 1 & in_zone == 1,na.rm = T),
      ZoneWhiff = sum(whiff == 1 & in_zone == 1,na.rm = T),
      Chase = sum(in_zone == 0 & swing ==1, na.rm =T),
      ChaseCon = sum(contact == 1 & in_zone == 0,na.rm = T),
      ChaseWhiff = sum(whiff == 1 & in_zone == 0,na.rm = T),
      `1PSw` = sum(PitchofPA == 1 & swing ==1, na.rm =T),
    ) %>%
    ungroup()%>%
    mutate(AB = PA - BB - SO - HBP)%>%
    mutate(xBA = xBA_sum / AB,
           xBACON = xBA_sum / BBE,
    )%>%
    mutate(TB = (HR*4)+(X3B*3)+(X2B*2)+(X1B),
           SO_pct = SO / PA,
           BB_pct = BB / PA,
           BA = H / AB,
           OBP = (BB+HBP+H)/PA,
           SLG = TB / AB,
           OPS = OBP + SLG,
           wOBA = ((BB * weights$wBB[weights$season==2024]) + (HBP * weights$wHBP[weights$season==2024]) +
                     (X1B * weights$w1B[weights$season==2024]) + (X2B * weights$w2B[weights$season==2024]) + 
                     (X3B * weights$w3B[weights$season==2024]) + (HR * weights$wHR[weights$season==2024]) ) / PA,
           xwOBA = xwoba_sum / PA,
           wOBACON = ((X1B * weights$w1B[weights$season==2024]) +  (X2B * weights$w2B[weights$season==2024]) +
                        (X3B * weights$w3B[weights$season==2024]) + (HR * weights$wHR[weights$season==2024]) ) / BIP,
           xWOBACON = xwobacon_sum / BIP,
           BABIP = (H - HR) / (AB - HR - SO),
           xBA = xBA_sum / (BBE + SO),
           xSLG = xSLG_sum / (BBE + SO),
           xBACON = xBA_sum / BBE,
    )
  
  ## league ----
  lg_h_bb <- all_data %>%
    group_by(SEASON) %>%
    dplyr::summarise(
      Batter = "Team Totals",
      BatterTeam ="FRONTIER LEAGUE",
      
      # savant Batting
      G = n_distinct(GameID),
      P = n(),
      PA = sum(is_pa, na.rm = T),
      AB = sum(is_ab, na.rm = T),
      BIP = sum(PitchCall == "InPlay", na.rm = T),
      BBE = sum(bbe[TaggedHitType!='Bunt'],na.rm = T),
      # nBBE = sum(PitchCall == 'InPlay' & is.na(ExitSpeed)),
      H = sum(is_hit, na.rm = T),
      X1B = sum(PlayResult=='Single', na.rm = T),
      X2B = sum(PlayResult=='Double', na.rm = T),
      X3B = sum(PlayResult=='Triple', na.rm = T),
      HR = sum(PlayResult=='HomeRun', na.rm = T),
      SO = sum(grepl('Strikeout',PlayResult), na.rm = T) ,
      BB = sum(PlayResult=='Walk', na.rm = T),
      HBP = sum(PlayResult=='HitByPitch', na.rm = T),
      SAC = sum(grepl('Sac',PlayResult), na.rm = T),
      xBA_sum = sum(xBA, na.rm = T),
      # xBA = xBA_sum / (BBE+K),
      # xBACON = xBA_sum / BBE,
      xSLG_sum = sum(xSLG, na.rm = T),
      xwoba_sum = sum(xwOBA, na.rm = T),
      xwobacon_sum = sum(xwOBACON[bbe == 1], na.rm = T),
      # xSLG = xSLG_sum / (BBE+K),
      ExitVelo = mean(ExitSpeed[bbe == 1], na.rm = T),
      ExitVelo_max = max(ExitSpeed[bbe == 1], na.rm = T),
      Dist_avg = mean(Distance[bbe==1], na.rm = T),
      Dist_max = max(Distance[bbe==1], na.rm = T),
      HardHit = sum(hardhit == 1 & bbe == 1,na.rm = T),
      Barrels = sum(barrel == 1 & bbe == 1,na.rm = T),
      Sweetspot = sum(sweetspot == "SweetSpot" & bbe == 1,na.rm = T),
      LA = mean(Angle[bbe == 1], na.rm = T),
      Angle_Hard = mean(Angle[hardhit == 1 & bbe == 1], na.rm = T),
      Angle_Sweet = mean(Angle[sweetspot == "SweetSpot" & bbe == 1], na.rm = T),
      RunValue = sum(run_value, na.rm = T),
      # wOBA_val = sum(woba_value, na.rm = T),
      # TB = sum(total_bases, na.rm = T),
      # batted ball profile
      BU = sum(HitType == "Bunt" & PitchCall == 'InPlay',na.rm = T),
      GB = sum(HitType == "GroundBall" & PitchCall == 'InPlay',na.rm = T),
      FB = sum(HitType == "FlyBall" & PitchCall == 'InPlay',na.rm = T),
      LD = sum(HitType == "LineDrive" & PitchCall == 'InPlay',na.rm = T),
      PU = sum(HitType == "PopUp" & PitchCall == 'InPlay',na.rm = T),
      Pull = sum(pull, na.rm = T),
      Center = sum(center, na.rm = T),
      Oppo = sum(oppo, na.rm = T),
      FlyLinePull = sum(pull==1 & between(Angle,10,40), na.rm = T),
      # discipline
      Swings = sum(swing, na.rm =T),
      Whiffs = sum(whiff, na.rm =T),
      Zone = sum(in_zone, na.rm = T),
      oZone = sum(in_zone == 0, na.rm =T),
      ZoneSw = sum(swing == 1 & in_zone == 1,na.rm = T),
      ZoneCon = sum(contact == 1 & in_zone == 1,na.rm = T),
      ZoneWhiff = sum(whiff == 1 & in_zone == 1,na.rm = T),
      Chase = sum(in_zone == 0 & swing ==1, na.rm =T),
      ChaseCon = sum(contact == 1 & in_zone == 0,na.rm = T),
      ChaseWhiff = sum(whiff == 1 & in_zone == 0,na.rm = T),
      `1PSw` = sum(PitchofPA == 1 & swing ==1, na.rm =T),
    ) %>%
    ungroup() %>%
    # mutate(AB = PA - BB - SO - HBP - SAC) %>%
    mutate(xBA = xBA_sum / AB,
           xBACON = xBA_sum / BBE,
    )%>%
    mutate(TB = (HR*4)+(X3B*3)+(X2B*2)+(X1B),
           SO_pct = SO / PA,
           BB_pct = BB / PA,
           BA = H / AB,
           OBP = (BB+HBP+H)/PA,
           SLG = TB / AB,
           OPS = OBP + SLG,
           wOBA = ((BB * weights$wBB[match(SEASON,weights$season)]) + (HBP * weights$wHBP[match(SEASON,weights$season)]) +
                     (X1B * weights$w1B[match(SEASON,weights$season)]) + (X2B * weights$w2B[match(SEASON,weights$season)]) + 
                     (X3B * weights$w3B[match(SEASON,weights$season)]) + (HR * weights$wHR[match(SEASON,weights$season)]) ) / PA,
           xwOBA = xwoba_sum / PA,
           wOBACON = ((X1B * weights$w1B[match(SEASON,weights$season)]) +  (X2B * weights$w2B[match(SEASON,weights$season)]) +
                        (X3B * weights$w3B[match(SEASON,weights$season)]) + (HR * weights$wHR[match(SEASON,weights$season)]) ) / BIP,
           xWOBACON = xwobacon_sum / BIP,
           BABIP = (H - HR) / (AB - HR - SO),
           xBA = xBA_sum / (BBE + SO),
           xSLG = xSLG_sum / (BBE + SO),
           xBACON = xBA_sum / BBE,
    )
  
  ## hitter and league ====
  hitter_lg_bb <- plyr::rbind.fill(hitter_bb, lg_h_bb) %>%
    # select(-c(#nBBE,BB,HBP,K,
    #   xBA_sum,xSLG_sum)) %>%
    mutate(HardHit_pct = HardHit / BBE,
           Barrel_pct = Barrels / BBE,
           Sweetspot_pct = Sweetspot / BBE,
           RV100 = RunValue / P * 100,
           GB_pct = GB / BIP,
           FB_pct = FB / BIP,
           LD_pct = LD / BIP,
           PU_pct = PU / BIP,
           Pull_pct = Pull / BBE,
           Center_pct = Center / BBE,
           Oppo_pct = Oppo / BBE,
           AirPull_pct = FlyLinePull / BBE,
           Swing_pct = Swings / P,
           Whiff_pct = Whiffs / Swings,
           Contact_pct = 1 - Whiff_pct,
           Zone_pct = Zone / P,
           oZone_pct = oZone / P,
           ZoneSw_pct = ZoneSw / Zone,
           ZoneCon_pct = ZoneCon / ZoneSw,
           ZoneWhiff_pct = ZoneWhiff / ZoneSw,
           Chase_pct = Chase / oZone,
           ChaseCon_pct = ChaseCon / Chase,
           ChaseWhiff_pct = ChaseWhiff / Chase,
           `1PSw_pct` = `1PSw` / PA
    ) %>%
    select(-c(HardHit, Sweetspot, GB, FB, LD, PU, Swings, Whiffs, Zone, oZone, ZoneSw, 
              ZoneCon, ZoneWhiff, Chase, ChaseCon, ChaseWhiff, `1PSw`, xBA_sum,
              xSLG_sum, xwoba_sum, xwobacon_sum, Pull, Center, Oppo, FlyLinePull)) %>%
    mutate(across(c(HardHit_pct, Sweetspot_pct, GB_pct, FB_pct, LD_pct, PU_pct, Swing_pct, Whiff_pct, Barrel_pct, Contact_pct, Zone_pct, 
                    oZone_pct, ZoneSw_pct, ZoneCon_pct, ZoneWhiff_pct, Chase_pct, ChaseCon_pct, ChaseWhiff_pct, `1PSw_pct`, Pull_pct,
                    Oppo_pct, Center_pct, AirPull_pct),
                  ~ round(.,4) * 100),
           across(c(ExitVelo, ExitVelo_max, LA, Angle_Hard, Angle_Sweet), ~ round(., 2)),
           # across(c(xBA, xBACON, xSLG), ~ round(.,3))
    ) %>%
    relocate(TB:BABIP, .after = SAC) %>%
    mutate(across(3:ncol(.), ~ ifelse(is.infinite(.), NA, .)))
  
  
  league_h_final <- hitter_lg_bb %>%
    filter(BatterTeam == 'FRONTIER LEAGUE')
  
  
  
  ## hitter and league career ----
  hitter_career_lg_bb <- plyr::rbind.fill(hitter_bb_career, lg_h_bb) %>%
    # select(-c(#nBBE,BB,HBP,K,
    #   xBA_sum,xSLG_sum)) %>%
    mutate(HardHit_pct = HardHit / BBE,
           Barrel_pct = Barrels / BBE,
           Sweetspot_pct = Sweetspot / BBE,
           RV100 = RunValue / P * 100,
           GB_pct = GB / BIP,
           FB_pct = FB / BIP,
           LD_pct = LD / BIP,
           PU_pct = PU / BIP,
           Pull_pct = Pull / BBE,
           Center_pct = Center / BBE,
           Oppo_pct = Oppo / BBE,
           AirPull_pct = FlyLinePull / BBE,
           Swing_pct = Swings / P,
           Whiff_pct = Whiffs / Swings,
           Contact_pct = 1 - Whiff_pct,
           Zone_pct = Zone / P,
           oZone_pct = oZone / P,
           ZoneSw_pct = ZoneSw / Zone,
           ZoneCon_pct = ZoneCon / ZoneSw,
           ZoneWhiff_pct = ZoneWhiff / ZoneSw,
           Chase_pct = Chase / oZone,
           ChaseCon_pct = ChaseCon / Chase,
           ChaseWhiff_pct = ChaseWhiff / Chase,
           `1PSw_pct` = `1PSw` / PA
    ) %>%
    select(-c(HardHit, Sweetspot, GB, FB, LD, PU, Swings, Whiffs, Zone, oZone, ZoneSw, 
              ZoneCon, ZoneWhiff, Chase, ChaseCon, ChaseWhiff, `1PSw`, xBA_sum,
              xSLG_sum, xwoba_sum, xwobacon_sum, Pull, Center, Oppo, FlyLinePull)) %>%
    mutate(across(c(HardHit_pct, Sweetspot_pct, GB_pct, FB_pct, LD_pct, PU_pct, Swing_pct, Whiff_pct, Barrel_pct, Contact_pct, Zone_pct, 
                    oZone_pct, ZoneSw_pct, ZoneCon_pct, ZoneWhiff_pct, Chase_pct, ChaseCon_pct, ChaseWhiff_pct, `1PSw_pct`, Pull_pct,
                    Oppo_pct, Center_pct, AirPull_pct),
                  ~ round(.,4) * 100),
           across(c(ExitVelo, ExitVelo_max, LA, Angle_Hard, Angle_Sweet), ~ round(., 2)),
           # across(c(xBA, xBACON, xSLG), ~ round(.,3))
    )  %>%
    relocate(TB:BABIP, .after = SAC)%>%
    mutate(across(3:ncol(.), ~ ifelse(is.infinite(.), NA, .)))
  
  
  
  # PITCHING - PLAYER, TEAM, SEASON -----------------------------------------------------------------------------------------------
  ## pitcher data ----
  pitcher_bb <-  all_data %>%
    group_by(Pitcher, PitcherId, SEASON) %>%
    dplyr::summarise(
      PitcherTeam = paste(unique(PitcherTeam), collapse = ", "),
      # savant Batting
      G = n_distinct(GameID),
      P = n(),
      PA = sum(is_pa, na.rm = T),
      AB = sum(is_ab, na.rm = T),
      BIP = sum(PitchCall == "InPlay", na.rm = T),
      BBE = sum(bbe[TaggedHitType!='Bunt'],na.rm = T),
      # nBBE = sum(PitchCall == 'InPlay' & is.na(ExitSpeed)),
      H = sum(is_hit, na.rm = T),
      X1B = sum(PlayResult=='Single', na.rm = T),
      X2B = sum(PlayResult=='Double', na.rm = T),
      X3B = sum(PlayResult=='Triple', na.rm = T),
      HR = sum(PlayResult=='HomeRun', na.rm = T),
      SO = sum(grepl('Strikeout',PlayResult), na.rm = T) ,
      BB = sum(PlayResult=='Walk', na.rm = T),
      HBP = sum(PlayResult=='HitByPitch', na.rm = T),
      SAC = sum(grepl('Sac',PlayResult), na.rm = T),
      xBA_sum = sum(xBA, na.rm = T),
      # xBA = xBA_sum / (BBE+K),
      # xBACON = xBA_sum / BBE,
      xSLG_sum = sum(xSLG, na.rm = T),
      xwoba_sum = sum(xwOBA, na.rm = T),
      xwobacon_sum = sum(xwOBACON[bbe == 1], na.rm = T),
      # xSLG = xSLG_sum / (BBE+K),
      ExitVelo = mean(ExitSpeed[bbe == 1], na.rm = T),
      ExitVelo_max = max(ExitSpeed[bbe == 1], na.rm = T),
      Dist_avg = mean(Distance[bbe==1], na.rm = T),
      Dist_max = max(Distance[bbe==1], na.rm = T),
      HardHit = sum(hardhit == 1 & bbe == 1,na.rm = T),
      Barrels = sum(barrel == 1 & bbe == 1,na.rm = T),
      Sweetspot = sum(sweetspot == "SweetSpot" & bbe == 1,na.rm = T),
      LA = mean(Angle[bbe == 1], na.rm = T),
      Angle_Hard = mean(Angle[hardhit == 1 & bbe == 1], na.rm = T),
      Angle_Sweet = mean(Angle[sweetspot == "SweetSpot" & bbe == 1], na.rm = T),
      RunValue = sum(run_value, na.rm = T),
      # wOBA_val = sum(woba_value, na.rm = T),
      # TB = sum(total_bases, na.rm = T),
      # batted ball profile
      BU = sum(HitType == "Bunt" & PitchCall == 'InPlay',na.rm = T),
      GB = sum(HitType == "GroundBall" & PitchCall == 'InPlay',na.rm = T),
      FB = sum(HitType == "FlyBall" & PitchCall == 'InPlay',na.rm = T),
      LD = sum(HitType == "LineDrive" & PitchCall == 'InPlay',na.rm = T),
      PU = sum(HitType == "PopUp" & PitchCall == 'InPlay',na.rm = T),
      # Pull,
      # Straight,
      # Oppo,
      
      # discipline
      Swings = sum(swing, na.rm =T),
      Whiffs = sum(whiff, na.rm =T),
      Zone = sum(in_zone, na.rm = T),
      oZone = sum(in_zone == 0, na.rm =T),
      ZoneSw = sum(swing == 1 & in_zone == 1,na.rm = T),
      ZoneCon = sum(PitchCall == "InPlay" & in_zone == 1,na.rm = T),
      ZoneWhiff = sum(whiff == 1 & in_zone == 1,na.rm = T),
      Chase = sum(in_zone == 0 & swing ==1, na.rm =T),
      ChaseCon = sum(PitchCall == "InPlay" & in_zone == 0,na.rm = T),
      ChaseWhiff = sum(whiff == 1 & in_zone == 0,na.rm = T),
      `1PSw` = sum(PitchofPA == 1 & swing ==1, na.rm =T),
    ) %>%
    ungroup()  %>%
    mutate(AB = PA - BB - SO - HBP) %>%
    mutate(xBA = xBA_sum / AB,
           xBACON = xBA_sum / BBE,
    )%>%
    mutate(TB = (HR*4)+(X3B*3)+(X2B*2)+(X1B),
           SO_pct = SO / PA,
           BB_pct = BB / PA,
           BA = H / AB,
           OBP = (BB+HBP+H)/PA,
           SLG = TB / AB,
           OPS = OBP + SLG,
           wOBA = ((BB * weights$wBB[match(SEASON,weights$season)]) + (HBP * weights$wHBP[match(SEASON,weights$season)]) +
                     (X1B * weights$w1B[match(SEASON,weights$season)]) + (X2B * weights$w2B[match(SEASON,weights$season)]) + 
                     (X3B * weights$w3B[match(SEASON,weights$season)]) + (HR * weights$wHR[match(SEASON,weights$season)]) ) / PA,
           xwOBA = xwoba_sum / PA,
           wOBACON = ((X1B * weights$w1B[match(SEASON,weights$season)]) +  (X2B * weights$w2B[match(SEASON,weights$season)]) +
                        (X3B * weights$w3B[match(SEASON,weights$season)]) + (HR * weights$wHR[match(SEASON,weights$season)]) ) / BIP,
           xWOBACON = xwobacon_sum / BIP,
           BABIP = (H - HR) / (AB - HR - SO),
           xBA = xBA_sum / (BBE + SO),
           xSLG = xSLG_sum / (BBE + SO),
           xBACON = xBA_sum / BBE,
    )
  
  ## pitcher career ----
  pitcher_bb_career <- all_data %>%
    filter(Pitcher !='') %>%
    group_by(Pitcher, PitcherId, PitcherTeam, SEASON) %>%
    dplyr::summarise(
      PitcherTeam = paste(unique(PitcherTeam), collapse = ', '),
      SEASON = paste(unique(SEASON), collapse = ', '),
      
      # savant Batting
      G = n_distinct(GameID),
      P = n(),
      PA = sum(is_pa, na.rm = T),
      AB = sum(is_ab, na.rm = T),
      BIP = sum(PitchCall == "InPlay", na.rm = T),
      BBE = sum(bbe[TaggedHitType!='Bunt'],na.rm = T),
      # nBBE = sum(PitchCall == 'InPlay' & is.na(ExitSpeed)),
      H = sum(is_hit, na.rm = T),
      X1B = sum(PlayResult=='Single', na.rm = T),
      X2B = sum(PlayResult=='Double', na.rm = T),
      X3B = sum(PlayResult=='Triple', na.rm = T),
      HR = sum(PlayResult=='HomeRun', na.rm = T),
      SO = sum(grepl('Strikeout',PlayResult), na.rm = T) ,
      BB = sum(PlayResult=='Walk', na.rm = T),
      HBP = sum(PlayResult=='HitByPitch', na.rm = T),
      SAC = sum(grepl('Sac',PlayResult), na.rm = T),
      xBA_sum = sum(xBA, na.rm = T),
      # xBA = xBA_sum / (BBE+K),
      # xBACON = xBA_sum / BBE,
      xSLG_sum = sum(xSLG, na.rm = T),
      xwoba_sum = sum(xwOBA, na.rm = T),
      xwobacon_sum = sum(xwOBACON[bbe == 1], na.rm = T),
      # xSLG = xSLG_sum / (BBE+K),
      ExitVelo = mean(ExitSpeed[bbe == 1], na.rm = T),
      ExitVelo_max = max(ExitSpeed[bbe == 1], na.rm = T),
      Dist_avg = mean(Distance[bbe==1], na.rm = T),
      Dist_max = max(Distance[bbe==1], na.rm = T),
      HardHit = sum(hardhit == 1 & bbe == 1,na.rm = T),
      Barrels = sum(barrel == 1 & bbe == 1,na.rm = T),
      Sweetspot = sum(sweetspot == "SweetSpot" & bbe == 1,na.rm = T),
      LA = mean(Angle[bbe == 1], na.rm = T),
      Angle_Hard = mean(Angle[hardhit == 1 & bbe == 1], na.rm = T),
      Angle_Sweet = mean(Angle[sweetspot == "SweetSpot" & bbe == 1], na.rm = T),
      RunValue = sum(run_value, na.rm = T),
      # wOBA_val = sum(woba_value, na.rm = T),
      # TB = sum(total_bases, na.rm = T),
      # batted ball profile
      BU = sum(HitType == "Bunt" & PitchCall == 'InPlay',na.rm = T),
      GB = sum(HitType == "GroundBall" & PitchCall == 'InPlay',na.rm = T),
      FB = sum(HitType == "FlyBall" & PitchCall == 'InPlay',na.rm = T),
      LD = sum(HitType == "LineDrive" & PitchCall == 'InPlay',na.rm = T),
      PU = sum(HitType == "PopUp" & PitchCall == 'InPlay',na.rm = T),
      # Pull,
      # Straight,
      # Oppo,
      
      # discipline
      Swings = sum(swing, na.rm =T),
      Whiffs = sum(whiff, na.rm =T),
      Zone = sum(in_zone, na.rm = T),
      oZone = sum(in_zone == 0, na.rm =T),
      ZoneSw = sum(swing == 1 & in_zone == 1,na.rm = T),
      ZoneCon = sum(PitchCall == "InPlay" & in_zone == 1,na.rm = T),
      ZoneWhiff = sum(whiff == 1 & in_zone == 1,na.rm = T),
      Chase = sum(in_zone == 0 & swing ==1, na.rm =T),
      ChaseCon = sum(PitchCall == "InPlay" & in_zone == 0,na.rm = T),
      ChaseWhiff = sum(whiff == 1 & in_zone == 0,na.rm = T),
      `1PSw` = sum(PitchofPA == 1 & swing ==1, na.rm =T),
    ) %>%
    ungroup()%>%
    mutate(AB = PA - BB - SO - HBP)%>%
    mutate(xBA = xBA_sum / AB,
           xBACON = xBA_sum / BBE,
    )%>%
    mutate(TB = (HR*4)+(X3B*3)+(X2B*2)+(X1B),
           SO_pct = SO / PA,
           BB_pct = BB / PA,
           BA = H / AB,
           OBP = (BB+HBP+H)/PA,
           SLG = TB / AB,
           OPS = OBP + SLG,
           wOBA = ((BB * weights$wBB[weights$season==2024]) + (HBP * weights$wHBP[weights$season==2024]) +
                     (X1B * weights$w1B[weights$season==2024]) + (X2B * weights$w2B[weights$season==2024]) + 
                     (X3B * weights$w3B[weights$season==2024]) + (HR * weights$wHR[weights$season==2024]) ) / PA,
           xwOBA = xwoba_sum / PA,
           wOBACON = ((X1B * weights$w1B[weights$season==2024]) +  (X2B * weights$w2B[weights$season==2024]) +
                        (X3B * weights$w3B[weights$season==2024]) + (HR * weights$wHR[weights$season==2024]) ) / BIP,
           xWOBACON = xwobacon_sum / BIP,
           BABIP = (H - HR) / (AB - HR - SO),
           xBA = xBA_sum / (BBE + SO),
           xSLG = xSLG_sum / (BBE + SO),
           xBACON = xBA_sum / BBE,
    )
  
  ## league ----
  p_lg_bb <-  all_data %>%
    group_by(SEASON) %>%
    dplyr::summarise(
      Pitcher = "Team Totals",
      PitcherTeam ="FRONTIER LEAGUE",
      
      # savant Batting
      G = n_distinct(GameID),
      P = n(),
      PA = sum(is_pa, na.rm = T),
      AB = sum(is_ab, na.rm = T),
      BIP = sum(PitchCall == "InPlay", na.rm = T),
      BBE = sum(bbe[TaggedHitType!='Bunt'],na.rm = T),
      # nBBE = sum(PitchCall == 'InPlay' & is.na(ExitSpeed)),
      H = sum(is_hit, na.rm = T),
      X1B = sum(PlayResult=='Single', na.rm = T),
      X2B = sum(PlayResult=='Double', na.rm = T),
      X3B = sum(PlayResult=='Triple', na.rm = T),
      HR = sum(PlayResult=='HomeRun', na.rm = T),
      SO = sum(grepl('Strikeout',PlayResult), na.rm = T) ,
      BB = sum(PlayResult=='Walk', na.rm = T),
      HBP = sum(PlayResult=='HitByPitch', na.rm = T),
      SAC = sum(grepl('Sac',PlayResult), na.rm = T),
      xBA_sum = sum(xBA, na.rm = T),
      # xBA = xBA_sum / (BBE+K),
      # xBACON = xBA_sum / BBE,
      xSLG_sum = sum(xSLG, na.rm = T),
      xwoba_sum = sum(xwOBA, na.rm = T),
      xwobacon_sum = sum(xwOBACON[bbe == 1], na.rm = T),
      # xSLG = xSLG_sum / (BBE+K),
      ExitVelo = mean(ExitSpeed[bbe == 1], na.rm = T),
      ExitVelo_max = max(ExitSpeed[bbe == 1], na.rm = T),
      Dist_avg = mean(Distance[bbe==1], na.rm = T),
      Dist_max = max(Distance[bbe==1], na.rm = T),
      HardHit = sum(hardhit == 1 & bbe == 1,na.rm = T),
      Barrels = sum(barrel == 1 & bbe == 1,na.rm = T),
      Sweetspot = sum(sweetspot == "SweetSpot" & bbe == 1,na.rm = T),
      LA = mean(Angle[bbe == 1], na.rm = T),
      Angle_Hard = mean(Angle[hardhit == 1 & bbe == 1], na.rm = T),
      Angle_Sweet = mean(Angle[sweetspot == "SweetSpot" & bbe == 1], na.rm = T),
      RunValue = sum(run_value, na.rm = T),
      # wOBA_val = sum(woba_value, na.rm = T),
      # TB = sum(total_bases, na.rm = T),
      # batted ball profile
      BU = sum(HitType == "Bunt" & PitchCall == 'InPlay',na.rm = T),
      GB = sum(HitType == "GroundBall" & PitchCall == 'InPlay',na.rm = T),
      FB = sum(HitType == "FlyBall" & PitchCall == 'InPlay',na.rm = T),
      LD = sum(HitType == "LineDrive" & PitchCall == 'InPlay',na.rm = T),
      PU = sum(HitType == "PopUp" & PitchCall == 'InPlay',na.rm = T),
      # Pull,
      # Straight,
      # Oppo,
      
      # discipline
      Swings = sum(swing, na.rm =T),
      Whiffs = sum(whiff, na.rm =T),
      Zone = sum(in_zone, na.rm = T),
      oZone = sum(in_zone == 0, na.rm =T),
      ZoneSw = sum(swing == 1 & in_zone == 1,na.rm = T),
      ZoneCon = sum(PitchCall == "InPlay" & in_zone == 1,na.rm = T),
      ZoneWhiff = sum(whiff == 1 & in_zone == 1,na.rm = T),
      Chase = sum(in_zone == 0 & swing ==1, na.rm =T),
      ChaseCon = sum(PitchCall == "InPlay" & in_zone == 0,na.rm = T),
      ChaseWhiff = sum(whiff == 1 & in_zone == 0,na.rm = T),
      `1PSw` = sum(PitchofPA == 1 & swing ==1, na.rm =T),
    ) %>%
    ungroup() %>%
    # mutate(AB = PA - BB - SO - HBP - SAC) %>%
    mutate(xBA = xBA_sum / AB,
           xBACON = xBA_sum / BBE,
    )%>%
    mutate(TB = (HR*4)+(X3B*3)+(X2B*2)+(X1B),
           SO_pct = SO / PA,
           BB_pct = BB / PA,
           BA = H / AB,
           OBP = (BB+HBP+H)/PA,
           SLG = TB / AB,
           OPS = OBP + SLG,
           wOBA = ((BB * weights$wBB[match(SEASON,weights$season)]) + (HBP * weights$wHBP[match(SEASON,weights$season)]) +
                     (X1B * weights$w1B[match(SEASON,weights$season)]) + (X2B * weights$w2B[match(SEASON,weights$season)]) + 
                     (X3B * weights$w3B[match(SEASON,weights$season)]) + (HR * weights$wHR[match(SEASON,weights$season)]) ) / PA,
           xwOBA = xwoba_sum / PA,
           wOBACON = ((X1B * weights$w1B[match(SEASON,weights$season)]) +  (X2B * weights$w2B[match(SEASON,weights$season)]) +
                        (X3B * weights$w3B[match(SEASON,weights$season)]) + (HR * weights$wHR[match(SEASON,weights$season)]) ) / BIP,
           xWOBACON = xwobacon_sum / BIP,
           BABIP = (H - HR) / (AB - HR - SO),
           xBA = xBA_sum / (BBE + SO),
           xSLG = xSLG_sum / (BBE + SO),
           xBACON = xBA_sum / BBE,
    )
  
  ## pitcher and league ----
  
  pitcher_lg_bb <- plyr::rbind.fill(pitcher_bb, p_lg_bb) %>%
    # select(-c(#nBBE,BB,HBP,K,
    #   xBA_sum,xSLG_sum)) %>%
    mutate(HardHit_pct = HardHit / BBE,
           Barrel_pct = Barrels / BBE,
           Sweetspot_pct = Sweetspot / BBE,
           RV100 = RunValue / P * 100,
           GB_pct = GB / BIP,
           FB_pct = FB / BIP,
           LD_pct = LD / BIP,
           PU_pct = PU / BIP,
           Swing_pct = Swings / P,
           Whiff_pct = Whiffs / Swings,
           Contact_pct = 1 - Whiff_pct,
           Zone_pct = Zone / P,
           oZone_pct = oZone / P,
           ZoneSw_pct = ZoneSw / Zone,
           ZoneCon_pct = ZoneCon / ZoneSw,
           ZoneWhiff_pct = ZoneWhiff / ZoneSw,
           Chase_pct = Chase / oZone,
           ChaseCon_pct = ChaseCon / Chase,
           ChaseWhiff_pct = ChaseWhiff / Chase,
           `1PSw_pct` = `1PSw` / PA
    ) %>%
    select(-c(HardHit, Sweetspot, GB, FB, LD, PU, Swings, Whiffs, Zone, oZone, ZoneSw, 
              ZoneCon, ZoneWhiff, Chase, ChaseCon, ChaseWhiff, `1PSw`, xBA_sum,
              xSLG_sum, xwoba_sum, xwobacon_sum)) %>%
    mutate(across(c(HardHit_pct, Sweetspot_pct, GB_pct, FB_pct, LD_pct, PU_pct, Swing_pct, Whiff_pct,Barrel_pct, Contact_pct, Zone_pct, 
                    oZone_pct, ZoneSw_pct, ZoneCon_pct, ZoneWhiff_pct, Chase_pct, ChaseCon_pct, ChaseWhiff_pct, `1PSw_pct`),
                  ~ round(.,4) * 100),
           across(c(ExitVelo, ExitVelo_max, LA, Angle_Hard, Angle_Sweet), ~ round(., 2)),
           # across(c(xBA, xBACON, xSLG), ~ round(.,3))
    ) %>%
    relocate(TB:BABIP, .after = SAC)
  
  league_p_final <- pitcher_lg_bb %>%
    filter(PitcherTeam == 'FRONTIER LEAGUE')
  
  
  ## pitcher and league career ----
  pitcher_career_lg_bb <- plyr::rbind.fill(pitcher_bb_career, p_lg_bb) %>%
    # select(-c(#nBBE,BB,HBP,K,
    #   xBA_sum,xSLG_sum)) %>%
    mutate(HardHit_pct = HardHit / BBE,
           Barrel_pct = Barrels / BBE,
           Sweetspot_pct = Sweetspot / BBE,
           RV100 = RunValue / P * 100,
           GB_pct = GB / BIP,
           FB_pct = FB / BIP,
           LD_pct = LD / BIP,
           PU_pct = PU / BIP,
           Swing_pct = Swings / P,
           Whiff_pct = Whiffs / Swings,
           Contact_pct = 1 - Whiff_pct,
           Zone_pct = Zone / P,
           oZone_pct = oZone / P,
           ZoneSw_pct = ZoneSw / Zone,
           ZoneCon_pct = ZoneCon / ZoneSw,
           ZoneWhiff_pct = ZoneWhiff / ZoneSw,
           Chase_pct = Chase / oZone,
           ChaseCon_pct = ChaseCon / Chase,
           ChaseWhiff_pct = ChaseWhiff / Chase,
           `1PSw_pct` = `1PSw` / PA
    ) %>%
    select(-c(HardHit, Sweetspot, GB, FB, LD, PU, Swings, Whiffs, Zone, oZone, ZoneSw, 
              ZoneCon, ZoneWhiff, Chase, ChaseCon, ChaseWhiff, `1PSw`, xBA_sum,
              xSLG_sum, xwoba_sum, xwobacon_sum)) %>%
    mutate(across(c(HardHit_pct, Sweetspot_pct, GB_pct, FB_pct, LD_pct, PU_pct, Swing_pct, Whiff_pct, Barrel_pct, Contact_pct, Zone_pct, 
                    oZone_pct, ZoneSw_pct, ZoneCon_pct, ZoneWhiff_pct, Chase_pct, ChaseCon_pct, ChaseWhiff_pct, `1PSw_pct`),
                  ~ round(.,4) * 100),
           across(c(ExitVelo, ExitVelo_max, LA, Angle_Hard, Angle_Sweet), ~ round(., 2)),
           # across(c(xBA, xBACON, xSLG), ~ round(.,3))
    ) %>%
    relocate(TB:BABIP, .after = SAC)
  
  }

dbWriteTable(conn = db, "stats_hitting_player_batted_ball", value = hitter_lg_bb, overwrite = T)
dbWriteTable(conn = db, "stats_hitting_player_batted_ball_career", value = hitter_career_lg_bb, overwrite = T)
dbWriteTable(conn = db, "stats_hitting_league_batted_ball", value = league_h_final, overwrite = T)

dbWriteTable(conn = db, "stats_pitching_player_batted_ball", value = pitcher_lg_bb, overwrite = T)
dbWriteTable(conn = db, "stats_pitching_player_batted_ball_career", value = pitcher_lg_bb, overwrite = T)
dbWriteTable(conn = db, "stats_pitching_league_batted_ball", value = league_p_final, overwrite = T)

slackr_msg(txt = paste("Trackman player batted ball stats tables created.") , 
           channel = "#flash",
           username = paste("FLASHBot",emoji('robot'))
)
Sys.sleep(5)
{ # HITTING TEAM SEASON ----
  team_h_bb <- all_data %>%
    group_by(BatterTeam, SEASON) %>%
    dplyr::summarise(
      # savant Batting
      G = n_distinct(GameID),
      P = n(),
      PA = sum(is_pa, na.rm = T),
      AB = sum(is_ab, na.rm = T),
      BIP = sum(PitchCall == "InPlay", na.rm = T),
      BBE = sum(bbe[TaggedHitType!='Bunt'],na.rm = T),
      # nBBE = sum(PitchCall == 'InPlay' & is.na(ExitSpeed)),
      H = sum(is_hit, na.rm = T),
      X1B = sum(PlayResult=='Single', na.rm = T),
      X2B = sum(PlayResult=='Double', na.rm = T),
      X3B = sum(PlayResult=='Triple', na.rm = T),
      HR = sum(PlayResult=='HomeRun', na.rm = T),
      SO = sum(grepl('Strikeout',PlayResult), na.rm = T) ,
      BB = sum(PlayResult=='Walk', na.rm = T),
      HBP = sum(PlayResult=='HitByPitch', na.rm = T),
      SAC = sum(grepl('Sac',PlayResult), na.rm = T),
      xBA_sum = sum(xBA, na.rm = T),
      # xBA = xBA_sum / (BBE+K),
      # xBACON = xBA_sum / BBE,
      xSLG_sum = sum(xSLG, na.rm = T),
      xwoba_sum = sum(xwOBA, na.rm = T),
      xwobacon_sum = sum(xwOBACON[bbe == 1], na.rm = T),
      # xSLG = xSLG_sum / (BBE+K),
      ExitVelo = mean(ExitSpeed[bbe == 1], na.rm = T),
      ExitVelo_max = max(ExitSpeed[bbe == 1], na.rm = T),
      Dist_avg = mean(Distance[bbe==1], na.rm = T),
      Dist_max = max(Distance[bbe==1], na.rm = T),
      HardHit = sum(hardhit == 1 & bbe == 1,na.rm = T),
      Barrels = sum(barrel == 1 & bbe == 1,na.rm = T),
      Sweetspot = sum(sweetspot == "SweetSpot" & bbe == 1,na.rm = T),
      LA = mean(Angle[bbe == 1], na.rm = T),
      Angle_Hard = mean(Angle[hardhit == 1 & bbe == 1], na.rm = T),
      Angle_Sweet = mean(Angle[sweetspot == "SweetSpot" & bbe == 1], na.rm = T),
      RunValue = sum(run_value, na.rm = T),
      # wOBA_val = sum(woba_value, na.rm = T),
      # TB = sum(total_bases, na.rm = T),
      # batted ball profile
      BU = sum(HitType == "Bunt" & PitchCall == 'InPlay',na.rm = T),
      GB = sum(HitType == "GroundBall" & PitchCall == 'InPlay',na.rm = T),
      FB = sum(HitType == "FlyBall" & PitchCall == 'InPlay',na.rm = T),
      LD = sum(HitType == "LineDrive" & PitchCall == 'InPlay',na.rm = T),
      PU = sum(HitType == "PopUp" & PitchCall == 'InPlay',na.rm = T),
      Pull = sum(pull, na.rm = T),
      Center = sum(center, na.rm = T),
      Oppo = sum(oppo, na.rm = T),
      FlyLinePull = sum(pull==1 & between(Angle,10,40), na.rm = T),
      # discipline
      Swings = sum(swing, na.rm =T),
      Whiffs = sum(whiff, na.rm =T),
      Zone = sum(in_zone, na.rm = T),
      oZone = sum(in_zone == 0, na.rm =T),
      ZoneSw = sum(swing == 1 & in_zone == 1,na.rm = T),
      ZoneCon = sum(PitchCall == "InPlay" & in_zone == 1,na.rm = T),
      ZoneWhiff = sum(whiff == 1 & in_zone == 1,na.rm = T),
      Chase = sum(in_zone == 0 & swing ==1, na.rm =T),
      ChaseCon = sum(PitchCall == "InPlay" & in_zone == 0,na.rm = T),
      ChaseWhiff = sum(whiff == 1 & in_zone == 0,na.rm = T),
      `1PSw` = sum(PitchofPA == 1 & swing ==1, na.rm =T),
    ) %>%
    ungroup() %>%
    mutate(AB = PA - BB - SO - HBP) %>%
    mutate(xBA = xBA_sum / AB,
           xBACON = xBA_sum / BBE,
    )%>%
    mutate(TB = (HR*4)+(X3B*3)+(X2B*2)+(X1B),
           SO_pct = SO / PA,
           BB_pct = BB / PA,
           BA = H / AB,
           OBP = (BB+HBP+H)/PA,
           SLG = TB / AB,
           OPS = OBP + SLG,
           wOBA = ((BB * weights$wBB[match(SEASON,weights$season)]) + (HBP * weights$wHBP[match(SEASON,weights$season)]) +
                     (X1B * weights$w1B[match(SEASON,weights$season)]) + (X2B * weights$w2B[match(SEASON,weights$season)]) + 
                     (X3B * weights$w3B[match(SEASON,weights$season)]) + (HR * weights$wHR[match(SEASON,weights$season)]) ) / PA,
           xwOBA = xwoba_sum / PA,
           wOBACON = ((X1B * weights$w1B[match(SEASON,weights$season)]) +  (X2B * weights$w2B[match(SEASON,weights$season)]) +
                        (X3B * weights$w3B[match(SEASON,weights$season)]) + (HR * weights$wHR[match(SEASON,weights$season)]) ) / BIP,
           xWOBACON = xwobacon_sum / BIP,
           BABIP = (H - HR) / (AB - HR - SO),
           xBA = xBA_sum / (BBE + SO),
           xSLG = xSLG_sum / (BBE + SO),
           xBACON = xBA_sum / BBE,
    ) %>%mutate(HardHit_pct = HardHit / BBE,
                Barrel_pct = Barrels / BBE,
                Sweetspot_pct = Sweetspot / BBE,
                RV100 = RunValue / P * 100,
                GB_pct = GB / BIP,
                FB_pct = FB / BIP,
                LD_pct = LD / BIP,
                PU_pct = PU / BIP,
                Pull_pct = Pull / BBE,
                Center_pct = Center / BBE,
                Oppo_pct = Oppo / BBE,
                AirPull_pct = FlyLinePull / BBE,
                Swing_pct = Swings / P,
                Whiff_pct = Whiffs / Swings,
                Contact_pct = 1 - Whiff_pct,
                Zone_pct = Zone / P,
                oZone_pct = oZone / P,
                ZoneSw_pct = ZoneSw / Zone,
                ZoneCon_pct = ZoneCon / ZoneSw,
                ZoneWhiff_pct = ZoneWhiff / ZoneSw,
                Chase_pct = Chase / oZone,
                ChaseCon_pct = ChaseCon / Chase,
                ChaseWhiff_pct = ChaseWhiff / Chase,
                `1PSw_pct` = `1PSw` / PA
    ) %>%
    select(-c(HardHit, Sweetspot, GB, FB, LD, PU, Swings, Whiffs, Zone, oZone, ZoneSw, 
              ZoneCon, ZoneWhiff, Chase, ChaseCon, ChaseWhiff, `1PSw`, xBA_sum,
              xSLG_sum, xwoba_sum, xwobacon_sum, Pull, Center, Oppo, FlyLinePull)) %>%
    mutate(across(c(HardHit_pct, Sweetspot_pct, GB_pct, FB_pct, LD_pct, PU_pct, Swing_pct, Whiff_pct, Barrel_pct, Contact_pct, Zone_pct, 
                    oZone_pct, ZoneSw_pct, ZoneCon_pct, ZoneWhiff_pct, Chase_pct, ChaseCon_pct, ChaseWhiff_pct, `1PSw_pct`, Pull_pct,
                    Oppo_pct, Center_pct, AirPull_pct),
                  ~ round(.,4) * 100),
           across(c(ExitVelo, ExitVelo_max, LA, Angle_Hard, Angle_Sweet), ~ round(., 2)),
           # across(c(xBA, xBACON, xSLG), ~ round(.,3))
    )  %>%
    relocate(TB:BABIP, .after = SAC)
  
  
  # %>%
  #   rbind(lg_h_bb %>% select(-Batter)%>% mutate(BatterTeam='FRONTIER LEAGUE'))%>%
  #   select(-c(nBBE,BB,HBP,K,xBA_sum,xSLG_sum)) %>%
  #   mutate(HardHit_pct = HardHit / BBE,
  #          Sweetspot_pct = Sweetspot / BBE,
  #          GB_pct = GB / BIP,
  #          FB_pct = FB / BIP,
  #          LD_pct = LD / BIP,
  #          PU_pct = PU / BIP,
  #          Swing_pct = Swings / P,
  #          Whiff_pct = Whiffs / Swings,
  #          Zone_pct = Zone / P,
  #          oZone_pct = oZone / P,
  #          ZoneSw_pct = ZoneSw / Zone,
  #          ZoneCon_pct = ZoneCon / Zone,
  #          ZoneWhiff_pct = ZoneWhiff / ZoneSw,
  #          Chase_pct = Chase / oZone,
  #          ChaseCon_pct = ChaseCon / Chase,
  #          ChaseWhiff_pct = ChaseWhiff / Chase,
  #          `1PSw_pct` = `1PSw` / PA
  #   ) %>%
  #   select(-c(HardHit, Sweetspot, GB, FB, LD, PU, Swings, Whiffs, Zone, oZone, ZoneSw, ZoneCon, ZoneWhiff, Chase, ChaseCon, ChaseWhiff, `1PSw`)) %>%
  #   mutate(across(c(HardHit_pct, Sweetspot_pct, GB_pct, FB_pct, LD_pct, PU_pct, Swing_pct, Whiff_pct, Zone_pct, 
  #                   oZone_pct, ZoneSw_pct, ZoneCon_pct, ZoneWhiff_pct, Chase_pct, ChaseCon_pct, ChaseWhiff_pct, `1PSw_pct`),
  #                 ~ round(.,3) * 100),
  #          across(c(ExitVelo, ExitVelo_max, LA, Angle_Hard, Angle_Sweet), ~ round(., 1)),
  #          across(c(xBA, xBACON, xSLG), ~ round(.,3))
  #   )
  # PITCHING TEAM SEASON----
  team_p_bb <- all_data %>%
    group_by(PitcherTeam, SEASON) %>%
    dplyr::summarise(
      # savant Batting
      # savant Batting
      G = n_distinct(GameID),
      P = n(),
      PA = sum(is_pa, na.rm = T),
      AB = sum(is_ab, na.rm = T),
      BIP = sum(PitchCall == "InPlay", na.rm = T),
      BBE = sum(bbe[TaggedHitType!='Bunt'],na.rm = T),
      # nBBE = sum(PitchCall == 'InPlay' & is.na(ExitSpeed)),
      H = sum(is_hit, na.rm = T),
      X1B = sum(PlayResult=='Single', na.rm = T),
      X2B = sum(PlayResult=='Double', na.rm = T),
      X3B = sum(PlayResult=='Triple', na.rm = T),
      HR = sum(PlayResult=='HomeRun', na.rm = T),
      SO = sum(grepl('Strikeout',PlayResult), na.rm = T) ,
      BB = sum(PlayResult=='Walk', na.rm = T),
      HBP = sum(PlayResult=='HitByPitch', na.rm = T),
      SAC = sum(grepl('Sac',PlayResult), na.rm = T),
      xBA_sum = sum(xBA, na.rm = T),
      # xBA = xBA_sum / (BBE+K),
      # xBACON = xBA_sum / BBE,
      xSLG_sum = sum(xSLG, na.rm = T),
      xwoba_sum = sum(xwOBA, na.rm = T),
      xwobacon_sum = sum(xwOBACON[PitchCall == 'InPlay'], na.rm = T),
      # xSLG = xSLG_sum / (BBE+K),
      ExitVelo = mean(ExitSpeed[PitchCall == 'InPlay'], na.rm = T),
      ExitVelo_max = max(ExitSpeed[PitchCall == 'InPlay'], na.rm = T),
      HardHit = sum(hardhit == 1 & PitchCall == 'InPlay',na.rm = T),
      Sweetspot = sum(sweetspot == "SweetSpot",na.rm = T),
      LA = mean(Angle[PitchCall == 'InPlay'], na.rm = T),
      Angle_Hard = mean(Angle[hardhit == 1 & PitchCall == 'InPlay'], na.rm = T),
      Angle_Sweet = mean(Angle[sweetspot == "SweetSpot" & PitchCall == 'InPlay'], na.rm = T),
      RunValue = sum(run_value, na.rm = T),
      # wOBA_val = sum(woba_value, na.rm = T),
      # TB = sum(total_bases, na.rm = T),
      # batted ball profile
      BU = sum(HitType == "Bunt" & PitchCall == 'InPlay',na.rm = T),
      GB = sum(HitType == "GroundBall" & PitchCall == 'InPlay',na.rm = T),
      FB = sum(HitType == "FlyBall" & PitchCall == 'InPlay',na.rm = T),
      LD = sum(HitType == "LineDrive" & PitchCall == 'InPlay',na.rm = T),
      PU = sum(HitType == "PopUp" & PitchCall == 'InPlay',na.rm = T),
      # Pull,
      # Straight,
      # Oppo,
      
      # discipline
      Swings = sum(swing, na.rm =T),
      Whiffs = sum(whiff, na.rm =T),
      Zone = sum(in_zone, na.rm = T),
      oZone = sum(in_zone == 0, na.rm =T),
      ZoneSw = sum(swing == 1 & in_zone == 1,na.rm = T),
      ZoneCon = sum(PitchCall == "InPlay" & in_zone == 1,na.rm = T),
      ZoneWhiff = sum(whiff == 1 & in_zone == 1,na.rm = T),
      Chase = sum(in_zone == 0 & swing ==1, na.rm =T),
      ChaseCon = sum(PitchCall == "InPlay" & in_zone == 0,na.rm = T),
      ChaseWhiff = sum(whiff == 1 & in_zone == 0,na.rm = T),
      `1PSw` = sum(PitchofPA == 1 & swing ==1, na.rm =T),
    ) %>%
    ungroup() %>%
    mutate(AB = PA - BB - SO - HBP) %>%
    mutate(xBA = xBA_sum / AB,
           xBACON = xBA_sum / BBE,
    )%>%
    mutate(TB = (HR*4)+(X3B*3)+(X2B*2)+(X1B),
           SO_pct = SO / PA,
           BB_pct = BB / PA,
           BA = H / AB,
           OBP = (BB+HBP+H)/PA,
           SLG = TB / AB,
           OPS = OBP + SLG,
           wOBA = ((BB * weights$wBB[match(SEASON,weights$season)]) + (HBP * weights$wHBP[match(SEASON,weights$season)]) +
                     (X1B * weights$w1B[match(SEASON,weights$season)]) + (X2B * weights$w2B[match(SEASON,weights$season)]) + 
                     (X3B * weights$w3B[match(SEASON,weights$season)]) + (HR * weights$wHR[match(SEASON,weights$season)]) ) / PA,
           xwOBA = xwoba_sum / PA,
           wOBACON = ((X1B * weights$w1B[match(SEASON,weights$season)]) +  (X2B * weights$w2B[match(SEASON,weights$season)]) +
                        (X3B * weights$w3B[match(SEASON,weights$season)]) + (HR * weights$wHR[match(SEASON,weights$season)]) ) / BIP,
           xWOBACON = xwobacon_sum / BIP,
           BABIP = (H - HR) / (AB - HR - SO),
           xBA = xBA_sum / (BBE + SO),
           xBACON = xBA_sum / BBE,
    ) %>% mutate(HardHit_pct = HardHit / BBE,
                 Sweetspot_pct = Sweetspot / BBE,
                 RV100 = RunValue / P * 100,
                 GB_pct = GB / BIP,
                 FB_pct = FB / BIP,
                 LD_pct = LD / BIP,
                 PU_pct = PU / BIP,
                 Swing_pct = Swings / P,
                 Whiff_pct = Whiffs / Swings,
                 Zone_pct = Zone / P,
                 oZone_pct = oZone / P,
                 ZoneSw_pct = ZoneSw / Zone,
                 ZoneCon_pct = ZoneCon / ZoneSw,
                 ZoneWhiff_pct = ZoneWhiff / ZoneSw,
                 Chase_pct = Chase / oZone,
                 ChaseCon_pct = ChaseCon / Chase,
                 ChaseWhiff_pct = ChaseWhiff / Chase,
                 `1PSw_pct` = `1PSw` / PA
    ) %>%
    select(-c(HardHit, Sweetspot, GB, FB, LD, PU, Swings, Whiffs, Zone, oZone, ZoneSw, 
              ZoneCon, ZoneWhiff, Chase, ChaseCon, ChaseWhiff, `1PSw`, xBA_sum,
              xSLG_sum, xwoba_sum, xwobacon_sum)) %>%
    mutate(across(c(HardHit_pct, Sweetspot_pct, GB_pct, FB_pct, LD_pct, PU_pct, Swing_pct, Whiff_pct, Zone_pct, 
                    oZone_pct, ZoneSw_pct, ZoneCon_pct, ZoneWhiff_pct, Chase_pct, ChaseCon_pct, ChaseWhiff_pct, `1PSw_pct`),
                  ~ round(.,4) * 100),
           across(c(ExitVelo, ExitVelo_max, LA, Angle_Hard, Angle_Sweet), ~ round(., 2)),
           # across(c(xBA, xBACON, xSLG), ~ round(.,3))
    ) %>%
    relocate(TB:BABIP, .after = SAC)
}

dbWriteTable(conn = db, "stats_hitting_team_batted_ball", value = team_h_bb, overwrite = T)
dbWriteTable(conn = db, "stats_pitching_team_batted_ball", value = team_p_bb, overwrite = T)

slackr_msg(txt = paste("Trackman team batted ball stats tables created.") , 
           channel = "#flash",
           username = paste("FLASHBot", emoji('robot'))
)

Sys.sleep(5)
