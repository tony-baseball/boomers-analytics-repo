# PITCHER PA TRACKER -----------------------

for(pitcher in pitchers) {
  pitcher_data <- yakker_day %>%
    arrange(PitchNo) %>%
    filter(Pitcher == pitcher) %>%
    select(Pitcher, Batter,  BatterSide, Inning, PA = Pitcher_PAofGame,`#` = PitchofPA, Pitch = TaggedPitchType, Count,Outs,PlayResult, Result, PlateLocSide, PlateLocHeight,
           Velo = RelSpeed, VB = InducedVertBreak, HB = HorzBreak, Spin = SpinRate, Tilt, EV = ExitSpeed, hc_x, hc_y, Date, PitcherTeam, RunsScored, Distance, PitchCall, BatterSide,KorBB, BatterTeam,
           Date, LA= Angle) %>%
    mutate(PlayResult = recode(PlayResult, Double = '2B', HomeRun = 'HR', Single = '1B', Triple = '3B', Sacrifice = 'SAC', Error = 'E', FieldersChoice = 'FC'),
           PlateLocSide = ifelse(is.na(PlateLocSide), 0,PlateLocSide),
           PlateLocHeight = ifelse(is.na(PlateLocHeight), 0, PlateLocHeight),
           Pitch = str_replace_all(Pitch, c('Fastball' = 'FB', 'Sinker' = 'SI', 'Cutter' = 'CT', 'Curveball' = 'CB', 'Slider' = 'SL', 'Changeup' = 'CH', 'Splitter' = 'SPL')),
           
    )
  
  date_of_game <- as.character(unique(pitcher_data$Date))
  
  params_boom <- list(
    pitcher_data = pitcher_data,
    pitcher_name = unique(pitcher_data$Pitcher),
    game_date = unique(pitcher_data$Date),
    opponent_team =  unique(pitcher_data$BatterTeam),
    team_logo = team_location
  )
  
  file_name <- paste(gsub(" ", "_", pitcher))
  
  date_of_game <- pitcher_data$Date[1]
  
  rmd_file <-  "C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/post_pitcher_PA_report.Rmd"
  file_save <- paste0("C:/Users/tdmed/OneDrive/_Advance/Boomers/P/", date_of_game,"_", file_name ,"_PA_tracker.pdf")
  rmd_file
  file_save
  
  
  #suppressWarnings({
  rmarkdown::render(input = rmd_file, 
                    output_file = file_save,
                    params = params_boom)
  #})
  #
} 
