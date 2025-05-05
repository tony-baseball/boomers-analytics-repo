{
  game_date <- unique(yakker_day$Date)
  print (game_date)
  # ************************* change team name below for manual
  pitchers <- unique(yakker_day$Pitcher[grepl('boomers|Boomers',yakker_day$PitcherTeam)])
  pitchers
  # pitchers<-pitchers[c(3)]
  catchers <-  unique(yakker_day$Catcher[grepl('boomers|Boomers',yakker_day$PitcherTeam)])
}
source("C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/_postgame_reports_functions.R")

{ # CATCHER REPORT ------------------------------------------------------------------------------
  for (catcher in catchers) {
    # catcher <- name
    master_postgame_boomers_catcher_report(yakker_day, catcher)
    
  }
}
