{
  game_date <- unique(yakker_day$Date)
  print (game_date)
  # ************************* change team name below for manual
  pitchers <- unique(yakker_day$Pitcher[grepl('boomers|Boomers',yakker_day$PitcherTeam)])
  pitchers
  # pitchers<-pitchers[c(3)]
  catchers <-  unique(yakker_day$Catcher[grepl('boomers|Boomers',yakker_day$PitcherTeam)])
}

# PITCHERS --------------------------------------------------------------------------------------------------------------------------------------------

# this will send a message telling you how many reports are being made
slackr_msg(txt = paste(length(pitchers),"new pitcher reports being generated."), 
           channel = "#reports-boomers",
           username = paste("FLASHBOT",emoji('robot')))

{ 
  source('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/_postgame_reports_functions.R')
  
  for (pitcher in pitchers) {
    
    master_postgame_boomers_pitcher_report(yakker_day = yakker_day, pitcher)
    
    # # For manual execution Knit the R Markdown file to PDF
    # rmarkdown::render(input = "C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/Postgame-Reports-P.Rmd",
    #                   output_file = paste0("C:/Users/tdmed/OneDrive/_Advance/Boomers/P/",file_date,"_",p, "_report_Calibri",".pdf"),
    #                   params = params)
  }
  #  
}