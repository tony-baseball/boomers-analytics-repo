# POSTGAME REPORTS
suppressWarnings(suppressMessages({ # LOAD LIBRARIES----
  # library(plyr)
  library(tidyverse)
  library(baseballr)
  library(discordr)
  library(cowplot)
  library(emojifont)
  library(flextable)
  library(GeomMLBStadiums)
  library(DT)
  library(ggplot2)
  library(grDevices)
  library(grid)
  library(gridExtra)
  library(gtsummary)
  library(janitor)
  library(kableExtra)
  library(knitr)
  library(magick)
  library(openxlsx)
  library(pak)
  # library(pandoc)
  library(stringi)
  library(stringr)
  library(tools)
  library(webshot)
  library(tinytex)
  library(RSQLite)# tinytex::install_tinytex()
  library(slackr)
  library(googledrive)
  library(ggh4x)
  library(gtable)
  
}))

{
  team_filter <- "Windy City ThunderBolts"
  home_team_in_series <- "Schaumburg Boomers"
  
  setwd("C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/")
  # getwd()
  db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")
  
  # CREATE A DISCORD WEBHOOK FOR AUTOMATED NOTIFICATIONS
  
  slackr_setup(channel = "#general", username = "slackr", icon_emoji = '',
               incoming_webhook_url = 'https://hooks.slack.com/services/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', 
               token = 'xoxb-7165085228646-7165145332742-bkkhpCh1WqnyS8PxZdlYBkbH', echo = FALSE)
  
  
  
  # slackr_msg(txt = "Generating Advanced Scouting Reports", 
  #            channel = "#reports-boomers",
  #            username = paste("FLASHBOT",emoji('robot'))
  # )
  
  opposing_team_code <- dbGetQuery(db, glue::glue('SELECT bats_team_code FROM teams WHERE Team = "{team_filter}"'))$bats_team_code
  home_team_in_series_code <-   dbGetQuery(db, glue::glue('SELECT bats_team_code FROM teams WHERE Team = "{home_team_in_series}"'))$bats_team_code 
  
  opposing_team_location <- gsub(" |-","", dbGetQuery(db, glue::glue('SELECT Location FROM teams WHERE Team = "{team_filter}"'))$Location)
  
  # YAKKERTECH/TRACKMAN DATA
  seasons_to_get <- 2024
  
  # SCRIPTS FILE PATH AND DICTIONARY ----
  r_scripts <- list(
    'stats' = 'adv_opponent_stats.R',
    'opposing_hitters' = 'adv_hitter_report.R',
    'opposing_pitchers' = 'adv_pitcher_report.R',
    'bullpen_usage' = 'adv_bullpen_report.R',
    'pocket_cards' = '_other/pocket_spray/pocket_spray_charts.R'
  )
  
  run_advanced_scouting <- list(
    'stats' = F,
    'opposing_hitters' = F,
    'opposing_pitchers' = F,
    'bullpen_usage' = F,
    'pocket_cards' = T
  )
  
  dcs <- names(run_advanced_scouting)[unlist(run_advanced_scouting)]
  
  scripts_to_run <- r_scripts[dcs]
  
}

dput(dbGetQuery(db, glue::glue('SELECT * from rosters where SEASON = 2025 and Team = "{team_filter}" and Position not like "P%" and Position not like "Inactive"'))$Player)

unique_hitters <- c("Christian Kuzemka",  "Ashton Creal","David Maberry", 
                    "Will Armbruester","Cam Phelts", "Zach Beadle",
                    "Garrett Broussard",
                    "Jose Curpa", "Winder Diaz", "Kendal Ewell", "JJ Figueroa", "Ruddy Gomez", 
                    "Anders Green", "Jalen Greer", "Kyle Harbison", "Anthony Herron Jr.", 
                    "Tyler Hill", "Dakota Kotowski",  
                    "Michael Sandle", "Oscar Serratos", "Donivan Williams"
)

pocket_cards_lineup_order <- 'auto' # manual or auto


# EXECUTE ----
{
  tictoc::tic()
  for (xyz in seq_along(scripts_to_run)) {
    
    cat(glue::glue('Running {dcs[xyz]}'), sep = '\n')
    source(scripts_to_run[xyz][[1]])
    cat(glue::glue('{dcs[xyz]} Completed!'), sep = '\n')
    
    logs <- list.files("", pattern = '.log', full.names = T)
    
    for (log in logs){
      file.rename(log,
                  to = paste0('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/logs/',basename(log)) )
    }
    
  }
  tictoc::toc()
}
