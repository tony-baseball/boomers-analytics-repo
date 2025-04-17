# POSTGAME REPORTS
suppressWarnings(suppressMessages({ # LOAD LIBRARIES----
  library(plyr)
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
  
}))


# CREATE A SLACK  WEBHOOK FOR AUTOMATED NOTIFICATIONS

slackr_setup(channel = "#general", username = "slackr", icon_emoji = '',
             incoming_webhook_url = 'https://hooks.slack.com/services/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', 
             token = 'xoxb-7165085228646-7165145332742-bkkhpCh1WqnyS8PxZdlYBkbH', echo = FALSE)

# slackr_msg(txt = "Generating Boomers Post Game Reports", 
#            channel = "#reports-boomers",
#            username = paste("FLASHBOT",emoji('robot'))
# )

db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

max_b_date <- dbGetQuery(db, 'SELECT max(Date) Date FROM pitch_data where BatterTeam = "Schaumburg Boomers" or PitcherTeam = "Schaumburg Boomers"')$Date
max_b_date <- '2024-06-22' # MANUAL DATE ENTRY, COMMENT OUT WHEN RUNNING AUTOMATION

query <- glue::glue('SELECT * FROM pitch_data 
                    WHERE (BatterTeam = "Schaumburg Boomers" or PitcherTeam = "Schaumburg Boomers") 
                    AND Date = "{max_b_date}"')

file <- dbGetQuery(db, query)

if(nrow(file) > 0 ){
  
  yakker_day <- file %>% # read.csv(file) %>% # read.csv(file) %>%
    arrange(Date, GameID, PitchNo)  %>%
    mutate(Count = paste(Balls,Strikes,sep = '-'),
           HitType = recode(HitType, FlyBall = 'FB', LineDrive = 'LD', GroundBall = 'GB', PopUp = 'PU'),
           Result = 
             case_when(
               PitchCall == 'InPlay' ~ paste(PlayResult, HitType, sep = '-'),
               PitchCall != 'InPlay' & !grepl('Strikeout|Walk', KorBB) ~ PitchCall,
               PitchCall != 'InPlay' & grepl('Strikeout',KorBB) ~ paste('K',PitchCall),
               PitchCall != 'InPlay' & KorBB == 'Walk' ~ paste('BB'),
               T ~ NA
             ),
           Result = str_replace_all(Result, c('StrikeCalled' = 'Strike', 'StrikeSwinging' = 'Whiff', 
                                              'BallCalled' = 'Ball', 'HitByPitch' = 'HBP', 'K Strike' = 'K-Look')) ,
           swing_result = 
             case_when(
               PitchCall == 'Foul' ~ 'Foul',
               PitchCall == 'StrikeSwinging' ~ 'Whiff',
               PitchCall == 'InPlay' ~ paste(PlayResult)
             ),
           sweet_spot = 
             case_when(
               Angle >= 40 ~ 'Under',
               between(Angle, 33, 39) ~ 'Flare',
               between(Angle, 8, 32) ~ 'SweetSpot',
               between(Angle, 0, 7) ~ 'Burner',
               Angle <= 0 ~ 'Topped'
             ),
           PitcherThrows = str_replace_all(PitcherThrows, c('Right' = 'RHP', 'Left' = 'LHP')),
           StolenStrike = ifelse(
             PitchCall == "StrikeCalled" & in_zone == 0, 1, 0),
           StrikeLost = ifelse(PitchCall == "BallCalled" & in_zone == 1, 1, 0)
    )   %>%
    group_by(Batter) %>%
    mutate(PAofGame = cumsum(!duplicated(paste(Inning, PAofInning))) ) %>%
    ungroup() %>%
    group_by(Pitcher) %>%
    mutate(Pitcher_PAofGame = cumsum(!duplicated(paste(Inning, PAofInning))) ) %>%
    ungroup() %>%
    tonybsbl::pitch_types_factor()
  
  
  team_location <- 'Schaumburg'
  
  # SCRIPTS FILE PATH AND DICTIONARY ----
  r_scripts <- list(
    # 'box_scrape' = 'C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Trackman_Pointstreak/PointStreak_pbp_box_same_day.R',
    # 'trackman_new_csv' = 'C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Trackman_Pointstreak/Trackman_ETL_1_Boomers.R',
    # 'trackman_batted_ball' = 'C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Trackman_Pointstreak/Trackman_ETL_2_bb_stats.R',
    'postgame_reports_pitchers' = 'post_pitcher_report.R',
    # 'postgame_reports_pitcher_PA' = 'post_pitcher_PA_report.R',
    'postgame_reports_catcher' = 'post_catcher_report.R',
    'postgame_reports_hitters' = 'post_hitter_report.R',
    'daily_ev_distance' = 'C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/FL EV Dist.R',
    'boomers_homeruns' = 'C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Boom FL Daily HR.R'
  )
  run_postgame_reports <- list(
    # 'box_scrape' = TRUE,
    # 'trackman_new_csv' = FALSE,
    # 'trackman_batted_ball' = FALSE,
    'postgame_reports_pitchers' = TRUE,
    # 'postgame_reports_pitcher_PA' = FALSE,
    'postgame_reports_catcher' = T,
    'postgame_reports_hitters' = T,
    'daily_ev_distance' = FALSE,
    'boomers_homeruns' = FALSE
  )
  
  dcs <- names(run_postgame_reports)[unlist(run_postgame_reports)]
  
  
  
  scripts_to_run <- r_scripts[dcs]
  
  
  # EXECUTE ----
  tictoc::tic()
  for (report_name in seq_along(scripts_to_run)) {
    
    cat(glue::glue('Running {dcs[report_name]}'), sep = '\n')
    source(scripts_to_run[report_name][[1]])
    cat(glue::glue('{dcs[report_name]} Completed!'), sep = '\n')
    
  }
  
} else{
  cat(glue::glue("No Game on {max_b_date}"))
}
tictoc::toc()
# source("C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Trackman_Pointstreak/Trackman_ETL_1_Boomers.R")
# scripts_to_run[i][[1]]
