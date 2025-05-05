suppressWarnings(suppressMessages({
  library(plyr)
  library(dplyr)
  library(stringi)
  library(stringr)
  library(tidyverse)
  library(rsconnect)
  library(discordr)
  library(emojifont)
  library(tools)
  library(baseballr)
  library(RSQLite)
  library(mgcv)
  library(nnet)
  library(caret)
  library(kernlab)
  library(FNN)
  library(slackr)
  library(tonybaseball)
  library(RSQLite)
}))

# SETUP ----
slackr_setup(channel = "#general", username = "slackr", icon_emoji = '',
             incoming_webhook_url = 'https://hooks.slack.com/services/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', 
             token = 'xoxb-7165085228646-7165145332742-bkkhpCh1WqnyS8PxZdlYBkbH', echo = FALSE)


db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

weights <- dbGetQuery(db, "SELECT * FROM weights where season = 2024")

umps <- dbGetQuery(db, 'select * from umps_')

# File Setup----
{
  slackr_msg(txt = paste(emoji('rotating_light'),"Boomers CSV processing initiated on",format(Sys.Date(), "%a %B %d, %Y"), "at", format(Sys.time(), "%d %X")),
             channel = "#flash",
             username = paste("FLASHBOT",emoji('robot'))
  )
  
  
  # one drive folder with YT games already copied/renamed
  current_files_folder <- "C:/Users/tdmed/OneDrive/_Trackman/2025/csv/_processed/"
  # file names
  current_files <-basename(list.files(current_files_folder, recursive = F, pattern = "\\.csv$", full.names = TRUE))
  # count
  current_precount <- length(list.files(current_files, recursive = F, pattern = "\\.csv$", full.names = TRUE))
  # daily downloaded files
  tm_new <- "C:/Users/tdmed/OneDrive/_Trackman/2025/csv/"
  # file names
  tm_new_files <- basename(list.files(tm_new, recursive = F, pattern = "\\.csv$", full.names = TRUE))
  # onedrive_new_files <-onedrive_new_files[1]
  # count of new files
  tm_new_count <- length(list.files(tm_new, recursive = F, pattern = "\\.csv$", full.names = TRUE))
  
  file_dif <- tm_new_count - current_precount
  
  boom_folder <- "C:/Users/tdmed/OneDrive/_Trackman/2025/boomers"
  
  file_in <- "C:/Users/tdmed/OneDrive/_Trackman/2025/csv/_in/"
}
Sys.sleep(5)
if (file_dif > 0 ) { # MASTER CODE ----
  
  # copy files from '/GameDataGD/2024' to '2024_new'
  file.copy(from = list.files(tm_new, pattern = "\\.csv$", full.names = TRUE, recursive = F), 
            to = file_in ,  overwrite = FALSE, recursive = TRUE)
  
  # list files in '/2024_new'
  files <- list.files(file_in, full.names = TRUE)
  
  # for each file in '/2024_new'
  for (file in files) {
    new_name <- str_to_lower(str_replace_all(file, "\\s+", "_"))  # Remove all spaces in the file name and convert to lower
    file.rename(file, new_name)  # Rename the file
  }
  
  # send_webhook_message(paste(file_dif,"new Trackman file(s) downloaded. Renaming and copying files."), conn = conn_obj2)
  slackr_msg(txt = paste(file_dif,"new Trackman file(s) downloaded. Renaming and copying files."),
             channel = "#flash",
             username = paste("FLASHBOT",emoji('robot'))
  )
  Sys.sleep(5)
  
  # '/2024_new/' files renamed 
  directory_path2 <- "C:/Users/tdmed/OneDrive/_Trackman/2025/csv/_in/"
  
  csv_files2 <- list.files(directory_path2, pattern = "\\.csv$", full.names = TRUE)
  
  team_info <- dbGetQuery(db,"SELECT * FROM teams") # read.csv('C:/Users/tdmed/OneDrive/_Shiny/_Coop2/frontier team info.csv')
  
  csv_files3 <- csv_files2
  
  daily_data <- data.frame()
  
  for (file in csv_files3) { # FOR EACH FILE LOOP ------
    
    Sys.sleep(10)
    
    data <- read.csv(file) %>% 
      tonybaseball::yt_clean_team_names()
    
    dates_for_pbp <- unique(as.character(as.Date(data$Date, "%m/%d/%Y")))
    
    if(length(unique(data$TaggedPitchType)) > 1){ # if unique pitch type is different
      
      data <- data %>% 
        tonybaseball::tm_transform_file() %>%
        tonybaseball::yt_add_team_codes()  %>%
        # tonybaseball::yt_add_umpire() %>% 
        tonybaseball::calc_total_bases() %>% 
        tonybaseball::calc_woba_value() %>%
        tonybaseball::calc_pa_ab_h_reach()%>%
        tonybaseball::xStats_yak() %>%
        tonybaseball::calc_arm_angle() %>% 
        tonybaseball::calc_run_value() %>%
        tonybaseball::power_value_model() %>%
        tonybaseball::contact_value_model() %>%
        tonybaseball::decision_value_model() %>%
        # tonybaseball::yt_event_from_pbp(dates_for_pbp = dates_for_pbp) %>%
        mutate(local_vid_file = NA) %>%
        tonybaseball::tm_column_order()
      
      
    } else { # if unique pitch type is the same, ie, need modeling.
      
      slackr_msg(txt = paste('Pitch Modeling Needed for', basename(file)),
                 channel = "#flash",
                 username = paste("FLASHBOT",emoji('robot'))
      )
      Sys.sleep(5)
      
      # source('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/FL_OD_P_Model.R')
      data$TaggedPitchType <- data$AutoTaggedPitchType
      
      data <- data %>% 
        mutate(TaggedPitchType= gsub('Sweeper|Slurve', 'Slider', TaggedPitchType))
      
      write.csv(data, file, row.names = F, na = '')
      
      data <- data%>% 
        tonybaseball::tm_transform_file() %>%
        tonybaseball::yt_add_team_codes()  %>%
        # tonybaseball::yt_add_umpire() %>% 
        tonybaseball::calc_total_bases() %>% 
        tonybaseball::calc_woba_value() %>%
        tonybaseball::calc_pa_ab_h_reach()%>%
        tonybaseball::xStats_yak() %>%
        tonybaseball::calc_arm_angle() %>% 
        tonybaseball::calc_run_value() %>%
        tonybaseball::power_value_model() %>%
        tonybaseball::contact_value_model() %>%
        tonybaseball::decision_value_model() %>%
        # tonybaseball::yt_event_from_pbp(dates_for_pbp = dates_for_pbp) %>%
        mutate(local_vid_file = NA) %>%
        tonybaseball::tm_column_order()
      
    }
    
    # rename file ----
    
    game <- ifelse(data$HomeTeam[1] == 'BOOM',
                   paste(format(as.Date(data$Date[1], format = "%Y-%m-%d"),"%Y-%m-%d"),
                         data$HomeTeam[1], 'vs', data$AwayTeam[1],data$Code[1]),
                   paste(format(as.Date(data$Date[1], format = "%Y-%m-%d"),"%Y-%m-%d"),
                         data$AwayTeam[1], 'at', data$HomeTeam[1],data$Code[1])
    )
    
    
    
    game_date <- paste(data$Date[1],data$AwayTeam[1], 'at', data$HomeTeam[1])
    
    tm_folder <- 'C:/Users/tdmed/OneDrive/_Trackman/2025/csv/_processed/'
    
    # copy and rename final csv
    if (!is.na(game)) {
      new_file_path <- paste0(tm_folder, game, "_processed.csv")
      
      if (!file.exists(new_file_path)) {
        # file.copy(file, new_file_path)
        
        file.copy(file,
                  to = new_file_path)
        # write.csv(data, new_file_path, na = '', row.names = F)
        print(paste("CSV", game, "renamed and copied!"))  # Modified printing statement
      } else {
        print(paste("Skipped", game, "CSV since it already exists."))
      }
    } else {
      
      slackr_msg(txt = paste("ERROR:", game_date,"is missing."),
                 channel = "#flash",
                 username = paste("FLASHBOT",emoji('robot'))
      )
      Sys.sleep(5)
      
      print(paste("ERROR:", game_date,"is missing."))
      
    }
    
    daily_data <- rbind.fill(daily_data, data) %>% distinct() %>%
      mutate(Date = as.character(Date))
    
    Sys.sleep(5)
    
  }
  
  # <<<REMOVE THIS>>> ----
  # daily_data$Date <-
  #   as.Date(daily_data$Date) + lubridate::years(4)
  
  # GET STUFF PLUS----
  slackr_msg(txt = paste("Removing previous data from temp_data table"),
             channel = "#flash",
             username = paste("FLASHBOT",emoji('robot'))
  )
  dbExecute(db, 'DELETE FROM temp_data where PitchNo > 0')
  dbExecute(db, 'DELETE FROM temp_data_py where PitchNo > 0')
  
  temp_data <- daily_data
  
  dbWriteTable(db, name = 'temp_data', temp_data, overwrite = T)
  
  system('python C:/Users/tdmed/OneDrive/_Github/tjstuff_plus/tj_stuff_plus.py')
  
  Sys.sleep(5)
  
  daily_data <- dbGetQuery(db, 'select * from temp_data_py')
  
  # CONTINUE ----
  copy_diff <- ( length(list.files(tm_new, recursive = F, pattern = '.csv')) ) - (length(list.files(tm_folder, recursive = F, pattern = '.csv')))
  
  length(list.files(tm_folder))
  
  if(copy_diff > 0){ 
    
    slackr_msg(txt = paste(emoji('heavy_exclamation_mark'),'Files renamed and copied with',copy_diff,'errors. Check for NA or Doubleheaders!'),
               channel = "#flash",
               username = paste("FLASHBOT",emoji('robot'))
    )
    
  } else { 
    
    slackr_msg(txt = paste('Files renamed and copied with',copy_diff,'errors. Copying Boomers file to folder, and removing new TM files.'),
               channel = "#flash",
               username = paste("FLASHBOT",emoji('robot'))
    )
    
  }
  Sys.sleep(5)
  
  
  file.copy(from = list.files(tm_folder, pattern = 'BOOM', full.names = TRUE, recursive = TRUE),
            to = boom_folder, overwrite = FALSE, recursive = TRUE)
  
  file.copy(from = list.files(tm_new, pattern = '*.csv', full.names = T, recursive = F),
            to = 'C:/Users/tdmed/OneDrive/_Trackman/2025/csv/_raw/')
  
  
  # COMBINE AND ADD TO SQL DB!!!!! ------------------------------
  
  dates_frmt <- 0
  
  if(dates_frmt < 1){
    
    slackr_msg(txt = paste("Date(s) correctly reformatted. Proceeding..."),
               channel = "#flash",
               username = paste("FLASHBOT",emoji('robot'))
    )
    
    # WRITING NEW DATA TO PITCH_DATA TABLE ----
    Sys.sleep(5)
    
    source('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/database_backup.R')
    
    Sys.sleep(5)
    
    slackr_msg(txt = paste("Writing updated pitch_data table to SQLite Database."),
               channel = "#flash",
               username = paste("FLASHBOT",emoji('robot'))
    )
    
    Sys.sleep(5)
    
    dbWriteTable(conn = db, "pitch_data", value = daily_data, append = T)
    
    Sys.sleep(5)
    
    
    # write.csv(yakker_szn_24, "C:/Users/tdmed/OneDrive/_FLASH/yakker24.csv", na="", row.names = FALSE)
    # write.csv(yakker_szn_24, "C:/Users/tdmed/OneDrive/Coop2 BACKUP/test/yakker24.csv", na="", row.names = FALSE)
    
    slackr_msg(txt = paste("pitch_data table updated in database."),
               channel = "#flash",
               username = paste("FLASHBOT",emoji('robot'))
    )
    
    Sys.sleep(5)
    
    # slackr_msg(txt = paste("Cleaning yak & Copying to SQLite Database..."),
    #            channel = "#flash",
    #            username = paste("FLASHBOT",emoji('robot'))
    # )
    
    # source('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/CleanYak.R')
    
    slackr_msg(txt = paste("Copying database to Google Drive."),
               channel = "#flash",
               username = paste("FLASHBOT",emoji('robot'))
    )
    
    od <- "C:/Users/tdmed/OneDrive/_FLASH/"
    gd <- 'J:/My Drive/2023 Boomers/_Files/FLASH'
    
    # CHECK GOOGLE DRIVE RUNNING
    {
      script_path <- "C:\\Users\\tdmed\\Desktop\\google_drive_restart.ps1"
      
      # Use the shell function to run the PowerShell script
      shell(paste("powershell -ExecutionPolicy Bypass -File", shQuote(script_path)), wait = TRUE)
    }
    
    file.copy(from ="C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite",
              to = paste0(gd,'frontier_league.sqlite'), overwrite = T)
    
    Sys.sleep(5)
    
    
    
    # { - OLD YAKKERTECH TO TM AUTOMATION ----
    #   slackr_msg(txt = "Copying and transforming database to Trackman format", 
    #              channel = "#flash",
    #              username = paste("FLASHBot",emoji('robot'))
    #   )
    #   
    #   # file.copy(from = 'C:/Users/tdmed/OneDrive/_Shiny/FLASH/flashdb.sqlite',
    #   #           to = "C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite", overwrite = T)
    #   
    #   # source('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Trackman/YT_to_TM_automation.R')
    #   
    #   for (q in clean_up$query) {
    #     
    #     dbExecute(db_temp, q)
    #     
    #     cat(paste("-----> Query:",q), sep = '\n\n', append = F)
    #   }
    #   
    #   source("C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/FL yakker stats_TM db.R")
    #   
    # }
    
    slackr_msg(txt = paste("Copied to database!"),
               channel = "#flash",
               username = paste("FLASHBOT",emoji('robot'))
    )
    Sys.sleep(5)
    
    slackr_msg(txt = paste("Running post processing automation R Code scripts!"),
               channel = "#flash",
               username = paste("FLASHBOT",emoji('robot'))
    )
    Sys.sleep(5)
    # - POST PROCESSING SCRIPTS ----
    # source('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Trackman_Pointstreak/Trackman_ETL_2_bb_stats.R')
    # source('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/FL EV Dist.R')
    # source('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Boom FL Daily HR.R')
    # source('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Boomers Hard Hit.R')
    # source('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/YT_DB to TM 24.R') # NO LONGER NEEDED!
    # source('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/2025 Boomers Postgame P-PPA-HPA reports.R')
    Sys.sleep(5)
    
    { # inform me and joey that the db is now in google drive
      # slackr_msg(txt = paste("SQLite database copied to Google Drive!"),
      #            channel = "U075T5K7HHN", # JOEY
      #            username = paste("FLASHBOT",emoji('robot'))
      # )
      
      slackr_msg(txt = paste("SQLite database copied to Google Drive!"),
                 channel = "U074YPWD8BX", # TONY ME 
                 username = paste("FLASHBOT",emoji('robot'))
      )
    }
    
    
    
    {
      slackr_msg(txt = paste(emoji('white_check_mark'),emoji('baseball'),"ALL BOOMERS SCRIPTS SUCCESSFULLY COMPLETED!"),
                 channel = "#flash",
                 username = paste("FLASHBOT",emoji('robot'))
      )
    }
    
  } 
} else {
  slackr_msg(txt = paste(emoji('heavy_exclamation_mark'),"Redeployment halted! Please check either Date Formats, NAs in file names, or Doublheaders"),
             channel = "#flash",
             username = paste("FLASHBOT",emoji('robot'))
  )
}





