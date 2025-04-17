library(stringr)
library(slackr)
library(googledrive)

slackr_setup(channel = "#general", username = "slackr", icon_emoji = '',
             incoming_webhook_url = 'https://hooks.slack.com/services/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', 
             token = 'xoxb-7165085228646-7165145332742-bkkhpCh1WqnyS8PxZdlYBkbH', echo = FALSE)

{
  file <- suppressMessages(googledrive::drive_get("tst.txt"))  
  1
  # Get the last modified time
  file_metadata <- suppressMessages(googledrive::drive_reveal(file, "lastModifyingUser"))
  last_modified <- as.POSIXct(file_metadata$drive_resource[[1]]$modifiedTime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  timestamp_cdt <- format(last_modified, tz = "America/Chicago", usetz = TRUE, format = "%I:%M:%S %p %Y-%m-%d")
  
  cat(paste('File 1:', timestamp_cdt),sep = '\n')
  
  googledrive::drive_ls('2023 Boomers/_Files/')
  file2 <- suppressMessages(googledrive::drive_get("tst.txt"))  
  1
  
  # See if the file has been modified
  file_metadata2 <- suppressMessages(googledrive::drive_reveal(file2, "lastModifyingUser"))
  last_modified2 <- as.POSIXct(file_metadata2$drive_resource[[1]]$modifiedTime,  format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  timestamp_cdt2 <- format(last_modified2, tz = "America/Chicago", usetz = TRUE, format = "%I:%M:%S %p %Y-%m-%d")
  cat(paste('File 2:', timestamp_cdt),sep = '\n')
  Sys.sleep(3)
  cat(paste('- - Checking at:',format( lubridate::now(),"%I:%M:%S %p %Y-%m-%d" ), '- -'),sep = '\n')
  
  # Assuming you have initialized timestamp_cdt and timestamp_cdt2 elsewhere in your code
  
  # Start the loop
  while(timestamp_cdt2 <= timestamp_cdt) {
    Sys.sleep(5)
    cat('- - - No updates found, waiting 60 seconds - - -', sep = '\n')
    Sys.sleep(55)  # Delay for 60 seconds to avoid excessive CPU usage
    
    cat(paste('- Rechecking at:',format( lubridate::now(),"%I:%M:%S %p %Y-%m-%d" ), "-"),sep = '\n')
    
    # Restart the loop
    file2 <- suppressMessages(googledrive::drive_get("tst.txt"))  # Replace with your file name or ID
    1
    # See if the file has been modified
    file_metadata2 <- suppressMessages(googledrive::drive_reveal(file2, "lastModifyingUser"))
    last_modified2 <- as.POSIXct(file_metadata2$drive_resource[[1]]$modifiedTime,  format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    timestamp_cdt2 <- format(last_modified2, tz = "America/Chicago", usetz = TRUE, format = "%I:%M:%S %p %Y-%m-%d")
    cat(paste('File 2:', timestamp_cdt2),sep = '\n')
  }
  
  # Once the condition timestamp_cdt2 > timestamp_cdt is TRUE, send a Slack message
  slackr_msg(
    txt = paste("Updated SQLite database uploaded at", timestamp_cdt2), 
    channel = "U074YPWD8BX", 
    username = paste("FLASHBOT", emoji('robot'))
  )
  cat(paste("Updated SQLite database uploaded at", timestamp_cdt2), sep = '\n')
  
}