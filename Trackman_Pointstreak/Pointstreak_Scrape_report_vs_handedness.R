library(httr)
library(tidyverse)
library(rvest)


# Define the URL
seasonid <- 33873
teamid <- 162819
url <- glue::glue("https://pointstreak.com/baseball/textstats/reports/seasons/{seasonid}/teams/{teamid}/team_left_right_report.txt")

# Get the text content from the URL
response <- GET(url)
content <- content(response, as = "text")

# Split the content by line breaks
lines <- str_split(content, "\n")[[1]]

table_indices_name <- grep("STATS", lines)  # Detect the table header

table_names <- c('vs_rhp', 'vs_lhp', 'vs_rhh', 'vs_lhh')

# Example: Extract the first table (adjust according to your actual pattern detection)
table_indices <- grep("PLAYER", lines)  # Detect the table header

rm(i)

for(i in 1:length(table_indices)){
  
  header <- str_squish(lines[table_indices[i]]) %>% 
    str_split_fixed(" ", n = length(str_split(str_squish(lines[table_indices[i]]), " ")[[1]]))  # Adjust the number of columns based on your header
  
  str_count(str_squish(lines[table_indices[i]]))
  # Extract the table lines (skip the first line with column names)
  if(i < length(table_indices)){
    table1 <- lines[table_indices[i]:(table_indices[i+1]-1)]   
  } else if(i == length(table_indices)) {
    table1 <- lines[table_indices[i]:length(lines)]  # From the current index to the end of lines
    
  }
  # Clean the lines (remove unwanted characters like '*', 'X', etc.)
  cleaned_lines <- gsub("[*X#]", "", table1)
  
  if(i <3){
    # Split by whitespace, keeping columns separated by space
    table_df <- cleaned_lines %>%
      str_squish() %>% 
      str_replace("([A-Za-z,]+\\s[A-Za-z,]+)\\s(\\.)", "\\1|\\2") %>%  # Replace space before BA with a pipe
      str_split_fixed("\\|", 2)  # Split only into two parts: Name and the rest
    
    # Convert to a data frame and split the rest of the data into columns
    df_table1 <- as.data.frame(table_df, stringsAsFactors = FALSE) %>%
      separate(V2, into = c("BA", "OtherData"), sep = " ", extra = "merge", fill = "right") 
    
    # Count the number of spaces in one of the rows to determine the number of columns
    num_columns <- max(str_count(df_table1$OtherData, " "), na.rm = T) + 1  # Add 1 because spaces separate columns
    
    # Dynamically generate column names like "V1", "V2", etc.
    col_names <- paste0("C", 1:num_columns)
    
    table_names <- c('vs_rhp', 'vs_lhp', 'vs_rhh', 'vs_lhh')
    
    df_table1 <- df_table1 %>%
      separate(OtherData, into = col_names, sep = " ") %>%
      rename(OPS = ncol(df_table1)) %>%
      mutate(across(2:ncol(.), ~ as.numeric(.)))%>%
      mutate(across(2:ncol(.), ~ as.numeric(.))) %>%
      filter(!grepl('PLAYER|STATS|TOTAL|\\d',V1),
             V1 !='')
    # Set column names
    colnames(df_table1) <- header  # Assign the first row as column names
    
  } else if(i >=3) {
    
    # Modify the code to split at the first occurrence of a numerical value
    table_df <- cleaned_lines %>%
      str_squish() %>%  # Remove extra spaces
      str_replace("([A-Za-z,\\s]+)(\\d)", "\\1|\\2") %>%  # Insert a pipe "|" before the first number
      str_split_fixed("\\|", 2)  # Split the string into two parts: name and the rest
    
    
    # Convert to a data frame and split the rest of the data into columns
    df_table1 <- as.data.frame(table_df, stringsAsFactors = FALSE) %>%
      separate(V2, into = c("G", "OtherData"), sep = " ", extra = "merge", fill = "right") 
    
    # Count the number of spaces in one of the rows to determine the number of columns
    num_columns <- max(str_count(df_table1$OtherData, " "), na.rm = T) + 1  # Add 1 because spaces separate columns
    
    # Dynamically generate column names like "V1", "V2", etc.
    col_names <- paste0("C", 1:num_columns)
    
    table_names <- c('vs_rhp', 'vs_lhp', 'vs_rhh', 'vs_lhh')
    
    df_table1 <- df_table1 %>%
      separate(OtherData, into = col_names, sep = " ") %>%
      rename(OPS = ncol(df_table1)) %>%
      mutate(across(2:ncol(.), ~ as.numeric(.))) %>%
      filter(!grepl('PLAYER|STATS|TOTAL|\\d',V1),
             V1 !='')
    # Set column names
    colnames(df_table1) <- header 
    
    
    
  }
  assign(table_names[i], 
         df_table1 
  )
  
}

rm(cleaned_lines, table_df, webpage,df_table1, header, #vs_lhh, vs_lhp, vs_rhh, vs_rhp,
   table_indices, table_indices_name, num_columns,lines,i, content, col_names, response,
   table1, table_names)
