library(dplyr)
library(rvest)
library(stringr)
library(tidyr)
library(tictoc)
db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

# URL of the webpage
url <- "https://baseball.pointstreak.com/boxscore.html?gameid=611879"

# Read the HTML content from the webpage
page <- read_html(url)

# Define the CSS selector for the table
css_selector <- "#ps-content-bootstrapper > div:nth-child(1) > div > div.col-md-10 > div:nth-child(3) > div:nth-child(3) > table"

# Extract table content using CSS selector
table_content <- page %>% html_nodes(css_selector) %>% html_table(fill = T) %>% .[[1]]

# Print the table content
print(table_content)

tbl <- table_content %>%
  filter(!grepl('LOB', X1)) %>%
  rename(Player = 1, Event = 2, Team = 3, Inning = 4) %>%
  select(1:4) %>%
  mutate(Inning = ifelse(grepl('Top of|Bottom of', Player), Player, NA),
         Team = ifelse(!grepl(',|\\.|-|\\(', Event), Event, NA)
  ) %>%
  fill(Team, Inning) %>%
  filter(Player != Inning & Player != Team) %>%
  mutate(Player = str_squish(gsub("[#0-9]", "", Player))) 




# aa <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/american_association.sqlite")

# ------------------ SCRAPE ALL BOX SCORES FULL SEASON--------------------------

# key words to find in a string later
key_plays <- c("1B", "2B", "3B", "FC", "K", "SAC")
events_to_check <- c("SB", "CS", "WP", "PB", "E", "balk", "out", "out-asst")

# play keywords
play_verbs <- paste(c('single', 'double', 'triple', 'home run', 'fly out', "fielder's choice", 'error by', 'SH', 'walk',
                      'strike out', 'putout'), collapse = '|')
# pitch key words
pitch_verbs <- paste(c('Ball', 'Strike', 'Foul'), collapse = '|')
#runner advance key words
adv_verbs <- paste(c('stolen', "DI", 'balk', 'caught', 'advances', 'pass ball', 'Scores'), collapse = '|')
# other
non_verbs <- paste(c('Substitution', 'Pickoff'), collapse = '|')

patterns_to_skip <- c("player_sub", "pickoff", "runner_adv")

# dates for season FULL SEASON SCRAPE
game_dates <- unique(
  format(
    as.Date(
      unique(
        
        read_html('https://baseball.pointstreak.com/schedule.html?leagueid=193&seasonid=33873') %>%
          html_table(fill = T) %>% .[[1]] %>%
          mutate(dt = as.Date(Date, "%b %d"),
                 dt = as.Date(gsub("2025-",'2024-',dt))) %>%
          filter(dt < Sys.Date())
      )$dt), "%m/%d/%Y")
)


game_dates
format(as.Date('2024-05-09'), "%m/%d/%Y")

pbp <- data.frame()
box_master <- data.frame()
bullpen_log <- data.frame()
# main site url
site_url <- 'https://baseball.pointstreak.com/'
{
  tic()
  # date <- game_dates[1]
  for (date in game_dates) {
    
    cat(date, sep = '\n')
    # empty scoreboard url 
    scoreboard_url <- 'https://baseball.pointstreak.com/scoreboard.html?leagueid=193&seasonid=33873&date='
    
    # date_url <- 'https://baseball.pointstreak.com/scoreboard.html?leagueid=193&seasonid=33873&date=08/21/2024'
    date_url <- paste0(scoreboard_url, date)
    
    # read webpage
    webpage <- read_html(date_url)
    
    # Extract the specific section where it has the scores of the days games. web page also contains a section for recent games
    specific_section <- webpage %>%
      html_node(css = "#ps-content-bootstrapper > div > div > div.col-md-10 > div:nth-child(3)")
    
    # Extract all links within that section
    links <- specific_section %>%
      html_nodes("a") %>%
      html_attr("href")
    
    # filter links containing the word boxscore
    links <- unique(links[grepl("boxscore\\.", links)])
    
    
    if(length(links) > 0) {
      # create the boxscores data frame for that date
      boxscores <- data.frame(url = links, date = NA,
                              home = NA, away = NA, game_id = str_extract(links, "(?<=gameid=).*"),
                              game_id_long = str_extract(links, "(?<=gameid=).*"), umpire = NA)
      
      
      # i <-2
      for (i in seq_along(links)) {
        
        # boxscore trimmed url
        box_short_url <- links[i]
        
        # full url
        box_url <- paste0(site_url,links[i])
        
        # game id from boxscore url
        gm_id <- str_extract(box_short_url, "(?<=gameid=).*")
        
        # Read the HTML content from the webpage
        page <- read_html(box_url)
        
        # date of game from webpage
        game_date <- page %>%
          html_node(css = "#ps-content-bootstrapper > div:nth-child(1) > div > div.col-md-10 > div:nth-child(3) > div:nth-child(3) > ul:nth-child(2) > li:nth-child(1)") %>%
          html_text(trim = TRUE)
        
        game_date <- as.character(as.Date(str_squish(gsub('Date:', '', game_date)), "%m/%d/%Y"))
        
        boxscores$date <- ifelse(box_short_url == boxscores$url, game_date, boxscores$date)
        
        # Scrape the umpire name
        umpire <- page %>%
          html_node(css = "#ps-content-bootstrapper > div:nth-child(1) > div > div.col-md-10 > div:nth-child(3) > div:nth-child(3) > ul:nth-child(4)") %>%
          html_text(trim = TRUE)
        
        umpire <-str_squish(gsub("Plate Umpire:","",(str_extract(umpire, "^[^\n]*")) ))
        
        boxscores$umpire <- ifelse(box_short_url == boxscores$url, umpire, boxscores$umpire)
        
        
        ## pitcher bullpen log ----
        home_team <- page %>% 
          html_nodes('#ps-content-bootstrapper > div:nth-child(1) > div > div.col-md-10 > div.row.nova-heading > div.col-sm-12 > h2 > span.nova-title') %>% 
          html_text() %>%
          str_trim() %>%
          sub(".*vs\\s*", "", .)%>%
          str_trim()  %>%
          sub("\\s*:.*", "", .) %>%
          str_trim()
        
        away_team <- page %>% 
          html_nodes('#ps-content-bootstrapper > div:nth-child(1) > div > div.col-md-10 > div.row.nova-heading > div.col-sm-12 > h2 > span.nova-title') %>% 
          html_text() %>%
          str_trim()  %>%
          sub("\\s*vs.*", "", .) %>%
          str_trim()
        
        pitcher_away_stats <-  
          page %>%
          html_nodes('#ps-content-bootstrapper > div:nth-child(1) > div > div.col-md-10 > div:nth-child(3) > div:nth-child(3) > div:nth-child(13) > div:nth-child(1) > table')%>%
          html_table(fill = T) %>% 
          .[[1]]%>%                  # Remove the second row
          setNames(as.character(.[2,])) %>%
          rename(Team = 10) %>%
          mutate(Date = game_date,
                 Team = away_team,
                 .before = 1
          ) %>%
          relocate(Team, .after = Date) %>%
          filter(Team != Pitcher) %>%
          filter(!Pitcher %in% c("", 'Pitcher')) %>%
          select(-`#`)  %>% 
          slice(-1) %>%
          mutate(pitcherid = page %>%
                   html_nodes('#ps-content-bootstrapper > div:nth-child(1) > div > div.col-md-10 > div:nth-child(3) > div:nth-child(3) > div:nth-child(13) > div:nth-child(1) > table a') %>% # Select all <a> tags within the table
                   html_attr('href')%>%
                   sub(".*playerid=\\s*", "", .) %>%
                   sub("\\s*&.*", "", .) %>%
                   as.numeric()
          )
        
        
        
        pitch_count_away <- page %>% 
          html_nodes('#ps-content-bootstrapper > div:nth-child(1) > div > div.col-md-10 > div:nth-child(3) > div:nth-child(3) > div:nth-child(14) > div:nth-child(1) > ul > li:nth-child(3)') %>% 
          html_text() %>%
          gsub("P-S: ", "", .) %>% 
          strsplit(split = ",") %>% 
          unlist() %>% 
          str_squish() %>%
          as.data.frame() %>% 
          rename(Pitcher = 1) %>%
          separate(Pitcher, into = c('Pitcher', 'count'), sep = " ") %>%
          separate(count, into = c('P', 'S'), sep = "-") %>%
          mutate(across(c('P','S'), ~ as.numeric(gsub("\\.","",.)))) %>%
          mutate(B = P - S,
                 Pitcher = gsub("^(.*)\\.(.*)$", "\\2, \\1", Pitcher))
        
        away_bullpen <- pitcher_away_stats %>%
          left_join(pitch_count_away, by = 'Pitcher') %>%
          mutate(GameID = gm_id)
        
        
        
        pitcher_home_stats <-  
          page %>% 
          html_nodes('#ps-content-bootstrapper > div:nth-child(1) > div > div.col-md-10 > div:nth-child(3) > div:nth-child(3) > div:nth-child(13) > div:nth-child(2) > table')%>% 
          html_table(fill = T) %>% 
          .[[1]]%>%                  # Remove the second row
          setNames(as.character(.[2,])) %>%
          rename(Team = 10) %>%
          mutate(Date = game_date,
                 Team = home_team,
                 .before = 1
          ) %>%
          relocate(Team, .after = Date) %>%
          filter(Team != Pitcher) %>%
          filter(!Pitcher %in% c("", 'Pitcher')) %>%
          select(-`#`) %>% slice(-1)  %>%
          mutate(pitcherid = page %>%
                   html_nodes('#ps-content-bootstrapper > div:nth-child(1) > div > div.col-md-10 > div:nth-child(3) > div:nth-child(3) > div:nth-child(13) > div:nth-child(2) > table a') %>% # Select all <a> tags within the table
                   html_attr('href')%>%
                   sub(".*playerid=\\s*", "", .) %>%
                   sub("\\s*&.*", "", .) %>%
                   as.numeric()
          )
        
        pitch_count_home <- page %>% 
          html_nodes('#ps-content-bootstrapper > div:nth-child(1) > div > div.col-md-10 > div:nth-child(3) > div:nth-child(3) > div:nth-child(14) > div:nth-child(2) > ul > li:nth-child(3)') %>% 
          html_text() %>%
          gsub("P-S: ", "", .) %>% 
          strsplit(split = ",") %>% 
          unlist() %>% 
          str_squish() %>%
          as.data.frame() %>% 
          rename(Pitcher = 1) %>%
          separate(Pitcher, into = c('Pitcher', 'count'), sep = " ") %>%
          separate(count, into = c('P', 'S'), sep = "-") %>%
          mutate(across(c('P','S'), ~ as.numeric(gsub("\\.","",.)))) %>%
          mutate(B = P - S,
                 Pitcher = gsub("^(.*)\\.(.*)$", "\\2, \\1", Pitcher))
        
        
        home_bullpen <- pitcher_home_stats %>%
          left_join(pitch_count_home, by = 'Pitcher')%>%
          mutate(GameID = gm_id) 
        
        bullpen_log <- rbind(bullpen_log, home_bullpen, away_bullpen) %>%
          group_by(GameID, Team) %>%
          mutate(Role = ifelse(row_number() == 1, 'SP', 'RP'))
        
        # get the away team name
        away_tm <- page %>%
          html_table(fill = T) %>% .[[2]] %>%
          setNames(ifelse(names(.) == "", paste0("V", seq_along(names(.))), names(.))) %>%
          select(1) %>%
          rename(Team = 1) %>%
          slice(1)  %>%
          pull(Team)
        
        boxscores$away <- ifelse(box_short_url == boxscores$url, away_tm, boxscores$away)
        
        # get the home team name
        
        home_tm <- page %>%
          html_table(fill = T) %>% .[[2]] %>%
          setNames(ifelse(names(.) == "", paste0("V", seq_along(names(.))), names(.))) %>%
          rename(Team =1) %>%
          slice(2)  %>%
          select(1) %>%
          pull(Team)
        
        boxscores$home <- ifelse(box_short_url == boxscores$url, home_tm, boxscores$home)
        
        # longer game id for quicker ref
        temp_id <- paste(home_tm,"vs",away_tm, sep = '_')
        
        boxscores$game_id_long <- ifelse(box_short_url == boxscores$url, paste0(boxscores$game_id_long,"_",temp_id), boxscores$game_id_long)
        
        # Define the CSS selector for the table
        css_selector <- "#ps-content-bootstrapper > div:nth-child(1) > div > div.col-md-10 > div:nth-child(3) > div:nth-child(3) > table"
        
        tables<- page %>% html_nodes(css_selector) %>% html_table(fill = T)
        
        box_master <- rbind(box_master, boxscores) %>% distinct(.keep_all = T) %>% filter(!is.na(date))
        
        
        if(length(tables) > 0 && nrow(tables[[1]]) > 0) {
          
          # Extract table content using CSS selector
          table_content <- page %>% html_nodes(css_selector) %>% html_table(fill = T) %>% .[[1]]
          
          # pbp data
          tbl <- table_content %>%
            filter(!grepl('LOB', X1)) %>%
            rename(Player = 1, Event = 2, Team = 3, Inning = 4) %>%
            select(1:4) %>%
            mutate(Inning = ifelse(grepl('Top of|Bottom of', Player), Player, NA),
                   Team = ifelse(!grepl(',|\\.|-|\\(', Event), Event, NA)
            ) %>%
            fill(Team, Inning) %>%
            filter(Player != Inning & Player != Team) %>%
            mutate(Player = str_squish(gsub("[#0-9]", "", Player)),
                   Date = game_date,
                   Umpire = umpire,
                   GameID = gm_id,
                   GameID_long = paste(GameID,temp_id, sep = '_'),
                   Opponent = NA,
                   PlayNo = row_number()
            ) %>%
            select(Date, PlayNo, Inning, Team, Opponent, Player, Event, Umpire, GameID, GameID_long) 
          
          teams <- unique(tbl$Team)
          
          tbl$Opponent <- ifelse(tbl$Team == teams[1], teams[2], teams[1])
          
          pbp <- rbind(pbp, tbl)
        }
      }
      
    }
    cat(date, sep = "\n", append = F)
  }
  
  
  pbp_clean <- pbp %>%
    mutate(Event = str_squish(trimws(Event)),
           Event = str_squish(if_else(str_detect(Event, "^\\d"), str_sub(Event, 3, str_length(Event)), Event)),
           Result = gsub("\\(|\\)","", str_extract(Event, "\\([^\\)]*\\)[^\\(]*$")),
           type = case_when (
             grepl(play_verbs, Event) ~ 'play',
             grepl(pitch_verbs, Event) ~ 'pitch',
             grepl(adv_verbs, Event) & !grepl(pitch_verbs, Event) & !grepl(play_verbs, Event)~ 'runner_adv',
             grepl('Pickoff', Event) ~ 'pickoff',
             grepl('Substituion|runs for|subs', Event) ~ 'player_sub',
             T ~ NA
           )
    ) %>%
    group_by(Date, GameID) %>%
    mutate(
      PAofGame = cumsum((!duplicated(Player) | lag(Player) != Player) & type != 'player_sub'),
      Result = case_when(
        type == 'play' & nchar(Result) <= 2 ~ str_extract(Event, play_verbs),
        type == 'play' & grepl("fielder's choice", Event) ~ 'FC',
        T ~ Result,
      ),
      
      Event_2 = str_squish(
        str_replace_all(Event, c(
          'home run' = 'HR', 'triple' = '3B', 'double' = '2B', 'single' = '1B',
          'intentional walk' = 'IBB', 'walk' = 'BB', 'hit by pitch' = 'HBP',
          'fielders choice' = 'FC', "fielder's choice" = 'FC', 'catchers interference' = 'CI',
          "catcher's interference" = 'CI',
          'reached first on a wild pitch' = 'K-WP',
          'struck out, reached first on a passed ball' = 'K-PB',
          'struck out swinging, reached first on a passed ball' = 'K-PB',
          'struck out lookingDropped' = 'K-WP',
          'struck out, reached first' = 'K-WP',
          'BB, grounded out to 1b unassisted\\. \\(1 out\\)' = 'BB',
          'Dropped foul ball, E5, flied out to\\. \\(1 out\\)' = 'reached on E',
          'Dropped foul ball, E3struck out looking' = 'K-WP', 'error by' = 'E',
          "reached first on a dropped fly by ss" = 'E',
          'to right field' = '', 'to center field' = '', 'to left field' = '',
          'for out number 1' = '', 'for out number 2' = '', 'for out number 3' = '',
          'strike out swinging' = 'out', 'strike out' = 'out', 
          '6-3|5-3|4-3|1-3|3U|3-1|3-4|2-4|P2|P1|P3|P4|P5|P6|L1|L2|L3|L4|L5|L6' = 'out',
          'fly out|ground out|putout|put out' = 'out', 'SH' = 'SAC'
        )
        )
      ) ,
      Result = gsub("\\(|\\)","", str_extract(Event_2, "\\([^\\)]*\\)[^\\(]*$")),
      
      test = str_extract_all(Event, "\\(([^)]+)\\)") %>%
        sapply(paste, collapse = ", ") %>%
        gsub("\\(\\d+\\)", "", .) %>%
        str_squish() %>%
        gsub(", , ,|, ,", ",", .) %>%
        gsub("\\(OBR\\)", "(ITB)", .) %>%
        gsub("\\(CO\\)", "(CI)", .) %>%
        gsub("\\(stolen base[^)]*\\)|\\(stolen base\\)", "(SB)", .) %>%
        # gsub("\\(.*Sb[^)]*\\)", "(SB)", .) %>%
        gsub("\\(intentional walk\\)", "(BB)", .) %>%
        gsub("\\(wild pitch[^)]*\\)|\\(wild pitch\\)", "(WP)", .) %>%
        # gsub("\\(.*WP[^)]*\\)", "(WP)", .) %>%
        gsub("\\(walk\\)", "(BB)", .) %>%
        gsub("\\(home run\\)", "(HR)", .) %>%
        gsub("\\(hit by pitch[^)]*\\)", "(HBP)", .) %>%
        gsub("\\(pass ball[^)]*\\)", "(PB)", .) %>%
        gsub("\\(fielder's choice\\)", "(FC)", .) %>%
        gsub("\\(strike out swinging\\)", "(K)", .) %>%
        gsub("\\(strike out\\)", "(K)", .) %>%
        gsub("^\\s*,|,\\s*$", "", .) %>%
        gsub("\\(\\d+-\\d+\\)|\\(out in foul territory\\)", "(out)", .) %>%
        gsub("\\d+-\\d+ DP", "(DP)", .) %>%
        gsub("\\d+-\\d+ Dp", "(DP)", .) %>%
        gsub("\\([^)]*DP[^)]*\\)|\\([^)]*Dp[^)]*\\)", "(DP)", .) %>%
        gsub("\\(\\d+-\\d+-\\d+ DP\\)", "(DP)", .) %>%
        gsub("\\d+-\\d+-\\d+-\\d+-\\d+-\\d+", "(out)", .) %>%
        gsub("\\d+-\\d+-\\d+-\\d+-\\d+", "(out)", .) %>%
        gsub("\\d+-\\d+-\\d+-\\d+", "(out)", .) %>%
        gsub("\\d+-\\d+-\\d+", "(out)", .) %>%
        gsub("\\(\\d+-\\d+-\\d+\\)", "(out)", .) %>%
        gsub("\\(\\d+U\\)|\\(U\\d+\\)", "(out)", .) %>%
        gsub("\\(\\d+ U\\)", "(out)", .) %>%
        gsub("\\(\\d+UA\\)", "(out)", .) %>%
        gsub("\\(\\d+ UA\\)", "(out)", .) %>%
        gsub("\\(\\d+un\\)", "(out)", .) %>%
        gsub("\\(\\d+u\\)", "(out)", .) %>%
        gsub("\\((P\\d+|L\\d+|\\d+ L|FO\\d+)\\)", "(out)", .) %>%
        gsub("\\((P \\d+|L \\d+|FO \\d+)\\)", "(out)", .) %>%
        gsub("\\(single[^)]+\\)|\\(single\\)", "(1B)", .) %>%
        gsub("\\(double[^)]+\\)|ground rule double|\\(double\\)", "(2B)", .) %>% 
        gsub("\\(triple[^)]+\\)|\\(triple\\)", "(3B)", .) %>%
        gsub("\\(error by the[^)]+\\)|\\(^E[^)]*\\)", "(E)", .) %>%
        gsub("\\(E-[^)]+\\)|\\(e[^)]*\\)|\\(E[^)]*\\)", "(E)", .) %>%
        gsub("\\(fly out[^)]+\\)", "(out)", .) %>%
        gsub("\\(dropped 3rd strike: KS[^)]+\\)", "(K)", .) %>%
        gsub("\\(dropped 3rd strike\\)", "(K-WP)", .) %>%
        gsub("\\(sacrifice [^)]+\\)", "(SAC)", .) %>%
        gsub("\\(pitcher to[^)]+\\)", "(POA)", .) %>%
        gsub("\\(catcher to[^)]+\\)", "(POA)", .) %>%
        gsub("\\(caught stealing[^)]+\\)", "(CS)", .) %>%
        gsub("\\(CS E[^)]+\\)", "(CS)", .) %>%
        gsub("\\(Picked[^)]+\\)", "(PO)", .) %>%
        gsub("\\(.*SAC[^)]*\\)", "(SAC)", .) %>%
        gsub("\\(.*Sac[^)]*\\)", "(SAC)", .) %>%
        gsub("\\(.*SH[^)]*\\)", "(SAC)", .) %>%
        gsub("\\(.*SF[^)]*\\)", "(SAC)", .) %>%
        gsub("\\(.*DI[^)]*\\)", "(DI)", .) %>%
        gsub("\\(.*FC[^)]*\\)", "(FC)", .) %>%
        gsub("\\(\\d+L|\\d+ F|G[^ ]*|\\d+G|[^ ]*IF[^ ]*\\)", "(out)", .) %>%
        gsub("\\(.*BI[^)]*\\)|\\(.*INT[^)]*\\)", "(out)", .) %>%
        gsub("\\(.*INF[^)]*\\)", "(out)", ., ignore.case = TRUE) %>%
        gsub("\\(\\d+F{1,2}|\\d+ P|PF[^ ]*\\)", "(out)", .) %>%
        gsub("\\(PO\\s*\\d+\\)", "(PO)", .) %>%
        gsub("\\(UUU| 21|p6|POout|S\\(out\\)", "(PO)", .) %>%
        gsub("\\((?!FC)[fF][^)]*\\)", "(out)", ., perl = TRUE) %>%
        gsub("\\(out[^)]*\\)", "(out)", .) %>%
        gsub("\\([^)]*-+[^)]*\\)", "(out)", .) %>%
        gsub("\\(\\d+ \\(out\\)", "(out)", .) %>%         
        gsub("\\(\\d+\\(out\\)", "(out)", .) %>%          
        gsub("\\(\\d+ WP\\)", "(WP)", .) %>%           
        gsub("\\(di\\)", "(DI)", .) %>%                  
        gsub("\\(IF[^)]*\\)", "(out)", .) %>%        
        gsub("\\(L\\d+[^)]*\\)", "(out)", .) %>%     
        gsub("\\(P\\d+[^)]*\\)", "(out)", .) %>%          
        gsub("\\(Sb \\d+\\)", "(SB)", .) %>%          
        gsub("\\(SP[^)]*\\)", "(SB)", .)  %>%
        gsub("\\(DP\\) L\\)", "(DP)", .)  %>%
        gsub("\\(out\\)-\\)", "(out)", .)  %>%
        gsub("\\(out\\) CS\\)", "(CS)", .)  %>%
        gsub("\\(out\\) OA\\)", "(out-asst)", .)  %>%
        gsub("\\(PO\\(out\\)", "(out)", .)  %>%
        # gsub("\\(.*\\(out\\)[^)]*//)", "(out)", .) %>%
        gsub("^,+|,+$", "", .) %>%
        gsub(", , ,|, ,", ",", .) %>%
        gsub("\\(\\(", "(", .) %>%
        gsub("\\)\\)", ")", .) %>%
        gsub("\\(out\\)KO\\)", "(out)", .)  %>%
        gsub("\\(out\\)O\\)", "(out)", .)  %>%
        gsub("\\(caught stealing: CS 2-4", "(CS)", .)  %>%
        str_trim(.) %>%
        str_squish(.),
      test = if_else(
        str_ends(test, ","),
        str_remove(test, ",$"),
        test
      ),
      test = if_else(
        str_starts(test, ","),
        str_remove(test, "^,"),
        test
      ),
      test = str_squish(gsub("\\(|\\)","",test)),
      
      
      lastplay = if_else(
        str_detect(test, ","),
        str_squish(str_extract(test, "(?<=,)[^,]+$")),
        test
      ),
      
      desired_result = purrr::map_chr(test, ~{
        # Find the last key play in the test string
        last_key_play <- last(str_extract_all(.x, paste(key_plays, collapse = "|"))[[1]])
        
        # Extract the part of the string after the last key play
        if (!is.na(last_key_play)) {
          after_key_play <- str_trim(str_split(.x, last_key_play)[[1]][2])
          
          # Check if any of the events_to_check are present after the last key play
          if (any(str_detect(after_key_play, paste(events_to_check, collapse = "|")))) {
            return(last_key_play)
          }
        }
        
        # Default to the current desired_result if conditions are not met
        return(NA_character_)
      }),
      desired_result = ifelse(is.na(desired_result), lastplay, desired_result),
      
      filtercol = lastplay == desired_result,
      RunsonPlay =  str_count(Event, "Scores") + str_count(Event, "scored"),
      OutsonPlay = str_count(Event, "out number"),
      runner_adv = grepl(adv_verbs,Event),
      desired_result = str_replace_all(desired_result, c(
        'CI' = 'E', 'DP' = 'out', 'K' = 'out'
      )),
    )   %>%
    ungroup() 
  
  # # # # # # # # # ## # # # # # # # # # # # # # # # 
  # select(-c(Umpire, Opponent, GameID_long)) %>%
  # select (Inning, Player, Event, test, lastplay, desired_result,filtercol)
  # # # # # # # # # ## # # # # # # # # # # # # # # # 
  
  
  # unique_values <- run_ex %>%
  #   pull(test) %>%                            # Extract the column as a vector
  #   # str_extract_all("\\(([^)]+)\\)") %>%       # Extract all text within parentheses
  #   unlist() %>%                               # Unlist the results into a single vector
  #   str_split(",\\s*") %>%                     # Split the text by commas and optional spaces
  #   unlist() %>%                               # Unlist the split results into a single vector
  #   trimws() %>%                               # Trim any leading or trailing whitespace
  #   unique() %>%                               # Get unique values
  #   sort()  
  # 
  # unique_values
  # 
  # 
  # %>%
  
  # Specify the relevant columns
  relevant_columns <- c("r1_name", "r2_name", "r3_name", "rScore_name")
  
  run_ex <- pbp_clean  %>%
    group_by(Inning,Team,GameID) %>%
    mutate(InningPA = cumsum((!duplicated(Player) | lag(Player) != Player) & type != 'player_sub')) %>%
    mutate(TotalRunsInning = sum(RunsonPlay),
           Runs_upto = lag(cumsum(RunsonPlay)),
           Runs_upto = ifelse(is.na(Runs_upto), 0, Runs_upto),
           Outs = lag(cumsum(OutsonPlay)),
           Outs = ifelse(is.na(Outs), 0, Outs)
    )  %>% 
    mutate(
      r1_name = case_when(
        str_detect(Event,"advances to 1st")==T   ~
          word(str_extract(Event, "\\b\\w+ \\w+ (?=advances to 1st)"), 1, 2)
      ),
      r2_name = case_when(
        str_detect(Event,"advances to 2nd")==T   ~
          word(str_extract(Event, "\\b\\w+ \\w+ (?=advances to 2nd)"), 1, 2)
      ),
      r3_name = case_when(
        str_detect(Event,"advances to 3rd")==T   ~
          word(str_extract(Event, "\\b\\w+ \\w+ (?=advances to 3rd)"), 1, 2)
      ),
      rScore_name = case_when(
        str_detect(Event,"Scores")==T   ~
          sapply(str_extract_all(Event, "\\b\\w+ \\w+ (?=Scores)"), function(x) paste(x, collapse = ", "))
      ),
      
      across(c(r1_name, r2_name, r3_name, rScore_name), ~ str_squish(.))
      
    ) %>%
    group_by(GameID, Inning)  %>%
    mutate(
      r1_name = case_when(
        r1_name == r2_name | r1_name == r3_name | str_detect(rScore_name, fixed(r1_name)) ~ NA, 
        T~ r1_name
      ),
      r2_name = case_when(
        r2_name == r1_name ~ NA,
        r2_name == r3_name ~ NA,
        str_detect(rScore_name, fixed(r2_name)) ~ NA, 
        T~ r2_name
      ),
      r3_name = case_when(
        r3_name == r1_name | r3_name == r2_name | str_detect(rScore_name, fixed(r3_name)) ~ NA, 
        T~ r3_name
      )
    ) %>%
    mutate(
      r1_name = case_when(is.na(r1_name) & runner_adv == F ~ lag(r1_name), T ~ r1_name),
      r1_name = case_when(is.na(r1_name) & runner_adv == F ~ lag(r1_name), T ~ r1_name),
      r1_name = case_when(is.na(r1_name) & runner_adv == F ~ lag(r1_name), T ~ r1_name),
      r1_name = case_when(is.na(r1_name) & runner_adv == F ~ lag(r1_name), T ~ r1_name),
      r2_name = case_when(is.na(r2_name) & runner_adv == F ~ lag(r2_name), T ~ r2_name),
      r2_name = case_when(is.na(r2_name) & runner_adv == F ~ lag(r2_name), T ~ r2_name),
      r2_name = case_when(is.na(r2_name) & runner_adv == F ~ lag(r2_name), T ~ r2_name),
      r2_name = case_when(is.na(r2_name) & runner_adv == F ~ lag(r2_name), T ~ r2_name),
      r3_name = case_when(is.na(r3_name) & runner_adv == F ~ lag(r3_name), T ~ r3_name),
      r3_name = case_when(is.na(r3_name) & runner_adv == F ~ lag(r3_name), T ~ r3_name),
      r3_name = case_when(is.na(r3_name) & runner_adv == F ~ lag(r3_name), T ~ r3_name),
      r3_name = case_when(is.na(r3_name) & runner_adv == F ~ lag(r3_name), T ~ r3_name),
      
    ) %>%
    # select(Date, GameID, Inning, Event, desired_result, Player, r1_name,r2_name, r3_name, rScore_name) %>%
    #View()
    ungroup() %>%
    group_by(GameID, Inning) %>%
    mutate(
      r1 = NA,
      r2 = NA,
      r3 = NA,
      r1_end = ifelse(is.na(r1_name), 0, 1),
      r2_end = ifelse(is.na(r2_name), 0, 2),
      r3_end = ifelse(is.na(r3_name), 0, 3),
      r1 = lag(r1_end, default = 0),
      r2 = lag(r2_end, default = 0),
      r3 = lag(r3_end, default = 0),
      r1_end = if_else(row_number() == n() & grepl('out',Event), 0, r1_end),
      r2_end = if_else(row_number() == n() & grepl('out',Event), 0, r2_end),
      r3_end = if_else(row_number() == n() & grepl('out',Event), 0, r3_end),
      r1 = if_else(InningPA == 0 | InningPA == 1, 0, r1),
      r2 = if_else(InningPA == 0 | InningPA == 1, 0, r2),
      r3 = if_else(InningPA == 0 | InningPA == 1, 0, r3),
    ) %>%
    mutate(
      Runners = paste0(r1,r2,r3),
      Runners_end = paste0(r1_end,r2_end,r3_end)
    )%>%
    # select(-c(R1,R2,R3,R1_end,R2_end,R3_end)) %>%
    select("Date", "PlayNo","PAofGame", "Inning", "InningPA", "Team", "Opponent", "Player", "Event", # Event_2, Result,
           "type",  "desired_result", "Runners", "Runners_end",  r1_name, r2_name, r3_name, rScore_name,
           "RunsonPlay", "OutsonPlay",  "Outs",
           "TotalRunsInning", "Runs_upto", "Umpire", "GameID", "GameID_long") %>%
    mutate(desired_result =
             str_replace_all(desired_result, c(
               'SAC' = 'out', "DP" = 'out' )),
           Runners = gsub("NA",'0',Runners)
    ) 
  
  
  dput(colnames(run_ex))
  run_ex$Event[6]
  
  sort(unique(run_ex$desired_result))
  sort(unique(run_ex$Runners))
  
  # run_ex <- dbGetQuery(aa,'select * from pbp_24')
  
  run_matrix <- run_ex %>%
    ungroup() %>%
    filter(desired_result!='' & Outs < 3) %>%
    # filter(grepl("1B|2B|3B|HR|HBP|BB|out", desired_result)) %>%
    group_by(Outs, Runners) %>%
    summarise(RE = round(mean(TotalRunsInning - Runs_upto), 3)) %>%
    mutate(Runners = factor(Runners, levels = c('000', '100', '020', '120', '003', '103', '023', '123'))) %>%
    arrange(Runners) %>%
    pivot_wider(names_from = Outs, values_from = RE)
  
  run_matrix_long <- run_ex %>%
    ungroup()%>%
    filter(desired_result!='' & Outs < 3) %>%
    group_by(Outs, Runners) %>%
    summarise(RE = round(mean(TotalRunsInning - Runs_upto), 3)) %>%
    mutate(Runners = factor(Runners, levels = c('000', '100', '020', '120', '003', '103', '023', '123')),
           State = paste(Outs, Runners)) %>%
    arrange(State) %>%
    ungroup() %>%
    select(-Outs, -Runners) 
  
  
  pbp_2 <- run_ex %>%
    # dbGetQuery(aa,'select * from pbp_24') %>%
    filter(grepl("1B|2B|3B|HR|HBP|BB|out", desired_result)) %>%
    mutate(State = paste(Outs, Runners),
           NewState = ifelse(Outs + OutsonPlay < 3, Outs + OutsonPlay , 0),
           NewState = paste(NewState, Runners_end),
           RE = run_matrix_long$RE[match(State, run_matrix_long$State)],
           .after = Event)%>%
    select(-Runners, -Outs, -Runners_end) %>%
    group_by(Inning, Team) %>%
    mutate(RE_end =ifelse(grepl('out number 3', Event) & desired_result == 'out', 0.00,  run_matrix_long$RE[match(NewState, run_matrix_long$State)]),
           RE_diff = RE_end - RE + RunsonPlay, .after = RE) %>%
    mutate(test = ifelse(InningPA == max(InningPA) & desired_result == 'out', 0.00, RE_end - RE + RunsonPlay)) %>%
    relocate(desired_result, .after = Event)
  
  run_matrix_pct <- run_ex %>%
    # dbGetQuery(aa,'select * from pbp_24') %>%
    filter(grepl("1B|2B|3B|HR|HBP|BB|out", desired_result)) %>%
    filter(Outs < 3) %>%
    group_by(Outs, Runners) %>%
    summarise(chances = n(),
              scored = sum(TotalRunsInning > 0 & Runs_upto< TotalRunsInning, na.rm = T)
    ) %>%
    mutate(Runners = factor(Runners, levels = c('000', '100', '020', '120', '003', '103', '023', '123')),
           Scoring_pct = round(scored / chances * 100, 1)) %>%
    arrange(Runners) %>%
    select(-chances, - scored) %>%
    pivot_wider(names_from = Outs, values_from = Scoring_pct)
  
  woba_scale_ <- dbGetQuery(db, 'select woba_scale from weights where SEASON = 2024;')$woba_scale
  
  weights <- pbp_2 %>%
    group_by(desired_result) %>%
    summarise(sum = sum(RE_diff, na.rm = TRUE),
              n = sum(RE_diff != 0, na.rm = TRUE)) %>%
    filter(desired_result %in% c('BB', 'HBP', '1B', '2B', '3B', 'HR', 'out')) %>%
    mutate(
      Result = factor(desired_result, levels = c('BB', 'HBP', '1B', '2B', '3B', 'HR', 'out')),
      weight = sum / n,
      new_weight = weight + abs(weight[Result == 'out']),
      scaled_weights = new_weight * woba_scale_) %>%
    arrange(desired_result)  %>%
    mutate(SEASON = 2025) %>%
    select(Result, sum, n, weight, new_weight, scaled_weights, SEASON)
  toc()
  
  dbExecute(db, 'DELETE FROM weights_raw WHERE SEASON = 2025;')
  
  dbWriteTable(db, name = 'weights_raw', value = weights, append = T)
  
  run_matrix <- run_matrix %>%
    mutate(SEASON = 2025, .before = 1)%>%
    rename(X0 = 3,
           X1 = 4,
           X2 = 5)
}

dbWriteTable(db, name = 'pbp_25', value = run_ex, append = T)

dbExecute(db, 'DELETE FROM run_matrix WHERE SEASON = 2025;')

dbWriteTable(db, name = 'run_matrix', value = run_matrix, append = T)
# END HERE ##################################################################################################################################################### #
lg_batting <- dbGetQuery(db, 'select * from stats_hitting_league where SEASON = 2024')
lg_pitching <- dbGetQuery(db, 'select * from stats_pitching_league where SEASON = 2024')

wght <- t(weights %>% arrange(scaled_weights) %>% select (Result, scaled_weights) %>% filter(!Result %in% c('Out','out')))  %>%
  as.data.frame() %>%
  janitor::row_to_names(1) %>%
  mutate(lg_woba = lg_batting$wOBA,
         woba_scale = woba_scale_,
         runSB = .2,
         runCS = -(2 * (lg_pitching$R / (lg_pitching$IP*3)) + 0.075), 
         lg_r_pa = lg_batting$R / lg_batting$PA,
         lg_r_w = lg_pitching$R / lg_pitching$W,
         cFIP = lg_pitching$ERA - (((13* lg_pitching$HR) + (3*(lg_batting$BB+lg_batting$HBP)) - (2*lg_batting$K)) / lg_pitching$IP),
         SEASON = 2025
  ) %>%
  relocate(c(SEASON, lg_woba, woba_scale), .before = 1) %>%
  rename(wBB = BB,
         wHBP = HBP,
         w1B = `1B`,
         w2B = `2B`,
         w3B = `3B`,
         wHR = HR)

dbExecute(db, 'DELETE FROM weights WHERE SEASON = 2025;')

dbWriteTable(db, name = 'weights', value = wght, append = T)

# WRITE BULLPEN LOGS ----
# todays_game_ids <- 
existing_game_ids <- dbGetQuery(db, 'select * from bullpen_log')



# PITCH LEVEL DATA REDO ---- -------------------- ----

# # ways to reach first
# reach_first <- "1B|BB|IBB|HBP|FC|CI|advances to 1st"
# # ways to reach second
# reach_second <- "2B|advances to 2nd|stole second|placed on second|place on second"
# # ways to reach third
# reach_third <-"3B|advances to 3rd|stole third"
# # double
# hit_second <- '2B'
# # triple
# hit_third <- '3B'
# # home run
# hr <- 'HR'
# # reach base
# reach_all <- paste0(reach_first,"|",reach_second,"|",reach_third)
# 
# # hits
# hits <- c('single', 'double', 'triple', 'home run', 'putout')
# h_h <- paste(hits,collapse = '|')
# 
# pitch <- pbp_clean %>%
#   separate_rows(Event, sep = ",") %>%
#   mutate(Event = str_squish(trimws(Event)),
#          Event = str_squish(if_else(str_detect(Event, "^\\d"), str_sub(Event, 3, str_length(Event)), Event)),
#          Result = gsub("\\(|\\)","",str_extract(Event, "\\(.*?\\)")),
#          PAofGame = cumsum((!duplicated(Player) | lag(Player) != Player) & Player != 'Offensive Substitution'),
#          PitchNo = row_number(),
#          type = case_when (
#            grepl(play_verbs, Event) ~ 'play',
#            grepl(pitch_verbs, Event) ~ 'pitch',
#            grepl(adv_verbs, Event) & !grepl(pitch_verbs, Event) & !grepl(play_verbs, Event)~ 'runner_adv',
#            grepl('Pickoff', Event) ~ 'pickoff',
#            grepl('Substituion|runs for|subs', Event) ~ 'player_sub',
#            T ~ NA
#          )
#   ) %>%
#   relocate(c(Date, PitchNo, PAofGame), .before = 1) %>%
#   relocate(Umpire, GameID, GameID_long, Opponent, .after = ncol(.)) %>%
#   group_by(Inning)%>%
#   mutate(PAofInning = cumsum((!duplicated(Player) | lag(Player) != Player) & Player != 'Offensive Substitution'),.after = PAofGame) %>%
#   group_by(Inning, Player, PAofGame) %>%
#   mutate(PitchofPA = row_number(), .after = PAofInning) %>%
#   ungroup() %>%
#   mutate(
#     PitchNo = cumsum(!type %in% patterns_to_skip)  # Increment PitchNo for valid rows
#   ) %>%
#   mutate(
#     PitchNo = ifelse(type %in% patterns_to_skip, lag(PitchNo, default = first(PitchNo)), PitchNo)  # Repeat PitchNo for skipped rows
#   ) %>%
#   group_by(Inning, Player, PAofGame) %>%
#   mutate(
#     PitchofPA = cumsum(!type %in% patterns_to_skip)  # Increment PitchNo for valid rows
#   ) %>%
#   mutate(
#     PitchofPA = ifelse(type %in% patterns_to_skip, lag(PitchofPA, default = first(PitchofPA)), PitchofPA)  # Repeat PitchNo for skipped rows
#   ) %>% 
#   ungroup()  
# 
# tst <- pitch %>% 
#   ungroup() %>%
#   mutate(
#     Result_2 = Result ,
#     Result_2 = case_when (!is.na(Result_2) ~ str_extract_all(Result_2, "\\(([^)]+)\\)") %>%
#                             sapply(paste, collapse = ", ") %>%
#                             gsub("\\(\\d+\\)", "", .) %>%
#                             str_squish() %>%
#                             gsub(", , ,|, ,", ",", .) %>%
#                             gsub("\\(OBR\\)", "(ITB)", .) %>%
#                             gsub("\\(CO\\)", "(CI)", .) %>%
#                             gsub("\\(stolen base[^)]*\\)|\\(stolen base\\)", "(SB)", .) %>%
#                             # gsub("\\(.*Sb[^)]*\\)", "(SB)", .) %>%
#                             gsub("\\(intentional walk\\)", "(BB)", .) %>%
#                             gsub("\\(wild pitch[^)]*\\)|\\(wild pitch\\)", "(WP)", .) %>%
#                             # gsub("\\(.*WP[^)]*\\)", "(WP)", .) %>%
#                             gsub("\\(walk\\)", "(BB)", .) %>%
#                             gsub("\\(home run\\)", "(HR)", .) %>%
#                             gsub("\\(hit by pitch[^)]*\\)", "(HBP)", .) %>%
#                             gsub("\\(pass ball[^)]*\\)", "(PB)", .) %>%
#                             gsub("\\(fielder's choice\\)", "(FC)", .) %>%
#                             gsub("\\(strike out swinging\\)", "(K)", .) %>%
#                             gsub("\\(strike out\\)", "(K)", .) %>%
#                             gsub("^\\s*,|,\\s*$", "", .) %>%
#                             gsub("\\(\\d+-\\d+\\)|\\(out in foul territory\\)", "(out)", .) %>%
#                             gsub("\\d+-\\d+ DP", "(DP)", .) %>%
#                             gsub("\\d+-\\d+ Dp", "(DP)", .) %>%
#                             gsub("\\([^)]*DP[^)]*\\)|\\([^)]*Dp[^)]*\\)", "(DP)", .) %>%
#                             gsub("\\(\\d+-\\d+-\\d+ DP\\)", "(DP)", .) %>%
#                             gsub("\\d+-\\d+-\\d+-\\d+-\\d+-\\d+", "(out)", .) %>%
#                             gsub("\\d+-\\d+-\\d+-\\d+-\\d+", "(out)", .) %>%
#                             gsub("\\d+-\\d+-\\d+-\\d+", "(out)", .) %>%
#                             gsub("\\d+-\\d+-\\d+", "(out)", .) %>%
#                             gsub("\\(\\d+-\\d+-\\d+\\)", "(out)", .) %>%
#                             gsub("\\(\\d+U\\)|\\(U\\d+\\)", "(out)", .) %>%
#                             gsub("\\(\\d+ U\\)", "(out)", .) %>%
#                             gsub("\\(\\d+UA\\)", "(out)", .) %>%
#                             gsub("\\(\\d+ UA\\)", "(out)", .) %>%
#                             gsub("\\(\\d+un\\)", "(out)", .) %>%
#                             gsub("\\(\\d+u\\)", "(out)", .) %>%
#                             gsub("\\((P\\d+|L\\d+|\\d+ L|FO\\d+)\\)", "(out)", .) %>%
#                             gsub("\\((P \\d+|L \\d+|FO \\d+)\\)", "(out)", .) %>%
#                             gsub("\\(single[^)]+\\)|\\(single\\)", "(1B)", .) %>%
#                             gsub("\\(double[^)]+\\)|ground rule double|\\(double\\)", "(2B)", .) %>% 
#                             gsub("\\(triple[^)]+\\)|\\(triple\\)", "(3B)", .) %>%
#                             gsub("\\(error by the[^)]+\\)|\\(^E[^)]*\\)", "(E)", .) %>%
#                             gsub("\\(E-[^)]+\\)|\\(e[^)]*\\)|\\(E[^)]*\\)", "(E)", .) %>%
#                             gsub("\\(fly out[^)]+\\)", "(out)", .) %>%
#                             gsub("\\(dropped 3rd strike: KS[^)]+\\)", "(K)", .) %>%
#                             gsub("\\(dropped 3rd strike\\)", "(K-WP)", .) %>%
#                             gsub("\\(sacrifice [^)]+\\)", "(SAC)", .) %>%
#                             gsub("\\(pitcher to[^)]+\\)", "(POA)", .) %>%
#                             gsub("\\(catcher to[^)]+\\)", "(POA)", .) %>%
#                             gsub("\\(caught stealing[^)]+\\)", "(CS)", .) %>%
#                             gsub("\\(CS E[^)]+\\)", "(CS)", .) %>%
#                             gsub("\\(Picked[^)]+\\)", "(PO)", .) %>%
#                             gsub("\\(.*SAC[^)]*\\)", "(SAC)", .) %>%
#                             gsub("\\(.*Sac[^)]*\\)", "(SAC)", .) %>%
#                             gsub("\\(.*SH[^)]*\\)", "(SAC)", .) %>%
#                             gsub("\\(.*SF[^)]*\\)", "(SAC)", .) %>%
#                             gsub("\\(.*DI[^)]*\\)", "(DI)", .) %>%
#                             gsub("\\(.*FC[^)]*\\)", "(FC)", .) %>%
#                             gsub("\\(\\d+L|\\d+ F|G[^ ]*|\\d+G|[^ ]*IF[^ ]*\\)", "(out)", .) %>%
#                             gsub("\\(.*BI[^)]*\\)|\\(.*INT[^)]*\\)", "(out)", .) %>%
#                             gsub("\\(.*INF[^)]*\\)", "(out)", ., ignore.case = TRUE) %>%
#                             gsub("\\(\\d+F{1,2}|\\d+ P|PF[^ ]*\\)", "(out)", .) %>%
#                             gsub("\\(PO\\s*\\d+\\)", "(PO)", .) %>%
#                             gsub("\\(UUU| 21|p6|POout|S\\(out\\)", "(PO)", .) %>%
#                             gsub("\\((?!FC)[fF][^)]*\\)", "(out)", ., perl = TRUE) %>%
#                             gsub("\\(out[^)]*\\)", "(out)", .) %>%
#                             gsub("\\([^)]*-+[^)]*\\)", "(out)", .) %>%
#                             gsub("\\(\\d+ \\(out\\)", "(out)", .) %>%         
#                             gsub("\\(\\d+\\(out\\)", "(out)", .) %>%          
#                             gsub("\\(\\d+ WP\\)", "(WP)", .) %>%           
#                             gsub("\\(di\\)", "(DI)", .) %>%                  
#                             gsub("\\(IF[^)]*\\)", "(out)", .) %>%        
#                             gsub("\\(L\\d+[^)]*\\)", "(out)", .) %>%     
#                             gsub("\\(P\\d+[^)]*\\)", "(out)", .) %>%          
#                             gsub("\\(Sb \\d+\\)", "(SB)", .) %>%          
#                             gsub("\\(SP[^)]*\\)", "(SB)", .)  %>%
#                             gsub("\\(DP\\) L\\)", "(DP)", .)  %>%
#                             gsub("\\(out\\)-\\)", "(out)", .)  %>%
#                             gsub("\\(out\\) CS\\)", "(CS)", .)  %>%
#                             gsub("\\(out\\) OA\\)", "(out-asst)", .)  %>%
#                             gsub("\\(PO\\(out\\)", "(out)", .)  %>%
#                             # gsub("\\(.*\\(out\\)[^)]*//)", "(out)", .) %>%
#                             gsub("^,+|,+$", "", .) %>%
#                             gsub(", , ,|, ,", ",", .) %>%
#                             gsub("\\(\\(", "(", .) %>%
#                             gsub("\\)\\)", ")", .) %>%
#                             gsub("\\(out\\)KO\\)", "(out)", .)  %>%
#                             gsub("\\(out\\)O\\)", "(out)", .)  %>%
#                             gsub("\\(caught stealing: CS 2-4", "(CS)", .)  %>%
#                             str_trim(.) %>%
#                             str_squish(.),
#                           T ~ NA),
#     Result_2 = if_else(
#       str_ends(Result_2, ","),
#       str_remove(Result_2, ",$"),
#       Result_2
#     ),
#     Result_2 = if_else(
#       str_starts(Result_2, ","),
#       str_remove(Result_2, "^,"),
#       test
#     ),
#     Result_2 = str_squish(gsub("\\(|\\)","",Result_2)),
#     .after = Result
#   ) %>%
#   # mutate(RunsonPlay = str_count(Event, "Scores") + str_count(Event, "scored"),
#   #        OutsonPlay = str_count(Event, "out number")) %>%
#   group_by(Inning,GameID, PAofInning)  %>%
#   filter(Result_2 == lastplay) %>%
#   mutate(tstEvent = case_when(
#     RunsonPlay == 1 ~ paste(Event,"-", RunsonPlay,'run scored.'),
#     RunsonPlay > 1 ~ paste(Event,"-", RunsonPlay,'runs scored.'),
#     T ~ Event
#   ), .after = Event
#   )

# %>%
#   mutate(
#     r1_name = case_when(
#       str_detect(Event,"advances to 1st")==T   ~
#         word(str_extract(Event, "\\b\\w+ \\w+ (?=advances to 1st)"), 1, 2)
#     ),
#     r2_name = case_when(
#       str_detect(Event,"advances to 2nd")==T   ~
#         word(str_extract(Event, "\\b\\w+ \\w+ (?=advances to 2nd)"), 1, 2)
#     ),
#     r3_name = case_when(
#       str_detect(Event,"advances to 3rd")==T   ~
#         word(str_extract(Event, "\\b\\w+ \\w+ (?=advances to 3rd)"), 1, 2)
#     ),
#     rScore_name = case_when(
#       str_detect(Event,"Scores")==T   ~
#         sapply(str_extract_all(Event, "\\b\\w+ \\w+ (?=Scores)"), function(x) paste(x, collapse = ", "))
#     ),
#     
#     across(c(r1_name, r2_name, r3_name, rScore_name), ~ str_squish(.))
#     
#   ) %>%
#   group_by(GameID, Inning) %>%
#   mutate(
#     r1_name = if_else(is.na(r1_name) & !r2_name %in% lag(r1_name) & !r3_name %in% lag(r1_name), lag(r1_name), r1_name),
#     r2_name = if_else(is.na(r2_name) & !r1_name %in% lag(r2_name) & !r3_name %in% lag(r2_name), lag(r2_name), r2_name),
#     r3_name = if_else(is.na(r3_name) & !r1_name %in% lag(r3_name) & !r2_name %in% lag(r3_name), lag(r3_name), r3_name)
#   ) %>%
#   ungroup() %>%
#   group_by(GameID, Inning) %>%
#   fill(r1_name, r2_name, r3_name) %>%
#   ungroup() %>%
#   mutate(
#     r1_name = case_when(
#       r1_name == r2_name | r1_name == r3_name | r1_name == rScore_name ~ NA, 
#       T~ r1_name
#     ),
#     r2_name = case_when(
#       r2_name == r1_name ~ NA,
#       r2_name == r3_name ~ NA,
#       r2_name == rScore_name ~ NA, 
#       T~ r2_name
#     ),
#     r3_name = case_when(
#       r3_name == r1_name | r3_name == r2_name | r3_name == rScore_name ~ NA, 
#       T~ r3_name
#     )
#   ) %>%
#   ungroup() %>%
#   group_by(GameID, Inning) %>%
#   mutate(
#     r1 = NA,
#     r2 = NA,
#     r3 = NA,
#     r1_end = ifelse(is.na(r1_name), 0, 1),
#     r2_end = ifelse(is.na(r2_name), 0, 2),
#     r3_end = ifelse(is.na(r3_name), 0, 3),
#     r1 = lag(r1_end, default = 0),
#     r2 = lag(r2_end, default = 0),
#     r3 = lag(r3_end, default = 0),
#     r1_end = if_else(row_number() == n() & grepl('out',Event), 0, r1_end),
#     r2_end = if_else(row_number() == n() & grepl('out',Event), 0, r2_end),
#     r3_end = if_else(row_number() == n() & grepl('out',Event), 0, r3_end),
#     r1 = if_else(PAofInning == 0 | PAofInning == 1, 0, r1),
#     r2 = if_else(PAofInning == 0 | PAofInning == 1, 0, r2),
#     r3 = if_else(PAofInning == 0 | PAofInning == 1, 0, r3),
#   ) %>%
#   mutate(
#     Runners = paste0(r1,r2,r3),
#     Runners_end = paste0(r1_end,r2_end,r3_end),
#     .after = Event
#   )
