library(dplyr)
library(rvest)
library(stringr)
library(tidyr)
library(tictoc)

# ------------------ SCRAPE ALL BOX SCORES FROM YESTERDAY --------------------------
db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

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

# dates for single_day
# game_dates <- unique(
#   format(
#     as.Date(
#       unique(
#         
#         read_html('https://baseball.pointstreak.com/schedule.html?leagueid=193&seasonid=33873') %>%
#           html_table(fill = T) %>% .[[1]] %>%
#           mutate(dt = as.Date(Date, "%b %d"),
#                  dt = as.Date(gsub("2025-",'2024-',dt))) %>%
#           filter(dt < Sys.Date())
#       )$dt), "%m/%d/%Y")
# )


# game_dates <- format(as.Date('2024-05-20'), "%m/%d/%Y")
# YESTERDAYs DATE
game_dates <- format(as.Date(with_tz(now(), tzone = 'America/Chicago'))-1, "%m/%d/%Y")

pbp <- data.frame()
box_master <- data.frame()
bullpen_log <- data.frame()
# main site url
site_url <- 'https://baseball.pointstreak.com/'

{
  tic()
  # date <- game_dates[1]
  for (date in game_dates) { # START LOOP ----
    
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
          mutate(GameID = gm_id)%>%
          relocate(pitcherid, .after = Pitcher)%>%
          ungroup() %>%
          mutate(across(5:15 ,~ as.numeric(.)))
        
        
        
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
          mutate(GameID = gm_id)%>%
          relocate(pitcherid, .after = Pitcher) %>%
          ungroup() %>%
          mutate(across(5:15 ,~ as.numeric(.)))
        
        bullpen_log <- rbind(bullpen_log, home_bullpen, away_bullpen) %>%
          group_by(GameID, Team) %>%
          mutate(Role = ifelse(row_number() == 1, 'SP', 'RP'), .after = pitcherid)
        
        
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
  } # END LOOP ----
  
  # pbp_clean  ----
  # this is the daily pbp data
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
  
  # run_ex ----
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
  
  days_game_ids <- unique(run_ex$GameID)
  
  for(id in days_game_ids){ ## DOES GAME ID ALREADY EXIST? ----
    
    run_ex_game_id_exists <- id %in% dbGetQuery(db, 'select * from pbp_25')$GameID
    
    if(run_ex_game_id_exists==T) {
      
      cat(paste('GameID',id,'already exists, skipping'), sep = '\n')
      
    } else{ 
      
      cat(paste('Writing GameID',id, 'to pbp_25 table'), sep = '\n')
      
      # dbExecute(db, 'DELETE FROM weights_raw WHERE SEASON = 2025;')
      
      dbWriteTable(db, name = 'pbp_25', value = run_ex, append = T)
      
    }
  }
  
  # SEASON RUN MATRIX ----
  run_ex_season <- dbGetQuery(db ,'select * from pbp_25')
  
  ### run_matrix ----
  run_matrix <- run_ex_season %>%
    ungroup() %>%
    filter(desired_result!='' & Outs < 3) %>%
    # filter(grepl("1B|2B|3B|HR|HBP|BB|out", desired_result)) %>%
    group_by(Outs, Runners) %>%
    summarise(RE = round(mean(TotalRunsInning - Runs_upto), 3)) %>%
    mutate(Runners = factor(Runners, levels = c('000', '100', '020', '120', '003', '103', '023', '123'))) %>%
    arrange(Runners) %>%
    pivot_wider(names_from = Outs, values_from = RE)
  
  ### run_matrix_long ----
  
  run_matrix_long <- run_ex_season %>%
    ungroup()%>%
    filter(desired_result!='' & Outs < 3) %>%
    group_by(Outs, Runners) %>%
    summarise(RE = round(mean(TotalRunsInning - Runs_upto), 3)) %>%
    mutate(Runners = factor(Runners, levels = c('000', '100', '020', '120', '003', '103', '023', '123')),
           State = paste(Outs, Runners)) %>%
    arrange(State) %>%
    ungroup() %>%
    select(-Outs, -Runners) 
  
  ### pbp_2 ====
  pbp_2 <- run_ex_season %>%
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
  
  ### run_matrix_pct ----
  run_matrix_pct <- run_ex_season %>%
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
  
  ### weights ----
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

today_gm_id_pbp <- sort(unique(pbp$GameID))

dbExecute(db, glue::glue('DELETE FROM pbp_25 WHERE GameID in ({today_gm_id_pbp});'))

dbWriteTable(db, name = 'pbp_25', value = run_ex_season, append = T)

dbExecute(db, 'DELETE FROM run_matrix WHERE SEASON = 2025;')

dbWriteTable(db, name = 'run_matrix', value = run_matrix, append = T)
# END HERE ----
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


today_gm_id_bp <- sort(unique(bullpen_log$GameID))

existing_game_id <- sort(unique(dbGetQuery(db, 'select GameID from pitcher_logs')$GameID))

bullpen_log_to_write <- bullpen_log %>% 
  filter(!GameID %in% existing_game_id)

dbWriteTable(db, name = 'pitcher_logs', bullpen_log_to_write, append = T)




today_gm_id_box <- sort(unique(box_master$game_id))

existing_game_id_box <- sort(unique(dbGetQuery(db, 'select GameID from game_pks_ps')$game_id))

box_master_to_write <- box_master %>% 
  filter(!game_id %in% existing_game_id_box) %>%
  group_by(date, home, away) %>%
  mutate(game_num = row_number()) 

dbWriteTable(db, name = 'game_pks_ps', box_master_to_write, append = T)