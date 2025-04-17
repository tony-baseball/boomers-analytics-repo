library(dplyr)
library(rvest)
library(slackr)
library(stringr)
library(tictoc)
library(RSQLite)

# American Association ================================================================================================================================= ====

{
  tic()
  db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")
  
  aa <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/american_association.sqlite")
  
  weights <- dbGetQuery(db, 'select * from weights')
  dbListTables(db)
  pf <- dbGetQuery(db, 'select * from park_factors')
  
  url_base <- 'https://baseball.pointstreak.com/'
  
  lg_url <- 'https://baseball.pointstreak.com/scoreboard.html?leagueid=193'
  
  # SCRAPE TEAM NAMES AND URLS ----
  url <- 'https://baseball.pointstreak.com/teamlist.html?leagueid=193&seasonid=33873'
  
  page <- read_html(url)
  
  # Extract links and their corresponding text
  links <- page %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    na.omit() %>%
    as.character()
  
  link_texts <- page %>%
    html_nodes("a") %>%
    html_text() %>%
    na.omit() %>%
    as.character()
  
  # Combine links and their texts into a data frame
  team_info <- data.frame(team = link_texts, url =paste0(url_base,links))  %>%
    filter(grepl("teamid=", url)) %>%
    mutate(team_id =as.numeric(sub(".*teamid=([0-9]+).*", "\\1", url)),
           stats = gsub("team_home", "team_stats", url),
           roster = gsub("team_home", "team_roster", url),
           transactions = gsub("team_home", "team_transactions", url)
    )
  
  # SCRAPING TEAM ROSTERS ------------------------------------------- ----
  # team_info <- team_info %>% slice(1)
  
  
  rosters <- data.frame()
  stats_batting <- data.frame()
  stats_pitching <- data.frame()
  
  
  for (i in seq_along(team_info$roster)) {
    
    team_name <- team_info$team[i]
    roster_url <- team_info$roster[i]
    team_id_ <- team_info$team_id[i]
    
    cat(paste('Scraping',team_name, "roster"),
        append = F, sep = '\n')
    
    page <- read_html(roster_url)
    
    h4_elements <- page %>% html_nodes("h4")
    
    # Extract text from h4 elements
    h4_text <- html_text(h4_elements) %>% as.data.frame() %>%
      rename(headings = 1) %>%
      filter(headings %in% c("Pitchers", 'Catchers', 'Infielders', 'Outfielders', 'Inactive', 'Disabled')) %>%
      mutate(Status = case_when(
        headings %in% c("Pitchers", 'Catchers', 'Infielders', 'Outfielders') ~ "Active",
        T ~ headings
      )
      )
    
    # Extract tables
    tables <- page %>% html_nodes("table")
    
    # Function to extract text and URLs from hyperlinks
    extract_links <- function(table_node) {
      links <- table_node %>% html_nodes("td:nth-child(2) a")
      urls <- links %>% html_attr("href")
      texts <- links %>% html_text()
      data.frame(text = texts, player_url = urls, stringsAsFactors = FALSE)
    }
    
    # Extract links from each table
    extracted_data <- lapply(tables, extract_links)
    
    # Combine results into a single data frame
    urls_df <- do.call(rbind, extracted_data)
    
    tables <- page %>% html_table(fill = T)
    
    team_roster <- data.frame() 
    # j <- 2
    for(j in seq_along(h4_text$headings)){
      
      status <- h4_text$Status[j]
      
      roster_group <- tables[[j]]  %>%
        mutate(Team = team_info$team[i],
               Position = h4_text$headings[j],
               Year = Status,
               Status = status) %>% 
        left_join(urls_df, by = c("Player" = 'text'))  %>%
        mutate(player_url = paste0(url_base, player_url),
               player_id = as.numeric(sub(".*playerid=([0-9]+).*", "\\1", player_url)),
               first_name = sub(" .*", "", Player),
               last_name= sub("^[^ ]* ", "", Player))
      
      team_roster <- rbind(team_roster, roster_group) 
      
    }
    rosters <- rbind(rosters, team_roster) %>%
      distinct(.keep_all = T) 
    
    # # TEAM STATS
    # stats_list <- c('batting', 'pitching')
    # 
    # 
    # for (k in stats_list) {
    #   stats_url <- paste0('https://baseball.pointstreak.com/printstats.html?teamid=', team_id_, '&seasonid=33873&view=', k)
    #   
    #   assign(paste0('team_', k), read_html(stats_url) %>% html_table(fill = TRUE) %>% .[[1]] %>%
    #            janitor::row_to_names(row_number = 1) %>%
    #            mutate(Team = team_name,
    #                   first_name = str_trim(str_extract(Player, "(?<=, ).*$")),
    #                   last_name = str_trim(str_extract(Player, "^[^,]+")),
    #            )
    #   ) 
    #   
    # }
    # 
    # stats_batting <- rbind(stats_batting, team_batting) %>%
    #   relocate(Team, .after = Player) %>% 
    #   distinct(.keep_all = T)
    # stats_pitching <- rbind(stats_pitching, team_pitching) %>%
    #   relocate(Team, .after = Player) %>% 
    #   distinct(.keep_all = T)
    # 
    # 
    # SCRAPING STATS ------------------------------------------- ----
    stats_list <- c('batting', 'pitching')
    
    team_batting <- data.frame()
    team_pitching <- data.frame()   
    
    for (stat in stats_list) {
      
      
      # stat = 'batting'
      
      if(stat =='batting') {
        
        # ALL PLAYERS STATS ----
        stats_url <- paste0("https://baseball.pointstreak.com/team_stats.html?teamid=",team_id_, 
                            "&seasonid=33873&view=", stat, "&bset=0&orderby=avg")
        
        page <- read_html(stats_url)
        
        table <- page %>% html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
          filter(!grepl('Total',Player))
        
        # Extracting links from column 1
        links <- page %>%
          html_nodes("table tr td:nth-child(1) a") %>%
          purrr::map_df(~ data.frame(link_text = html_text(.), link_url = html_attr(., "href")))
        
        # Combine links with table data
        table_with_links <- table %>% left_join(links, by = c('Player' = 'link_text'))  %>%
          mutate(player_id = as.numeric(sub(".*playerid=([0-9]+).*", "\\1", link_url)))
        
        team_batting <- rbind(team_batting, table_with_links)
        
        # EXTENDED HITTING - - - - - - - - - - - - - 
        stats_url <- paste0("https://baseball.pointstreak.com/team_stats.html?teamid=",team_id_, 
                            "&seasonid=33873&view=", stat, "&bset=1&orderby=avg")
        
        page <- read_html(stats_url)
        
        table <- page %>% html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
          filter(!grepl('Total',Player))
        
        # Extracting links from column 1
        links <- page %>%
          html_nodes("table tr td:nth-child(1) a") %>%
          purrr::map_df(~ data.frame(link_text = html_text(.), link_url = html_attr(., "href")))
        
        # Combine links with table data
        table_with_links <- table
        # %>%
        #   mutate(player_id = as.numeric(sub(".*playerid=([0-9]+).*", "\\1", link_url)))
        
        team_batting <- team_batting %>% left_join(table_with_links, by = c('Player', 'AVG', 'P'))
        
        stats_url <- paste0("https://baseball.pointstreak.com/team_stats.html?teamid=",team_id_, 
                            "&seasonid=33873&view=", stat, "&bset=2&orderby=avg")
        
        page <- read_html(stats_url)
        
        table <- page %>% html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
          filter(!grepl('Total',Player))
        
        # Extracting links from column 1
        links <- page %>%
          html_nodes("table tr td:nth-child(1) a") %>%
          purrr::map_df(~ data.frame(link_text = html_text(.), link_url = html_attr(., "href")))
        
        # Combine links with table data
        table_with_links <- table
        # %>%
        #   mutate(player_id = as.numeric(sub(".*playerid=([0-9]+).*", "\\1", link_url)))
        
        team_batting <- team_batting %>% left_join(table_with_links, by = c('Player', 'AVG', 'P'))  %>%
          mutate(Player = ifelse(substr(Player,1,2)=='x ', gsub('x ',"",Player), Player)) %>%
          mutate(Team = team_name, .after = Player) %>%
          mutate(name = rosters$Player[match(player_id, rosters$player_id)],
                 Player = ifelse( !is.na(name), name, Player)) %>%
          select(-name)
        
        
        
      } else if (stat== 'pitching') {
        # stat = 'pitching'
        
        stats_url <- paste0("https://baseball.pointstreak.com/team_stats.html?teamid=",team_id_, 
                            "&seasonid=33873&view=", stat, "&pset=0&orderby=era")
        
        page <- read_html(stats_url)
        
        table <- page %>% html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
          filter(!grepl('Total',Player))
        
        # Extracting links from column 1
        links <- page %>%
          html_nodes("table tr td:nth-child(1) a") %>%
          purrr::map_df(~ data.frame(link_text = html_text(.), link_url = html_attr(., "href")))
        
        # Combine links with table data
        table_with_links <- table %>% left_join(links, by = c('Player' = 'link_text'))  %>%
          mutate(player_id = as.numeric(sub(".*playerid=([0-9]+).*", "\\1", link_url)))
        
        team_pitching <- rbind(team_pitching, table_with_links)
        
        # EXTENDED PITCHING - - - - - - - - - - - - - 
        stats_url <- paste0("https://baseball.pointstreak.com/team_stats.html?teamid=",team_id_, 
                            "&seasonid=33873&view=", stat, "&pset=1&orderby=era")
        
        page <- read_html(stats_url)
        
        table <- page %>% html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
          filter(!grepl('Total',Player))
        
        # Extracting links from column 1
        links <- page %>%
          html_nodes("table tr td:nth-child(1) a") %>%
          purrr::map_df(~ data.frame(link_text = html_text(.), link_url = html_attr(., "href")))
        
        # Combine links with table data
        table_with_links <- table
        # %>%
        #   mutate(player_id = as.numeric(sub(".*playerid=([0-9]+).*", "\\1", link_url)))
        
        team_pitching <- team_pitching %>% left_join(table_with_links, by = c('Player', 'ERA'))
        
        stats_url <- paste0("https://baseball.pointstreak.com/team_stats.html?teamid=",team_id_, 
                            "&seasonid=33873&view=", stat, "&pset=2&orderby=era")
        
        page <- read_html(stats_url)
        
        table <- page %>% html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
          filter(!grepl('Total',Player))
        
        # Extracting links from column 1
        links <- page %>%
          html_nodes("table tr td:nth-child(1) a") %>%
          purrr::map_df(~ data.frame(link_text = html_text(.), link_url = html_attr(., "href")))
        
        # Combine links with table data
        table_with_links <- table
        # %>%
        #   mutate(player_id = as.numeric(sub(".*playerid=([0-9]+).*", "\\1", link_url)))
        
        team_pitching <- team_pitching %>% left_join(table_with_links, by = c('Player', 'ERA')) %>%
          mutate(Player = ifelse(substr(Player,1,2)=='x ', gsub('x ',"",Player), Player)) %>%
          mutate(Team = team_name, .after = Player) %>%
          mutate(name = rosters$Player[match(player_id, rosters$player_id)],
                 Player = ifelse( !is.na(name), name, Player)) %>%
          select(-name) 
        
        
      }
      
      stats_batting <- rbind(stats_batting, team_batting) %>% distinct(.keep_all = T)
      stats_pitching <- rbind(stats_pitching, team_pitching) %>% distinct(.keep_all = T) 
      
    }
    
    # 
    # # Read the webpage
    # page <- read_html(url)
    # 
    # # Extract table
    # table <- page %>% html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
    #   filter(!grepl('Total',Player))
    # 
    # # Extracting links from column 1
    # links <- page %>%
    #   html_nodes("table tr td:nth-child(1) a") %>%
    #   purrr::map_df(~ data.frame(link_text = html_text(.), link_url = html_attr(., "href")))
    # 
    # # Combine links with table data
    # table_with_links <- table %>% left_join(links, by = c('Player' = 'link_text')) %>%
    #   mutate(player_id = as.numeric(sub(".*playerid=([0-9]+).*", "\\1", link_url)))
    # 
    # # Display the combined data
    # print(table_with_links)
    # 
    # 
    
  }
  
  
  
  
  
  
  # STANDINGS - ------------------------------------------ ----
  
  
  standings_url <- 'https://baseball.pointstreak.com/standings.html?leagueid=193&seasonid=33873'
  
  page <- read_html(standings_url)
  
  east <- page %>% html_table(fill = T) %>% .[[1]] %>%
    rename(Team = 1) %>% 
    mutate(Team = str_squish(gsub('†','', Team))) %>%
    mutate(Division = 'East', 
           Season = 2024,
           GB = as.numeric(ifelse(is.na(as.numeric(GB)), 0, GB)))
  
  west <- page %>% html_table(fill = T) %>% .[[2]] %>%
    rename(Team = 1) %>% 
    mutate(Team = str_squish(gsub('†','', Team))) %>%
    mutate(Division = 'West', 
           Season = 2024,
           GB = as.numeric(ifelse(is.na(as.numeric(GB)), 0, GB)))
  
  standings <- rbind(east,west) %>% 
    arrange(desc(PCT))
  
  
  
  
  suppressWarnings(
    suppressMessages(
      rm(
        extracted_data, h4_elements, h4_text, links, page, roster_group, table, 
        table_with_links, tables, team_batting, team_pitching, team_roster, 
        urls_df,i,j,link_texts,roster_url, stat, stats_list, stats_url, status, 
        team_id_, team_name,url, standings_url, east, west)
    )
  )
  
  # TEAM STATS --------------------------------- -----
  
  
  
  stats_team_batting <- stats_batting %>% select(-player_id) %>%
    group_by(Team) %>%
    summarise(across(is.numeric, ~sum(., na.rm = T))) %>%
    mutate(G = standings$W[match(Team,standings$Team)] + standings$L[match(Team, standings$Team)])
  
  stats_team_pitching <- stats_pitching %>% select(-player_id) %>%
    mutate(full_inn = floor(IP),
           part_inn = (IP - floor(IP)) * 10) %>%
    group_by(Team) %>%
    summarise(across(is.numeric, ~sum(., na.rm = T))) %>%
    ungroup() %>%
    mutate(W = standings$W[match(Team,standings$Team)],
           L = standings$L[match(Team, standings$Team)],
           G = W + L) %>%
    mutate(IP = round(full_inn + (part_inn/3),1)) %>% 
    select(-part_inn,-full_inn)
  
  
  missing_stats_bp <- rbind(
    stats_batting %>%
      filter(grepl(',', Player)) %>% 
      select(Player, link_url, player_id), 
    stats_pitching %>%
      filter(grepl(',', Player)) %>% 
      select(Player, link_url, player_id)
  )
  
  missing_players <- data.frame()
  
  
  
  missing_stats_bp <- missing_stats_bp %>% slice(2:nrow(missing_stats_bp))
  for(p in seq_along(missing_stats_bp$player_id)) {
    
    pl_id <- missing_stats_bp$player_id[p]
    
    pl_url <- paste0(url_base,missing_stats_bp$link_url[p])
    
    css_selector <- "#ps-content-bootstrapper > div > div > div.col-md-10 > div.row.nova-heading > div.col-sm-9 > h2 > span.nova-title"
    
    player_name <- str_squish(gsub("[#0-9]", "", 
                                   read_html(pl_url) %>% html_nodes(css_selector) %>% html_text(trim = TRUE) ))
    
    length(player_name)
    
    if( length(player_name) > 0){
      new_player <- data.frame(Player = player_name,
                               player_id = pl_id)
      
      missing_players <- rbind(missing_players, new_player)
      
      cat(paste('Found missing name', player_name, p, 'of', nrow(missing_stats_bp)), sep = '\n')
    }
  }
  

  
  
  stats_batting <- stats_batting %>%
    mutate(new_name = 
             ifelse(grepl(",",Player), missing_players$Player[match(player_id, missing_players$player_id)], Player),.after=Player,
           Player = ifelse(!is.na(new_name), new_name, Player )
    ) %>%
    select(-new_name)
  
  stats_pitching <- stats_pitching %>%
    mutate(new_name = 
             ifelse(grepl(",",Player), missing_players$Player[match(player_id, missing_players$player_id)], Player),.after=Player,
           Player = ifelse(!is.na(new_name), new_name, Player )
    ) %>%
    select(-new_name)
  
  lg_batting <- stats_batting %>% select(-player_id) %>%
    summarise(Team = 'League', across(is.numeric, ~sum(., na.rm = T))) %>%
    mutate(across(2:ncol(.), ~ as.numeric(.))) %>%
    mutate(`1B` = H - HR - `2B` - `3B`,
           AVG = as.numeric(formatC(formatC(round((H / AB), 3), format = 'f', digits = 3), format = 'f', digits = 3)),
           OBP = as.numeric(formatC(round(((H + BB + HBP) / PA) ,3), format = 'f', digits = 3)),
           SLG = as.numeric(formatC(round(((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)) / AB) ,3), format = 'f', digits = 3)),
           OPS = as.numeric(formatC(round((OBP + SLG),3),format = 'f', digits = 3)),
           #   wOBA = #((.69* uIBB)+(.719*HBP)+(.87*1B)+(1.217*2B)+(1.529*3B)+(1.94*HR))/(AB+uiBB+HBP+SF)
           wOBA= as.numeric(formatC(round((((0.6954259*HBP)+(BB*0.6662854)+
                                              (0.8949178*`1B`)+(1.1374972*`2B`)+
                                              (1.3042143*`3B`)+(1.7754110*HR))/
                                             (AB+BB+HBP+SF+SH)) ,3), format = 'f', digits = 3)) #*1.096875
           )
  
  lg_pitching <- stats_pitching %>% select(-player_id) %>%
    mutate(full_inn = floor(IP),
           part_inn = (IP - floor(IP)) * 10,
           AB = round(H / BAA),
           PA = AB + BB + HBP)  %>%
    summarise(Team = 'League', across(is.numeric, ~sum(., na.rm = T))) %>%
    mutate(IP = round(full_inn + (part_inn/3),1)) %>% 
    select(-part_inn,-full_inn) %>%
    mutate(`1B` = H - `2B` - `3B` - HR,
           ERA = round(ER / IP * 9,2),
           PA = AB + BB + HBP ,
           BAA = round(H / AB, 3)
    )
  
  lg_obp <- lg_batting$OBP
  
  lg_woba_raw <- lg_batting$wOBA
  
  lg_woba_scale <- lg_obp / lg_woba_raw
  
  lg_woba <- lg_batting$wOBA * lg_woba_scale
  
  RunsPerWin<-round((9*(lg_pitching$R / lg_pitching$IP)*1.5)+3,2)
  
  lg_r_pa <- lg_pitching$R / lg_pitching$PA
  
  lg_run_out <- lg_pitching$R / (lg_pitching$IP*3)
  
  lg_runSB <- .2
  
  lg_runCS <- 2 * lg_run_out + 0.075
  
  lgwSB <- ((lg_batting$SB * .2) + (lg_batting$CS * lg_runCS)) / (lg_batting$`1B` + lg_batting$BB + lg_batting$HBP)
  
  
  
  league_batting <- lg_batting %>%
    mutate(
      wOBA = wOBA * lg_woba_scale,
      # `GO/FO` = round(GO/FO,1),
      `AB/HR` = round(HR/AB,3),
      `BB/PA` = round(BB/PA,3),
      `BB/K` = round(BB/SO,3),
      `SB%` = formatC(round((((SB/(SB + CS)))*100),1),format = 'f', digits = 1),
      `SB%`= sub('NaN%','0',`SB%`),
      `SB%`= sub('NaN','0',`SB%`),
      `SB%` = as.numeric(`SB%`),
      wRAA = as.numeric(formatC(round((((wOBA - lg_woba) / lg_woba_scale) * PA),2),format = 'f', digits=2)),
      RC = as.numeric(formatC(round(((((H+BB-CS)*
                                         ((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)+
                                             .26*(BB+HBP))+(.52*(SF+SH+SB))))) / 
                                       PA),2),format = 'f', digits = 2)),
      wRC = as.numeric(formatC(round(((((wOBA - lg_woba) / lg_woba_scale) + 
                                         (lg_r_pa))*PA),2),format = 'f', digits =2)),
      
      `wRC+` = round(((((wRAA/PA) + lg_r_pa) + (lg_r_pa - (1 * lg_r_pa))) / (tail(wRC, n=1)/ tail(PA, n=1)) )*100),
      `BB%` = round((BB / PA),3)*100,
      `K%` = round((SO / PA),3)*100,
      
      BattingRuns= round(wRAA +((lg_r_pa -(.1*lg_r_pa))*PA),2),
      BABIP = as.numeric(formatC(round(((H - HR) / (AB - HR - SO + SF)),3),format = 'f', digits = 3))    ,
      BABIP = ifelse(BABIP =='NaN', 0, BABIP),
      wSB = round( ((SB * lg_runSB)+ (CS * lg_runCS))  -  (lgwSB*(`1B`+BB+HBP)),2),
      wSB = ifelse(wSB == 'NaN', 0, wSB),
      oWAR= round((BattingRuns + wSB)/RunsPerWin,2),
      ISO = SLG - AVG,
      ISOP = SLG - AVG,
      `OPS+` = round(((OBP/tail(OBP, n=1)) + (SLG/tail(SLG, n=1)) -1) * 1 * 100),
      SEASON = 2024
      
    ) %>%
    # mutate_all(~ifelse(.=='NaN',0,.)) %>%
    mutate(across(2:ncol(.), ~ as.numeric(.))) %>%
    mutate(woba_scale = lg_woba_scale)
  
  cfip_w <- lg_pitching$ERA - (((13*lg_pitching$HR)+(3*(lg_pitching$BB+lg_pitching$HBP))-(2*(lg_pitching$SO)))/lg_pitching$IP)
  
  league_pitching <- lg_pitching %>%
    mutate(
      vOBP = as.numeric(formatC(round(((H + BB + HBP) / PA) ,3), format = 'f', digits = 3)),
      vSLG = as.numeric(formatC(round(((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)) / AB) ,3), format = 'f', digits = 3)),
      vOPS = as.numeric(formatC(round((vOBP + vSLG),3),format = 'f', digits = 3)),
      vwOBA = as.numeric(formatC(round((((0.6572260*HBP)+(BB*0.6603780)+
                                           (0.8869003*`1B`)+(1.1696127*`2B`)+
                                           (1.4508623*`3B`)+(1.7302153*HR))/
                                          (AB+BB+HBP)) ,3), format = 'f', digits = 3)) * lg_woba_scale,
      vwRAA = as.numeric(formatC(round((((vwOBA - lg_woba) / lg_woba_scale) * 
                                          PA),2),format = 'f', digits=2)),
      vRC = as.numeric(formatC(round(((((H+BB)*
                                          ((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)+
                                              .26*(BB+HBP))))) / 
                                        PA),2),format = 'f', digits = 2)),
      vwRC = as.numeric(formatC(round(((((vwOBA - lg_woba) / lg_woba_scale) + 
                                          (lg_r_pa))*PA),2),format = 'f', digits =2)),
      `vwRC+` = round(((((vwRAA/PA) + lg_r_pa) + (lg_r_pa - (1 * lg_r_pa))) / (tail(vwRC, n=1)/ tail(PA, n=1)) )*100),
      `BB%` = round((BB / PA),3)*100,
      `K%` = round((SO / PA),3)*100,
      vBattingRuns= round(vwRAA +((lg_r_pa -(.1*lg_r_pa))*PA),2),
      # Team FIP = (((13 * HR) + (3 * (BB + HBP)) - (2 * K)) / IP) + FIP constant
      FIP = as.numeric(formatC(round(((((13 * HR) + (3 * (BB+HBP)) - (2 * SO))/ IP) + cfip_w),2),format = 'f',digits = 2)),
      `K/9` = formatC(round((SO / IP * 9), 2), format = 'f', digits = 2)   ,
      `BB/9` = formatC(round((BB / IP * 9), 2), format = 'f', digits = 2),
      `K%` = formatC(round((SO / PA), 2), format = 'f', digits = 2),
      #BB%
      `BB%` = formatC(round((BB / PA), 2), format = 'f', digits = 2),
      #WHIP
      WHIP = formatC(round(((BB + H) / IP), 2), format = 'f', digits = 2),
      vISO = vSLG - BAA,
      vBABIP = as.numeric(formatC(round(((H - HR) / (AB - HR - SO)),3),format = 'f', digits = 3)),
      BIPA = as.numeric(formatC(round(((H - HR) / (AB - HR - SO)),3),format = 'f', digits = 3)),
      `FIP-` = as.numeric(formatC(round(100*((FIP / FIP)     ))))
    )
  
  
  toc()
}

# PLAYER LEVEL ----


sb <- stats_batting %>% 
  mutate(`1B` = H - HR - `2B` - `3B`,
         AVG = as.numeric(formatC(formatC(round((H / AB), 3), format = 'f', digits = 3), format = 'f', digits = 3)),
         OBP = as.numeric(formatC(round(((H + BB + HBP) / PA) ,3), format = 'f', digits = 3)),
         SLG = as.numeric(formatC(round(((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)) / AB) ,3), format = 'f', digits = 3)),
         OPS = as.numeric(formatC(round((OBP + SLG),3),format = 'f', digits = 3)),
         #   wOBA = #((.69* uIBB)+(.719*HBP)+(.87*1B)+(1.217*2B)+(1.529*3B)+(1.94*HR))/(AB+uiBB+HBP+SF)
         wOBA= as.numeric(formatC(round((((0.6572260*HBP)+(BB*0.6603780)+
                                            (0.8869003*`1B`)+(1.1696127*`2B`)+
                                            (1.4508623*`3B`)+(1.7302153*HR))/
                                           (AB+BB+HBP+SF+SH)) ,3), format = 'f', digits = 3)) * lg_woba_scale,
         #wOBA = wOBA * 
         # `GO/FO` = round(GO/FO,1),
         `AB/HR` = round(HR/AB,3),
         `BB/PA` = round(BB/PA,3),
         `BB/K` = round(BB/SO,3),
         `SB%` = formatC(round((((SB/(SB + CS)))*100),1),format = 'f', digits = 1),
         `SB%`= sub('NaN%','0',`SB%`),
         `SB%`= sub('NaN','0',`SB%`),
         `SB%` = as.numeric(`SB%`),
         wRAA = as.numeric(formatC(round((((wOBA - lg_woba) / lg_woba_scale) * PA),2),format = 'f', digits=2)),
         RC = as.numeric(formatC(round(((((H+BB-CS)*
                                            ((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)+
                                                .26*(BB+HBP))+(.52*(SF+SH+SB))))) / 
                                          PA),2),format = 'f', digits = 2)),
         wRC = as.numeric(formatC(round(((((wOBA - lg_woba) / lg_woba_scale) + 
                                            (lg_r_pa))*PA),2),format = 'f', digits =2)),
         
         `wRC+` = round(((((wRAA/PA) + lg_r_pa) + (lg_r_pa - (1 * lg_r_pa))) / (league_batting$RC / league_batting$PA) )*100),
         `BB%` = round((BB / PA),3)*100,
         `K%` = round((SO / PA),3)*100,
         
         BattingRuns= round(wRAA +((lg_r_pa -(.1*lg_r_pa))*PA),2),
         BABIP = as.numeric(formatC(round(((H - HR) / (AB - HR - SO + SF)),3),format = 'f', digits = 3))    ,
         BABIP = ifelse(BABIP =='NaN', 0, BABIP),
         wSB = round( ((SB * lg_runSB)+ (CS * lg_runCS))  -  (lgwSB*(`1B`+BB+HBP)),2),
         wSB = ifelse(wSB == 'NaN', 0, wSB),
         oWAR= round((BattingRuns + wSB)/RunsPerWin,2),
         ISO = SLG - AVG,
         ISOP = SLG - AVG,
         `OPS+` = round(((OBP/league_batting$OBP) + (SLG/league_batting$SLG) -1) * 1 * 100),
         SEASON = 2024
         
  ) 


sp <- stats_pitching %>%
  mutate(AB = round(H / BAA),
         PA = AB + BB + HBP,
         `1B` = H - `2B` - `3B` - HR,
         PA = AB + BB + HBP ,
         # BAA = round(H/AB,3),
         vOBP = as.numeric(formatC(round(((H + BB + HBP) / PA) ,3), format = 'f', digits = 3)),
         vSLG = as.numeric(formatC(round(((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)) / AB) ,3), format = 'f', digits = 3)),
         vOPS = as.numeric(formatC(round((vOBP + vSLG),3),format = 'f', digits = 3)),
         vwOBA = as.numeric(formatC(round((((0.6572260*HBP)+(BB*0.6603780)+
                                              (0.8869003*`1B`)+(1.1696127*`2B`)+
                                              (1.4508623*`3B`)+(1.7302153*HR))/
                                             (AB+BB+HBP)) ,3), format = 'f', digits = 3)) * lg_woba_scale,
         vwRAA = as.numeric(formatC(round((((vwOBA - lg_woba) / lg_woba_scale) * 
                                             PA),2),format = 'f', digits=2)),
         vRC = as.numeric(formatC(round(((((H+BB)*
                                             ((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)+
                                                 .26*(BB+HBP))))) / 
                                           PA),2),format = 'f', digits = 2)),
         vwRC = as.numeric(formatC(round(((((vwOBA - lg_woba) / lg_woba_scale) + 
                                             (lg_r_pa))*PA),2),format = 'f', digits =2)),
         `vwRC+` = round(((((vwRAA/PA) + lg_r_pa) + (lg_r_pa - (1 * lg_r_pa))) / (league_batting$`RC`/ league_batting$PA) )*100),
         `BB%` = round((BB / PA),3)*100,
         `K%` = round((SO / PA),3)*100,
         vBattingRuns= round(vwRAA +((lg_r_pa -(.1*lg_r_pa))*PA),2),
         # Team FIP = (((13 * HR) + (3 * (BB + HBP)) - (2 * K)) / IP) + FIP constant
         FIP = as.numeric(formatC(round(((((13 * HR) + (3 * (BB+HBP)) - (2 * SO))/ IP) + cfip_w),2),format = 'f',digits = 2)),
         `K/9` = formatC(round((SO / IP * 9), 2), format = 'f', digits = 2)   ,
         `BB/9` = formatC(round((BB / IP * 9), 2), format = 'f', digits = 2),
         `K%` = formatC(round((SO / PA), 2), format = 'f', digits = 2),
         #BB%
         `BB%` = formatC(round((BB / PA), 2), format = 'f', digits = 2),
         #WHIP
         WHIP = formatC(round(((BB + H) / IP), 2), format = 'f', digits = 2),
         vISO = vSLG - BAA,
         `OPS+` = round(((vOBP/league_batting$OBP) + (vSLG/league_batting$SLG) -1) * 1 * 100),
         vBABIP = as.numeric(formatC(round(((H - HR) / (AB - HR - SO)),3),format = 'f', digits = 3)),
         `FIP-` = as.numeric(formatC(round(100*((FIP / league_pitching$FIP)     ))))
  )

# TEAM LEVEL ----

stb <- stats_team_batting %>%
  mutate(`1B` = H - HR - `2B` - `3B`,
         AVG = as.numeric(formatC(formatC(round((H / AB), 3), format = 'f', digits = 3), format = 'f', digits = 3)),
         OBP = as.numeric(formatC(round(((H + BB + HBP) / PA) ,3), format = 'f', digits = 3)),
         SLG = as.numeric(formatC(round(((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)) / AB) ,3), format = 'f', digits = 3)),
         OPS = as.numeric(formatC(round((OBP + SLG),3),format = 'f', digits = 3)),
         #   wOBA = #((.69* uIBB)+(.719*HBP)+(.87*1B)+(1.217*2B)+(1.529*3B)+(1.94*HR))/(AB+uiBB+HBP+SF)
         wOBA= as.numeric(formatC(round((((0.6572260*HBP)+(BB*0.6603780)+
                                            (0.8869003*`1B`)+(1.1696127*`2B`)+
                                            (1.4508623*`3B`)+(1.7302153*HR))/
                                           (AB+BB+HBP+SF+SH)) ,3), format = 'f', digits = 3)) * lg_woba_scale,
         #wOBA = wOBA * 
         # `GO/FO` = round(GO/FO,1),
         `AB/HR` = round(HR/AB,3),
         `BB/PA` = round(BB/PA,3),
         `BB/K` = round(BB/SO,3),
         `SB%` = formatC(round((((SB/(SB + CS)))*100),1),format = 'f', digits = 1),
         `SB%`= sub('NaN%','0',`SB%`),
         `SB%`= sub('NaN','0',`SB%`),
         `SB%` = as.numeric(`SB%`),
         wRAA = as.numeric(formatC(round((((wOBA - lg_woba) / lg_woba_scale) * PA),2),format = 'f', digits=2)),
         RC = as.numeric(formatC(round(((((H+BB-CS)*
                                            ((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)+
                                                .26*(BB+HBP))+(.52*(SF+SH+SB))))) / 
                                          PA),2),format = 'f', digits = 2)),
         wRC = as.numeric(formatC(round(((((wOBA - lg_woba) / lg_woba_scale) + 
                                            (lg_r_pa))*PA),2),format = 'f', digits =2)),
         
         `wRC+` = round(((((wRAA/PA) + lg_r_pa) + (lg_r_pa - (1 * lg_r_pa))) / (league_batting$RC / league_batting$PA) )*100),
         `BB%` = round((BB / PA),3)*100,
         `K%` = round((SO / PA),3)*100,
         
         BattingRuns= round(wRAA +((lg_r_pa -(.1*lg_r_pa))*PA),2),
         BABIP = as.numeric(formatC(round(((H - HR) / (AB - HR - SO + SF)),3),format = 'f', digits = 3))    ,
         BABIP = ifelse(BABIP =='NaN', 0, BABIP),
         wSB = round( ((SB * lg_runSB)+ (CS * lg_runCS))  -  (lgwSB*(`1B`+BB+HBP)),2),
         wSB = ifelse(wSB == 'NaN', 0, wSB),
         oWAR= round((BattingRuns + wSB)/RunsPerWin,2),
         ISO = SLG - AVG,
         ISOP = SLG - AVG,
         `OPS+` = round(((OBP/league_batting$OBP) + (SLG/league_batting$SLG) -1) * 1 * 100),
         SEASON = 2024
         
  ) 

stp <-  stats_team_pitching %>%
  mutate(AB = round(H / BAA),
         PA = AB + BB + HBP,
         `1B` = H - `2B` - `3B` - HR,
         PA = AB + BB + HBP ,
         # BAA = round(H/AB,3),
         vOBP = as.numeric(formatC(round(((H + BB + HBP) / PA) ,3), format = 'f', digits = 3)),
         vSLG = as.numeric(formatC(round(((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)) / AB) ,3), format = 'f', digits = 3)),
         vOPS = as.numeric(formatC(round((vOBP + vSLG),3),format = 'f', digits = 3)),
         vwOBA = as.numeric(formatC(round((((0.6572260*HBP)+(BB*0.6603780)+
                                              (0.8869003*`1B`)+(1.1696127*`2B`)+
                                              (1.4508623*`3B`)+(1.7302153*HR))/
                                             (AB+BB+HBP)) ,3), format = 'f', digits = 3)) * lg_woba_scale,
         vwRAA = as.numeric(formatC(round((((vwOBA - lg_woba) / lg_woba_scale) * 
                                             PA),2),format = 'f', digits=2)),
         vRC = as.numeric(formatC(round(((((H+BB)*
                                             ((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)+
                                                 .26*(BB+HBP))))) / 
                                           PA),2),format = 'f', digits = 2)),
         vwRC = as.numeric(formatC(round(((((vwOBA - lg_woba) / lg_woba_scale) + 
                                             (lg_r_pa))*PA),2),format = 'f', digits =2)),
         `vwRC+` = round(((((vwRAA/PA) + lg_r_pa) + (lg_r_pa - (1 * lg_r_pa))) / (league_batting$`RC`/ league_batting$PA) )*100),
         `BB%` = round((BB / PA),3)*100,
         `K%` = round((SO / PA),3)*100,
         vBattingRuns= round(vwRAA +((lg_r_pa -(.1*lg_r_pa))*PA),2),
         # Team FIP = (((13 * HR) + (3 * (BB + HBP)) - (2 * K)) / IP) + FIP constant
         FIP = as.numeric(formatC(round(((((13 * HR) + (3 * (BB+HBP)) - (2 * SO))/ IP) + cfip_w),2),format = 'f',digits = 2)),
         `K/9` = formatC(round((SO / IP * 9), 2), format = 'f', digits = 2)   ,
         `BB/9` = formatC(round((BB / IP * 9), 2), format = 'f', digits = 2),
         `K%` = formatC(round((SO / PA), 2), format = 'f', digits = 2),
         #BB%
         `BB%` = formatC(round((BB / PA), 2), format = 'f', digits = 2),
         #WHIP
         WHIP = formatC(round(((BB + H) / IP), 2), format = 'f', digits = 2),
         vISO = vSLG - BAA,
         `OPS+` = round(((vOBP/league_batting$OBP) + (vSLG/league_batting$SLG) -1) * 1 * 100),
         vBABIP = as.numeric(formatC(round(((H - HR) / (AB - HR - SO)),3),format = 'f', digits = 3)),
         `FIP-` = as.numeric(formatC(round(100*((FIP / league_pitching$FIP)     ))))
  )

dbWriteTable(aa, name = 'rosters', rosters, overwrite = T)
dbWriteTable(aa, name = 'teams', team_info, overwrite = T)
dbWriteTable(aa, name = 'standings', standings, overwrite = T)
dbWriteTable(aa, name = 'stats_batting', sb, overwrite = T)
dbWriteTable(aa, name = 'stats_team_batting', stb, overwrite = T)
dbWriteTable(aa, name = 'stats_league_batting', league_batting, overwrite = T)
dbWriteTable(aa, name = 'stats_pitching', sp, overwrite = T)
dbWriteTable(aa, name = 'stats_team_pitching', stp, overwrite = T)
dbWriteTable(aa, name = 'stats_league_pitching', league_pitching, overwrite = T)


rm("css_selector", "extract_links", "league_batting", 
   "league_pitching", "lg_batting", "lg_pitching", "lg_url", "lgwSB", 
   "missing_players", "missing_stats_bp", "new_player", "p", "pf", 
   "pl_id", "pl_url", "player_name", "RunsPerWin", "sb", 
   "sp", "standings", "stats_batting", "stats_pitching", "stats_team_batting", 
   "stats_team_pitching", "stb", "stp", "team_info", "url_base", 
   "weights")

dput(ls())


# Atlantic League ================================================================================================================================= ====
{
  tic()
  db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Shiny/FLASH/flashdb.sqlite")
  
  atl <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/atlantic_league.sqlite")
  
  weights <- dbGetQuery(db, 'select * from weights')
  dbListTables(db)
  pf <- dbGetQuery(db, 'select * from park_factors')
  
  url_base <- 'https://baseball.pointstreak.com/'
  
  lg_url <- 'https://baseball.pointstreak.com/scoreboard.html?leagueid=174'
  
  # SCRAPE TEAM NAMES AND URLS ----
  url <- 'https://baseball.pointstreak.com/teamlist.html?leagueid=174&seasonid=33927'
  
  page <- read_html(url)
  
  # Extract links and their corresponding text
  links <- page %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    na.omit() %>%
    as.character()
  
  link_texts <- page %>%
    html_nodes("a") %>%
    html_text() %>%
    na.omit() %>%
    as.character()
  
  # Combine links and their texts into a data frame
  team_info <- data.frame(team = link_texts, url =paste0(url_base,links))  %>%
    filter(grepl("teamid=", url)) %>%
    mutate(team_id =as.numeric(sub(".*teamid=([0-9]+).*", "\\1", url)),
           stats = gsub("team_home", "team_stats", url),
           roster = gsub("team_home", "team_roster", url),
           transactions = gsub("team_home", "team_transactions", url)
    )
  
  # SCRAPING TEAM ROSTERS ------------------------------------------- ----
  # team_info <- team_info %>% slice(1)
  
  
  rosters <- data.frame()
  stats_batting <- data.frame()
  stats_pitching <- data.frame()
  
  
  for (i in seq_along(team_info$roster)) {
    
    team_name <- team_info$team[i]
    roster_url <- team_info$roster[i]
    team_id_ <- team_info$team_id[i]
    
    cat(paste('Scraping',team_name, "roster"),
        append = F, sep = '\n')
    
    page <- read_html(roster_url)
    
    h4_elements <- page %>% html_nodes("h4")
    
    # Extract text from h4 elements
    h4_text <- html_text(h4_elements) %>% as.data.frame() %>%
      rename(headings = 1) %>%
      filter(headings %in% c("Pitchers", 'Catchers', 'Infielders', 'Outfielders', 'Inactive', 'Disabled')) %>%
      mutate(Status = case_when(
        headings %in% c("Pitchers", 'Catchers', 'Infielders', 'Outfielders') ~ "Active",
        T ~ headings
      )
      )
    
    # Extract tables
    tables <- page %>% html_nodes("table")
    
    # Function to extract text and URLs from hyperlinks
    extract_links <- function(table_node) {
      links <- table_node %>% html_nodes("td:nth-child(2) a")
      urls <- links %>% html_attr("href")
      texts <- links %>% html_text()
      data.frame(text = texts, player_url = urls, stringsAsFactors = FALSE)
    }
    
    # Extract links from each table
    extracted_data <- lapply(tables, extract_links)
    
    # Combine results into a single data frame
    urls_df <- do.call(rbind, extracted_data)
    
    tables <- page %>% html_table(fill = T)
    
    team_roster <- data.frame() 
    # j <- 2
    for(j in seq_along(h4_text$headings)){
      
      status <- h4_text$Status[j]
      
      roster_group <- tables[[j]]  %>%
        rename(Position = 3) %>%
        mutate(Team = team_info$team[i],
               Position = h4_text$headings[j],
               Year = NA,
               Status = NA
               ) %>% 
        left_join(urls_df, by = c("Player" = 'text'))  %>%
        mutate(player_url = paste0(url_base, player_url),
               player_id = as.numeric(sub(".*playerid=([0-9]+).*", "\\1", player_url)),
               first_name = sub(" .*", "", Player),
               last_name= sub("^[^ ]* ", "", Player))
      
      team_roster <- rbind(team_roster, roster_group) 
      
    }
    rosters <- rbind(rosters, team_roster) %>%
      distinct(.keep_all = T) 
    
    # # TEAM STATS
    # stats_list <- c('batting', 'pitching')
    # 
    # 
    # for (k in stats_list) {
    #   stats_url <- paste0('https://baseball.pointstreak.com/printstats.html?teamid=', team_id_, '&seasonid=33873&view=', k)
    #   
    #   assign(paste0('team_', k), read_html(stats_url) %>% html_table(fill = TRUE) %>% .[[1]] %>%
    #            janitor::row_to_names(row_number = 1) %>%
    #            mutate(Team = team_name,
    #                   first_name = str_trim(str_extract(Player, "(?<=, ).*$")),
    #                   last_name = str_trim(str_extract(Player, "^[^,]+")),
    #            )
    #   ) 
    #   
    # }
    # 
    # stats_batting <- rbind(stats_batting, team_batting) %>%
    #   relocate(Team, .after = Player) %>% 
    #   distinct(.keep_all = T)
    # stats_pitching <- rbind(stats_pitching, team_pitching) %>%
    #   relocate(Team, .after = Player) %>% 
    #   distinct(.keep_all = T)
    # 
    # 
    # SCRAPING STATS ------------------------------------------- ----
    stats_list <- c('batting', 'pitching')
    
    team_batting <- data.frame()
    team_pitching <- data.frame()   
    
    for (stat in stats_list) {
      
      
      # stat = 'batting'
      
      if(stat =='batting') {
        
        # ALL PLAYERS STATS ----
        stats_url <- paste0("https://baseball.pointstreak.com/team_stats.html?teamid=",team_id_, 
                            "&seasonid=33927&view=", stat, "&bset=0&orderby=avg")
        
        page <- read_html(stats_url)
        
        table <- page %>% html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
          filter(!grepl('Total',Player))
        
        # Extracting links from column 1
        links <- page %>%
          html_nodes("table tr td:nth-child(1) a") %>%
          purrr::map_df(~ data.frame(link_text = html_text(.), link_url = html_attr(., "href")))
        
        # Combine links with table data
        table_with_links <- table %>% left_join(links, by = c('Player' = 'link_text'))  %>%
          mutate(player_id = as.numeric(sub(".*playerid=([0-9]+).*", "\\1", link_url)))
        
        team_batting <- rbind(team_batting, table_with_links)
        
        # EXTENDED HITTING - - - - - - - - - - - - - 
        stats_url <- paste0("https://baseball.pointstreak.com/team_stats.html?teamid=",team_id_, 
                            "&seasonid=33927&view=", stat, "&bset=1&orderby=avg")
        
        page <- read_html(stats_url)
        
        table <- page %>% html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
          filter(!grepl('Total',Player))
        
        # Extracting links from column 1
        links <- page %>%
          html_nodes("table tr td:nth-child(1) a") %>%
          purrr::map_df(~ data.frame(link_text = html_text(.), link_url = html_attr(., "href")))
        
        # Combine links with table data
        table_with_links <- table
        # %>%
        #   mutate(player_id = as.numeric(sub(".*playerid=([0-9]+).*", "\\1", link_url)))
        
        team_batting <- team_batting %>% left_join(table_with_links, by = c('Player', 'AVG', 'P'))
        
        stats_url <- paste0("https://baseball.pointstreak.com/team_stats.html?teamid=",team_id_, 
                            "&seasonid=33927&view=", stat, "&bset=2&orderby=avg")
        
        page <- read_html(stats_url)
        
        table <- page %>% html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
          filter(!grepl('Total',Player))
        
        # Extracting links from column 1
        links <- page %>%
          html_nodes("table tr td:nth-child(1) a") %>%
          purrr::map_df(~ data.frame(link_text = html_text(.), link_url = html_attr(., "href")))
        
        # Combine links with table data
        table_with_links <- table
        # %>%
        #   mutate(player_id = as.numeric(sub(".*playerid=([0-9]+).*", "\\1", link_url)))
        
        team_batting <- team_batting %>% left_join(table_with_links, by = c('Player', 'AVG', 'P'))  %>%
          mutate(Player = ifelse(substr(Player,1,2)=='x ', gsub('x ',"",Player), Player)) %>%
          mutate(Team = team_name, .after = Player) %>%
          mutate(name = rosters$Player[match(player_id, rosters$player_id)],
                 Player = ifelse( !is.na(name), name, Player)) %>%
          select(-name)
        
        
        
      } else if (stat== 'pitching') {
        # stat = 'pitching'
        
        stats_url <- paste0("https://baseball.pointstreak.com/team_stats.html?teamid=",team_id_, 
                            "&seasonid=33927&view=", stat, "&pset=0&orderby=era")
        
        page <- read_html(stats_url)
        
        table <- page %>% html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
          filter(!grepl('Total',Player))
        
        # Extracting links from column 1
        links <- page %>%
          html_nodes("table tr td:nth-child(1) a") %>%
          purrr::map_df(~ data.frame(link_text = html_text(.), link_url = html_attr(., "href")))
        
        # Combine links with table data
        table_with_links <- table %>% left_join(links, by = c('Player' = 'link_text'))  %>%
          mutate(player_id = as.numeric(sub(".*playerid=([0-9]+).*", "\\1", link_url)))
        
        team_pitching <- rbind(team_pitching, table_with_links)
        
        # EXTENDED PITCHING - - - - - - - - - - - - - 
        stats_url <- paste0("https://baseball.pointstreak.com/team_stats.html?teamid=",team_id_, 
                            "&seasonid=33927&view=", stat, "&pset=1&orderby=era")
        
        page <- read_html(stats_url)
        
        table <- page %>% html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
          filter(!grepl('Total',Player))
        
        # Extracting links from column 1
        links <- page %>%
          html_nodes("table tr td:nth-child(1) a") %>%
          purrr::map_df(~ data.frame(link_text = html_text(.), link_url = html_attr(., "href")))
        
        # Combine links with table data
        table_with_links <- table
        # %>%
        #   mutate(player_id = as.numeric(sub(".*playerid=([0-9]+).*", "\\1", link_url)))
        
        team_pitching <- team_pitching %>% left_join(table_with_links, by = c('Player', 'ERA'))
        
        stats_url <- paste0("https://baseball.pointstreak.com/team_stats.html?teamid=",team_id_, 
                            "&seasonid=33927&view=", stat, "&pset=2&orderby=era")
        
        page <- read_html(stats_url)
        
        table <- page %>% html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
          filter(!grepl('Total',Player))
        
        # Extracting links from column 1
        links <- page %>%
          html_nodes("table tr td:nth-child(1) a") %>%
          purrr::map_df(~ data.frame(link_text = html_text(.), link_url = html_attr(., "href")))
        
        # Combine links with table data
        table_with_links <- table
        # %>%
        #   mutate(player_id = as.numeric(sub(".*playerid=([0-9]+).*", "\\1", link_url)))
        
        team_pitching <- team_pitching %>% left_join(table_with_links, by = c('Player', 'ERA')) %>%
          mutate(Player = ifelse(substr(Player,1,2)=='x ', gsub('x ',"",Player), Player)) %>%
          mutate(Team = team_name, .after = Player) %>%
          mutate(name = rosters$Player[match(player_id, rosters$player_id)],
                 Player = ifelse( !is.na(name), name, Player)) %>%
          select(-name) 
        
        
      }
      
      stats_batting <- rbind(stats_batting, team_batting) %>% distinct(.keep_all = T)
      stats_pitching <- rbind(stats_pitching, team_pitching) %>% distinct(.keep_all = T) 
      
    }
    
    # 
    # # Read the webpage
    # page <- read_html(url)
    # 
    # # Extract table
    # table <- page %>% html_nodes("table") %>% html_table(fill = TRUE) %>% .[[1]] %>%
    #   filter(!grepl('Total',Player))
    # 
    # # Extracting links from column 1
    # links <- page %>%
    #   html_nodes("table tr td:nth-child(1) a") %>%
    #   purrr::map_df(~ data.frame(link_text = html_text(.), link_url = html_attr(., "href")))
    # 
    # # Combine links with table data
    # table_with_links <- table %>% left_join(links, by = c('Player' = 'link_text')) %>%
    #   mutate(player_id = as.numeric(sub(".*playerid=([0-9]+).*", "\\1", link_url)))
    # 
    # # Display the combined data
    # print(table_with_links)
    # 
    # 
    
  }
  
  
  
  
  
  
  # STANDINGS - ------------------------------------------ ----
  
  
  standings_url <- 'https://baseball.pointstreak.com/standings.html?leagueid=174&seasonid=33927'
  
  page <- read_html(standings_url)
  
  north <- page %>% html_table(fill = T) %>% .[[1]] %>%
    rename(Team = 1) %>% 
    mutate(Team = str_squish(gsub('†','', Team))) %>%
    mutate(Division = 'North', 
           Season = 2024,
           GB = as.numeric(ifelse(is.na(as.numeric(GB)), 0, GB)))
  
  south <- page %>% html_table(fill = T) %>% .[[2]] %>%
    rename(Team = 1) %>% 
    mutate(Team = str_squish(gsub('†','', Team))) %>%
    mutate(Division = 'South', 
           Season = 2024,
           GB = as.numeric(ifelse(is.na(as.numeric(GB)), 0, GB)))
  
  standings <- rbind(north,south) %>% 
    arrange(desc(PCT))
  
  
  
  
  suppressWarnings(
    suppressMessages(
      rm(
        extracted_data, h4_elements, h4_text, links, page, roster_group, table, 
        table_with_links, tables, team_batting, team_pitching, team_roster, 
        urls_df,i,j,link_texts,roster_url, stat, stats_list, stats_url, status, 
        team_id_, team_name,url, standings_url, east, west)
    )
  )
  
  # TEAM STATS --------------------------------- -----
  
  
  
  stats_team_batting <- stats_batting %>% select(-player_id) %>%
    group_by(Team) %>%
    summarise(across(is.numeric, ~sum(., na.rm = T))) %>%
    mutate(G = standings$W[match(Team,standings$Team)] + standings$L[match(Team, standings$Team)])
  
  stats_team_pitching <- stats_pitching %>% select(-player_id) %>%
    mutate(full_inn = floor(IP),
           part_inn = (IP - floor(IP)) * 10) %>%
    group_by(Team) %>%
    summarise(across(is.numeric, ~sum(., na.rm = T))) %>%
    ungroup() %>%
    mutate(W = standings$W[match(Team,standings$Team)],
           L = standings$L[match(Team, standings$Team)],
           G = W + L) %>%
    mutate(IP = round(full_inn + (part_inn/3),1)) %>% 
    select(-part_inn,-full_inn)
  
  
  missing_stats_bp <- rbind(
    stats_batting %>%
      filter(grepl(',', Player)) %>% 
      select(Player, link_url, player_id), 
    stats_pitching %>%
      filter(grepl(',', Player)) %>% 
      select(Player, link_url, player_id)
  )
  
  missing_players <- data.frame()
  
  # missing_stats_bp <- missing_stats_bp %>% slice(1:nrow(missing_stats_bp))
  
  for(p in seq_along(missing_stats_bp$player_id)) {
    
    pl_id <- missing_stats_bp$player_id[p]
    
    pl_url <- paste0(url_base,missing_stats_bp$link_url[p])
    
    css_selector <- "#ps-content-bootstrapper > div > div > div.col-md-10 > div.row.nova-heading > div.col-sm-9 > h2 > span.nova-title"
    
    player_name <- str_squish(gsub("[#0-9]", "", 
                                   read_html(pl_url) %>% html_nodes(css_selector) %>% html_text(trim = TRUE) ))
    
    length(player_name)
    
    if( length(player_name) > 0){
      new_player <- data.frame(Player = player_name,
                               player_id = pl_id)
      
      missing_players <- rbind(missing_players, new_player)
      
      cat(paste('Found missing name', player_name, p, 'of', nrow(missing_stats_bp)), sep = '\n')
    }
  }
  
  
  
  
  stats_batting <- stats_batting %>%
    mutate(new_name = 
             ifelse(grepl(",",Player), missing_players$Player[match(player_id, missing_players$player_id)], Player),.after=Player,
           Player = ifelse(!is.na(new_name), new_name, Player )
    ) %>%
    select(-new_name)
  
  stats_pitching <- stats_pitching %>%
    mutate(new_name = 
             ifelse(grepl(",",Player), missing_players$Player[match(player_id, missing_players$player_id)], Player),.after=Player,
           Player = ifelse(!is.na(new_name), new_name, Player )
    ) %>%
    select(-new_name)
  
  lg_batting <- stats_batting %>% select(-player_id) %>%
    summarise(Team = 'League', across(is.numeric, ~sum(., na.rm = T))) %>%
    mutate(across(2:ncol(.), ~ as.numeric(.))) %>%
    mutate(`1B` = H - HR - `2B` - `3B`,
           AVG = as.numeric(formatC(formatC(round((H / AB), 3), format = 'f', digits = 3), format = 'f', digits = 3)),
           OBP = as.numeric(formatC(round(((H + BB + HBP) / PA) ,3), format = 'f', digits = 3)),
           SLG = as.numeric(formatC(round(((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)) / AB) ,3), format = 'f', digits = 3)),
           OPS = as.numeric(formatC(round((OBP + SLG),3),format = 'f', digits = 3)),
           #   wOBA = #((.69* uIBB)+(.719*HBP)+(.87*1B)+(1.217*2B)+(1.529*3B)+(1.94*HR))/(AB+uiBB+HBP+SF)
           wOBA= as.numeric(formatC(round((((0.6572260*HBP)+(BB*0.6603780)+
                                              (0.8869003*`1B`)+(1.1696127*`2B`)+
                                              (1.4508623*`3B`)+(1.7302153*HR))/
                                             (AB+BB+HBP+SF+SH)) ,3), format = 'f', digits = 3)) #* 0.9841693
    )
  
  lg_pitching <- stats_pitching %>% select(-player_id) %>%
    mutate(full_inn = floor(IP),
           part_inn = (IP - floor(IP)) * 10,
           AB = round(H / BAA),
           PA = AB + BB + HBP)  %>%
    summarise(Team = 'League', across(is.numeric, ~sum(., na.rm = T))) %>%
    mutate(IP = round(full_inn + (part_inn/3),1)) %>% 
    select(-part_inn,-full_inn) %>%
    mutate(`1B` = H - `2B` - `3B` - HR,
           ERA = round(ER / IP * 9,2),
           PA = AB + BB + HBP ,
           BAA = round(H / AB, 3)
    )
  
  lg_obp <- lg_batting$OBP
  
  lg_woba_raw <- lg_batting$wOBA
  
  lg_woba_scale <- lg_obp / lg_woba_raw

  lg_woba <- lg_batting$wOBA * lg_woba_scale
    
  RunsPerWin<-round((9*(lg_pitching$R / lg_pitching$IP)*1.5)+3,2)
  
  lg_r_pa <- lg_pitching$R / lg_pitching$PA
  
  lg_run_out <- lg_pitching$R / (lg_pitching$IP*3)
  
  lg_runSB <- .2
    
  lg_runCS <- 2 * lg_run_out + 0.075
  
  lgwSB <- ((lg_batting$SB * .2) + (lg_batting$CS * lg_runCS)) / (lg_batting$`1B` + lg_batting$BB + lg_batting$HBP)
  
    
    
  league_batting <- lg_batting %>%
    mutate(
      wOBA = wOBA * lg_woba_scale,
      # `GO/FO` = round(GO/FO,1),
      `AB/HR` = round(HR/AB,3),
      `BB/PA` = round(BB/PA,3),
      `BB/K` = round(BB/SO,3),
      `SB%` = formatC(round((((SB/(SB + CS)))*100),1),format = 'f', digits = 1),
      `SB%`= sub('NaN%','0',`SB%`),
      `SB%`= sub('NaN','0',`SB%`),
      `SB%` = as.numeric(`SB%`),
      wRAA = as.numeric(formatC(round((((wOBA - lg_woba) / lg_woba_scale) * PA),2),format = 'f', digits=2)),
      RC = as.numeric(formatC(round(((((H+BB-CS)*
                                         ((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)+
                                             .26*(BB+HBP))+(.52*(SF+SH+SB))))) / 
                                       PA),2),format = 'f', digits = 2)),
      wRC = as.numeric(formatC(round(((((wOBA - lg_woba) / lg_woba_scale) + 
                                         (lg_r_pa))*PA),2),format = 'f', digits =2)),
      
      `wRC+` = round(((((wRAA/PA) + lg_r_pa) + (lg_r_pa - (1 * lg_r_pa))) / (tail(wRC, n=1)/ tail(PA, n=1)) )*100),
      `BB%` = round((BB / PA),3)*100,
      `K%` = round((SO / PA),3)*100,
      
      BattingRuns= round(wRAA +((lg_r_pa -(.1*lg_r_pa))*PA),2),
      BABIP = as.numeric(formatC(round(((H - HR) / (AB - HR - SO + SF)),3),format = 'f', digits = 3))    ,
      BABIP = ifelse(BABIP =='NaN', 0, BABIP),
      wSB = round( ((SB * lg_runSB)+ (CS * lg_runCS))  -  (lgwSB*(`1B`+BB+HBP)),2),
      wSB = ifelse(wSB == 'NaN', 0, wSB),
      oWAR= round((BattingRuns + wSB)/RunsPerWin,2),
      ISO = SLG - AVG,
      ISOP = SLG - AVG,
      `OPS+` = round(((OBP/tail(OBP, n=1)) + (SLG/tail(SLG, n=1)) -1) * 1 * 100),
      SEASON = 2024
      
    ) %>%
    # mutate_all(~ifelse(.=='NaN',0,.)) %>%
    mutate(across(2:ncol(.), ~ as.numeric(.))) %>%
    mutate(woba_scale = lg_woba_scale)
  
  cfip_w <- lg_pitching$ERA - (((13*lg_pitching$HR)+(3*(lg_pitching$BB+lg_pitching$HBP))-(2*(lg_pitching$SO)))/lg_pitching$IP)
  
  league_pitching <- lg_pitching %>%
    mutate(
      vOBP = as.numeric(formatC(round(((H + BB + HBP) / PA) ,3), format = 'f', digits = 3)),
      vSLG = as.numeric(formatC(round(((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)) / AB) ,3), format = 'f', digits = 3)),
      vOPS = as.numeric(formatC(round((vOBP + vSLG),3),format = 'f', digits = 3)),
      vwOBA = as.numeric(formatC(round((((0.6572260*HBP)+(BB*0.6603780)+
                                           (0.8869003*`1B`)+(1.1696127*`2B`)+
                                           (1.4508623*`3B`)+(1.7302153*HR))/
                                          (AB+BB+HBP)) ,3), format = 'f', digits = 3)) * lg_woba_scale,
      vwRAA = as.numeric(formatC(round((((vwOBA - lg_woba) / lg_woba_scale) * 
                                          PA),2),format = 'f', digits=2)),
      vRC = as.numeric(formatC(round(((((H+BB)*
                                          ((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)+
                                              .26*(BB+HBP))))) / 
                                        PA),2),format = 'f', digits = 2)),
      vwRC = as.numeric(formatC(round(((((vwOBA - lg_woba) / lg_woba_scale) + 
                                          (lg_r_pa))*PA),2),format = 'f', digits =2)),
      `vwRC+` = round(((((vwRAA/PA) + lg_r_pa) + (lg_r_pa - (1 * lg_r_pa))) / (tail(vwRC, n=1)/ tail(PA, n=1)) )*100),
      `BB%` = round((BB / PA),3)*100,
      `K%` = round((SO / PA),3)*100,
      vBattingRuns= round(vwRAA +((lg_r_pa -(.1*lg_r_pa))*PA),2),
      # Team FIP = (((13 * HR) + (3 * (BB + HBP)) - (2 * K)) / IP) + FIP constant
      FIP = as.numeric(formatC(round(((((13 * HR) + (3 * (BB+HBP)) - (2 * SO))/ IP) + cfip_w),2),format = 'f',digits = 2)),
      `K/9` = formatC(round((SO / IP * 9), 2), format = 'f', digits = 2)   ,
      `BB/9` = formatC(round((BB / IP * 9), 2), format = 'f', digits = 2),
      `K%` = formatC(round((SO / PA), 2), format = 'f', digits = 2),
      #BB%
      `BB%` = formatC(round((BB / PA), 2), format = 'f', digits = 2),
      #WHIP
      WHIP = formatC(round(((BB + H) / IP), 2), format = 'f', digits = 2),
      vISO = vSLG - BAA,
      vBABIP = as.numeric(formatC(round(((H - HR) / (AB - HR - SO)),3),format = 'f', digits = 3)),
      BIPA = as.numeric(formatC(round(((H - HR) / (AB - HR - SO)),3),format = 'f', digits = 3)),
      `FIP-` = as.numeric(formatC(round(100*((FIP / FIP)     ))))
    )
  
  
  toc()
}

# PLAYER LEVEL ----


sb <- stats_batting %>% 
  mutate(`1B` = H - HR - `2B` - `3B`,
         AVG = as.numeric(formatC(formatC(round((H / AB), 3), format = 'f', digits = 3), format = 'f', digits = 3)),
         OBP = as.numeric(formatC(round(((H + BB + HBP) / PA) ,3), format = 'f', digits = 3)),
         SLG = as.numeric(formatC(round(((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)) / AB) ,3), format = 'f', digits = 3)),
         OPS = as.numeric(formatC(round((OBP + SLG),3),format = 'f', digits = 3)),
         #   wOBA = #((.69* uIBB)+(.719*HBP)+(.87*1B)+(1.217*2B)+(1.529*3B)+(1.94*HR))/(AB+uiBB+HBP+SF)
         wOBA= as.numeric(formatC(round((((0.6572260*HBP)+(BB*0.6603780)+
                                            (0.8869003*`1B`)+(1.1696127*`2B`)+
                                            (1.4508623*`3B`)+(1.7302153*HR))/
                                           (AB+BB+HBP+SF+SH)) ,3), format = 'f', digits = 3)) * lg_woba_scale,
         #wOBA = wOBA * 
         # `GO/FO` = round(GO/FO,1),
         `AB/HR` = round(HR/AB,3),
         `BB/PA` = round(BB/PA,3),
         `BB/K` = round(BB/SO,3),
         `SB%` = formatC(round((((SB/(SB + CS)))*100),1),format = 'f', digits = 1),
         `SB%`= sub('NaN%','0',`SB%`),
         `SB%`= sub('NaN','0',`SB%`),
         `SB%` = as.numeric(`SB%`),
         wRAA = as.numeric(formatC(round((((wOBA - lg_woba) / lg_woba_scale) * PA),2),format = 'f', digits=2)),
         RC = as.numeric(formatC(round(((((H+BB-CS)*
                                            ((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)+
                                                .26*(BB+HBP))+(.52*(SF+SH+SB))))) / 
                                          PA),2),format = 'f', digits = 2)),
         wRC = as.numeric(formatC(round(((((wOBA - lg_woba) / lg_woba_scale) + 
                                            (lg_r_pa))*PA),2),format = 'f', digits =2)),
         
         `wRC+` = round(((((wRAA/PA) + lg_r_pa) + (lg_r_pa - (1 * lg_r_pa))) / (league_batting$RC / league_batting$PA) )*100),
         `BB%` = round((BB / PA),3)*100,
         `K%` = round((SO / PA),3)*100,
         
         BattingRuns= round(wRAA +((lg_r_pa -(.1*lg_r_pa))*PA),2),
         BABIP = as.numeric(formatC(round(((H - HR) / (AB - HR - SO + SF)),3),format = 'f', digits = 3))    ,
         BABIP = ifelse(BABIP =='NaN', 0, BABIP),
         wSB = round( ((SB * weights$runSB)+ (CS * weights$runCS))  -  (lgwSB*(`1B`+BB+HBP)),2),
         wSB = ifelse(wSB == 'NaN', 0, wSB),
         oWAR= round((BattingRuns + wSB)/RunsPerWin,2),
         ISO = SLG - AVG,
         ISOP = SLG - AVG,
         `OPS+` = round(((OBP/league_batting$OBP) + (SLG/league_batting$SLG) -1) * 1 * 100),
         SEASON = 2024
         
  ) 


sp <- stats_pitching %>%
  mutate(AB = round(H / BAA),
         PA = AB + BB + HBP,
         `1B` = H - `2B` - `3B` - HR,
         PA = AB + BB + HBP ,
         # BAA = round(H/AB,3),
         vOBP = as.numeric(formatC(round(((H + BB + HBP) / PA) ,3), format = 'f', digits = 3)),
         vSLG = as.numeric(formatC(round(((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)) / AB) ,3), format = 'f', digits = 3)),
         vOPS = as.numeric(formatC(round((vOBP + vSLG),3),format = 'f', digits = 3)),
         vwOBA = as.numeric(formatC(round((((0.6572260*HBP)+(BB*0.6603780)+
                                              (0.8869003*`1B`)+(1.1696127*`2B`)+
                                              (1.4508623*`3B`)+(1.7302153*HR))/
                                             (AB+BB+HBP)) ,3), format = 'f', digits = 3)) * lg_woba_scale,
         vwRAA = as.numeric(formatC(round((((vwOBA - lg_woba) / lg_woba_scale) * 
                                             PA),2),format = 'f', digits=2)),
         vRC = as.numeric(formatC(round(((((H+BB)*
                                             ((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)+
                                                 .26*(BB+HBP))))) / 
                                           PA),2),format = 'f', digits = 2)),
         vwRC = as.numeric(formatC(round(((((vwOBA - lg_woba) / lg_woba_scale) + 
                                             (lg_r_pa))*PA),2),format = 'f', digits =2)),
         `vwRC+` = round(((((vwRAA/PA) + lg_r_pa) + (lg_r_pa - (1 * lg_r_pa))) / (league_batting$`RC`/ league_batting$PA) )*100),
         `BB%` = round((BB / PA),3)*100,
         `K%` = round((SO / PA),3)*100,
         vBattingRuns= round(vwRAA +((lg_r_pa -(.1*lg_r_pa))*PA),2),
         # Team FIP = (((13 * HR) + (3 * (BB + HBP)) - (2 * K)) / IP) + FIP constant
         FIP = as.numeric(formatC(round(((((13 * HR) + (3 * (BB+HBP)) - (2 * SO))/ IP) + cfip_w),2),format = 'f',digits = 2)),
         `K/9` = formatC(round((SO / IP * 9), 2), format = 'f', digits = 2)   ,
         `BB/9` = formatC(round((BB / IP * 9), 2), format = 'f', digits = 2),
         `K%` = formatC(round((SO / PA), 2), format = 'f', digits = 2),
         #BB%
         `BB%` = formatC(round((BB / PA), 2), format = 'f', digits = 2),
         #WHIP
         WHIP = formatC(round(((BB + H) / IP), 2), format = 'f', digits = 2),
         vISO = vSLG - BAA,
         `OPS+` = round(((vOBP/league_batting$OBP) + (vSLG/league_batting$SLG) -1) * 1 * 100),
         vBABIP = as.numeric(formatC(round(((H - HR) / (AB - HR - SO)),3),format = 'f', digits = 3)),
         `FIP-` = as.numeric(formatC(round(100*((FIP / league_pitching$FIP)     ))))
  )

# TEAM LEVEL ----

stb <- stats_team_batting %>%
  mutate(`1B` = H - HR - `2B` - `3B`,
         AVG = as.numeric(formatC(formatC(round((H / AB), 3), format = 'f', digits = 3), format = 'f', digits = 3)),
         OBP = as.numeric(formatC(round(((H + BB + HBP) / PA) ,3), format = 'f', digits = 3)),
         SLG = as.numeric(formatC(round(((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)) / AB) ,3), format = 'f', digits = 3)),
         OPS = as.numeric(formatC(round((OBP + SLG),3),format = 'f', digits = 3)),
         #   wOBA = #((.69* uIBB)+(.719*HBP)+(.87*1B)+(1.217*2B)+(1.529*3B)+(1.94*HR))/(AB+uiBB+HBP+SF)
         wOBA= as.numeric(formatC(round((((0.6572260*HBP)+(BB*0.6603780)+
                                            (0.8869003*`1B`)+(1.1696127*`2B`)+
                                            (1.4508623*`3B`)+(1.7302153*HR))/
                                           (AB+BB+HBP+SF+SH)) ,3), format = 'f', digits = 3)) * lg_woba_scale,
         #wOBA = wOBA * 
         # `GO/FO` = round(GO/FO,1),
         `AB/HR` = round(HR/AB,3),
         `BB/PA` = round(BB/PA,3),
         `BB/K` = round(BB/SO,3),
         `SB%` = formatC(round((((SB/(SB + CS)))*100),1),format = 'f', digits = 1),
         `SB%`= sub('NaN%','0',`SB%`),
         `SB%`= sub('NaN','0',`SB%`),
         `SB%` = as.numeric(`SB%`),
         wRAA = as.numeric(formatC(round((((wOBA - lg_woba) / lg_woba_scale) * PA),2),format = 'f', digits=2)),
         RC = as.numeric(formatC(round(((((H+BB-CS)*
                                            ((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)+
                                                .26*(BB+HBP))+(.52*(SF+SH+SB))))) / 
                                          PA),2),format = 'f', digits = 2)),
         wRC = as.numeric(formatC(round(((((wOBA - lg_woba) / lg_woba_scale) + 
                                            (lg_r_pa))*PA),2),format = 'f', digits =2)),
         
         `wRC+` = round(((((wRAA/PA) + lg_r_pa) + (lg_r_pa - (1 * lg_r_pa))) / (league_batting$RC / league_batting$PA) )*100),
         `BB%` = round((BB / PA),3)*100,
         `K%` = round((SO / PA),3)*100,
         
         BattingRuns= round(wRAA +((lg_r_pa -(.1*lg_r_pa))*PA),2),
         BABIP = as.numeric(formatC(round(((H - HR) / (AB - HR - SO + SF)),3),format = 'f', digits = 3))    ,
         BABIP = ifelse(BABIP =='NaN', 0, BABIP),
         wSB = round( ((SB * weights$runSB)+ (CS * weights$runCS))  -  (lgwSB*(`1B`+BB+HBP)),2),
         wSB = ifelse(wSB == 'NaN', 0, wSB),
         oWAR= round((BattingRuns + wSB)/RunsPerWin,2),
         ISO = SLG - AVG,
         ISOP = SLG - AVG,
         `OPS+` = round(((OBP/league_batting$OBP) + (SLG/league_batting$SLG) -1) * 1 * 100),
         SEASON = 2024
         
  ) 

stp <-  stats_team_pitching %>%
  mutate(AB = round(H / BAA),
         PA = AB + BB + HBP,
         `1B` = H - `2B` - `3B` - HR,
         PA = AB + BB + HBP ,
         # BAA = round(H/AB,3),
         vOBP = as.numeric(formatC(round(((H + BB + HBP) / PA) ,3), format = 'f', digits = 3)),
         vSLG = as.numeric(formatC(round(((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)) / AB) ,3), format = 'f', digits = 3)),
         vOPS = as.numeric(formatC(round((vOBP + vSLG),3),format = 'f', digits = 3)),
         vwOBA = as.numeric(formatC(round((((0.6572260*HBP)+(BB*0.6603780)+
                                              (0.8869003*`1B`)+(1.1696127*`2B`)+
                                              (1.4508623*`3B`)+(1.7302153*HR))/
                                             (AB+BB+HBP)) ,3), format = 'f', digits = 3)) * lg_woba_scale,
         vwRAA = as.numeric(formatC(round((((vwOBA - lg_woba) / lg_woba_scale) * 
                                             PA),2),format = 'f', digits=2)),
         vRC = as.numeric(formatC(round(((((H+BB)*
                                             ((`1B`+(`2B`*2)+(`3B`*3)+(HR*4)+
                                                 .26*(BB+HBP))))) / 
                                           PA),2),format = 'f', digits = 2)),
         vwRC = as.numeric(formatC(round(((((vwOBA - lg_woba) / lg_woba_scale) + 
                                             (lg_r_pa))*PA),2),format = 'f', digits =2)),
         `vwRC+` = round(((((vwRAA/PA) + lg_r_pa) + (lg_r_pa - (1 * lg_r_pa))) / (league_batting$`RC`/ league_batting$PA) )*100),
         `BB%` = round((BB / PA),3)*100,
         `K%` = round((SO / PA),3)*100,
         vBattingRuns= round(vwRAA +((lg_r_pa -(.1*lg_r_pa))*PA),2),
         # Team FIP = (((13 * HR) + (3 * (BB + HBP)) - (2 * K)) / IP) + FIP constant
         FIP = as.numeric(formatC(round(((((13 * HR) + (3 * (BB+HBP)) - (2 * SO))/ IP) + cfip_w),2),format = 'f',digits = 2)),
         `K/9` = formatC(round((SO / IP * 9), 2), format = 'f', digits = 2)   ,
         `BB/9` = formatC(round((BB / IP * 9), 2), format = 'f', digits = 2),
         `K%` = formatC(round((SO / PA), 2), format = 'f', digits = 2),
         #BB%
         `BB%` = formatC(round((BB / PA), 2), format = 'f', digits = 2),
         #WHIP
         WHIP = formatC(round(((BB + H) / IP), 2), format = 'f', digits = 2),
         vISO = vSLG - BAA,
         `OPS+` = round(((vOBP/league_batting$OBP) + (vSLG/league_batting$SLG) -1) * 1 * 100),
         vBABIP = as.numeric(formatC(round(((H - HR) / (AB - HR - SO)),3),format = 'f', digits = 3)),
         `FIP-` = as.numeric(formatC(round(100*((FIP / league_pitching$FIP)     ))))
  )

dbWriteTable(atl, name = 'rosters', rosters, overwrite = T)
dbWriteTable(atl, name = 'teams', team_info, overwrite = T)
dbWriteTable(atl, name = 'standings', standings, overwrite = T)
dbWriteTable(atl, name = 'stats_batting', sb, overwrite = T)
dbWriteTable(atl, name = 'stats_team_batting', stb, overwrite = T)
dbWriteTable(atl, name = 'stats_league_batting', league_batting, overwrite = T)
dbWriteTable(atl, name = 'stats_pitching', sp, overwrite = T)
dbWriteTable(atl, name = 'stats_team_pitching', stp, overwrite = T)
dbWriteTable(atl, name = 'stats_league_pitching', league_pitching, overwrite = T)


rm("css_selector", "extract_links", "league_batting", 
   "league_pitching", "lg_batting", "lg_pitching", "lg_url", "lgwSB", 
   "missing_players", "missing_stats_bp", "new_player", "p", "pf", 
   "pl_id", "pl_url", "player_name", "rosters", "RunsPerWin", "sb", 
   "sp", "standings", "stats_batting", "stats_pitching", "stats_team_batting", 
   "stats_team_pitching", "stb", "stp", "team_info", "url_base", 
   "weights")

dput(ls())
