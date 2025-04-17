library(dplyr)
library(rvest)
library(slackr)
library(stringr)
library(tictoc)
library(RSQLite)

# # PECOS LEAGUE - 116 / 109 seconds 11/14/2024
# pldb <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/pecos_league.sqlite")
# 
# # ATLANTIC LEAGUE - 200 / 187 seconds 11/14/2024
atldb <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/atlantic_league.sqlite")
# 
# # AMERICAN ASSOCIATION - 150 / 80 seconds 11/14/2024
# aadb <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/american_association.sqlite")
# Summer collegiate leagues - MLB Draft, Northwoods, Prospect
# # CAPE COD
# db_summer<- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/summer_collegiate.sqlite") 

# cc_h_p <- data.frame()
# cc_h_t <- data.frame()
# cc_h_l <- data.frame()

cc_p_p <- data.frame()
cc_p_t <- data.frame()
cc_p_l <- data.frame()
# FRONTIER LEAGUE
db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")
yrz <- 2023:2021
lg_name <- 'Cape'
tictoc::tic()
for (yr in yrz) {
  
  indy_leagues<-dbGetQuery(db, glue::glue('select * from indy_leagues_pointstreak where league like "%{lg_name}%" 
                         and season = {yr} and season_type = "regular" '))
  
  # create scrape function for rosters, player stats, team stats, league stats, etc.
  leagueid <- indy_leagues$leagueid
  seasonid <- indy_leagues$seasonid
  leagueseason <- indy_leagues$season
  leaguename <- indy_leagues$league
  league_code <- indy_leagues$league_code
  {
    if(leaguename == 'Atlantic League') {
      # Atlantic League 2021-2024
      db_atl <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/atlantic_league.sqlite") 
      w<<- dbGetQuery(db_atl, 'select * from weights') %>%
        mutate(across(1:14, ~as.numeric(.)))
    } else if (leaguename == 'American Association') {
      # American Association 2021-2024
      db_aa <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/american_association.sqlite") 
      w<<- dbGetQuery(db_aa, 'select * from weights')%>%
        mutate(across(1:14, ~as.numeric(.)))
    } else if (leaguename == 'Frontier League') {
      # American Association 2021-2024
      db_fl <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite") 
      w<<- dbGetQuery(db_aa, 'select * from weights')%>%
        mutate(across(1:14, ~as.numeric(.)))
    }
    
    # tictoc::tic()
    
    cat(paste('Scraping', leagueseason, leaguename),sep = '\n')
    
    tonybaseball::pointstreak_scrape_team_links(glue::glue('https://baseball.pointstreak.com/teamlist.html?leagueid={leagueid}&seasonid={seasonid}'))
    
    tonybaseball::pointstreak_scrape_rosters_player_stats(team_links)
    
    tonybaseball::pointstreak_scrape_standings(glue::glue('https://baseball.pointstreak.com/standings.html?leagueid={leagueid}&seasonid={seasonid}'))
    
    tonybaseball::pointstreak_scrape_team_stats_abbr(glue::glue('https://pointstreak.com/baseball/stats.html?leagueid={leagueid}&seasonid={seasonid}&view=teampitching'))
    
    {
      stats_team_batting <- stats_batting %>% select(-player_id) %>%
        group_by(Team) %>%
        summarise(across(is.numeric, ~sum(., na.rm = T))) %>%
        mutate(G = standings$W[match(Team,standings$Team)] + standings$L[match(Team, standings$Team)],
               AVG = H/AB)
      
      stats_team_pitching <- p_tm_  %>%
        left_join(stats_pitching %>% 
                    select(Team, X2B= `2B`, X3B = `3B`)%>%
                    group_by(Team) %>%
                    summarise(across(c(`X2B`, `X3B`), ~sum(., na.rm = T))) %>%
                    ungroup(), 
                  by = 'Team') %>%
        relocate(X2B, X3B,HR, .after = H) %>%
        relocate(BF, AB, .after = IP) %>%
        select(-c(url,team_id))
      
      stats_team_pitching <- p_tm_ %>%
        left_join(
          stats_pitching %>%
            group_by(Team) %>%
            summarise(
              X2B = ifelse("2B" %in% names(stats_pitching), sum(`2B`, na.rm = TRUE), sum(`X2B`, na.rm = TRUE)), # Default to 0 if 2B doesn't exist
              X3B = ifelse("3B" %in% names(stats_pitching), sum(`3B`, na.rm = TRUE), sum(`X3B`, na.rm = TRUE))  # Default to 0 if 3B doesn't exist
            ) %>%
            ungroup(),
          by = 'Team'
        ) %>%
        relocate(X2B, X3B, HR, .after = H) %>%
        relocate(BF, AB, .after = IP) %>%
        select(-c(url, team_id))
      
    }
    
    tonybaseball::pointstreak_missing_players(stats_batting, stats_pitching)
    
    {
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
    }
    
    tonybaseball::pointstreak_lg_stats(stats_batting, stats_pitching)
    
    tonybaseball::pointstreak_stats_transform(stats_batting, stats_pitching, stats_team_batting, stats_team_pitching)
    
    # suppressWarnings(rm(stats_team_batting, stats_team_pitching, stats_batting, stats_pitching, p_tm_,
    #                     "cfip_w","lg_obp", "lg_r_pa", "lg_run_out", "lg_runCS", "lg_runSB", "lg_woba",
    #                     "lg_woba_raw", "lg_woba_scale", "lgwSB","RunsPerWin"))
    
    # tictoc::toc()
    cat(paste('Completed Scraping', leagueseason, indy_leagues$league),sep = '\n')
  }
  
  cat(paste('Completed Scraping', leagueseason, indy_leagues$league),sep = '\n')
  #
  
  
  
  # sort(setdiff(colnames(stats_pitching_player), colnames(all_pitchers_)))
  # sort(setdiff(colnames(all_pitchers_), colnames(stats_pitching_player)))
  
  # atl_h_p <- rbind(atl_h_p, stats_hitting_player)
  # atl_h_t <- rbind(atl_h_t, stats_hitting_team)
  # atl_h_l <- rbind(atl_h_l, stats_hitting_league)
  
  cc_p_p <- plyr::rbind.fill(cc_p_p, stats_pitching_player)
  cc_p_t <- plyr::rbind.fill(cc_p_t, stats_pitching_team)
  cc_p_l <- plyr::rbind.fill(cc_p_l, stats_pitching_league)
  # # OVERWRITE -------------
  {
    # cat(paste('Overwriting', leagueseason, indy_leagues$league, 'to database'),sep = '\n')
    # 
    # dbWriteTable(atldb, name = 'rosters', rosters, overwrite = T)
    # dbWriteTable(atldb, name = 'teams', team_links, overwrite = T)
    # dbWriteTable(atldb, name = 'standings', standings, overwrite = T)
    # dbWriteTable(atldb, name = 'stats_hitting_player', stats_hitting_player, overwrite = T)
    # dbWriteTable(atldb, name = 'stats_hitting_team', stats_hitting_team, overwrite = T)
    # dbWriteTable(atldb, name = 'stats_hitting_league', stats_hitting_league, overwrite = T)
    # dbWriteTable(atldb, name = 'stats_pitching_player', stats_pitching_player, overwrite = T)
    # dbWriteTable(atldb, name = 'stats_pitching_team', stats_pitching_team, overwrite = T)
    # dbWriteTable(atldb, name = 'stats_pitching_league', stats_pitching_league, overwrite = T)
  }
  # Append  -------------
  # { 
  #   cat(paste('Appending', leagueseason, indy_leagues$league, 'to database'),sep = '\n')
  #   dbWriteTable(atldb, name = 'rosters', rosters, append = T)
  #   dbWriteTable(atldb, name = 'teams', team_links, append = T)
  #   dbWriteTable(atldb, name = 'standings', standings, append = T)
  #   dbWriteTable(atldb, name = 'stats_hitting_player', stats_hitting_player, append = T)
  #   dbWriteTable(atldb, name = 'stats_hitting_team', stats_hitting_team, append = T)
  #   dbWriteTable(atldb, name = 'stats_hitting_league', stats_hitting_league, append = T)
  #   dbWriteTable(atldb, name = 'stats_pitching_player', stats_pitching_player, append = T)
  #   dbWriteTable(atldb, name = 'stats_pitching_team', stats_pitching_team, append = T)
  #   dbWriteTable(atldb, name = 'stats_pitching_league', stats_pitching_league, append = T)
  # }
}
tictoc::toc()


spp <- cc_p_p %>%
  mutate(Outs = (floor(IP) * 3) + ((IP - floor(IP) )* 10),
         XBH = `2B`+`3B`+HR) %>%
  # filter(!is.na(SEASON)) %>%
  select(any_of(c(tonybaseball::p_cols_to_select))) 

spl <- cc_p_l %>%
  # filter(!is.na(SEASON)) %>% 
  select(-any_of(c('K%', 'SLG'))) %>%
  mutate(Outs = (floor(IP) * 3) + ((IP - floor(IP) )* 10),
         XBH = `X2B`+`X3B`+HR) %>%
  select(any_of(c(tonybaseball::p_cols_to_select)))

spt <- cc_p_t %>%
  # filter(!is.na(SEASON)) %>% 
  select(-any_of(c('K%', 'SLG'))) %>%
  mutate(Outs = (floor(IP) * 3) + ((IP - floor(IP) )* 10),
         XBH = `X2B`+`X3B`+HR) %>%
  select(any_of(c(tonybaseball::p_cols_to_select)))


# REMOVE PREVIOUS 2025 Data
queries_to_execute <- c(
  'DELETE FROM stats_hitting_player 
WHERE SEASON = 2025;',
'DELETE FROM stats_hitting_team 
WHERE SEASON = 2025;',
'DELETE FROM stats_hitting_league 
WHERE SEASON = 2025;',
'DELETE FROM stats_pitching_player 
WHERE SEASON = 2025;',
'DELETE FROM stats_pitching_team 
WHERE SEASON = 2025;',
'DELETE FROM stats_pitching_league 
WHERE SEASON = 2025;')

for (query in queries_to_execute) {
  dbExecute(db_aa, query)  
  cat(paste('Removing 2025 data from', sub(".*FROM\\s+(.*?)\\s+WHERE.*", "\\1", query) ), sep = '\n')  
  Sys.sleep(2)
}

# -----


dbWriteTable(db_aa, 'stats_pitching_player', spp, overwrite = T)
dbWriteTable(db_aa, 'stats_pitching_team', spt, overwrite = T)
dbWriteTable(db_aa, 'stats_pitching_league', spl, overwrite = T)
