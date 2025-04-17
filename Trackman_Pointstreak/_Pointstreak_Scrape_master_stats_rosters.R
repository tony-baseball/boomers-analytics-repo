library(dplyr)
library(rvest)
library(slackr)
library(stringr)
library(tictoc)
library(RSQLite)

source("C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/CopyDB.R")

db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

indy_leagues<-dbGetQuery(db, 'select * from indy_leagues_pointstreak where league = "Frontier League" and season = 2025 and season_type ="regular" ')

# w <- dbGetQuery(db, "SELECT * FROM weights where season = 2023")
# create scrape function for rosters, player stats, team stats, league stats, etc.
leagueid <- indy_leagues$leagueid
seasonid <- indy_leagues$seasonid
leagueseason <- indy_leagues$season
# leagueid <- 200
# seasonid <- 32988
# leagueseason <- 2021
{
  tictoc::tic()
  
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
  
  if(nrow(missing_players)>0){
    miss_player_master <- dbGetQuery(db, 'select * from z_missing_players') %>% 
      rbind(missing_players %>% 
              left_join(missing_stats_bp %>% select(-Player), by = 'player_id') )%>%
      distinct() %>%
      arrange(Player)
    
    stats_batting <- stats_batting %>%
      mutate(new_name = 
               ifelse(grepl(",",Player), miss_player_master$Player[match(player_id, miss_player_master$player_id)], Player),.after=Player,
             Player = ifelse(!is.na(new_name), new_name, Player )
      ) %>%
      select(-new_name)
    
    stats_pitching <- stats_pitching %>%
      mutate(new_name = 
               ifelse(grepl(",",Player), miss_player_master$Player[match(player_id, miss_player_master$player_id)], Player),.after=Player,
             Player = ifelse(!is.na(new_name), new_name, Player )
      ) %>%
      select(-new_name)
  } else if (nrow(missing_players)==0) {
    miss_player_master <- dbGetQuery(db, 'select * from z_missing_players') %>%
      distinct() %>%
      arrange(Player)
    
    stats_batting <- stats_batting %>%
      mutate(new_name = 
               ifelse(grepl(",",Player), miss_player_master$Player[match(player_id, miss_player_master$player_id)], Player),.after=Player,
             Player = ifelse(!is.na(new_name), new_name, Player )
      ) %>%
      select(-new_name)
    
    stats_pitching <- stats_pitching %>%
      mutate(new_name = 
               ifelse(grepl(",",Player), miss_player_master$Player[match(player_id, miss_player_master$player_id)], Player),.after=Player,
             Player = ifelse(!is.na(new_name), new_name, Player )
      ) %>%
      select(-new_name)
  } 
  
  w <- dbGetQuery(db, 'select * from weights where season = 2023')
  
  league_code <- 'frontier'
  
  tonybaseball::pointstreak_lg_stats(stats_batting, stats_pitching)
  
  tonybaseball::pointstreak_stats_transform(stats_batting, stats_pitching, stats_team_batting, stats_team_pitching)
  
  suppressWarnings(rm(stats_team_batting, stats_team_pitching, stats_batting, stats_pitching,
                      p_tm_,
                      "cfip_w","lg_obp", "lg_r_pa", "lg_run_out", "lg_runCS", "lg_runSB", "lg_woba", 
                      "lg_woba_raw", "lg_woba_scale", "lgwSB","RunsPerWin", "w", "league_code"))
  
  tictoc::toc()
}

# PECOS LEAGUE - 43.9 seconds 11/1/2024
# ATLANTIC LEAGUE - 191.9 seconds 11/1/2024
# AMERICAN ASSOCIATION - 167.19 seconds 11/1/2024
# FRONTIER LEAGUE - 

# ADD PLAYER_ID_FL TO RELEVANT TABLES ----------------------------------------------
# as it is in the database
rosters_pre <- dbGetQuery(db, 'select * from rosters')
# as it is in the database
people_pre <- dbGetQuery(db, 'select * from people')
# as it is in the database
people_years_pre <- dbGetQuery(db, 'select * from people_years')
# as it is in the database
max_player_id <- max(people_years_pre$player_id_fl, na.rm = T)

# roster scraped + rosters_pre
rosters_scraped <- rosters_pre %>% 
  slice(1) %>%
  rbind(rosters %>%
          rename(FirstName = first_name,
                 LastName = last_name,
                 B_T = `B/T`,
                 Last_Team = `Last Team`,
                 player_id_point = player_id
          ) %>%
          mutate(BATS = substr(B_T,1,1),
                 THROWS = substr(B_T,3,3),
                 player_id_presto = NA,
                 bbref_id = NA,
                 bbref_link = NA,
                 Name_ = Player,
                 HOMETOWN = NA,
                 player_id_fl = NA
          )) %>%
  filter(SEASON != '2024') %>%
  arrange(LastName) %>%
  mutate(player_id_fl = people_pre$player_id_fl[match(Player, people_pre$Player)] ) %>%
  arrange(LastName) %>%
  mutate(player_id_fl = case_when(
    is.na(player_id_fl) ~ cumsum(Player != lag(Player, default = "") & is.na(player_id_fl)) + max_player_id,
    T ~ player_id_fl
  )
  )

people_years_to_append <- rosters_scraped %>%
  filter(!player_id_fl %in% people_pre$player_id_fl)  

if(nrow(people_years_to_append) > 0 ) {
  people_years_to_append <- people_years_to_append %>%
    mutate(Ht_in = tonybaseball::convert_to_inches(Ht), .after = Ht ,
           k_zone_top = ifelse(!is.na(Ht_in), Ht_in * .57 / 12, mean(Ht_in, na.rm = T) * .57 / 12),
           k_zone_bot = ifelse(!is.na(Ht_in), Ht_in * .25 / 12, mean(Ht_in, na.rm = T) * .25 / 12),
    ) 
}

rosters <- rosters_scraped

# add_people_years <- players_to_add 
new_people_years <- plyr::rbind.fill(people_years_pre, people_years_to_append) %>%
  select(1:19) %>%
  arrange(player_id_fl) %>%
  group_by(player_id_fl) %>%
  tidyr::fill("player_id_point", "BATS", "THROWS", 
              "Wt", "Ht", "player_id_point",
              "bbref_id", "bbref_link", .direction = 'updown') %>%
  distinct() %>%
  ungroup() %>%
  mutate(Ht_in = tonybaseball::convert_to_inches(Ht), .after = Ht ,
         k_zone_top = ifelse(!is.na(Ht_in), Ht_in * .57 / 12, mean(Ht_in, na.rm = T) * .57 / 12),
         k_zone_bot = ifelse(!is.na(Ht_in), Ht_in * .25 / 12, mean(Ht_in, na.rm = T) * .25 / 12),
  )

new_people <- new_people_years %>%
  group_by(Player, player_id_fl) %>%
  mutate(Seasons = paste(unique(SEASON), collapse = ", ") ) %>%
  mutate(presto_ids = paste(unique(player_id_presto),  collapse = ", ") ) %>%
  mutate(point_ids = paste(unique(player_id_point),  collapse = ", ") ) %>%
  filter(SEASON == max(SEASON)) %>%
  distinct(Player, player_id_fl, .keep_all = T) %>%
  select(-c(SEASON, Team, player_url, player_id_presto, player_id_point)) %>%
  relocate(Seasons, .after = player_id_fl) %>%
  ungroup() %>%
  mutate(across(c(presto_ids, point_ids), ~ str_trim(gsub("NA, |, NA |NA", "", .))))


# OVERWRITE PEOPLE, PEOPLE_YEARS
dbWriteTable(db, name = "people_years", value = new_people_years, overwrite = T)
dbWriteTable(db, name = "people", value = new_people, overwrite = T)


stats_hitting_player <- plyr::rbind.fill(
  dbGetQuery(db, 'select * from stats_hitting_player limit 1'),
  stats_hitting_player
) %>%
  slice(-1)%>%
  select(any_of(c(tonybaseball::h_cols_select))) %>%
  mutate(player_id_fl = new_people_years$player_id_fl[match(Player,new_people$Player)])

stats_pitching_player <- plyr::rbind.fill(
  dbGetQuery(db, 'select * from stats_pitching_player limit 1') ,
  stats_pitching_player
) %>% 
  slice(-1)%>%
  select(any_of(c(tonybaseball::p_cols_to_select))) %>%
  mutate(player_id_fl = new_people_years$player_id_fl[match(Player,new_people$Player)])

standings_ <- dbGetQuery(db, 'select * from standings limit 1') %>%
  plyr::rbind.fill(standings %>% 
                     rename(SEASON = Season,
                            Streak = STREAK,
                            Last10 = `LAST 10`))%>%
  slice(-1)
  

# ------

tables_to_alter <- c('rosters', 'standings', 
                     'stats_hitting_player', 'stats_hitting_team', 'stats_hitting_league',
                     'stats_pitching_player', 'stats_pitching_team', 'stats_pitching_league'
)



dbWriteTable(db, name = 'z_missing_players', value = miss_player_master, overwrite = T)
# 
for(temp_table in tables_to_alter) {
  
  cat(glue::glue('Removing previous 2025 data from {temp_table}'), sep = '\n')
  
  Sys.sleep(1)
  
  # dbExecute(db, glue::glue('DELETE FROM {temp_table} WHERE SEASON = 2025;'))
  
  cat(glue::glue('Writing new 2025 data to {temp_table}'), sep = '\n')
  
  # dbWriteTable(db, name = temp_table, value = temp_table, append = T)
  
  Sys.sleep(1)
  
  if(temp_table == 'stats_pitching_league') {
    cat(paste('FINISHED UPDATING 2025 DATA TO FRONTIER LEAGUE DATABASE'), sep = '\n')
  }
}
# 
# source('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Trackman_Pointstreak/PointStreak_pbp_box_prev_day.R')
# dbWriteTable(db, name = 'teams', team_links, overwrite = T)
# dbWriteTable(db, name = 'standings', standings, overwrite = T)
# dbWriteTable(db, name = 'stats_hitting_player', stats_hitting_player, overwrite = T)
# dbWriteTable(db, name = 'stats_hitting_team', stats_hitting_team, overwrite = T)
# dbWriteTable(db, name = 'stats_hitting_league', stats_hitting_league, overwrite = T)
# dbWriteTable(db, name = 'stats_pitching_player', stats_pitching_player, overwrite = T)
# dbWriteTable(db, name = 'stats_pitching_team', stats_pitching_team, overwrite = T)
# dbWriteTable(db, name = 'stats_pitching_league', stats_pitching_league, overwrite = T)