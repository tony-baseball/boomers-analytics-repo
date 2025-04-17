library(RSQLite)
library(tidyr)
library(dplyr)
db <- dbConnect(SQLite(), "C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

league_stats <- dbGetQuery(db, 'select * from stats_pitching_team where SEASON = 2024 and RK = "Total"') %>%
  mutate(ERA = ER / IP * 9,
         X1B = H - HR - X2B - X3B) %>%
  select(SEASON, IP, PA, AB, H, X1B, X2B, X3B, HR, BB) %>%
  full_join(dbGetQuery(db, 'select * from stats_hitting_team where SEASON = 2024 and RK = "Total"') %>%
              select(SEASON, SF, SH, SO= K, HBP = HBP), by = 'SEASON') %>% mutate(SAC = SF + SH) %>% select( -SH,SF)%>%
  mutate(nonSOout = (floor(IP*3) + round((IP - floor(IP))/.33) - SO))

lg_avg <- league_stats  %>%
  mutate(across(5:15, ~ . / PA))


weights_raw <- dbGetQuery(db, 'select * from weights_raw')
  
  
  
pitcher_stats <- dbGetQuery(db, 'select * from stats_pitching_player where Team like "%Boom%" and SEASON = 2024 order by IP desc') %>%
  mutate(nonSOout = ((floor(IP*3)) + (round((IP - floor(IP))/.33) )) - SO
         )%>%
  select(NAME,IP, PA, AB, H, X1B, X2B, X3B, HR, SO, BB, HBP, nonSOout, pWAR, FIP. ) %>%
  mutate(#SO_RV = SO - (PA * lg_avg$SO),
         
         SO_RV = (SO - (PA * lg_avg$SO)) * abs(weights_raw$weight[weights_raw$Result=='Out']),
         HR_RV = (HR - (PA * lg_avg$HR)) * abs(weights_raw$weight[weights_raw$Result=='HR'])*-1,
         BB_RV = (BB - (PA * lg_avg$BB)) * abs(weights_raw$weight[weights_raw$Result=='BB'])*-1,
         HBP_RV = (HBP - (PA * lg_avg$HBP)) * abs(weights_raw$weight[weights_raw$Result=='HBP'])*-1,
         TTO_RV = SO_RV + HR_RV + BB_RV + HBP_RV,
         X1B_RV = (X1B - (PA * lg_avg$X1B)) * abs(weights_raw$weight[weights_raw$Result=='1B'])*-1,
         X2B_RV = (X2B - (PA * lg_avg$X2B)) * abs(weights_raw$weight[weights_raw$Result=='2B'])*-1,
         X3B_RV = (X3B - (PA * lg_avg$X3B)) * abs(weights_raw$weight[weights_raw$Result=='3B'])*-1,
         nonSOout_RV = (nonSOout - (PA * lg_avg$nonSOout)) * abs(weights_raw$weight[weights_raw$Result=='Out']),
         BIP_RV = X1B_RV+X2B_RV+X3B_RV+nonSOout_RV,
         TOTAL_RV = TTO_RV+BIP_RV
         ) %>%
  dplyr::relocate(TOTAL_RV, TTO_RV, BIP_RV, .before = pWAR)
