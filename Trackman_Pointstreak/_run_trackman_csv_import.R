library(RSQLite)

db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

r_scripts <- list(
  'boxscore_scrape_today' = 'C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Trackman_Pointstreak/PointStreak_pbp_box_same_day.R',
  'boxscore_scrape_yesterday' = 'C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Trackman_Pointstreak/PointStreak_pbp_box_prev_day.R',
  'trackman_new_csv' = 'C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Trackman_Pointstreak/Trackman_ETL_1_Boomers.R',
  'trackman_batted_ball' = 'C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Trackman_Pointstreak/Trackman_ETL_2_bb_stats.R'
  # ,
  # 'postgame_reports' = 'C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/_run_postgame_reports.R'
  
)

run_trackman_import <- list(
  'boxscore_scrape_today' = TRUE,
  'boxscore_scrape_yesterday' = FALSE,
  'trackman_new_csv' = TRUE,
  'trackman_batted_ball' = TRUE
  # ,
  # 'postgame_reports' = TRUE
)


dcs <- names(run_trackman_import)[unlist(run_trackman_import)]

scripts_to_run <- r_scripts[dcs]

tictoc::tic()

pitch_count_1 <- dbGetQuery(db, 'select count(*) pitch_count from pitch_data where SEASON = 2025')
# pitch_count_1 <- 5
pitch_count_1 

for (report_name in seq_along(scripts_to_run)) {
  
  cat(glue::glue('Running {dcs[report_name]}'), sep = '\n')
  # source(scripts_to_run[report_name][[1]])
  cat(glue::glue('{dcs[report_name]} Completed!'), sep = '\n')
  
  
} 
pitch_count_2 <- dbGetQuery(db, 'select count(*) pitch_count from pitch_data where SEASON = 2025')
# pitch_count_2 <- 10
pitch_count_2

if(pitch_count_2 > pitch_count_1) {
  
  setwd('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/')
  
  source('_run_postgame_reports.R')
  
}
getwd()
tictoc::toc()