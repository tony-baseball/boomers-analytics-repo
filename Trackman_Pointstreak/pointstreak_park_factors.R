library(httr)
library(tidyverse)
library(rvest)

db_aa <- dbConnect(SQLite(), "C:/Users/tdmed/OneDrive/_Trackman/american_association.sqlite")

lg <- dbGetQuery(db_aa, "select * from teams where league_url_season like '%33873%'")
# Define the URL
seasonid <- 33873

url <- "https://baseball.pointstreak.com/textstats/menu_teams.html?seasonid=33873"

page <- read_html(url)

teams_df <- page %>%
  html_elements("a") %>%
  purrr::map_df(~{
    tibble(
      text = html_text2(.x),
      href = html_attr(.x, "href")
    )
  }) %>%
  mutate(href = paste0("https://baseball.pointstreak.com", href)) %>%
  filter(grepl("team",href)) %>%
  mutate(team_id = sub(".*teamid=([^&]+).*", "\\1", href))

team_ids <- teams_df$team_id

home_stats_for_total <- data.frame()
home_stats_against_total <- data.frame()

for(i in team_ids) {
  
  url <- glue::glue("https://pointstreak.com/baseball/textstats/reports/seasons/{seasonid}/teams/{i}/team_home_road_report.txt")
  
  team_name <- teams_df$text[match(i,teams_df$team_id)]
  # Get the text content from the URL
  response <- GET(url)
  content <- content(response, as = "text")
  
  # Split the content by line breaks
  lines <- str_split(content, "\n")[[1]]
  
  text_lines <- readLines(url)
  # team_name <- str_squish(text_lines[4])
  # Find the line where HOME TEAM TOTAL BATTING STATS begins
  start_line <- grep("HOME TEAM TOTAL BATTING STATS", text_lines)
  stat_lines <- text_lines[start_line + 1:3]  # Next two lines after the header
  
  # Split the lines into vectors
  headers <- strsplit(trimws(stat_lines[2]), "\\s+")[[1]]
  values <- strsplit(trimws(stat_lines[3]), "\\s+")[[1]]
  
  # Convert to dataframe
  home_stats <- as.data.frame(t(values), stringsAsFactors = FALSE)
  colnames(home_stats) <- headers
  
  # Optionally, convert to appropriate types
  home_stats_for <- type.convert(home_stats, as.is = TRUE) %>%
    mutate(ballpark = team_name,
           season = 2024, .before = 1)
  
  home_stats_for_total <- rbind(home_stats_for_total, home_stats_for)
  print(home_stats_for)
  
  # Find the line where HOME TEAM TOTAL BATTING STATS begins
  start_line <- grep("HOME TEAM TOTAL PITCHING STATS", text_lines)
  stat_lines <- text_lines[start_line + 1:3]  # Next two lines after the header
  
  # Split the lines into vectors
  headers <- strsplit(trimws(stat_lines[2]), "\\s+")[[1]]
  values <- strsplit(trimws(stat_lines[3]), "\\s+")[[1]]
  
  # Convert to dataframe
  home_stats <- as.data.frame(t(values), stringsAsFactors = FALSE)
  colnames(home_stats) <- headers
  
  # Optionally, convert to appropriate types
  home_stats_against <- type.convert(home_stats, as.is = TRUE)%>%
    mutate(ballpark = team_name,
           season = 2024, .before = 1)
  
  home_stats_against_total <- rbind(home_stats_against_total, home_stats_against)
  # print(home_stats_against)
  
  print(i)
}

colnames(home_stats_for_total)
colnames(home_stats_against_total)

ballparks <- home_stats_for_total %>%
  select(ballpark, G, R, H, `2B`, `3B`, HR, BB, HBP = HP) %>% 
  mutate(`1B` = H - `2B` - `3B` - HR,
         h_a = 'home') %>%
  rbind(home_stats_against_total %>%
          select(ballpark, G, R, H, `2B`, `3B`, HR, BB, HBP = HB) %>%
          mutate(`1B` = H - `2B` - `3B` - HR,
                 h_a = 'away') 
        ) %>%
  arrange(ballpark, desc(h_a)) %>%
  group_by(ballpark) %>%
  summarise(G = mean(G),
            across(R:`1B`, ~ sum(., na.rm = T)))

league_totals <- ballparks %>%
  summarise(across(c(G:`1B`), sum))

league_avgs <- league_totals %>%
  mutate(
    R_pg = R / G,
    H_pg = H / G,
    `1B_pg` = `1B` / G,
    `2B_pg` = `2B` / G,
    `3B_pg` = `3B` / G,
    HR_pg = HR / G,
    BB_pg = BB / G,
    HBP_pg = HBP / G
  )

park_factors <- ballparks %>%
  mutate(
    R_PF  = (R / G) / league_avgs$R_pg * 100,
    H_PF  = (H / G) / league_avgs$H_pg * 100,
    `1B_PF` = (`1B` / G) / league_avgs$`1B_pg` * 100,
    `2B_PF` = (`2B` / G) / league_avgs$`2B_pg` * 100,
    `3B_PF` = (`3B` / G) / league_avgs$`3B_pg` * 100,
    HR_PF = (HR / G) / league_avgs$HR_pg * 100,
    BB_PF = (BB / G) / league_avgs$BB_pg * 100,
    HBP_PF = (HBP / G) / league_avgs$HBP_pg * 100,
    PF = (R_PF + H_PF + `2B_PF` + `3B_PF` + HR_PF + `1B_PF` + BB_PF + HBP_PF)/8,
    across(R_PF:PF, ~ ((. * G) + (100 * 50)) / (G + 50) ),
    # Regressed_PF = ((Raw_PF * G) + (100 * 150)) / (G + 150)
  ) %>%
  select(ballpark, ends_with("PF"))
