library(RSQLite)
library(tidyverse)
library(stringr)

db <- dbConnect(SQLite(), "C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")


dbGetQuery(db, 'select * from pitch_data where BatterTeam = "Evansville Otters" and TaggedPitchType in ("Fastball", "Sinker")') %>%
  group_by(Batter)

dbGetQuery(db, 'select * from pitch_data where TaggedPitchType in ("Fastball", "Sinker") and RelSpeed is not null') %>%
  summarize(
    MeanRelSpeed = mean(RelSpeed, na.rm = TRUE),
    StdDevRelSpeed = sd(RelSpeed, na.rm = TRUE),
    Percentile25 = quantile(RelSpeed, 0.25, na.rm = TRUE),
    Percentile50 = quantile(RelSpeed, 0.50, na.rm = TRUE),
    Percentile75 = quantile(RelSpeed, 0.75, na.rm = TRUE)
  )
  

fb <- dbGetQuery(db, 'select SEASON, Pitcher, PitcherThrows, TaggedPitchType, RelSpeed from pitch_data where TaggedPitchType in ("Fastball", "Sinker") and RelSpeed between 80 and 103') %>%
  mutate(RelSpeed = round(RelSpeed)) %>% 
  group_by(SEASON,PitcherThrows)



# Create the bar chart
ggplot(fb, aes(x = RelSpeed)) +
    geom_bar(aes(fill = PitcherThrows), color = "black") +
    labs(
          title = "Frequency of Pitches by Speed",
              x = "Pitch Speed (mph)",
              y = "Frequency"
    ) +
    theme_minimal() +
  facet_wrap(~SEASON, scales = 'free_y')
    

# Step 1: Calculate the median (50th percentile) and standard deviation
stats <- dbGetQuery(db, 'select * from pitch_data where TaggedPitchType in ("Fastball", "Sinker") and RelSpeed between 80 and 103') %>%
  summarize(
    MedianRelSpeed = quantile(RelSpeed, 0.50, na.rm = TRUE),
    StdDevRelSpeed = sd(RelSpeed, na.rm = TRUE)
  )

# Step 2: Define the bucket boundaries
lowest_bound = round(stats$MedianRelSpeed - 1.5 * stats$StdDevRelSpeed)
lower_bound = round(stats$MedianRelSpeed - 0.5 * stats$StdDevRelSpeed)
upper_bound = round(stats$MedianRelSpeed + 0.5 * stats$StdDevRelSpeed)
upper_bound_2 = round(stats$MedianRelSpeed + 1.5 * stats$StdDevRelSpeed)

# Step 3: Create a new column with bucket assignments
buckets <- dbGetQuery(db, 'select * from pitch_data where TaggedPitchType in ("Fastball", "Sinker") and RelSpeed between 80 and 103') %>%
  group_by(Batter, PitcherThrows) %>%
  summarise(P = n(),
            FB_Speed = mean(RelSpeed, na.rm = T)) %>%
  mutate(
    SpeedBucket = cut(
      FB_Speed,
      breaks = c(-Inf, lowest_bound, lower_bound, upper_bound, upper_bound_2, Inf),
      labels = c("Lowest", "Low", "Middle-Low", "Middle-High", "High"),
      right = FALSE
    )
  )  %>%
  select (Batter, FB_Speed, SpeedBucket)
  
cur_team <- dbGetQuery(db, 'select distinct(Batter), BatterTeam, max(Date) from pitch_data group by Batter') 

# BATTER TENDENCIES
hitter_velo_buckets <- dbGetQuery(db, 'select * from pitch_data where TaggedPitchType in ("Fastball", "Sinker") and RelSpeed between 80 and 103') %>%
  mutate(
    FB_Speed = cut(
      RelSpeed,
      breaks = c(-Inf, lowest_bound, lower_bound, upper_bound, upper_bound_2, Inf),
      labels = c("80-84", "85-87", "88-90", "91-93", "94+"),
      right = FALSE
    )
  )   %>%
  group_by(Batter, FB_Speed) %>%
  summarise(P = n(),
            PA = sum(!is.na(PlayResult)& PlayResult !="") ,
            AB = sum(!is.na(PlayResult) & !PlayResult %in% c("","Walk", "HitByPitch", "Sacrifice", "CatchersInterference")),
            swings = sum(swing, na.rm = T),
            whiffs = sum(whiff, na.rm = T),
            hardhit = sum(hardhit, na.rm = T),
            inplay = sum(PitchCall =='InPlay', na.rm = T),
            BBE = sum(bbe, na.rm = T),
            is_hit = sum(PlayResult %in% c('Single', 'Double', 'Triple', 'HomeRun'), na.rm = T),
            bb_hbp = sum(PlayResult %in% c('Walk', 'HitByPitch'), na.rm = T) ,
            SO = sum(grepl('Strikeout', PlayResult)) ,
            total_bases = sum(total_bases, na.rm = T),
            EV = mean(ExitSpeed[bbe==1], na.rm = T),
  ) %>%
  filter(
    !grepl('Aaa', Batter)
  ) %>%
  ungroup() %>%
  mutate(Sw_pct = swings / P,
         whiff_pct = whiffs / swings,
         BIP_pct = inplay / swings,
         hard_pct = hardhit / BBE,
         BA = round(is_hit / AB, 3),
         OBP = round((bb_hbp+is_hit)/PA,3),
         SLG = round(total_bases/AB, 3),
         OPS = OBP + SLG,
         BABIP = round(is_hit / inplay, 3)) %>%
  mutate(across(16:19, ~ round( . *100,1))) %>%
  mutate(BatterTeam = cur_team$BatterTeam[match(Batter,cur_team$Batter)], .after = Batter)


league_velo_buckets <- dbGetQuery(db, 'select * from pitch_data where TaggedPitchType in ("Fastball", "Sinker") and RelSpeed between 80 and 103') %>%
  mutate(
    FB_Speed = cut(
      RelSpeed,
      breaks = c(-Inf, lowest_bound, lower_bound, upper_bound, upper_bound_2, Inf),
      labels = c("80-84", "85-87", "88-90", "91-93", "94+"),
      right = FALSE
    )
  )   %>%
  group_by(FB_Speed) %>%
  summarise(Batter = 'League',
            BatterTeam = 'League',
            P = n(),
            PA = sum(!is.na(PlayResult)& PlayResult !="") ,
            AB = sum(!is.na(PlayResult) & !PlayResult %in% c("","Walk", "HitByPitch", "Sacrifice", "CatchersInterference")),
            swings = sum(swing, na.rm = T),
            whiffs = sum(whiff, na.rm = T),
            hardhit = sum(hardhit, na.rm = T),
            inplay = sum(PitchCall =='InPlay', na.rm = T),
            BBE = sum(bbe, na.rm = T),
            is_hit = sum(PlayResult %in% c('Single', 'Double', 'Triple', 'HomeRun'), na.rm = T),
            bb_hbp = sum(PlayResult %in% c('Walk', 'HitByPitch'), na.rm = T) ,
            SO = sum(grepl('Strikeout', PlayResult)) ,
            total_bases = sum(total_bases, na.rm = T),
            EV = mean(ExitSpeed[bbe==1], na.rm = T),
  ) %>%
  ungroup() %>%
  mutate(Sw_pct = swings / P,
         whiff_pct = whiffs / swings,
         BIP_pct = inplay / swings,
         hard_pct = hardhit / BBE,
         BA = round(is_hit / AB, 3),
         OBP = round((bb_hbp+is_hit)/PA,3),
         SLG = round(total_bases/AB, 3),
         OPS = OBP + SLG,
         BABIP = round(is_hit / inplay, 3)) %>%
  mutate(across(17:20, ~ round( . *100,1))) %>%
  relocate(c(Batter,BatterTeam), .before = FB_Speed)# %>%
  # mutate(BatterTeam = cur_team$BatterTeam[match(Batter,cur_team$Batter)], .after = Batter)



# BATTER TENDENCIES
hitter_pt_buckets <- dbGetQuery(db, 'select * from pitch_data') %>%
  # mutate(
  #   FB_Speed = cut(
  #     RelSpeed,
  #     breaks = c(-Inf, lowest_bound, lower_bound, upper_bound, upper_bound_2, Inf),
  #     labels = c("80-84", "85-87", "88-91", "92-94", "94+"),
  #     right = FALSE
  #   )
  # )   %>%
  group_by(Batter, PitchClass) %>%
  summarise(P = n(),
            AB = sum(!is.na(PlayResult) & !PlayResult %in% c("","Walk", "HitByPitch", "Sacrifice", "CatchersInterference")),
            swings = sum(swing, na.rm = T),
            whiffs = sum(whiff, na.rm = T),
            hardhit = sum(hardhit, na.rm = T),
            inplay = sum(PitchCall =='InPlay', na.rm = T),
            BBE = sum(bbe, na.rm = T),
            is_hit = sum(PlayResult %in% c('Single', 'Double', 'Triple', 'HomeRun'), na.rm = T),
            EV = mean(ExitSpeed[bbe==1], na.rm = T),
  ) %>%
  filter(
    !grepl('Aaa', Batter)
  ) %>%
  ungroup() %>%
  mutate(Sw_pct = swings / P,
         whiff_pct = whiffs / swings,
         BIP_pct = inplay / swings,
         hard_pct = hardhit / BBE,
         BA = round(is_hit / AB, 3),
         BABIP = round(is_hit / inplay, 3)) %>%
  mutate(across(12:15, ~ round( . *100,1))) %>%
  mutate(BatterTeam = cur_team$BatterTeam[match(Batter,cur_team$Batter)], .after = Batter)


library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, 'LeagueVelo')
writeDataTable(wb, 'LeagueVelo', league_velo_buckets)
addWorksheet(wb, 'PlayerVelo')
writeDataTable(wb, 'PlayerVelo', hitter_velo_buckets)

saveWorkbook(wb, 'C:/Users/tdmed/OneDrive/Hitter vs Pitch Velo.xlsx', overwrite = T)


ggplot(league_velo_buckets, 
       aes(x = FB_speed))


data_long <- league_velo_buckets %>%
  pivot_longer(cols = c(Sw_pct, whiff_pct, BIP_pct, hard_pct),
               names_to = "Metric", values_to = "Value")

# Plot using ggplot
ggplot(data_long, aes(x = FB_Speed, y = Value, color = Metric, group = Metric)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Fastball Speed (mph)", y = "Percentage (%)",
       title = "Fastball Metrics by Speed Range",
       color = "Metric") +
  scale_color_manual(values = c("Sw_pct" = "purple", 
                                "whiff_pct" = "orange", 
                                "BIP_pct" = "darkcyan", 
                                "hard_pct" = "lightblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        panel.grid.major = element_line(size = 0.5, color = "lightgray"),
        plot.title = element_text(hjust = .5))
