# Pitcher Performance Days in between
library(RSQLite)
library(tidyr)
library(dplyr)

db <- dbConnect(SQLite(), "C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

bullpen_log <- dbGetQuery(db, 'select * from bullpen_log where Team = "Schaumburg Boomers"') %>%
  group_by(Pitcher) %>%
  mutate(Date = as.Date(Date),
         DaysBetween = case_when( 
           Date == lag(Date) ~ NA,
           T ~ as.numeric(Date - lag(Date))
         ),
         DaysBetween = ifelse(is.na(DaysBetween), 0, DaysBetween),
         ThreeG = case_when (
           DaysBetween == 1 & lag(DaysBetween) == 1 ~ T,
           T ~ F
         )
  ) 



pitcher_data <- dbGetQuery(db, 'select * from pitch_data where PitcherId in (100102
,100833
,101405
,100549
,101482
,101542
,100209
,101279
,100931
,101397
,100463
,101809
,101487
--,101058
,101535
,101545
,101547
)') %>%
  mutate(Date = as.Date(Date))

avg_fb_velo <- pitcher_data %>%
  group_by(Pitcher, PitcherId) %>%
  summarise(Days_of_Rest = 'AvgVelo',
            Type = 'AvgVelo',
            FB_velo = mean(RelSpeed[TaggedPitchType %in% c('Fastball','Sinker')], na.rm = T),
            Stuff = mean(tj_stuff_plus, na.rm = T),
  )


pitch_logs_tm <- pitcher_data%>%
  select(Date, Inning, Pitcher,PitcherId, PitcherThrows, BatterSide, PitchCall, PlayResult, TaggedPitchType, RelSpeed, InducedVertBreak, HorzBreak, SpinRate, SpinAxis, yt_Efficiency,
         ExitSpeed, hardhit, OutsOnPlay, is_pa, RunsScored,tj_stuff_plus) %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date, Pitcher, PitcherId) %>%
  summarise(
    P = n(),
    IP = round(sum(OutsOnPlay, na.rm = T) / 3, 1),
    BF = sum(is_pa, na.rm = T),
    FB_velo = mean(RelSpeed[TaggedPitchType %in% c('Fastball','Sinker')], na.rm = T),
    Stuff = mean(tj_stuff_plus, na.rm = T),
    BB = sum(PlayResult == 'Walk', na.rm = T),
    SO = sum(PlayResult == 'Strikeout', na.rm = T),
    HBP = sum(PitchCall == 'HitByPitch', na.rm = T),
    RA = sum(RunsScored,na.rm = T),
    H = sum(PlayResult %in% c('Single','Double','Triple','HomeRun')),
    XBH = sum(PlayResult %in% c('Double','Triple','HomeRun')),
    TB = sum(PlayResult == 'Single', na.rm = T) + (sum(PlayResult == 'Double', na.rm = T) * 2) + 
      (sum(PlayResult == 'Triple', na.rm = T) * 3) + (sum(PlayResult == 'HomeRun', na.rm = T) * 4),
    Balls = sum(PitchCall == 'BallCalled'),
    Strks = sum(PitchCall %in% c('StrikeCalled', 'StrikeSwinging', 'Foul', 'FoulBall', 'InPlay') ),
    `Strk%` = round(Strks/P,3) ,
    Whiffs = sum(PitchCall == 'StrikeSwinging'),
    Swings = sum(PitchCall %in% c('StrikeSwinging', 'Foul', 'FoulBall', 'InPlay') ),
  ) %>%
  ungroup() %>%
  group_by(Pitcher, PitcherId) %>%
  mutate(DaysBetween = case_when( 
    Date == lag(Date) ~ NA,
    T ~ as.numeric(Date - lag(Date))
  ),
  Days_of_Rest = DaysBetween - 1,
  
  Type = case_when( Days_of_Rest == 0 ~ 'back_to_back',
                    Days_of_Rest == 1 ~ 'one_day',
                    Days_of_Rest == 2 ~ 'two_days',
                    between(Days_of_Rest, 3,4) ~ 'three_to_four_days',
                    between(Days_of_Rest, 5,6) ~ 'five_to_six_days',
                    Days_of_Rest >= 7 ~ 'seven_plus_days',
                    is.na(Days_of_Rest) ~ 'seven_plus_days',
                    T ~ NA
  ),
 
  Days_of_Rest = case_when(
    Days_of_Rest == 0 ~ '0',
    Days_of_Rest == 1 ~ '1',
    Days_of_Rest == 2 ~ '2',
    between(Days_of_Rest, 3,4) ~ '3-4',
    between(Days_of_Rest, 5,6) ~ '5-6',
    Days_of_Rest >= 7 ~ '7+',
    is.na(Days_of_Rest) ~ '7+',
    T ~ as.character(Days_of_Rest)
  ),
  ) %>%
  arrange(Pitcher) %>%
  plyr::rbind.fill(avg_fb_velo) %>%
  mutate(Type = factor(Type, levels = c('AvgVelo','back_to_back','one_day',
                               'two_days','three_to_four_days',
                               'five_to_six_days','seven_plus_days')),
         Days_of_Rest = factor(Days_of_Rest, levels = c('AvgVelo','0','1','2','3-4',
                                                        '5-6','7+'))
         
         ) %>%
  arrange(Pitcher, Type)

means <- pitch_logs_tm %>%
  group_by(Pitcher,PitcherId,Type,Days_of_Rest) %>%
  summarise(across(2:19, ~ mean(.,na.rm = T)))

library(ggplot2)

ggplot(means, aes(x = Days_of_Rest, y = FB_velo, color = Type, group = interaction(Pitcher, Type))) +
  geom_hline(data = avg_fb_velo, aes(yintercept = FB_velo), linetype = "dashed", color = "tomato") +
  # geom_line(linewidth = 1) +
  geom_point(size = 5) +
  facet_wrap( ~ Pitcher, scales = 'free')+
  labs(title = "Pitcher Avg FB Velo Based on Days of Rest")

+
  ylim(80,100)
  
  %>%
  ungroup()%>%
  group_by(Pitcher, Date) %>%
  tidyr::fill(DaysBetween) %>%
  mutate(DaysBetween = ifelse(is.na(DaysBetween), 0, DaysBetween)) %>%
  group_by(Pitcher, Date, DaysBetween, TaggedPitchType) %>%
  summarise(
    P = n(),
    Speed = mean(RelSpeed, na.rm = TRUE),
    SpinRate = mean(SpinRate, na.rm = TRUE),
    IVB = mean(InducedVertBreak, na.rm = TRUE),
    HB = mean(HorzBreak, na.rm = TRUE),
    SpinAxis = mean(SpinAxis, na.rm = T),
  ) %>%
  rename(DaysOfRest = DaysBetween) %>%
  mutate(DaysOfRest = DaysOfRest -1 )

performance_data <- bullpen_log %>%
  select(-P, -ThreeG) %>%
  left_join(pitcher_data %>%
              select(Date, Inning, Pitcher, PitcherThrows, BatterSide, PitchCall, PlayResult, OutsOnPlay, RunsScored, KorBB,
                     ExitSpeed, hardhit, Batter, PAofInning) , by = c('Date', 'Pitcher')) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(DaysBetween = case_when( 
    Date == lag(Date) ~ NA,
    T ~ as.numeric(Date - lag(Date))
  )
  ) %>%
  group_by(Pitcher, Date) %>%
  fill(DaysBetween) %>%
  mutate(DaysBetween = ifelse(is.na(DaysBetween), 0, DaysBetween)) %>%
  group_by(Pitcher, Date, DaysBetween) %>%
  summarise(
    P = n(),
    IP = round(sum(OutsOnPlay, na.rm = T) / 3, 1),
    BF = n_distinct(Batter,PAofInning,Inning),
    BB = sum(KorBB == 'Walk', na.rm = T),
    SO = sum(KorBB == 'Strikeout', na.rm = T),
    HBP = sum(PitchCall == 'HitByPitch', na.rm = T),
    RA = sum(RunsScored,na.rm = T),
    H = sum(PlayResult %in% c('Single','Double','Triple','HomeRun')),
    XBH = sum(PlayResult %in% c('Double','Triple','HomeRun')),
    TB = sum(PlayResult == 'Single', na.rm = T) + (sum(PlayResult == 'Double', na.rm = T) * 2) + 
      (sum(PlayResult == 'Triple', na.rm = T) * 3) + (sum(PlayResult == 'HomeRun', na.rm = T) * 4),
    Balls = sum(PitchCall == 'BallCalled'),
    Strks = sum(PitchCall %in% c('StrikeCalled', 'StrikeSwinging', 'Foul', 'FoulBall', 'InPlay') ),
    `Strk%` = round(Strks/P,3) ,
    Whiffs = sum(PitchCall == 'StrikeSwinging'),
    Swings = sum(PitchCall %in% c('StrikeSwinging', 'Foul', 'FoulBall', 'InPlay') ),
  ) %>%
  ungroup() %>%
  mutate(across(c(3:15), ~ ifelse(P < 2, NA, .)))  %>%
  rename(DaysOfRest = DaysBetween) %>%
  mutate(DaysOfRest = DaysOfRest -1 )

consec <- performance_data %>%
  filter(!is.na(P)) %>%
  group_by(Pitcher, DaysOfRest) %>%
  summarise(across(c(2:16), ~ sum(., na.rm = T)),
            G = n()
  ) %>%
  ungroup() %>%
  mutate(`Strk%` =  round(Strks/P,3),
         `R/G` = RA / G,
         BAA = round(H / (BF-BB-HBP),3),
         OBP = round((H+BB+HBP)/BF ,3),
         SLG = round(TB / (BF-BB-HBP),3)
  ) %>%
  relocate(G, .after = DaysOfRest) %>%
  filter(DaysOfRest >= 0)

# ----------------------------------

