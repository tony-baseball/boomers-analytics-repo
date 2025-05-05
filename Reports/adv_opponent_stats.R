suppressMessages({
  library(knitr)
  library(plyr)
  library(dplyr)
  library(baseballr)
  library(stringr)
  library(ggplot2)
  library(plotly)
  library(tinytex)
  library(DT)
  library(lubridate)
  library(webshot)
  library(kableExtra)
  library(pak)
  library(discordr)
  library(emojifont)
  library(tools)
  library(baseballr)
  library(pandoc)
  library(RSQLite)
}) 
# team_filter <- "Florence Y'alls"
# team_filter <- 'Windy City Thunderbolts'
print(paste("Loading CSVs and obtaining", team_filter, 'data.'))
db <- dbConnect(SQLite(), "C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

h<- dbGetQuery(db, "SELECT * FROM stats_hitting_player WHERE SEASON = 2024")
  #read.csv("C:/Users/tdmed/OneDrive/_FLASH/frontier_all_hitters23.csv")
unique(sort(h$POS))
# FILES -----

hitters<-dbGetQuery(db, 
glue::glue("select r.Status,s.* 
from stats_hitting_player s
left join rosters_ r on s.Player = r.NAME AND s.SEASON = r.SEASON
where STATUS = 'Active' and s.SEASON = 2024
AND s.Team ='{team_filter}'")
)%>% 
#read.csv("C:/Users/tdmed/OneDrive/_FLASH/frontier_all_hitters23.csv") %>%
  filter(Team == team_filter) %>%
  dplyr::rename(`2B` = "X2B",
                `3B` = "X3B",
                `GO/FO` = "GO_FO",
                `1B` = "X1B",
                `SB%` = "SB_pct",
                `wRC+` = "wRC_plus",
                `BB%` = "BB_pct",
                `K%` = "SO_pct") %>%
  mutate(Player = str_squish(Player)) %>%
  filter(Player != 'TOTALS',
         !Pos %in% c('P', 'RHP', 'LHP', 'SHP', 'TB'),
         !grepl('breaker|Sudden', Player, ignore.case = T)
  )

pitchers<- dbGetQuery(db, glue::glue(
                      "select r.Status,s.* from stats_pitching_player s
left join rosters_ r on s.Player = r.NAME AND s.SEASON = r.SEASON
where STATUS = 'Active' and s.SEASON = 2024
AND s.Team ='{team_filter}'")) %>%
#read.csv("C:/Users/tdmed/OneDrive/_FLASH/frontier_all_pitchers23.csv") %>%
  # filter(Team == team_filter) %>%
  dplyr::rename(`2B` = "X2B",
                `3B` = "X3B",
                `1B` = "X1B",
                `wRC+` = "wRC_plus",
                `BB%` = "BB_pct",
                `K%` = "SO_pct",
                `BB/9` = "BB9",
                `K/9` = "SO9",
                `FIP-` = "FIP_minus") %>%
  mutate(Player = str_squish(Player)) %>%
  filter(Player != 'TOTALS',
         !grepl('breaker', Player, ignore.case = T),
         !grepl('breaker|Sudden', Player, ignore.case = T)
  )


hitters_boom<-dbGetQuery(db, 
                         "select r.Status,s.* from stats_hitting_player s
left join rosters_ r on s.Player = r.NAME AND s.SEASON = r.SEASON
where STATUS = 'Active' and s.SEASON = 2024") %>%
  # read.csv("C:/Users/tdmed/OneDrive/_FLASH/frontier_all_hitters23.csv") %>%
  filter(Team == 'Schaumburg Boomers') %>%
  dplyr::rename(`2B` = "X2B",
                `3B` = "X3B",
                `GO/FO` = "GO_FO",
                `1B` = "X1B",
                `SB%` = "SB_pct",
                `wRC+` = "wRC_plus",
                `BB%` = "BB_pct",
                `K%` = "SO_pct") %>%
  mutate(Player = str_squish(Player))  %>%
  filter(Player != 'TOTALS',
         !Pos %in% c('P', 'RHP', 'LHP', 'SHP', 'TB'),
         !grepl('breaker|Sudden', Player, ignore.case = T)
         )

pitchers_boom<- dbGetQuery(db, 
                           "select r.Status,s.* from stats_pitching_player s
left join rosters_ r on s.Player = r.NAME AND s.SEASON = r.SEASON
where STATUS = 'Active' and s.SEASON = 2024") %>% #
 # read.csv("C:/Users/tdmed/OneDrive/_FLASH/frontier_all_pitchers23.csv") %>%
  filter(Team == 'Schaumburg Boomers') %>%
  dplyr::rename(`2B` = "X2B",
                `3B` = "X3B",
                `1B` = "X1B",
                `wRC+` = "wRC_plus",
                `BB%` = "BB_pct",
                `K%` = "SO_pct",
                `BB/9` = "BB9",
                `K/9` = "SO9",
                `FIP-` = "FIP_minus") %>%
  mutate(Player = str_squish(Player)) %>%
  filter(Player != 'TOTALS',
         !grepl('breaker|Sudden', Player, ignore.case = T)
         )


text <- paste0('SELECT * 
               FROM pitch_data 
               where (PitcherTeam in ("Schaumburg Boomers", "',team_filter,'")) ',
               'or (BatterTeam in ("Schaumburg Boomers", "',team_filter,'"))'
) 

yakker <- dbGetQuery(db, text)
unique(yakker$PitcherTeam) 
unique(yakker$BatterTeam)   
# yakker <- yak_23  %>%
#   mutate(Batter = gsub('Brendon Dadson', 'Brendan Dadson', Batter),
#          Batter = gsub('Kemuel Thomas-Rivero', 'Kemuel Thomas-Rivera', Batter),
#          Batter = gsub('Todd Isaacs Jr.', 'Todd Isaacs', Batter),
#          Batter = gsub("Ti'quan Forbes", "Ti'Quan Forbes", Batter),
#          
#          Batter = gsub("Jarrod Watking", "Jarrod Watkins", Batter))

#sort(unique(yakker$Batter[yakker$BatterTeam=='New Jersey Jackals']))

team_info<- dbGetQuery(db, "SELECT * FROM teams")

# HITTING BOOM ----
{ print("Creating Boomers Stats")
  
  hit_std_boom <- hitters_boom %>%
    dplyr::select(Player,  G, AB, H, `2B`, `3B`, HR, RBI, R, AVG, OBP, SLG, BB, SO, SB, CS,
                  HBP, SF, SH, HDP) %>%
    arrange(Player)
  
  hit_adv_boom <- hitters_boom %>%
    dplyr::select(Player,  G, AB, H, XBH, R, RBI, AVG, OBP, SLG, OPS, `BB%`, `K%`, ISO, BABIP, wRAA, wOBA, RC, wRC, `wRC+`)%>%
    arrange((Player))
  
  active_hitters_boom <- unique(hit_std_boom$Player)
  
  
  
  hit_splits_boom_LHP <- 
    yakker %>%
    filter(Batter %in% active_hitters_boom) %>%
    filter(PitcherThrows == 'Left') %>%
    group_by(Batter) %>%
    summarise(PA = sum(is_pa == 1 , na.rm = TRUE),
              AB = sum(is_ab == 1, na.rm = TRUE),
              # AB = PA - sum(KorBB == 'Walk') - sum(PitchCall == 'HitByPitch'),
              H = sum(total_bases >=1 , na.rm =T),
              `2B` = sum(total_bases ==2 , na.rm =T),
              `3B` = sum(total_bases==3 , na.rm =T),
              `HR` = sum(total_bases ==4 , na.rm =T),
              K = sum(grepl('Strikeout',PlayResult) , na.rm = TRUE),
              BB = sum(KorBB =='Walk' , na.rm = TRUE),
              HBP = sum(PitchCall == 'HitByPitch'),
              #  XBH = sum(PlayResult %in% c('Double', 'Triple', 'HomeRun') ),
              AVG = round(H/AB,3),
              OBP = round( (H + sum(KorBB =='Walk') )/ PA,3),
              SLG = round(sum(total_bases, na.rm = T) / AB,3),
              `BB%` = round(sum(KorBB =='Walk' , na.rm = TRUE)/PA,3)*100 ,
              `K%` = round(sum(KorBB =='Strikeout' , na.rm = TRUE)/PA,3)*100 ,
              `AvgEV` = round(mean(ExitSpeed[PitchCall =='InPlay' & HitType != 'Bunt'],na.rm=TRUE) ,1),
              `Hard%` = round( sum(hardhit==1 ,na.rm=TRUE ) / sum(PitchCall == 'InPlay' ),3)*100,
              `Whiff%` = round( sum(whiff==1 ,na.rm=TRUE) / sum(swing==1 ,na.rm=TRUE),3)*100,
              `GB%` = round(sum(HitType=='GroundBall' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100,
              `LD%` = round(sum(HitType=='LineDrive' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100,
              `FB%` = round(sum(HitType=='FlyBall' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100
    )  %>%
    arrange(Batter)
  
  hit_splits_boom_RHP <- 
    yakker %>%
    filter(Batter %in% active_hitters_boom) %>%
    filter(PitcherThrows == 'Right') %>%
    group_by(Batter) %>%
    summarise(PA = sum(is_pa == 1 , na.rm = TRUE),
              AB = sum(is_ab == 1, na.rm = TRUE),
              # AB = PA - sum(KorBB == 'Walk') - sum(PitchCall == 'HitByPitch'),
              H = sum(total_bases >=1 , na.rm =T),
              `2B` = sum(total_bases ==2 , na.rm =T),
              `3B` = sum(total_bases==3 , na.rm =T),
              `HR` = sum(total_bases ==4 , na.rm =T),
              K = sum(grepl('Strikeout',PlayResult) , na.rm = TRUE),
              BB = sum(KorBB =='Walk' , na.rm = TRUE),
              HBP = sum(PitchCall == 'HitByPitch'),
              #  XBH = sum(PlayResult %in% c('Double', 'Triple', 'HomeRun') ),
              AVG = round(H/AB,3),
              OBP = round( (H + sum(KorBB =='Walk') )/ PA,3),
              SLG = round(sum(total_bases, na.rm = T) / AB,3),
              `BB%` = round(sum(KorBB =='Walk' , na.rm = TRUE)/PA,3)*100 ,
              `K%` = round(sum(KorBB =='Strikeout' , na.rm = TRUE)/PA,3)*100 ,
              `AvgEV` = round(mean(ExitSpeed[PitchCall =='InPlay' & HitType != 'Bunt'],na.rm=TRUE) ,1),
              `Hard%` = round( sum(hardhit==1 ,na.rm=TRUE ) / sum(PitchCall == 'InPlay' ),3)*100,
              `Whiff%` = round( sum(whiff==1 ,na.rm=TRUE) / sum(swing==1 ,na.rm=TRUE),3)*100,
              `GB%` = round(sum(HitType=='GroundBall' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100,
              `LD%` = round(sum(HitType=='LineDrive' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100,
              `FB%` = round(sum(HitType=='FlyBall' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100
    )  %>%
    arrange(Batter)
  
  hit_bb_boom <- yakker %>%  
    dplyr::filter(Batter %in% active_hitters_boom) %>% 
    mutate(ExitSpeed_rd = round(ExitSpeed),
           Angle_rd = round(Angle)) %>%
    # left_join(ev_la %>% dplyr::select(1:15), by = c ('Angle_rd' = 'Angle', 'ExitSpeed_rd' = 'ExitSpeed'), relationship = "many-to-many") %>%
    mutate( #wOBA_test = round( (.884*`1B` + 1.26*`2B` + 1.60*`3B` + 2.07*HR)/BBE,3),
      #        wOBA_dif = wOBA_test - wOBA ,.after = wOBA,
      launch_angle = Angle,
      launch_speed = ExitSpeed) %>% 
    code_barrel() %>% 
    dplyr::group_by(Batter) %>%
    dplyr::summarise(G = n_distinct(GameID),
                     BIP = n(),
                     BBE = sum(!is.na(Angle)),
                     #    xBABIP = round(sum(AVG/BBE, na.rm = TRUE),3),
                     #    xWOBACON = round(sum(wOBA/BBE, na.rm = TRUE),3),
                     AvgEV = round(mean(ExitSpeed[PitchCall == 'InPlay'& HitType != 'Bunt'], na.rm=TRUE),1),
                     `EV+` = round(AvgEV / mean(yakker$ExitSpeed[yakker$PitchCall == 'InPlay' & yakker$HitType != 'Bunt'], na.rm = TRUE),2) * 100,
                     MaxEV = as.numeric(round(max(ExitSpeed, na.rm=TRUE),1) ),                                                               
                     AirEV = round(mean(ExitSpeed[HitType=="FlyBall"|HitType=='LineDrive'], na.rm=TRUE),1),
                     GBEV = round(mean(ExitSpeed[HitType=="GroundBall"], na.rm=TRUE),1),
                     AvgDist =  round(mean(Distance, na.rm=TRUE),1),
                     MaxDist =  round(max(Distance, na.rm=TRUE),1),
                     AvgHRDist = round(mean(Distance[PlayResult=='HomeRun'], na.rm=TRUE),1),
                     AvgLA = round(mean(Angle,na.rm=TRUE),1),
                     HardHit = sum(ExitSpeed >= 95, na.rm=TRUE),
                     `HardHit%` = round(HardHit/BBE,3)*100,
                     HardHitLA = round(mean(Angle[ExitSpeed >= 95], na.rm=TRUE),1),
                     Barrels = sum(barrel, na.rm = TRUE),
                     `Barrel%` = round(Barrels/BBE,3)*100 ) %>%
    select(1,2,4,5,6,7,14,15)%>%
    arrange(Batter)
}

# PITCHING BOOM ----
{
  pitch_std_boom <- pitchers_boom %>%
    dplyr::select(Player, G, GS, SV, IP, ERA, SO, BB, H, WHIP, R, ER, BAA, WP, HBP, BK) %>%
    arrange(desc(GS), desc(G))%>%
    arrange(Player)
  
  pitch_adv_boom <- pitchers_boom %>%
    dplyr::select(Player, G, GS, IP, ERA, FIP, `FIP-`, R, ER, H, SO, BB, `K/9`, `BB/9`, `K%`, `BB%`, WHIP, BAA, OBP, SLG, OPS)%>%
    arrange(desc(GS), desc(G))%>%
    arrange(Player)%>%
    mutate(across(c(`K/9`, `BB/9`), ~ round(as.numeric(.),1)),
           across(c(`K%`, `BB%`), ~ round(as.numeric(.)*100,1)),
    )
  
  active_pitchers_boom <- unique(pitch_std_boom$Player)
  
  
  pitch_splits_boom_RHP <- yakker %>%
    filter(Pitcher %in% active_pitchers_boom) %>%
    filter(BatterSide == 'Right') %>%
    group_by(Pitcher) %>%
    summarise(PA = sum(is_pa == 1 , na.rm = TRUE),
              AB = sum(is_ab == 1, na.rm = TRUE),
              # AB = PA - sum(KorBB == 'Walk') - sum(PitchCall == 'HitByPitch'),
              H = sum(total_bases >=1 , na.rm =T),
              `2B` = sum(total_bases ==2 , na.rm =T),
              `3B` = sum(total_bases==3 , na.rm =T),
              `HR` = sum(total_bases ==4 , na.rm =T),
              K = sum(grepl('Strikeout',PlayResult) , na.rm = TRUE),
              BB = sum(KorBB =='Walk' , na.rm = TRUE),
              HBP = sum(PitchCall == 'HitByPitch'),
              #  XBH = sum(PlayResult %in% c('Double', 'Triple', 'HomeRun') ),
              AVG = round(H/AB,3),
              OBP = round( (H + sum(KorBB =='Walk') )/ PA,3),
              SLG = round(sum(total_bases, na.rm = T) / AB,3),
              `BB%` = round(sum(KorBB =='Walk' , na.rm = TRUE)/PA,3)*100 ,
              `K%` = round(sum(KorBB =='Strikeout' , na.rm = TRUE)/PA,3)*100 ,
              `AvgEV` = round(mean(ExitSpeed[PitchCall =='InPlay' & HitType != 'Bunt'],na.rm=TRUE) ,1),
              `Hard%` = round( sum(hardhit==1 ,na.rm=TRUE ) / sum(PitchCall == 'InPlay' ),3)*100,
              `Whiff%` = round( sum(whiff==1 ,na.rm=TRUE) / sum(swing==1 ,na.rm=TRUE),3)*100,
              `GB%` = round(sum(HitType=='GroundBall' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100,
              `LD%` = round(sum(HitType=='LineDrive' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100,
              `FB%` = round(sum(HitType=='FlyBall' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100
    )  %>% 
    filter(Pitcher %in% active_pitchers_boom)%>%
    arrange(Pitcher)
  
  pitch_splits_boom_LHP <- yakker %>%
    filter(Pitcher %in% active_pitchers_boom) %>%
    filter(BatterSide == 'Left') %>%
    group_by(Pitcher) %>%
    summarise(PA = sum(is_pa == 1 , na.rm = TRUE),
              AB = sum(is_ab == 1, na.rm = TRUE),
              # AB = PA - sum(KorBB == 'Walk') - sum(PitchCall == 'HitByPitch'),
              H = sum(total_bases >=1 , na.rm =T),
              `2B` = sum(total_bases ==2 , na.rm =T),
              `3B` = sum(total_bases==3 , na.rm =T),
              `HR` = sum(total_bases ==4 , na.rm =T),
              K = sum(grepl('Strikeout',PlayResult) , na.rm = TRUE),
              BB = sum(KorBB =='Walk' , na.rm = TRUE),
              HBP = sum(PitchCall == 'HitByPitch'),
              #  XBH = sum(PlayResult %in% c('Double', 'Triple', 'HomeRun') ),
              AVG = round(H/AB,3),
              OBP = round( (H + sum(KorBB =='Walk') )/ PA,3),
              SLG = round(sum(total_bases, na.rm = T) / AB,3),
              `BB%` = round(sum(KorBB =='Walk' , na.rm = TRUE)/PA,3)*100 ,
              `K%` = round(sum(KorBB =='Strikeout' , na.rm = TRUE)/PA,3)*100 ,
              `AvgEV` = round(mean(ExitSpeed[PitchCall =='InPlay' & HitType != 'Bunt'],na.rm=TRUE) ,1),
              `Hard%` = round( sum(hardhit==1 ,na.rm=TRUE ) / sum(PitchCall == 'InPlay' ),3)*100,
              `Whiff%` = round( sum(whiff==1 ,na.rm=TRUE) / sum(swing==1 ,na.rm=TRUE),3)*100,
              `GB%` = round(sum(HitType=='GroundBall' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100,
              `LD%` = round(sum(HitType=='LineDrive' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100,
              `FB%` = round(sum(HitType=='FlyBall' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100
    )  %>% 
    filter(Pitcher %in% active_pitchers_boom)%>%
    arrange(Pitcher)
}
# OPP HITTING ----
{ 
  print(paste("Creating",team_filter,"Stats"))
  
  hit_std <- hitters %>%
    dplyr::select(Player,  G, AB, H, `2B`, `3B`, HR, RBI, R, AVG, OBP, SLG, BB, SO, SB, CS,
                  HBP, SF, SH, HDP) %>%
    arrange(desc(AB)) %>%
    arrange(Player)
  
  hit_adv <- hitters %>%
    dplyr::select(Player,  G, AB, H, XBH, R, RBI, AVG, OBP, SLG, OPS, `BB%`, `K%`, ISO, BABIP, wRAA, wOBA, RC, wRC, `wRC+`)%>%
    arrange(desc(AB))%>%
    arrange(Player)
  
  active_hitters <- unique(hit_std$Player)
  
  hit_splits_RHP <- yakker %>%
    filter(Batter %in% active_hitters) %>%
    filter(PitcherThrows == 'Right') %>%
    group_by(Batter) %>%
    summarise(PA = sum(is_pa == 1 , na.rm = TRUE),
              AB = sum(is_ab == 1, na.rm = TRUE),
              # AB = PA - sum(KorBB == 'Walk') - sum(PitchCall == 'HitByPitch'),
              H = sum(total_bases >=1 , na.rm =T),
              `2B` = sum(total_bases ==2 , na.rm =T),
              `3B` = sum(total_bases==3 , na.rm =T),
              `HR` = sum(total_bases ==4 , na.rm =T),
              K = sum(grepl('Strikeout',PlayResult) , na.rm = TRUE),
              BB = sum(KorBB =='Walk' , na.rm = TRUE),
              HBP = sum(PitchCall == 'HitByPitch'),
              #  XBH = sum(PlayResult %in% c('Double', 'Triple', 'HomeRun') ),
              AVG = round(H/AB,3),
              OBP = round( (H + sum(KorBB =='Walk') )/ PA,3),
              SLG = round(sum(total_bases, na.rm = T) / AB,3),
              `BB%` = round(sum(KorBB =='Walk' , na.rm = TRUE)/PA,3)*100 ,
              `K%` = round(sum(KorBB =='Strikeout' , na.rm = TRUE)/PA,3)*100 ,
              `AvgEV` = round(mean(ExitSpeed[PitchCall =='InPlay' & HitType != 'Bunt'],na.rm=TRUE) ,1),
              `Hard%` = round( sum(hardhit==1 ,na.rm=TRUE ) / sum(PitchCall == 'InPlay' ),3)*100,
              `Whiff%` = round( sum(whiff==1 ,na.rm=TRUE) / sum(swing==1 ,na.rm=TRUE),3)*100,
              `GB%` = round(sum(HitType=='GroundBall' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100,
              `LD%` = round(sum(HitType=='LineDrive' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100,
              `FB%` = round(sum(HitType=='FlyBall' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100
    )  %>%
    filter(Batter %in% active_hitters)%>%
    arrange(Batter)
  
  
  hit_splits_LHP <- yakker %>%
    filter(Batter %in% active_hitters) %>%
    filter(PitcherThrows == 'Left') %>%
    group_by(Batter) %>%
    summarise(PA = sum(is_pa == 1 , na.rm = TRUE),
              AB = sum(is_ab == 1, na.rm = TRUE),
              # AB = PA - sum(KorBB == 'Walk') - sum(PitchCall == 'HitByPitch'),
              H = sum(total_bases >=1 , na.rm =T),
              `2B` = sum(total_bases ==2 , na.rm =T),
              `3B` = sum(total_bases==3 , na.rm =T),
              `HR` = sum(total_bases ==4 , na.rm =T),
              K = sum(grepl('Strikeout',PlayResult) , na.rm = TRUE),
              BB = sum(KorBB =='Walk' , na.rm = TRUE),
              HBP = sum(PitchCall == 'HitByPitch'),
              #  XBH = sum(PlayResult %in% c('Double', 'Triple', 'HomeRun') ),
              AVG = round(H/AB,3),
              OBP = round( (H + sum(KorBB =='Walk') )/ PA,3),
              SLG = round(sum(total_bases, na.rm = T) / AB,3),
              `BB%` = round(sum(KorBB =='Walk' , na.rm = TRUE)/PA,3)*100 ,
              `K%` = round(sum(KorBB =='Strikeout' , na.rm = TRUE)/PA,3)*100 ,
              `AvgEV` = round(mean(ExitSpeed[PitchCall =='InPlay' & HitType != 'Bunt'],na.rm=TRUE) ,1),
              `Hard%` = round( sum(hardhit==1 ,na.rm=TRUE ) / sum(PitchCall == 'InPlay' ),3)*100,
              `Whiff%` = round( sum(whiff==1 ,na.rm=TRUE) / sum(swing==1 ,na.rm=TRUE),3)*100,
              `GB%` = round(sum(HitType=='GroundBall' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100,
              `LD%` = round(sum(HitType=='LineDrive' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100,
              `FB%` = round(sum(HitType=='FlyBall' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100
    )  %>%
    filter(Batter %in% active_hitters)%>%
    arrange(Batter)
  
  hit_bb <- yakker %>%  
    dplyr::filter(Batter %in% active_hitters) %>% 
    mutate(ExitSpeed_rd = round(ExitSpeed),
           Angle_rd = round(Angle)) %>%
    # left_join(ev_la %>% dplyr::select(1:15), by = c ('Angle_rd' = 'Angle', 'ExitSpeed_rd' = 'ExitSpeed'), relationship = "many-to-many") %>%
    mutate( #wOBA_test = round( (.884*`1B` + 1.26*`2B` + 1.60*`3B` + 2.07*HR)/BBE,3),
      #        wOBA_dif = wOBA_test - wOBA ,.after = wOBA,
      launch_angle = Angle,
      launch_speed = ExitSpeed) %>% 
    code_barrel() %>% 
    dplyr::group_by(Batter) %>%
    dplyr::summarise(G = n_distinct(GameID),
                     BIP = n(),
                     BBE = sum(!is.na(Angle)),
                     #    xBABIP = round(sum(AVG/BBE, na.rm = TRUE),3),
                     #    xWOBACON = round(sum(wOBA/BBE, na.rm = TRUE),3),
                     AvgEV = round(mean(ExitSpeed[PitchCall == 'InPlay'& HitType != 'Bunt'], na.rm=TRUE),1),
                     `EV+` = round(AvgEV / mean(yakker$ExitSpeed[yakker$PitchCall == 'InPlay' & yakker$HitType != 'Bunt'], na.rm = TRUE),2) * 100,
                     MaxEV = as.numeric(round(max(ExitSpeed, na.rm=TRUE),1) ),                                                               
                     AirEV = round(mean(ExitSpeed[HitType=="FlyBall"|HitType=='LineDrive'], na.rm=TRUE),1),
                     GBEV = round(mean(ExitSpeed[HitType=="GroundBall"], na.rm=TRUE),1),
                     AvgDist =  round(mean(Distance, na.rm=TRUE),1),
                     MaxDist =  round(max(Distance, na.rm=TRUE),1),
                     AvgHRDist = round(mean(Distance[PlayResult=='HomeRun'], na.rm=TRUE),1),
                     AvgLA = round(mean(Angle,na.rm=TRUE),1),
                     HardHit = sum(ExitSpeed >= 95, na.rm=TRUE),
                     `HardHit%` = round(HardHit/BBE,3)*100,
                     HardHitLA = round(mean(Angle[ExitSpeed >= 95], na.rm=TRUE),1),
                     Barrels = sum(barrel, na.rm = TRUE),
                     `Barrel%` = round(Barrels/BBE,3)*100 ) %>%
    arrange(desc(G)) %>%
    select(1,2,4,5,6,7,14,15)%>%
    arrange(Batter)
}
# PITCHING ----
{
  pitch_std <- pitchers %>%
    dplyr::select(Player, G, GS, SV, IP, ERA, SO, BB, H, WHIP, R, ER, BAA, WP, HBP, BK) %>%
    arrange(desc(GS), desc(G))%>%
    arrange(Player)
  
  pitch_adv <- pitchers%>%
    dplyr::select(Player, G, GS, IP, ERA, FIP, `FIP-`, R, ER, H, SO, BB, `K/9`, `BB/9`, `K%`, `BB%`, WHIP, BAA, OBP, SLG, OPS)%>%
    arrange(desc(GS), desc(G))%>%
    arrange(Player) %>%
    mutate(across(c(`K/9`, `BB/9`), ~ round(as.numeric(.),1)),
           across(c(`K%`, `BB%`), ~ round(as.numeric(.)*100,1)),
    )
  
  active_pitchers <- unique(pitch_std$Player)
  
  pitch_splits_RHH <- yakker %>%
    filter(Pitcher %in% active_pitchers) %>%
    filter(BatterSide == 'Right') %>%
    group_by(Pitcher) %>%
    summarise(PA = sum(is_pa == 1 , na.rm = TRUE),
              AB = sum(is_ab == 1, na.rm = TRUE),
              # AB = PA - sum(KorBB == 'Walk') - sum(PitchCall == 'HitByPitch'),
              H = sum(total_bases >=1 , na.rm =T),
              `2B` = sum(total_bases ==2 , na.rm =T),
              `3B` = sum(total_bases==3 , na.rm =T),
              `HR` = sum(total_bases ==4 , na.rm =T),
              K = sum(grepl('Strikeout',PlayResult) , na.rm = TRUE),
              BB = sum(KorBB =='Walk' , na.rm = TRUE),
              HBP = sum(PitchCall == 'HitByPitch'),
              #  XBH = sum(PlayResult %in% c('Double', 'Triple', 'HomeRun') ),
              AVG = round(H/AB,3),
              OBP = round( (H + sum(KorBB =='Walk') )/ PA,3),
              SLG = round(sum(total_bases, na.rm = T) / AB,3),
              `BB%` = round(sum(KorBB =='Walk' , na.rm = TRUE)/PA,3)*100 ,
              `K%` = round(sum(KorBB =='Strikeout' , na.rm = TRUE)/PA,3)*100 ,
              `AvgEV` = round(mean(ExitSpeed[PitchCall =='InPlay' & HitType != 'Bunt'],na.rm=TRUE) ,1),
              `Hard%` = round( sum(hardhit==1 ,na.rm=TRUE ) / sum(PitchCall == 'InPlay' ),3)*100,
              `Whiff%` = round( sum(whiff==1 ,na.rm=TRUE) / sum(swing==1 ,na.rm=TRUE),3)*100,
              `GB%` = round(sum(HitType=='GroundBall' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100,
              `LD%` = round(sum(HitType=='LineDrive' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100,
              `FB%` = round(sum(HitType=='FlyBall' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100
    )   %>%
    filter(Pitcher %in% active_pitchers)%>%
    arrange(Pitcher)
  
  pitch_splits_LHH <- yakker %>%
    filter(Pitcher %in% active_pitchers) %>%
    filter(BatterSide == 'Left') %>%
    group_by(Pitcher) %>%
    summarise(PA = sum(is_pa == 1 , na.rm = TRUE),
              AB = sum(is_ab == 1, na.rm = TRUE),
              # AB = PA - sum(KorBB == 'Walk') - sum(PitchCall == 'HitByPitch'),
              H = sum(total_bases >=1 , na.rm =T),
              `2B` = sum(total_bases ==2 , na.rm =T),
              `3B` = sum(total_bases==3 , na.rm =T),
              `HR` = sum(total_bases ==4 , na.rm =T),
              K = sum(grepl('Strikeout',PlayResult) , na.rm = TRUE),
              BB = sum(KorBB =='Walk' , na.rm = TRUE),
              HBP = sum(PitchCall == 'HitByPitch'),
              #  XBH = sum(PlayResult %in% c('Double', 'Triple', 'HomeRun') ),
              AVG = round(H/AB,3),
              OBP = round( (H + sum(KorBB =='Walk') )/ PA,3),
              SLG = round(sum(total_bases, na.rm = T) / AB,3),
              `BB%` = round(sum(KorBB =='Walk' , na.rm = TRUE)/PA,3)*100 ,
              `K%` = round(sum(KorBB =='Strikeout' , na.rm = TRUE)/PA,3)*100 ,
              `AvgEV` = round(mean(ExitSpeed[PitchCall =='InPlay' & HitType != 'Bunt'],na.rm=TRUE) ,1),
              `Hard%` = round( sum(hardhit==1 ,na.rm=TRUE ) / sum(PitchCall == 'InPlay' ),3)*100,
              `Whiff%` = round( sum(whiff==1 ,na.rm=TRUE) / sum(swing==1 ,na.rm=TRUE),3)*100,
              `GB%` = round(sum(HitType=='GroundBall' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100,
              `LD%` = round(sum(HitType=='LineDrive' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100,
              `FB%` = round(sum(HitType=='FlyBall' & PitchCall == 'InPlay', na.rm = T) / sum(PitchCall == 'InPlay', na.rm = T),3) * 100
    )   %>%
    filter(Pitcher %in% active_pitchers) %>%
    arrange(Pitcher)
}
opp_loc <- gsub(" ","",team_info$Location[match(team_filter, team_info$Team)])
# --------------
params <-list(team_filter = team_filter,
              hit_std = hit_std,
              hit_adv = hit_adv,
              hit_bb = hit_bb,
              hit_splits_RHP = hit_splits_RHP,
              hit_splits_LHP = hit_splits_LHP,
              pitch_std = pitch_std,
              pitch_adv = pitch_adv ,
              # pitch_bb = pitch_bb,
              pitch_splits_RHH = pitch_splits_RHH,
              pitch_splits_LHH = pitch_splits_LHH,
              hit_std = hit_std_boom,
              hit_adv = hit_adv_boom,
              hit_bb = hit_bb_boom,
              hit_splits_boom_RHP = hit_splits_boom_RHP,
              hit_splits_boom_LHP = hit_splits_boom_LHP,
              pitch_std = pitch_std_boom,
              pitch_adv = pitch_adv_boom ,
              # pitch_bb = pitch_bb,
              pitch_splits_boom_RHP = pitch_splits_boom_RHP,
              pitch_splits_boom_LHP = pitch_splits_boom_LHP,
              opp_loc = opp_loc
)

team_code <- team_info$bats_team_code[team_info$team_FL == team_filter]
print("Saving as PDF")
suppressWarnings({
  rmarkdown::render(input = "C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/adv_opponent_stats.Rmd",
                    output_file = paste0("C:/Users/tdmed/OneDrive/R_Markdown/AdvanceReports/",team_code," Stats",".pdf"),
                    params = params)
})

folder_path <- paste0("C:/Users/tdmed/OneDrive/_Advance//", team_code)

if (!file.exists(folder_path)) { dir.create(folder_path) }
# copy file 
file.copy(list.files("C:/Users/tdmed/OneDrive/R_Markdown/AdvanceReports/", pattern = team_code, full.names = TRUE),
          paste0("C:/Users/tdmed/OneDrive/_Advance//",team_code,"/"), recursive = TRUE)
# readline("Stats Report Completed and has been copied to One Drive! 
# Proceeding...")
