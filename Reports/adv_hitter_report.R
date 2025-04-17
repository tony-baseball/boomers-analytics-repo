suppressMessages(
  suppressWarnings({
    library(knitr)
    library(plyr)
    library(dplyr)
    library(baseballr)
    library(ggrepel)
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
    library(pdftools)
    library(grDevices)
    library(gridExtra)    
    library(GeomMLBStadiums)
    library(tidyr)
    library(ggnewscale)
    
  }))

opposite_hand <- c('Right', 'Left')

file.remove(list.files(paste0("C:/Users/tdmed/OneDrive/R_Markdown/hitter_reports/",opposing_team_code,"/"), pattern = '.pdf', full.names = T))
file.remove(list.files(paste0("C:/Users/tdmed/OneDrive/_Advance/",opposing_team_code,"/H/"), pattern = '.pdf', full.names = T))

find_zone <- function(x_coord, y_coord, rect_data, poly_data) {
  # Ensure inputs are valid
  if (is.na(x_coord) || is.na(y_coord)) {
    return(NA) # Return "Ball" if coordinates are missing
  }
  
  # First check rectangular zones (1–9)
  rect_zone <- rect_data[
    rect_data$x_min <= x_coord & x_coord <= rect_data$x_max &
      rect_data$y_min <= y_coord & y_coord <= rect_data$y_max,
  ]
  
  if (nrow(rect_zone) == 1) {
    return(paste(rect_zone$zone_number))
  }
  
  # Check polygon zones (10–13)
  for (zone in poly_data) {
    # Ensure coordinates are interpreted as a closed polygon
    x_poly <- c(zone$x, zone$x[1])
    y_poly <- c(zone$y, zone$y[1])
    inside <- sp::point.in.polygon(x_coord, y_coord, x_poly, y_poly)
    if (inside >= 1) {  # Treat both "inside" (1) and "on edge" (2) as inside
      return(unique(zone$zone_number))
    }
  }
  
  # If no match, return "Ball"
  return(NA)
}

zone_data <- data.frame(
  zone_number = 1:9,
  x_min = c(-9.96, -3.32, 3.32, -9.96, -3.32, 3.32, -9.96, -3.32, 3.32),
  x_max = c(-3.32, 3.32, 9.96, -3.32, 3.32, 9.96, -3.32, 3.32, 9.96),
  y_min = c(34, 34, 34, 26, 26, 26, 18, 18, 18),
  y_max = c(42, 42, 42, 34, 34, 34, 26, 26, 26)
) %>%
  mutate(across(c('x_min','x_max','y_min','y_max'), ~ . / 12))

zone_10 <- data.frame(
  zone_number = 10,
  x = c( 0,  0, -9.96, -9.96, -13.28, -13.28,0 ),
  y = c(45, 42,    42,    30,      30, 45, 45 )
) %>%
  mutate(across(c('x','y'), ~ . / 12))

zone_11 <- data.frame(
  zone_number = 11,
  x = c( 0,  0, 9.96, 9.96, 13.28, 13.28,0  ),
  y = c(45, 42,    42,    30,      30, 45 , 45)
)%>%
  mutate(across(c('x','y'), ~ . / 12))
zone_12 <- data.frame(
  zone_number = 12,
  x = c( 0,  0, -13.28, -13.28, -9.96, -9.96,0 ),
  y = c(18, 15,    15,    30,      30, 18 ,18)
)%>%
  mutate(across(c('x','y'), ~ . / 12))
zone_13 <- data.frame(
  zone_number = 13,
  x = c( 0,  0, 13.28, 13.28, 9.96, 9.96, 0 ),
  y = c(18, 15,    15,    30,      30, 18, 18 )
)%>%
  mutate(across(c('x','y'), ~ . / 12))
edge_zones <- rbind(
  transform(zone_10, zone_number = 10),
  transform(zone_11, zone_number = 11),
  transform(zone_12, zone_number = 12),
  transform(zone_13, zone_number = 13)
)
polygon_zones <- list(
  zone_10,
  zone_11,
  zone_12,
  zone_13
)
edge_zone_label <- data.frame(
  zone_number = 10:13,
  x = c(-11.62, 11.62, -11.62, 11.62),
  y = c(43.5, 43.5, 16.5, 16.5)
) %>%
  mutate(across(c('x','y'), ~ . / 12))

# hh_tbl_opp$PlateLocSide[1]

# ev_zone_tbl_opp
# ggplotly(



# ggplot()+
#   # geom_point(aes(color = zone_loc))+
#   xlim(-1.5,1.5) + ylim(0,4)+
#   geom_rect(data = ev_zone_tbl, aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = EV),
#             color = "black", alpha = 1, inherit.aes = F)+
#   scale_fill_gradient2(midpoint = 88, low="cornflowerblue", mid="white",high="red") +
#   geom_polygon(data = ev_edge_zone_tbl, aes(x = x, y = y, group = zone_number, fill = EV), color = "black", size = 1  ) +
#   geom_segment(data = tonybaseball::home_plate_p_pov, aes(x = x, y = y, xend = xend, yend = yend), size = 1, color = "black")+
#   # annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0, size = 1) +
#   geom_text( data = ev_zone_tbl,
#              aes(x = (x_min + x_max) / 2, y = (y_min + y_max) / 2, label = EV), size = 5, color = "black" )+
#   geom_text( data = ev_edge_zone_label,
#              aes(x = x, y = y , label = EV), size = 5, color = "black" )+
#   coord_fixed()+
#   theme_void()+
#   labs(title = 'Exit Velo by Zone')+
#   theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
# # )

db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

print("Loading data from database...")

# team_filter <- "Gateway Grizzlies"

home_team_series <- opposing_team_code

team_info <- dbGetQuery(db, 'SELECT * FROM teams')# read.csv("C:/Users/tdmed/OneDrive/_FLASH/frontier team info.csv")

all_hitters <- dbGetQuery(db, glue::glue('SELECT * FROM stats_hitting_player where TEAM = "{team_filter}"')) %>%
  dplyr::rename(`2B` = "X2B",
                `3B` = "X3B",
                `GO/FO` = "GO_FO",
                `1B` = "X1B",
                `SB%` = "SB_pct",
                `wRC+` = "wRC_plus",
                `BB%` = "BB_pct",
                `K%` = "SO_pct",
                `NO.` = 'Num') %>%
  mutate(Player = str_squish(Player),
         `NO.` = as.numeric(`NO.`)) 

rosters<-  dbGetQuery(db, 'SELECT * FROM rosters') # read.csv("C:/Users/tdmed/OneDrive/_FLASH/front_rosters24.csv")

print("Data loaded!")


#---------

# team_filter <- "Florence Y'alls"

team_code <- team_info$bats_team_code[team_info$team_FL == team_filter]

team_location <- gsub(" |-","",team_info$Location[team_info$bats_team_code == team_code])

folder_path <- paste0("C:/Users/tdmed/OneDrive/R_Markdown/hitter_reports/", team_code)

ballparks <- dbGetQuery(db, glue::glue('select * from ballpark_dimensions where HomeTeamCode like "{team_code}"') )

if (nrow(ballparks) < 1) {
  dbGetQuery(db, glue::glue('select * from ballpark_dimensions where HomeTeamCode like "BOOM"') )
}

if (!file.exists(folder_path)) { dir.create(folder_path) }
# ------------------------
yakker <-  dbGetQuery(db, 
                      glue::glue(
                        'SELECT *
               FROM pitch_data 
               where BatterTeam = "{team_filter}" --and SEASON = 2024')   ) %>%
  dplyr::filter(
    TaggedPitchType !='', 
    BatterSide != '',
    TaggedPitchType !='NA',
    !is.na(TaggedPitchType)) %>%
  mutate(swing = 
           ifelse(PitchCall %in% c('Foul','FoulBall','StrikeSwinging', 'CatchersInterference', 'CatchersInt', 'InPlay'),
                  1,0
           )
  )

unique(yakker$TaggedPitchType)
unique(yakker$BatterTeam)
# set a factor to manually order the pitch types
yakker$TaggedPitchType <- factor(yakker$TaggedPitchType, levels = c("Fastball", "Sinker", "Cutter","Curveball", "Slider", "Changeup", "Splitter", 'Knuckleball', 'Other'))


team_roster <- rosters %>%
  filter(TEAM == team_filter,
         SEASON == 2024,
         !grepl("P", POSITION),
         STATUS != 'Inactive',
         !grepl('Tie-breaker|Sudden',NAME))

roster_hitters <- sort(unique(team_roster$NAME)) 
roster_hitters
hitters <- sort(unique(yakker$Batter)) 
hitters


team_h <- team_roster %>% filter(NAME %in% hitters)

unique_hitters <-sort(team_h$NAME)
intersect(unique_hitters, unique(yakker$Batter))
# unique_hitters <- unique_hitters[c(4:6)]

get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}
# LOOP ---------

# unique_hitters <- unique_hitters[13:length(unique_hitters)]
unique_hitters
{  
  # For each pitcher in the dataset, fun through the following code
  for (hitter in unique_hitters) {
    # suppressMessages({
    # Filter the data for the current pitcher
    # hitter_data <- yakker[yakker$Batter == hitter, ]
    # hitter_data <- yakker[yakker$Batter == 'Allante Hall', ]  %>%
    
    hitter_data_ <- dbGetQuery(db,  glue::glue(
      'SELECT * FROM pitch_data where Batter = "{hitter}" --and SEASON = 2024')   ) %>%
      dplyr::filter(TaggedPitchType !='', BatterSide != '',TaggedPitchType !='NA',!is.na(TaggedPitchType)) %>%
      mutate(swing = ifelse(PitchCall %in% c('Foul','FoulBall','StrikeSwinging', 'CatchersInterference', 'CatchersInt', 'InPlay'),
                            1,0) ) %>%
      mutate(hit_direction_5 = 
               case_when(
                 PitchCall == 'InPlay' & Bearing <= -27 ~ 1,
                 PitchCall == 'InPlay' & between(Bearing, -27, -9) ~ 2,
                 PitchCall == 'InPlay' & between(Bearing, -9, 9) ~ 3,
                 PitchCall == 'InPlay' & between(Bearing, 9, 27) ~ 4,
                 PitchCall == 'InPlay' & Bearing >= 27 ~ 5,
                 T ~ NA
               ),
             hit_direction_7 = 
               case_when(
                 PitchCall == 'InPlay' & Bearing <= -32.1 ~ 1,
                 PitchCall == 'InPlay' & between(Bearing, -32.1, -19.2) ~ 2,
                 PitchCall == 'InPlay' &  between(Bearing, -19.2, -6.4) ~ 3,
                 PitchCall == 'InPlay' &  between(Bearing, -6.4, 6.4) ~ 4,
                 PitchCall == 'InPlay' &  between(Bearing, 6.4, 19.2) ~ 5,
                 PitchCall == 'InPlay' &  between(Bearing, 19.2, 32.1) ~ 6,
                 PitchCall == 'InPlay' &  Bearing >= 32.1 ~ 7,
                 T ~ NA
               ),
             hit_direction_9 = 
               case_when(
                 PitchCall == 'InPlay' & Bearing <= -35 ~ 1,
                 PitchCall == 'InPlay' & between(Bearing, -35, -25) ~ 2,
                 PitchCall == 'InPlay' & between(Bearing, -25, -15) ~ 3,
                 PitchCall == 'InPlay' & between(Bearing, -15, -5) ~ 4,
                 PitchCall == 'InPlay' & between(Bearing, -5, 5) ~ 5,
                 PitchCall == 'InPlay' & between(Bearing, 5, 15) ~ 6,
                 PitchCall == 'InPlay' & between(Bearing, 15, 25) ~ 7,
                 PitchCall == 'InPlay' & between(Bearing, 25, 35) ~ 8,
                 PitchCall == 'InPlay' & Bearing >= 35 ~ 9,
                 T ~ NA
               ),
             across(c(hc_x,hc_y), ~ round(.,2)),
             zone_loc =  mapply(
               find_zone,
               x_coord = PlateLocSide,
               y_coord = PlateLocHeight,
               MoreArgs = list(rect_data = zone_data, poly_data = polygon_zones)
             )
      ) 
    
    szns <- if(length(unique(hitter_data_$SEASON)) > 1) {
      glue::glue( "{min(hitter_data_$SEASON)} - {max(hitter_data_$SEASON)}"  )
    } else if(length(unique(hitter_data_$SEASON)) == 1) {
      glue::glue("{unique(hitter_data_$SEASON)}")
    }
    
    ev_zone_tbl <- zone_data %>% 
      left_join(
        hitter_data_ %>% 
          mutate(zone_loc = as.numeric(zone_loc))%>%
          filter(!is.na(zone_loc),PitchCall == 'InPlay') %>%
          group_by(zone_loc)  %>%
          summarise(bb = n(),
                    EV = round(mean(ExitSpeed, na.rm = T),1)
          ),
        by = c('zone_number' = 'zone_loc')) 
    
    ev_edge_zone_tbl <- edge_zones %>% 
      left_join(
        hitter_data_ %>% 
          mutate(zone_loc = as.numeric(zone_loc))%>%
          filter(!is.na(zone_loc),PitchCall == 'InPlay') %>%
          group_by(zone_loc)  %>%
          summarise(bb = n(),
                    EV = round(mean(ExitSpeed, na.rm = T),1)
          ),
        by = c('zone_number' = 'zone_loc')) 
    
    
    ev_edge_zone_label <- edge_zone_label %>% 
      left_join(
        hitter_data_ %>% 
          mutate(zone_loc = as.numeric(zone_loc))%>%
          filter(!is.na(zone_loc),PitchCall == 'InPlay') %>%
          group_by(zone_loc)  %>%
          summarise(bb = n(),
                    EV = round(mean(ExitSpeed, na.rm = T))
          ),
        by = c('zone_number' = 'zone_loc')) 
    
    
    oppo_hand <- unique(opposite_hand[!opposite_hand %in% unique(hitter_data_$BatterSide)])
    
    season_stats <- all_hitters %>%
      filter(Player == hitter, SEASON == 2024)  %>%
      dplyr::select(SEASON, G, PA, AB, H, AVG, `2B`, `3B`, HR, RBI, R,  OBP, SLG, BB, SO, SB, CS,
                    HBP, HDP)
    
    
    batter_sides <- team_h$BATS[team_h$NAME==hitter]
    batter_sides <- unique(case_when(batter_sides %in% c('B','S','Both','Switch') ~ c('Right', 'Left'),
                                     batter_sides %in% c('R','Right', 'RHH') ~ 'Right',
                                     batter_sides %in% c('L', 'Left', 'LHH') ~ 'Left',
                                     T ~ ''
    ))
    
    
    for(i in batter_sides){
      hitter_data <-  hitter_data_ %>%
        filter(BatterSide == i)
      
      
      zones_stats_tm <- hitter_data %>% 
        mutate(zone_loc = as.numeric(zone_loc))%>%
        filter(!is.na(zone_loc),) %>%
        group_by(zone_loc)  %>%
        summarise(pa = sum(is_pa, na.rm = T),
                  ab = sum(is_ab, na.rm = T),
                  rb = sum(reach_base, na.rm = T),
                  h = sum(is_hit, na.rm = T),
                  tb = sum(total_bases, na.rm = T),
                  bip = sum(PitchCall=='InPlay', na.rm = T),
                  bbe = sum(bbe, na.rm = T),
                  w_v = sum(woba_value, na.rm = T),
                  wob_con = sum(woba_value[PitchCall=='InPlay'], namr. = T),
                  EV = round(mean(ExitSpeed, na.rm = T),1)
        ) %>%
        mutate(BA = round(h/ab,3),
               OBP = round(rb/pa,3),
               SLG = round(tb/ab,3),
               wOBA = round(w_v/pa,3),
               wOBACON = round(wob_con/bip,3),
        )
      
      
      zone_stats_tbl <- zone_data %>% 
        left_join(
          zones_stats_tm,
          by = c('zone_number' = 'zone_loc')) 
      
      edge_zone_stats_tbl <- edge_zones %>% 
        left_join(
          zones_stats_tm,
          by = c('zone_number' = 'zone_loc')) 
      
      
      edge_zone_stats_label <- edge_zone_label %>% 
        left_join(
          zones_stats_tm,
          by = c('zone_number' = 'zone_loc')) 
      
      
      { # EV BY ZONE PLOT ----
        # ev_zone_tbl_opp <-
        #   hitter_data %>% filter(PitchCall == 'InPlay', HitType != "Bunt") %>%
        #   dplyr::group_by(zone_x,zone_y) %>%
        #   dplyr::summarise(BIP = sum(PitchCall == 'InPlay'),
        #                    EV = round(mean(ExitSpeed,na.rm = TRUE),1),
        #                    HardHit = sum(ExitSpeed > 95, na.rm = TRUE),
        #                    GB = sum(HitType == 'GroundBall', na.rm = TRUE),
        #                    FB = sum(HitType == 'FlyBall', na.rm = TRUE),
        #                    LD = sum(HitType == 'LineDrive', na.rm = TRUE)) %>%
        #   mutate(`HardHit%` = round(HardHit/(BIP)*100,1),
        #          `GB%` = paste(round(GB/BIP*100,1),'%' ),
        #          `LD%` = paste(round(LD/(BIP)*100,1),'%' ),
        #          `FB%` = paste(round(FB/(BIP)*100,1),'%' ) ) %>%
        #   filter(zone_x != 2,
        #          zone_y != 2)
        # 
        # ev_point_tbl_opp <- hitter_data %>% 
        #   filter(PitchCall == 'InPlay', 
        #          HitType != "Bunt", 
        #          !is.na(ExitSpeed))
        
        EV_zone_opp <- 
          # ggplot(ev_zone_tbl_opp, aes(zone_x,zone_y))+  
          # #  geom_rect(aes(xmin = -0.7, xmax = 0.708, ymin = 1.5, ymax = 3.5), alpha = .1, size = 1.2, color = "black")+
          # geom_tile(aes(fill = EV )) +
          # geom_point(aes_string(color = 'EV')) +
          # scale_fill_gradient2(midpoint = 85.9, low="lightblue", mid="white",high="red")+
          # scale_color_gradient2(midpoint = 85.9, low="lightblue", mid="white",high="red")+
          # geom_segment(data = tonybaseball::home_plate_p_pov, aes(x = x, y = y, xend = xend, yend = yend), size = 1, color = "black")+
          # xlim(-1,1) + ylim(0,3.7)+
          # coord_fixed()+
          # theme_void()+
          # labs(title = 'Exit Velocity by Zone', x='',y='')+
        # theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
        # theme(plot.title = element_text(hjust = 0.5, size = 10),
        #       legend.position = "none", 
        #       panel.border = element_blank(),
        #       panel.grid.major = element_blank(),
        #       panel.grid.minor = element_blank(),
        #       axis.text.x=element_blank(),
        #       axis.ticks.x=element_blank(),
        #       axis.text.y=element_blank(),
        #       axis.ticks.y=element_blank() )+
        # geom_text(aes(label = EV), fontface = 'bold', size = 3)
        ggplot()+
          xlim(-1.5,1.5) + ylim(0,4)+
          geom_rect(data = zone_stats_tbl, aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = EV),
                    color = "black", alpha = 1, inherit.aes = F)+
          scale_fill_gradient2(midpoint = 86, low="cornflowerblue", mid="white",high="red") +
          geom_polygon(data = edge_zone_stats_tbl, aes(x = x, y = y, group = zone_number, fill = EV), color = "black", size = 1  ) +
          geom_segment(data = tonybaseball::home_plate_p_pov, aes(x = x, y = y, xend = xend, yend = yend), size = 1, color = "black")+
          geom_text( data = zone_stats_tbl,
                     aes(x = (x_min + x_max) / 2, y = (y_min + y_max) / 2, label = EV), size = 3, color = "black" )+
          geom_text( data = edge_zone_stats_label,
                     aes(x = ifelse(x>0,x -.1,x+.1), y = y , label = EV), size = 2.75, color = "black" )+
          geom_text(aes(x = 0, y = 3.9 , label = paste('Lg EV = 86 | ',nrow(hitter_data %>% filter(bbe==1)), 'BIP')),  size = 3, color = "black", fontface = 'bold' )+
          coord_fixed()+
          theme_void()+
          labs(title = 'EV by Zone')+
          theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
                legend.position = 'none')
        
        EV_zone_opp
        }
      { # AVG BY ZONE ----
        avg_by_zone <- ggplot()+
          # geom_point(aes(color = zone_loc))+
          xlim(-1.5,1.5) + ylim(0,4)+
          geom_rect(data = zone_stats_tbl, aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = BA),
                    color = "black", alpha = 1, inherit.aes = F)+
          scale_fill_gradient2(midpoint = .259, low="cornflowerblue", mid="white",high="red") +
          geom_polygon(data = edge_zone_stats_tbl, aes(x = x, y = y, group = zone_number, fill = BA), color = "black", size = 1  ) +
          geom_segment(data = tonybaseball::home_plate_p_pov, aes(x = x, y = y, xend = xend, yend = yend), size = 1, color = "black")+
          # annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0, size = 1) +
          geom_text( data = zone_stats_tbl,
                     aes(x = (x_min + x_max) / 2, y = (y_min + y_max) / 2, label = sub("^0", "", sprintf("%.3f", BA))), size = 3, color = "black" )+
          geom_text( data = edge_zone_stats_label,
                     aes(x = ifelse(x>0,x -.1,x+.1), y = y , label = sub("^0", "", sprintf("%.3f", BA))), size = 2.75, color = "black" )+
          geom_text(aes(x = 0, y = 3.9 , label = paste('Lg BA = .259')),  size = 3, color = "black", fontface = 'bold' )+
          coord_fixed()+
          theme_void()+
          labs(title = 'AVG by Zone')+
          theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
                legend.position = 'none')
        avg_by_zone
      }
      
      { # SLG by Zone ----
        slg_by_zone <- ggplot()+
          # geom_point(aes(color = zone_loc))+
          xlim(-1.5,1.5) + ylim(0,4)+
          geom_rect(data = zone_stats_tbl, aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = SLG),
                    color = "black", alpha = 1, inherit.aes = F)+
          scale_fill_gradient2(midpoint = 0.388, low="cornflowerblue", mid="white",high="red") +
          geom_polygon(data = edge_zone_stats_tbl, aes(x = x, y = y, group = zone_number, fill = SLG), color = "black", size = 1  ) +
          geom_segment(data = tonybaseball::home_plate_p_pov, aes(x = x, y = y, xend = xend, yend = yend), size = 1, color = "black")+
          # annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0, size = 1) +
          geom_text( data = zone_stats_tbl,
                     aes(x = (x_min + x_max) / 2, y = (y_min + y_max) / 2, label = sub("^0", "", sprintf("%.3f", SLG))), size = 3, color = "black" )+
          geom_text( data = edge_zone_stats_label,  aes(x = ifelse(x>0,x -.1,x+.1), y = y , label = sub("^0", "", sprintf("%.3f", SLG))), size = 2.75, color = "black" )+
          geom_text(aes(x = 0, y = 3.9 , label = paste('Lg SLG = .388')),  size = 3, color = "black", fontface = 'bold' )+
          coord_fixed()+
          theme_void()+
          labs(title = 'SLG by Zone')+
          theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
                legend.position = 'none')
        
        slg_by_zone
      }
      
      { # HARD HIT HEATMAP ----
        
        if(nrow(hitter_data %>%
                filter(hardhit == 1 )) > 1){
          
          hh_tbl_opp <- hitter_data %>%
            filter(hardhit == 1 ) %>%
            tidyr::drop_na(PlateLocHeight)  %>%
            mutate(density = get_density(PlateLocSide,PlateLocHeight, h = c(1, 1), n= 500 ))
          
          hh_tbl_opp$zone_loc <- mapply(
            find_zone,
            x_coord = hh_tbl_opp$PlateLocSide,
            y_coord = hh_tbl_opp$PlateLocHeight,
            MoreArgs = list(rect_data = zone_data, poly_data = polygon_zones)
          )
          
          home_plate_p_pov <- data.frame(
            x = c(-0.708, -0.708, -0.708, 0, 0.708),
            y = c(0.15, 0.3, 0.3, 0.5, 0.3),
            xend = c(0.708, -0.708, 0, 0.708, 0.708),
            yend = c(0.15, 0.15, 0.5, 0.3, 0.15)
          )
          
          home_plate_c_pov <- data.frame(
            x = c(-0.708, -0.708, -0.708, 0, 0.708),
            y = c(0.5, 0.3, 0.3, 0.15, 0.5),
            xend = c(0.708, -0.708, 0, 0.708, 0.708),
            yend = c(0.5, 0.5, 0.15, 0.3, 0.3)
          )
          
          ggplot(hh_tbl_opp, aes(x = PlateLocSide, y = PlateLocHeight))+
            # stat_density_2d(geom = 'polygon', aes(fill = after_stat(nlevel))) +
            geom_segment(data = tonybaseball::home_plate_p_pov, aes(x = x, y = y, xend = xend, yend = yend), size = 1, color = "black")+
            stat_density2d(aes(fill = as.numeric(..level..)), geom = "polygon", bins = 6, adjust = 1, contour = T, show.legend = F) +
            scale_fill_gradient2(low = 'cornflowerblue', mid = "white", high = "red") +
            annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0, size = 1) +
            xlim(-1.5,1.5) + ylim(0,4)+
            coord_fixed()+
            labs(title = "Hard Hit Heatmap") +
            theme_void() + 
            # geom_segment(data = tonybaseball::home_plate_c_pov, aes(x = x, y = y, xend = xend, yend = yend),  size = 1, color = "black")+
            theme(plot.title = element_text(hjust = 0.5,size = 10),
                  legend.position = "none")
          
          hh_opp <-  ggplot(hh_tbl_opp, aes(x = PlateLocSide, y = PlateLocHeight)) +
            geom_segment(data = tonybaseball::home_plate_p_pov, aes(x = x, y = y, xend = xend, yend = yend), size = 1, color = "black")+
            stat_density2d(aes(fill = as.numeric(..level..)), geom = "polygon", bins = 8, adjust = 1, contour = T, show.legend = F) +
            scale_fill_gradient2(low = 'cornflowerblue', mid = "white", high = "red") +
            annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0, size = 1) +
            xlim(-1.5,1.5) + ylim(0,4)+
            theme_void()+    
            coord_fixed()+
            theme(axis.title = element_text(size = 15, color = "black", face = "bold")) + 
            theme(plot.title = element_text(size = 12, face = "bold", vjust = 1)) + 
            theme(axis.text = element_text(size = 13, face = "bold", color = "black")) + 
            # theme(legend.title = element_text(size = 5)) + 
            theme(legend.text = element_text(size = 12)) +
            labs(title = "Hard Hit Heatmap") +
            ylab("")+xlab("") +
            theme(plot.title = element_text(hjust = 0.5,size = 10),
                  legend.position = "none", 
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank() )
          hh_opp
          
        }else{
          
          hh_opp <-  
            ggplot() +
            
            xlim(-1,1) + ylim(0,3.7)+        
            theme_void()+    
            coord_fixed()+
            
            theme(axis.title = element_text(size = 15, color = "black", face = "bold")) + 
            theme(plot.title = element_text(size = 12, face = "bold", vjust = 1)) + 
            theme(axis.text = element_text(size = 13, face = "bold", color = "black")) + 
            
            theme(legend.text = element_text(size = 12)) +
            labs(title = "Hard Hit Heatmap") +
            geom_segment(data = tonybaseball::home_plate_p_pov, aes(x = x, y = y, xend = xend, yend = yend), size = 1, color = "black")+
            annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0, size = 1) +
            ylab("")+xlab("") +
            annotate('text', x = 0, y = 2.5, label = 'NO\nDATA\nAVAILABLE', size = 5)+
            theme(plot.title = element_text(hjust = 0.5,size = 10),
                  legend.position = "none", 
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank() )
          
        }
        
        
        
      }
      
      { # SWING HEATMAP ----
        swing_tbl_opp <-  hitter_data %>%
          filter(swing == 1)%>%
          tidyr::drop_na(PlateLocHeight)  %>%
          mutate(density = get_density(PlateLocSide,PlateLocHeight, h = c(1, 1), n= 500 ))
        
        
        
        sw_opp <-  ggplot(swing_tbl_opp, aes(x = PlateLocSide, y = PlateLocHeight)) +
          geom_segment(data = tonybaseball::home_plate_p_pov, aes(x = x, y = y, xend = xend, yend = yend), size = 1, color = "black")+
          stat_density2d(aes(fill = as.numeric(..level..)), geom = "polygon", bins = 8, adjust = 1, contour = T, show.legend = F) +
          scale_fill_gradient2(low = 'cornflowerblue', mid = "white", high = "red") +
          annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0, size = 1) +
          theme_void()+    
          coord_fixed()+
          
          xlim(-1,1) + ylim(0,3.7)+        
          # scale_y_continuous(limits = c(1,4)) +
          #  annotate("rect", xmin = -0.95, xmax = 0.95, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0) +
          theme(axis.title = element_text(size = 15, color = "black", face = "bold")) + 
          theme(plot.title = element_text(size = 12, face = "bold", vjust = 1)) + 
          theme(axis.text = element_text(size = 13, face = "bold", color = "black")) + 
          # theme(legend.title = element_text(size = 5)) + 
          theme(legend.text = element_text(size = 12)) +
          labs(title = "Swing Heatmap") +
          ylab("")+xlab("") +
          theme(plot.title = element_text(hjust = 0.5,size = 10),
                legend.position = "none", 
                panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank() )
      }
      
      { # WEAK HIT HEATMAP ----
        if(nrow(hitter_data %>%
                filter(weakhit == 1))> 1){
          wh_tbl_opp <- hitter_data %>%
            filter(weakhit == 1) %>%
            tidyr::drop_na(PlateLocHeight)  %>%
            mutate(density = get_density(PlateLocSide,PlateLocHeight, h = c(1, 1), n= 500 ))
          
          
          wh_opp <- 
            ggplot(wh_tbl_opp, aes(x = PlateLocSide, y = PlateLocHeight)) +
            geom_segment(data = tonybaseball::home_plate_p_pov, aes(x = x, y = y, xend = xend, yend = yend), size = 1, color = "black")+
            stat_density2d(aes(fill = as.numeric(..level..)), geom = "polygon", bins = 8, adjust = 1, contour = T, show.legend = F) +
            scale_fill_gradient2(low = 'cornflowerblue', mid = "white", high = "red") +
            annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0, size = 1) +
            theme_void()+    
            coord_fixed()+
            
            xlim(-1,1) + ylim(0,3.7)+        
            # scale_y_continuous(limits = c(1,4)) +
            #  annotate("rect", xmin = -0.95, xmax = 0.95, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0) +
            theme(axis.title = element_text(size = 15, color = "black", face = "bold")) + 
            theme(plot.title = element_text(size = 12, face = "bold", vjust = 1)) + 
            theme(axis.text = element_text(size = 13, face = "bold", color = "black")) + 
            # theme(legend.title = element_text(size = 5)) + 
            theme(legend.text = element_text(size = 12)) +
            labs(title = "Weak Hit Heatmap") +
            ylab("")+xlab("") +
            theme(plot.title = element_text(hjust = 0.5,size = 10),
                  legend.position = "none", 
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank() )
          
        }else{
          
          wh_opp <-  ggplot() +
            
            xlim(-1,1) + ylim(0,3.7)+        
            
            theme(axis.title = element_text(size = 15, color = "black", face = "bold")) + 
            theme(plot.title = element_text(size = 12, face = "bold", vjust = 1)) + 
            theme(axis.text = element_text(size = 13, face = "bold", color = "black")) + 
            theme_void()+    
            coord_fixed()+
            
            theme(legend.text = element_text(size = 12)) +
            labs(title = "Hard Hit Heatmap") +
            geom_segment(data = tonybaseball::home_plate_p_pov, aes(x = x, y = y, xend = xend, yend = yend), size = 1, color = "black")+
            annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0, size = 1) +
            ylab("")+xlab("") +
            annotate('text', x = 0, y = 2.5, label = 'NO\nDATA\nAVAILABLE', size = 5)+
            theme(plot.title = element_text(hjust = 0.5,size = 10),
                  legend.position = "none", 
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank() )
          
        }
        
        
        
        
      }
      
      { # WHIFF HEATMAP ----
        
        if(nrow(hitter_data %>%
                filter(whiff == 1)) > 1){
          whiff_tbl_opp <-  hitter_data %>%
            filter(whiff == 1)%>%
            tidyr::drop_na(PlateLocHeight)  %>%
            mutate(density = get_density(PlateLocSide,PlateLocHeight, h = c(1, 1), n= 500 ))
          
          
          whiff_opp<- ggplot(whiff_tbl_opp, aes(x = PlateLocSide, y = PlateLocHeight)) +
            geom_segment(data = tonybaseball::home_plate_p_pov, aes(x = x, y = y, xend = xend, yend = yend), size = 1, color = "black")+
            stat_density2d(aes(fill = as.numeric(..level..)), geom = "polygon", bins = 8, adjust = 1, contour = T, show.legend = F) +
            scale_fill_gradient2(low = 'cornflowerblue', mid = "white", high = "red") +
            annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0, size = 1) +
            xlim(-1,1) + ylim(0,3.7)+        
            theme_void()+    
            coord_fixed()+
            
            theme(axis.title = element_text(size = 15, color = "black", face = "bold")) + 
            theme(plot.title = element_text(size = 12, face = "bold", vjust = 1)) + 
            theme(axis.text = element_text(size = 13, face = "bold", color = "black")) + 
            # theme(legend.title = element_text(size = 5)) + 
            theme(legend.text = element_text(size = 12)) +
            labs(title = "Whiff Heatmap") +
            ylab("")+xlab("") +
            theme(plot.title = element_text(hjust = 0.5,size = 10),
                  legend.position = "none", 
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank() )
          
        }else{
          
          whiff_opp <- 
            ggplot() +
            
            xlim(-1,1) + ylim(0,3.7)+        
            theme_void()+    
            coord_fixed()+
            
            theme(axis.title = element_text(size = 15, color = "black", face = "bold")) + 
            theme(plot.title = element_text(size = 12, face = "bold", vjust = 1)) + 
            theme(axis.text = element_text(size = 13, face = "bold", color = "black")) + 
            
            theme(legend.text = element_text(size = 12)) +
            labs(title = "Hard Hit Heatmap") +
            geom_segment(data = tonybaseball::home_plate_p_pov, aes(x = x, y = y, xend = xend, yend = yend), size = 1, color = "black")+
            annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0, size = 1) +
            ylab("")+xlab("") +
            annotate('text', x = 0, y = 2.5, label = 'NO\nDATA\nAVAILABLE', size = 4)+
            theme(plot.title = element_text(hjust = 0.5,size = 10),
                  legend.position = "none", 
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank() )
          whiff_opp
        }
        
        
        
      }
      
      # Function to calculate coordinates along an arc
      calculate_arc <- function(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points) {
        # Calculate the circle's radius and center
        center_x <- mid_x
        center_y <- mid_y - sqrt((start_x - mid_x)^2 + (start_y - mid_y)^2)
        radius <- sqrt((start_x - center_x)^2 + (start_y - center_y)^2)
        
        # Angles for the start and end points
        start_angle <- atan2(start_y - center_y, start_x - center_x)
        end_angle <- atan2(end_y - center_y, end_x - center_x)
        
        # Generate points along the arc
        angles <- seq(start_angle, end_angle, length.out = num_points)
        data.frame(
          x = center_x + radius * cos(angles),
          y = center_y + radius * sin(angles)
        )
      }
      
      { # IF SPRAY PLOT ----
        spray_point_opp_tbl <- hitter_data %>%
          filter(PitchCall == 'InPlay', HitType != 'Bunt', !is.na(HitType) ) %>%
          dplyr::select(Date, HomeTeam,AwayTeam, `Top.Bottom`, PitchNo, PitcherThrows, Batter, BatterSide,ExitSpeed,TaggedPitchType, PitchCall ,PlayResult, HitType, Angle, HitSpinRate, Direction, PlateLocSide,  Distance, Bearing, pfxx, pfxz, hit_direction_5,hit_direction_7,hit_direction_9  )%>%
          mutate(hc_x = sin(Bearing * pi/180)*(Distance-10) ,
                 hc_y = cos(Bearing * pi/180)*(Distance-10)) %>%
          mutate(PlayResult = recode(PlayResult, Double = '2B', HomeRun = 'HR', Single = '1B', Triple = '3B', Sacrifice = 'SAC', Error = 'E', FieldersChoice = 'FC') ) %>%
          mutate(HitType = recode(HitType, FlyBall = 'FB', LineDrive = 'LD', GroundBall = 'GB', PopUp = 'PU'))
        
        # Parameters
        {
          start_x <- -95
          start_y <- 80
          mid_x <- 0.45
          mid_y <- 250
          end_x <- 95
          end_y <- 80
          num_points <- 100 # Number of points along the arc
          }
        
        # Calculate arc points
        arc_points_if <- calculate_arc(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points)
        
        {
          start_x <- -253
          start_y <- 245
          mid_x <- 0.45
          mid_y <- 450
          end_x <- 257
          end_y <- 246
          num_points <- 100 # Number of points along the arc
        }
        
        arc_points_of <- calculate_arc(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points)
        
        # Fit a spline function to the arc points
        arc_spline <- splinefun(arc_points_if$x, arc_points_if$y)
        
        # Add arc_y to the spray_point_opp_tbl
        spray_point_opp_tbl <- spray_point_opp_tbl %>%
          mutate(arc_y = arc_spline(hc_x)) %>% 
          mutate(below_arc = hc_y < arc_y)
        
        if_spray <- spray_point_opp_tbl %>%
          filter(!is.na(hc_x))%>%
          filter(below_arc)%>%
          filter(!is.na(Bearing))
        
        
        if_spray_pct <- if_spray %>%
          arrange(Bearing) %>%
          group_by(hit_direction_9)%>%
          summarise(bip = n()) %>%
          filter(!is.na(hit_direction_9))%>%
          mutate(pct = bip / sum(bip, na.rm = T),
                 label = round(pct * 100,1),
                 
          ) 
        
        polygons <- bind_rows(
          lapply(1:(nrow(arc_points_if) - 1), function(i) {
            data.frame(
              polygon_id = i, # Unique ID for each polygon
              x = c(0, arc_points_if$x[i], arc_points_if$x[i + 1]),
              y = c(-13.8, arc_points_if$y[i], arc_points_if$y[i + 1])
            )
          })
        ) %>%
          left_join(if_spray_pct, by = c('polygon_id' = 'hit_direction_9')) %>%
          mutate(across(c(bip, pct, label), ~ifelse(is.na(.),0,.)))
        
        poly_labels <- polygons %>%
          filter(x != 0) %>%
          group_by(polygon_id) %>%
          summarise(across(c(x,y,pct,label), ~ mean(.,na.rm = T)),
                    
                    bip = sum(bip, na.rm = T)) %>%
          mutate(across(c(pct, label), ~ ifelse(is.na(.),0,.)))
        
        hit_colors <- c(
          "GB" = "green",
          "LD" = "red",
          "FB" = "gold",
          "PU" = "purple"
        )
        
        if_bip <- sum(spray_point_opp_tbl$below_arc==T, na.rm = T)
        of_bip <- sum(spray_point_opp_tbl$below_arc==F, na.rm = T)
        
        # INFIELD ONLY 
        if(if_bip>=80) {
          
          {
            start_x <- -95
            start_y <- 80
            mid_x <- 0.45
            mid_y <- 250
            end_x <- 95
            end_y <- 80
            num_points <- 10 # Number of points along the arc
          }
          
          # Calculate arc points
          arc_points_if <- calculate_arc(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points)
          
          {
            start_x <- -253
            start_y <- 245
            mid_x <- 0.45
            mid_y <- 450
            end_x <- 257
            end_y <- 246
            num_points <- 10 # Number of points along the arc
          }
          
          arc_points_of <- calculate_arc(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points)
          
          if_spray <- spray_point_opp_tbl %>%
            filter(!is.na(hc_x))%>%
            filter(below_arc)%>%
            filter(!is.na(Bearing))
          
          
          if_spray_pct <- if_spray %>%
            arrange(Bearing) %>%
            group_by(BatterSide, hit_direction_9)%>%
            summarise(bip = n()) %>%
            filter(!is.na(hit_direction_9))%>%
            group_by(BatterSide)%>%
            mutate(pct = bip / sum(bip, na.rm = T),
                   label = round(pct * 100,1),
                   
            ) 
          
          polygons <- bind_rows(
            lapply(1:(nrow(arc_points_if) - 1), function(i) {
              data.frame(
                polygon_id = i, # Unique ID for each polygon
                x = c(0, arc_points_if$x[i], arc_points_if$x[i + 1]),
                y = c(-13.8, arc_points_if$y[i], arc_points_if$y[i + 1])
              )
            })
          ) %>%
            left_join(if_spray_pct, by = c('polygon_id' = 'hit_direction_9')) %>%
            mutate(across(c(bip, pct, label), ~ifelse(is.na(.),0,.)))
          
          poly_labels <- polygons %>%
            filter(x != 0) %>%
            group_by(polygon_id) %>%
            summarise(across(c(x,y,pct,label), ~ mean(.,na.rm = T)),
                      
                      bip = sum(bip, na.rm = T)) %>%
            mutate(across(c(pct, label), ~ ifelse(is.na(.),0,.)))
          
          if_spray_plot <- ggplot(data = if_spray, aes(x=hc_x, y=hc_y))+ 
            geom_mlb_stadium(stadium_ids = 'cubs',
                             stadium_transform_coords = TRUE,
                             stadium_segments = c('home_plate', 'foul_lines', 'infield_outer', 'infield_inner') ,
                             size = 1,
                             color = 'black')+
            geom_point(aes(x=-63,y=63-5), pch = 23, color = 'black', fill = 'white', show.legend = F, size = 4)+
            geom_point(aes(x=63,y=63-5), pch = 23, color = 'black', fill = 'white', show.legend = F, size = 4)+
            geom_point(aes(x=0,y=125-5), pch = 23, color = 'black', fill = 'white', show.legend = F, size = 4)+
            geom_polygon(data = polygons, aes(x=x,y=y, group = polygon_id, fill = pct), color = "black", show.legend = FALSE, alpha = .5) +
            # geom_segment(data = ballparks, aes(x = Start_x , y = Start_y, xend = End_x, yend = End_y), color= 'black', size =1) +
            geom_path (data=arc_points_if, aes(x=x,y=y), size = 2)+
            scale_fill_gradient2(low = 'cornflowerblue', high = '#c22727', midpoint = .1) +
            coord_fixed()+
            labs(title = paste(nrow(if_spray), 'IF BIP'))+
            xlim(-105,105)+ylim(-35,160)+
            theme_void()+
            theme(legend.position = 'bottom',
                  plot.title = element_text(hjust = .5))+
            new_scale_fill() +
            geom_point(aes(fill = HitType),  size = 2, pch = 21, alpha = .7) +
            scale_fill_manual(values = hit_colors) +
            geom_label(data = poly_labels, aes(x=x, y=y, label = label), size = 4) 
          
        } else if(between(if_bip, 40,80)) {
          {
            start_x <- -95
            start_y <- 80
            mid_x <- 0.45
            mid_y <- 250
            end_x <- 95
            end_y <- 80
            num_points <- 8 # Number of points along the arc
          }
          
          # Calculate arc points
          arc_points_if <- calculate_arc(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points)
          
          {
            start_x <- -253
            start_y <- 245
            mid_x <- 0.45
            mid_y <- 450
            end_x <- 257
            end_y <- 246
            num_points <- 8 # Number of points along the arc
          }
          
          arc_points_of <- calculate_arc(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points)
          
          if_spray <- spray_point_opp_tbl %>%
            filter(!is.na(hc_x))%>%
            filter(below_arc)%>%
            filter(!is.na(Bearing))
          
          
          if_spray_pct <- if_spray %>%
            arrange(Bearing) %>%
            group_by(hit_direction_7)%>%
            summarise(bip = n()) %>%
            filter(!is.na(hit_direction_7))%>%
            mutate(pct = bip / sum(bip, na.rm = T),
                   label = round(pct * 100,1),
                   
            ) 
          
          polygons <- bind_rows(
            lapply(1:(nrow(arc_points_if) - 1), function(i) {
              data.frame(
                polygon_id = i, # Unique ID for each polygon
                x = c(0, arc_points_if$x[i], arc_points_if$x[i + 1]),
                y = c(-13.8, arc_points_if$y[i], arc_points_if$y[i + 1])
              )
            })
          ) %>%
            left_join(if_spray_pct, by = c('polygon_id' = 'hit_direction_7')) %>%
            mutate(across(c(bip, pct, label), ~ifelse(is.na(.),0,.)))
          
          poly_labels <- polygons %>%
            filter(x != 0) %>%
            group_by(polygon_id) %>%
            summarise(across(c(x,y,pct,label), ~ mean(.,na.rm = T)),
                      
                      bip = sum(bip, na.rm = T)) %>%
            mutate(across(c(pct, label), ~ ifelse(is.na(.),0,.)))
          
          if_spray_plot <- ggplot(data = if_spray, aes(x=hc_x, y=hc_y))+ 
            geom_mlb_stadium(stadium_ids = 'cubs',
                             stadium_transform_coords = TRUE,
                             stadium_segments = c('home_plate', 'foul_lines', 'infield_outer', 'infield_inner') ,
                             size = 1,
                             color = 'black')+
            geom_point(aes(x=-63,y=63-5), pch = 23, color = 'black', fill = 'white', show.legend = F, size = 4)+
            geom_point(aes(x=63,y=63-5), pch = 23, color = 'black', fill = 'white', show.legend = F, size = 4)+
            geom_point(aes(x=0,y=125-5), pch = 23, color = 'black', fill = 'white', show.legend = F, size = 4)+
            geom_polygon(data = polygons, aes(x=x,y=y, group = polygon_id, fill = pct), color = "black", show.legend = FALSE, alpha = .5) +
            # geom_segment(data = ballparks, aes(x = Start_x , y = Start_y, xend = End_x, yend = End_y), color= 'black', size =1) +
            geom_path (data=arc_points_if, aes(x=x,y=y), size = 2)+
            scale_fill_gradient2(low = 'cornflowerblue', high = '#c22727', midpoint = .15) +
            coord_fixed()+
            labs(title = paste(nrow(if_spray), 'IF BIP'))+
            xlim(-105,105)+ylim(-35,160)+
            theme_void()+
            theme(legend.position = 'bottom',
                  plot.title = element_text(hjust = .5))+
            new_scale_fill() +
            geom_point(aes(fill = HitType),  size = 2, pch = 21, alpha = .7) +
            scale_fill_manual(values = hit_colors) +
            geom_label(data = poly_labels, aes(x=x, y=y, label = label), size = 4)
          
        } else if(if_bip < 40) {
          {
            start_x <- -95
            start_y <- 80
            mid_x <- 0.45
            mid_y <- 250
            end_x <- 95
            end_y <- 80
            num_points <- 6 # Number of points along the arc
          }
          
          # Calculate arc points
          arc_points_if <- calculate_arc(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points)
          
          {
            start_x <- -253
            start_y <- 245
            mid_x <- 0.45
            mid_y <- 450
            end_x <- 257
            end_y <- 246
            num_points <- 6 # Number of points along the arc
          }
          
          arc_points_of <- calculate_arc(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points)
          
          if_spray <- spray_point_opp_tbl %>%
            filter(!is.na(hc_x))%>%
            filter(below_arc)%>%
            filter(!is.na(Bearing))
          
          
          if_spray_pct <- if_spray %>%
            arrange(Bearing) %>%
            group_by(hit_direction_5)%>%
            summarise(bip = n()) %>%
            filter(!is.na(hit_direction_5))%>%
            mutate(pct = bip / sum(bip, na.rm = T),
                   label = round(pct * 100,1),
                   
            ) 
          
          polygons <- bind_rows(
            lapply(1:(nrow(arc_points_if) - 1), function(i) {
              data.frame(
                polygon_id = i, # Unique ID for each polygon
                x = c(0, arc_points_if$x[i], arc_points_if$x[i + 1]),
                y = c(-13.8, arc_points_if$y[i], arc_points_if$y[i + 1])
              )
            })
          ) %>%
            left_join(if_spray_pct, by = c('polygon_id' = 'hit_direction_5')) %>%
            mutate(across(c(bip, pct, label), ~ifelse(is.na(.),0,.)))
          
          poly_labels <- polygons %>%
            filter(x != 0) %>%
            group_by(polygon_id) %>%
            summarise(across(c(x,y,pct,label), ~ mean(.,na.rm = T)),
                      
                      bip = sum(bip, na.rm = T)) %>%
            mutate(across(c(pct, label), ~ ifelse(is.na(.),0,.)))
          
          if_spray_plot <- ggplot(data = if_spray, aes(x=hc_x, y=hc_y))+ 
            geom_mlb_stadium(stadium_ids = 'cubs',
                             stadium_transform_coords = TRUE,
                             stadium_segments = c('home_plate', 'foul_lines', 'infield_outer', 'infield_inner') ,
                             size = 1,
                             color = 'black')+
            geom_point(aes(x=-63,y=63-5), pch = 23, color = 'black', fill = 'white', show.legend = F, size = 4)+
            geom_point(aes(x=63,y=63-5), pch = 23, color = 'black', fill = 'white', show.legend = F, size = 4)+
            geom_point(aes(x=0,y=125-5), pch = 23, color = 'black', fill = 'white', show.legend = F, size = 4)+
            geom_polygon(data = polygons, aes(x=x,y=y, group = polygon_id, fill = pct), color = "black", show.legend = FALSE, alpha = .5) +
            # geom_segment(data = ballparks, aes(x = Start_x , y = Start_y, xend = End_x, yend = End_y), color= 'black', size =1) +
            geom_path (data=arc_points_if, aes(x=x,y=y), size = 2)+
            scale_fill_gradient2(low = 'cornflowerblue', high = '#c22727', midpoint = .15) +
            coord_fixed()+
            labs(title = paste(nrow(if_spray), 'IF BIP'))+
            xlim(-105,105)+ylim(-35,160)+
            theme_void()+
            theme(legend.position = 'bottom',
                  plot.title = element_text(hjust = .5))+
            new_scale_fill() +
            geom_point(aes(fill = HitType),  size = 2, pch = 21, alpha = .7) +
            scale_fill_manual(values = hit_colors) +
            geom_label(data = poly_labels, aes(x=x, y=y, label = label), size = 4)
        }
        
        if_spray_plot
      }
      
      { # OF SPRAY PLOT ----
        if(of_bip >= 80) {
          
          {
            start_x <- -95
            start_y <- 80
            mid_x <- 0.45
            mid_y <- 250
            end_x <- 95
            end_y <- 80
            num_points <- 10 # Number of points along the arc
          }
          
          # Calculate arc points
          arc_points_if <- calculate_arc(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points)
          
          {
            start_x <- -253
            start_y <- 245
            mid_x <- 0.45
            mid_y <- 450
            end_x <- 257
            end_y <- 246
            num_points <- 10 # Number of points along the arc
          }
          
          arc_points_of <- calculate_arc(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points)
          
          
          of_spray <- spray_point_opp_tbl %>%
            filter(!is.na(hc_x))%>%
            # filter(!HitType %in% c('GroundBall','GB')) %>%
            filter(below_arc==F)%>%
            filter(!is.na(Bearing))
          
          
          of_spray_pct <- of_spray %>%
            arrange(Bearing) %>%
            group_by(hit_direction_9)%>%
            summarise(bip = n()) %>%
            filter(!is.na(hit_direction_9))%>%
            mutate(pct = bip / sum(bip, na.rm = T),
                   label = round(pct * 100,1),
            ) 
          
          polygons_of <- read.csv('C://Users/tdmed/OneDrive/Documents/of_spray_9.csv') %>%
            left_join(of_spray_pct, by = c('polygon_id' = 'hit_direction_9')) %>%
            mutate(across(c(bip, pct, label), ~ifelse(is.na(.),0,.)))
          
          poly_labels_of <- polygons_of %>%
            filter(x != 0) %>% 
            group_by(polygon_id) %>%
            mutate(row = row_number()) %>%
            filter(row >=3) %>%
            summarise(across(c(x,y,pct,label), ~ mean(.,na.rm = T)),
                      
                      bip = sum(bip, na.rm = T)) %>%
            mutate(across(c(pct, label), ~ ifelse(is.na(.),0,.)))
          
          
          of_spray_plot <- ggplot(data = of_spray, aes(x=hc_x, y=hc_y))+
            geom_segment(aes(x = -45, y = 80, xend = 177.82, yend = 300),color = "grey", size = 1, linetype = "dotted") +
            geom_segment(aes(x = 0, y = 80, xend = 0, yend = 350),color = "grey", size = 1, linetype = "dotted") +
            geom_segment(aes(x = 45, y = 80, xend = -177.82, yend = 300),color = "grey", size = 1, linetype = "dotted") + 
            geom_polygon(data = polygons_of, aes(x=x,y=y, group = polygon_id, fill = pct), color = "black", show.legend = FALSE, alpha = .5) +
            # geom_path (data=arc_points_of, aes(x=x,y=y), size = 2)+
            geom_segment(data = ballparks, aes(x = Start_x , y = Start_y, xend = End_x, yend = End_y), color= 'black', size = 2) +
            geom_mlb_stadium(stadium_ids = 'cubs',
                             stadium_transform_coords = TRUE,
                             stadium_segments = c('home_plate', 'foul_lines', 'infield_outer', 'infield_inner') ,
                             size = 1,
                             color = 'black')+
            scale_fill_gradient2(low = 'cornflowerblue', high = '#820000', midpoint = .1)+
            coord_fixed()+
            labs(title = paste(nrow(of_spray), 'OF BIP'),
                 subtitle = paste(unique(ballparks$Team), 'Field Overlay'))+
            ylim(80,435)+
            theme_void()+
            theme(legend.position = 'bottom',
                  plot.title = element_text(hjust = .5),
                  plot.subtitle = element_text(hjust = .5))+
            new_scale_fill() +
            geom_point(aes(fill = HitType),  size = 2, pch = 21, alpha = .7) +
            scale_fill_manual(values = hit_colors) +
            geom_label(data = poly_labels_of, aes(x=x, y=y, label = label), size = 4)+
            geom_text(data = subset(ballparks, Loc == 'LF'), aes(x = End_x, y = End_y + 35, label = Distance), color = 'black', fontface = 'bold') +
            geom_text(data = subset(ballparks, Loc == 'LCF'), aes(x = End_x, y = End_y + 35, label = Distance), color = 'black', fontface = 'bold') +
            geom_text(data = subset(ballparks, Loc == 'CF'), aes(x = End_x, y = End_y + 35, label = Distance), color = 'black', fontface = 'bold') +
            geom_text(data = subset(ballparks, Loc == 'RCF'), aes(x = End_x, y = End_y + 35, label = Distance), color = 'black', fontface = 'bold') +
            geom_text(data = subset(ballparks, Loc == 'RF'), aes(x = End_x, y = End_y + 35, label = Distance), color = 'black', fontface = 'bold') 
          
          
        } else if(between(of_bip,40,80)) {
          
          {
            start_x <- -95
            start_y <- 80
            mid_x <- 0.45
            mid_y <- 250
            end_x <- 95
            end_y <- 80
            num_points <- 8 # Number of points along the arc
          }
          
          # Calculate arc points
          arc_points_if <- calculate_arc(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points)
          
          {
            start_x <- -253
            start_y <- 245
            mid_x <- 0.45
            mid_y <- 450
            end_x <- 257
            end_y <- 246
            num_points <- 8 # Number of points along the arc
          }
          
          arc_points_of <- calculate_arc(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points)
          
          
          of_spray <- spray_point_opp_tbl %>%
            filter(!is.na(hc_x))%>%
            # filter(!HitType %in% c('GroundBall','GB')) %>%
            filter(below_arc==F)%>%
            filter(!is.na(Bearing))
          
          
          of_spray_pct <- of_spray %>%
            arrange(Bearing) %>%
            group_by(hit_direction_7)%>%
            summarise(bip = n()) %>%
            filter(!is.na(hit_direction_7))%>%
            mutate(pct = bip / sum(bip, na.rm = T),
                   label = round(pct * 100,1),
            ) 
          
          polygons_of <-  read.csv('C://Users/tdmed/OneDrive/Documents/of_spray_7.csv') %>%
            left_join(of_spray_pct, by = c('polygon_id' = 'hit_direction_7')) %>%
            mutate(across(c(bip, pct, label), ~ifelse(is.na(.),0,.)))
          
          poly_labels_of <- polygons_of %>%
            filter(x != 0) %>% 
            group_by(polygon_id) %>%
            mutate(row = row_number()) %>%
            filter(row >=3) %>%
            summarise(across(c(x,y,pct,label), ~ mean(.,na.rm = T)),
                      
                      bip = sum(bip, na.rm = T)) %>%
            mutate(across(c(pct, label), ~ ifelse(is.na(.),0,.)))
          
          
          of_spray_plot <- ggplot(data = of_spray, aes(x=hc_x, y=hc_y))+ 
            geom_segment(aes(x = -45, y = 80, xend = 177.82, yend = 300),color = "grey", size = 1, linetype = "dotted") +
            geom_segment(aes(x = 0, y = 80, xend = 0, yend = 350),color = "grey", size = 1, linetype = "dotted") +
            geom_segment(aes(x = 45, y = 80, xend = -177.82, yend = 300),color = "grey", size = 1, linetype = "dotted") + 
            geom_polygon(data = polygons_of, aes(x=x,y=y, group = polygon_id, fill = pct), color = "black", show.legend = FALSE, alpha = .5) +
            # geom_path (data=arc_points_of, aes(x=x,y=y), size = 2)+
            geom_segment(data = ballparks, aes(x = Start_x , y = Start_y, xend = End_x, yend = End_y), color= 'black', size = 2) +
            geom_mlb_stadium(stadium_ids = 'cubs',
                             stadium_transform_coords = TRUE,
                             stadium_segments = c('home_plate', 'foul_lines', 'infield_outer', 'infield_inner') ,
                             size = 1,
                             color = 'black')+
            scale_fill_gradient2(low = 'cornflowerblue', high = '#820000', midpoint = .15)+
            coord_fixed()+
            labs(title = paste(nrow(of_spray), 'OF BIP'),
                 subtitle = paste(unique(ballparks$Team), 'Field Overlay'))+
            ylim(80,435)+
            theme_void()+
            theme(legend.position = 'bottom',
                  plot.title = element_text(hjust = .5),
                  plot.subtitle = element_text(hjust = .5))+
            new_scale_fill() +
            geom_point(aes(fill = HitType),  size = 2, pch = 21, alpha = .7) +
            scale_fill_manual(values = hit_colors) +
            geom_label(data = poly_labels_of, aes(x=x, y=y, label = label), size = 4)+
            geom_text(data = subset(ballparks, Loc == 'LF'), aes(x = End_x, y = End_y + 35, label = Distance), color = 'black', fontface = 'bold') +
            geom_text(data = subset(ballparks, Loc == 'LCF'), aes(x = End_x, y = End_y + 35, label = Distance), color = 'black', fontface = 'bold') +
            geom_text(data = subset(ballparks, Loc == 'CF'), aes(x = End_x, y = End_y + 35, label = Distance), color = 'black', fontface = 'bold') +
            geom_text(data = subset(ballparks, Loc == 'RCF'), aes(x = End_x, y = End_y + 35, label = Distance), color = 'black', fontface = 'bold') +
            geom_text(data = subset(ballparks, Loc == 'RF'), aes(x = End_x, y = End_y + 35, label = Distance), color = 'black', fontface = 'bold') 
          
          
          
        } else if(of_bip < 40) {
          
          {
            start_x <- -95
            start_y <- 80
            mid_x <- 0.45
            mid_y <- 250
            end_x <- 95
            end_y <- 80
            num_points <- 6 # Number of points along the arc
          }
          
          # Calculate arc points
          arc_points_if <- calculate_arc(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points)
          
          {
            start_x <- -253
            start_y <- 245
            mid_x <- 0.45
            mid_y <- 450
            end_x <- 257
            end_y <- 246
            num_points <- 6 # Number of points along the arc
          }
          
          arc_points_of <- calculate_arc(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points)
          
          
          of_spray <- spray_point_opp_tbl %>%
            filter(!is.na(hc_x))%>%
            # filter(!HitType %in% c('GroundBall','GB')) %>%
            filter(below_arc==F)%>%
            filter(!is.na(Bearing))
          
          
          of_spray_pct <- of_spray %>%
            arrange(Bearing) %>%
            group_by(hit_direction_5)%>%
            summarise(bip = n()) %>%
            filter(!is.na(hit_direction_5))%>%
            mutate(pct = bip / sum(bip, na.rm = T),
                   label = round(pct * 100,1),
            ) 
          
          polygons_of <-  read.csv('C://Users/tdmed/OneDrive/Documents/of_spray_5.csv') %>%
            # bind_rows(lapply(1:(nrow(arc_points_of) - 1), function(i) {
            #   data.frame(
            #     polygon_id = i, # Unique ID for each polygon
            #     x = c(0, arc_points_of$x[i], arc_points_of$x[i + 1]),
            #     y = c(-13.8, arc_points_of$y[i], arc_points_of$y[i + 1])
            #   )
            # }) ) %>%
            left_join(of_spray_pct, by = c('polygon_id' = 'hit_direction_5')) %>%
            mutate(across(c(bip, pct, label), ~ifelse(is.na(.),0,.)))
          
          poly_labels_of <- polygons_of %>%
            filter(x != 0) %>% 
            group_by(polygon_id) %>%
            mutate(row = row_number()) %>%
            filter(row >=3) %>%
            summarise(across(c(x,y,pct,label), ~ mean(.,na.rm = T)),
                      
                      bip = sum(bip, na.rm = T)) %>%
            mutate(across(c(pct, label), ~ ifelse(is.na(.),0,.)))
          
          
          of_spray_plot <- ggplot(data = of_spray, aes(x=hc_x, y=hc_y))+ 
            geom_segment(aes(x = -45, y = 80, xend = 177.82, yend = 300),color = "grey", size = 1, linetype = "dotted") +
            geom_segment(aes(x = 0, y = 80, xend = 0, yend = 350),color = "grey", size = 1, linetype = "dotted") +
            geom_segment(aes(x = 45, y = 80, xend = -177.82, yend = 300),color = "grey", size = 1, linetype = "dotted") + 
            geom_polygon(data = polygons_of, aes(x=x,y=y, group = polygon_id, fill = pct), color = "black", show.legend = FALSE, alpha = .5) +
            # geom_path (data=arc_points_of, aes(x=x,y=y), size = 2)+
            geom_segment(data = ballparks, aes(x = Start_x , y = Start_y, xend = End_x, yend = End_y), color= 'black', size = 2) +
            geom_mlb_stadium(stadium_ids = 'cubs',
                             stadium_transform_coords = TRUE,
                             stadium_segments = c('home_plate', 'foul_lines', 'infield_outer', 'infield_inner') ,
                             size = 1,
                             color = 'black')+
            scale_fill_gradient2(low = 'cornflowerblue', high = '#820000', midpoint = .15)+
            coord_fixed()+
            labs(title = paste(nrow(of_spray), 'OF BIP'),
                 subtitle = paste(unique(ballparks$Team), 'Field Overlay'))+
            ylim(80,435)+
            theme_void()+
            theme(legend.position = 'bottom',
                  plot.title = element_text(hjust = .5),
                  plot.subtitle = element_text(hjust = .5))+
            new_scale_fill() +
            geom_point(aes(fill = HitType),  size = 2, pch = 21, alpha = .7) +
            scale_fill_manual(values = hit_colors) +
            geom_label(data = poly_labels_of, aes(x=x, y=y, label = label), size = 4)+
            geom_text(data = subset(ballparks, Loc == 'LF'), aes(x = End_x, y = End_y + 35, label = Distance), color = 'black', fontface = 'bold') +
            geom_text(data = subset(ballparks, Loc == 'LCF'), aes(x = End_x, y = End_y + 35, label = Distance), color = 'black', fontface = 'bold') +
            geom_text(data = subset(ballparks, Loc == 'CF'), aes(x = End_x, y = End_y + 35, label = Distance), color = 'black', fontface = 'bold') +
            geom_text(data = subset(ballparks, Loc == 'RCF'), aes(x = End_x, y = End_y + 35, label = Distance), color = 'black', fontface = 'bold') +
            geom_text(data = subset(ballparks, Loc == 'RF'), aes(x = End_x, y = End_y + 35, label = Distance), color = 'black', fontface = 'bold') 
          
          
          
        }
        of_spray_plot
      }
      
      { # BIP TABLE ----
        if(nrow(hitter_data %>% filter(PitchCall =='InPlay')) > 1 ){
          
          bat_v_p_bip <-
            hitter_data %>% 
            filter(PitchCall =='InPlay') %>%
            group_by(Pitcher = PitcherThrows) %>%
            summarise(#PA = sum(is_pa),
              BIP = sum(PitchCall == 'InPlay', na.rm = T),
              BBE = sum(PitchCall == 'InPlay' & bbe ==1,na.rm = T),
              EV = mean(ExitSpeed[bbe==1], na.rm = T),
              MaxEV = max(ExitSpeed[bbe==1], na.rm = T),
              LA = mean(Angle[bbe==1], na.rm = T),
              Hard = sum(hardhit[bbe==1], na.rm = T),
              `Hard%`= sum(hardhit[bbe==1]/BIP, na.rm = T),
              `GB%`= sum((HitType=='GroundBall')/BIP, na.rm = T),
              `LD%`= sum((HitType=='LineDrive')/BIP, na.rm = T),
              `FB%`= sum((HitType=='FlyBall')/BIP, na.rm = T),
              `PU%`= sum((HitType=='PopUp')/BIP, na.rm = T),
              `Pull%` = sum(HitDirection1 == BatterSide, na.rm = T) / BBE,
              `Center%` = sum(HitDirection1 == 'Center', na.rm = T) / BBE,
              # `Oppo%` = sum(!HitDirection1 %in% c(BatterSide,'Center',''), na.rm = T) / BIP,
              `Oppo%` = sum(HitDirection1 == oppo_hand, na.rm = TRUE   ) / BBE
              
            ) %>%
            ungroup() %>%
            mutate(across(c(4:7), ~ round(., 1)),
                   across(c(8:15), ~ round(.*100, 1)))
          bat_v_p_bip
        }else{
          
          # Create an empty data frame with specified column names
          bat_v_p_bip <- data.frame(
            PA = integer(),
            BIP = integer(),
            EV = numeric(),
            MaxEV = numeric(),
            LA = numeric(),
            Hard = integer(),
            `Hard%` = numeric(),
            `GB%` = numeric(),
            `LD%` = numeric(),
            `FB%` = numeric(),
            `PU%` = numeric(),
            `Pull%` = numeric(),
            `Center%` = numeric(),
            `Oppo%` = numeric(),
            stringsAsFactors = FALSE
          )
          
          
          
        }
        
        
        
      }
      
      { # BATTER DISCIPLINE TABLE ----
        bat_v_p_disc <-
          hitter_data %>% 
          group_by(Pitcher = PitcherThrows) %>%
          summarise(PA = sum(is_pa),
                    `1PSw%` = sum(swing == 1 & PitchofPA == 1, na.rm = T) / PA,
                    `Z_Sw%` = sum(swing == 1 & in_zone == 1, na.rm = T) / n(),
                    `Whiff%` = sum(whiff, na.rm = T) / sum(swing, na.rm = T),
                    `Z_Whiff%` = sum(whiff==1 & in_zone == 1, na.rm = T) / sum(swing, na.rm = T),
                    `Chase%` = sum(whiff==1 & in_zone == 0, na.rm = T) / sum(in_zone==0, na.rm = T),
                    `Chase_Whiff%` = sum(whiff==1 & in_zone == 0, na.rm = T) / sum(swing==1 & in_zone==0, na.rm = T),
          ) %>%
          mutate(across(c(3:8), ~ round(.*100, 1))
          )
        
      }
      
      { # BATTER Splits TABLE ----
        splits <-
          hitter_data %>% 
          group_by(Pitcher = PitcherThrows) %>%
          summarise(PA = sum(is_pa),
                    AB = sum(is_ab, na.rm = T),
                    H = sum(is_hit, na.rm = T),
                    XBH = sum(total_bases >= 2, na.rm = T),
                    HR = sum(total_bases==4, na.rm = T),
                    TB = sum(total_bases, na.rm = T),
                    BB = sum(PlayResult=='Walk',na.rm = T),
                    HBP = sum(PitchCall=='HitByPitch', na.rm = T),
                    SO = sum(grepl('Strikeout', PlayResult), na.rm = T),
                    RB = sum(reach_base, na.rm = T),
                    woba_v = sum(woba_value, na.rm = T)
          ) %>%
          mutate(wOBA = round(woba_v / PA,3),
                 BA = round(H/AB,3),
                 OBP = round(RB / PA, 3),
                 SLG = round(TB / AB, 3),
                 OPS = OBP+SLG,
                 BB_pct = round(BB / PA, 3)*100,
                 SO_pct = round(SO / PA,3)*100,
                 
          ) %>%
          dplyr::select(-c(TB,RB,HBP,woba_v))
        
      }
      
      
      { # VS RHP / LHP ----
        pitch_vs_lhp <- hitter_data %>% filter(PitcherThrows =='Left') %>%
          group_by(Pitcher = PitcherThrows, PitchClass) %>%
          summarise(PA = sum(is_pa,na.rm = T),
                    BIP = sum(PitchCall=='InPlay', na.rm = T),
                    BBE = sum(bbe, na.rm = T),
                    `1PSw` = sum(PitchofPA==1 & swing==1) ,
                    Whiff = sum(whiff==1),
                    Swings = sum(swing==1),
                    `Chase` = sum(swing==1 & in_zone == 0, na.rm = T),
                    OZ = sum(in_zone==0, na.rm = T),
                    `Hard` = sum(hardhit==1, na.rm = T)
          ) %>%
          janitor::adorn_totals() %>%
          mutate(`1PSw%` = round(`1PSw`/ PA * 100, 1),
                 `Whiff%` = round(Whiff / Swings * 100, 1),
                 `Chase%` = round(Chase / OZ * 100, 1 ),
                 `Hard%` =  round(Hard / BBE * 100,1)
                 
          ) %>%
          mutate(PitchClass = factor(PitchClass, levels = c('Fastballs', 'BreakingBalls', 'Offspeed', '-'))) %>%
          arrange(PitchClass) %>%
          ungroup() %>%
          dplyr::select(-1) %>%
          mutate(PitchClass = str_replace_all(PitchClass,  c('Fastballs' = 'Fastballs', 'BreakingBalls' = 'Breaking', 'Offspeed' = 'Offspeed', '-' = 'Total'))
          ) %>%
          ungroup() %>%
          dplyr::select(-c(4:10))
        
        pitch_vs_lhp
        
        pitch_vs_rhp <- hitter_data %>% filter(PitcherThrows =='Right') %>%
          group_by(Pitcher = PitcherThrows, PitchClass) %>%
          summarise(PA = sum(is_pa,na.rm = T),
                    BIP = sum(PitchCall=='InPlay', na.rm = T),
                    BBE = sum(bbe, na.rm = T),
                    `1PSw` = sum(PitchofPA==1 & swing==1) ,
                    Whiff = sum(whiff==1),
                    Swings = sum(swing==1),
                    `Chase` = sum(swing==1 & in_zone == 0, na.rm = T),
                    OZ = sum(in_zone==0, na.rm = T),
                    `Hard` = sum(hardhit==1, na.rm = T)
          ) %>%
          janitor::adorn_totals() %>%
          mutate(`1PSw%` = round(`1PSw`/ PA * 100, 1),
                 `Whiff%` = round(Whiff / Swings * 100, 1),
                 `Chase%` = round(Chase / OZ * 100, 1 ),
                 `Hard%` =  round(Hard / BBE * 100,1)
                 
          ) %>%
          mutate(PitchClass = factor(PitchClass, levels = c('Fastballs', 'BreakingBalls', 'Offspeed', '-'))) %>%
          arrange(PitchClass) %>%
          ungroup() %>%
          dplyr::select(-1) %>%
          mutate(PitchClass = str_replace_all(PitchClass,  c('Fastballs' = 'Fastballs', 'BreakingBalls' = 'Breaking', 'Offspeed' = 'Offspeed', '-' = 'Total'))
          )%>%
          ungroup() %>%
          dplyr::select(-c(4:10))
        
        
      }
      
      yakker %>%
        dplyr::select(PitchCall, swing, whiff) %>%
        group_by(PitchCall) %>%
        summarise(swing = sum(swing, na.rm = T),
                  whiff = sum(whiff, na.rm = T))
      
      batter_side <- case_when(i %in% c('B','S','Both','Switch') ~ c('Switch'),
                               i %in% c('R','Right', 'RHH') ~ 'RHH',
                               i %in% c('L', 'Left', 'LHH') ~ 'LHH',
                               T ~ ''
      )
      
      # PARAMETERS FOR THE R MARKDOWN FILE ----
      params <- list(
        szns = szns,
        hitter = hitter,
        season_stats = season_stats,
        team = hitter_data$BatterTeam[1],
        splits = splits,
        bat_v_p_bip = bat_v_p_bip,
        bat_v_p_disc = bat_v_p_disc,
        pitch_vs_rhp = pitch_vs_rhp,
        pitch_vs_lhp = pitch_vs_lhp,
        batter_side = batter_side,
        EV_zone_opp = EV_zone_opp,
        hh_opp = hh_opp,
        sw_opp = sw_opp,
        wh_opp = wh_opp,
        whiff_opp = whiff_opp,
        if_spray_plot = if_spray_plot,
        of_spray_plot = of_spray_plot,
        team_logo = team_location,
        avg_by_zone = avg_by_zone,
        slg_by_zone = slg_by_zone
        
      )
      
      # SETS THE DATE FOR THE FILE Player
      # file_date <- format(hitter_data$Date[1], "%m-%d")
      
      # Knit the R Markdown file to PDF
      
      
      # )
      # RUN ----
      suppressWarnings({
        rmarkdown::render(input = "C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/adv_hitter_report.Rmd",
                          output_file = paste0("C:/Users/tdmed/OneDrive/R_Markdown/hitter_reports/",team_code,"/", hitter,"-", batter_side," report",".pdf"),
                          params = params)
      })
      #  
    } # for i in batter_sides
  } # for  hitter in unique_hitters
}
# END OF LOOP

# COPY ---------
{
  
  print(paste(team_filter, "Hitter Reports Created"))
  od_adv_folder <- paste0("C:/Users/tdmed/OneDrive/_Advance/",team_code,"/H/")
  
  pdf_folder  <- paste0("C:/Users/tdmed/OneDrive/R_Markdown/hitter_reports/",team_code,"/")
  #copy to advance folders
  file.copy(from = list.files(pdf_folder, pattern = "\\.pdf", full.names = TRUE),
            to = od_adv_folder, overwrite = TRUE)
  
  print(paste(team_filter, "Hitter copied to OneDrive"))
  
  # Combine into 1 pdf
  pdf_files <- list.files(od_adv_folder, pattern = "\\.pdf$", full.names = TRUE)
  output_file <- paste0("C:/Users/tdmed/OneDrive/_Advance/",team_code,"/",team_code," Hitters.pdf")
  pdf_combine(pdf_files, output_file)
}


# }

logs <- list.files("C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/", pattern = '.log', full.names = T)
for (log in logs){
  file.rename(log,
              to = paste0('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/logs/',basename(log)) )
}

#}

# {
#   
#   # This hcunk of code copies the r markdown files to a google drive folder. I have folders for each individual pitcher as well. The report then gets copied to that hitters folder as long as the hitters name is spelled correctly.
#   reports<- list.files("C:/Users/tdmed/OneDrive/R_Markdown/pitcher_reports/", pattern = ".pdf")
#   gd_folder<- "C:/Users/tdmed/OneDrive/R_Markdown/pitcher_reports/"
#   
#   all_p<- 'G:/My Drive/2023 Boomers/Boomers Reports/hitters'
#   
#   p<- paste0(all_p,"/",list.files(all_p))
#   p
#   daily_p_folder<-'G:/My Drive/2023 Boomers/Boomers Reports/hitters/_Daily/'
#   
#   reports
#   send_webhook_message("Copying reports to Google Drive!")
#   
#   file.rename(from = paste0(gd_folder,reports),
#               to =paste0(daily_p_folder,reports))
#   
#   daily_p <- list.files('G:/My Drive/2023 Boomers/Boomers Reports/hitters/_Daily')
#   daily_p
#   
#   daily_p <- as.data.frame(daily_p) %>%
#     mutate(Pitcher = str_trim(str_sub(daily_p, start = 6)),
#            Pitcher = gsub('.pdf','',Pitcher),
#            Pitcher = gsub(' report','',Pitcher)) %>%
#     filter(Pitcher != '_Daily')
#   
#   p <- as.data.frame(p) %>%
#     mutate(Pitcher = str_extract(p, "(?<=/)[^/]*$"))
#   
#   copy_df <- p %>% full_join(daily_p, by = 'Pitcher', relationship = 'many-to-many') %>%
#     filter(!is.na(daily_p), !is.na(p)) %>%
#     mutate(copy_from = paste0(daily_p_folder,"/",daily_p),
#            copy_to = paste0(p,"/",daily_p))
#   
#   if(length(copy_df) > 0) {
#     file.rename(from = copy_df$copy_from,
#                 to = copy_df$copy_to)
#     
#     send_webhook_message(paste(emoji('baseball'), "Boomers Reports Finished Generating and copied to each pitcher's folder!"))
#     
#   } else (send_webhook_message("Error Copying Reports. Please Check")
#   )
# }
