library(ggplot2)
library(GeomMLBStadiums)
library(ggnewscale)
library(cowplot)
library(tidyverse)
library(stringr)
library(RSQLite)
library(grid)
library(gridExtra)
library(cowplot)
library(glue)
library(emojifont)

emojifont::search_emoji('yellow')
red_circle <- 'red'
green_circle <- "green"
yellow_circle <- "yellow"
white_circle <- "white"

# sb_w <- 1
# x3b_w <- .5
# stats <- dbGetQuery(db, 'select * from stats_hitting_player') %>%
#   # mutate(SBA = SB + CS,
#   #        speed = ((sb_w * (SBA)) + (x3b_w * X3B)) / PA * G,
#   #        # speed_log = log1p(speed),  # log(1 + x) to handle 0s
#   #        Speed_Index_Z = (speed - mean(speed, na.rm = TRUE)) / sd(speed, na.rm = TRUE),
#   #        Speed_Plus = round(100 + 10 * Speed_Index_Z)
#   #        
#   # ) %>%
#   mutate(h_1b = X1B / H,
#          SBA = SB + CS,.after = G) %>%
#   select(Player, player_id_fl, SEASON, Team, G,H, X1B, X3B,SB, SBA)  %>%
#   distinct() %>%
#   filter(G > 1,
#          # SEASON == 2024
#          ) %>%
#   group_by(Player, player_id_fl) %>%
#   summarise(Seasons = n_distinct(SEASON),#recent_season = glue::glue("{min(SEASON)} - {max(SEASON)}"),
#             across(G:SBA, ~sum(.,na.rm = T)),
#             .groups = 'drop')%>%
#   mutate(G_Szn = G / Seasons,
#          SBA_3B = (SBA+X3B),
#          speed = (SBA + X3B) / G,
#          Speed_Index_Z = (speed - mean(speed[G_Szn >= 10], na.rm = TRUE)) / sd(speed[G_Szn >= 30], na.rm = TRUE),
#          Speed_Plus = round(100 + 10 * Speed_Index_Z),
#          color = ifelse(G_Szn <= 10, white_circle, ifelse(Speed_Plus >=100, green_circle, ifelse(between(Speed_Plus, 95,100), yellow_circle, red_circle)))
#          
#   ) %>%
#   arrange(desc(Speed_Plus))
# 
# mean(stats$speed,na.rm = T)
# sd(stats$speed,na.rm = T)
# mean(stats$speed[stats$G_Szn >= 10],na.rm = T)
# sd(stats$speed[stats$G_Szn >= 10],na.rm = T)
# 
# ggplot(stats, aes(x = Speed_Plus)) +
#   geom_histogram(aes(y = ..density..), bins = 30, fill = "#4e55d9", alpha = 0.7, color = "white") +
#   # geom_density(color = "#242766", size = 1.2) +
#   geom_vline(xintercept = 100, linetype = "dashed", color = "red", size = 1) +
#   labs(
#     title = "Distribution of Speed+",
#     x = "Speed+ (100 = Average)",
#     y = "Density"
#   ) +
#   theme_minimal(base_size = 14)

db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")


opposing_hittier_pos_cards <- function(database, hitters, home_team_code) {
  
  hit_colors <- c(
    "GB" = "forestgreen",
    "LD" = "#C70039",
    "FB" = "gold",
    "PU" = "#42d4fb"
  )
  
  get_legend <- function(my_plot) {
    tmp <- ggplotGrob(my_plot)
    leg <- gtable:::`gtable_filter`(tmp, "guide-box-bottom")
    # (tmp, "guide-box-bottom")
    return(leg)
  }
  
  df <- data.frame(x=1:4, y=1:4, HitType = c("GB","LD", "FB", "PU"))
  
  # a dummy plot to pull the legend from
  plot_leg <- ggplot(df, aes(x=x,y=y, color = HitType))+
    geom_point(size = 2)+
    scale_color_manual(values = hit_colors)+
    labs(color = "")+
    theme_minimal()+
    theme(legend.position = 'bottom')
  
  legend <- get_legend(plot_leg)
  
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
  
  # hitters <- c('Alec Craig', 'Chase Dawson', 'Christian Fedko', 'Anthony Calarco', 'Andrew Sojka',
  #              'Will Prater', 'Kyle Fitzgerald', 'Brett Milazzo', 'Allante Hall', 'Blake Grant-Parks',
  #              'Nick Oddo', 'Braxton Davidson')
  
  # if("unique_hitters" %in% ls(envir = .GlobalEnv)==F){
  #   unique_hitters <- dbGetQuery(db, glue::glue('SELECT * from rosters where SEASON = 2025 and Team = "{team_filter}" and Position not like "P%"'))$Player
  # }
  
  if(pocket_cards_lineup_order == 'manual') {
    unique_hitters <- unique_hitters
  } else if(pocket_cards_lineup_order == 'auto'){
    unique_hitters <- dbGetQuery(db, glue::glue('SELECT * from rosters where SEASON = 2025 and Team = "{team_filter}" and Position not like "P%" and Position not like "Inactive"'))$Player
  }
  
  hitters <- unique_hitters
  
  hitter_spots <- 1:length(hitters)
  
  lineup_spots <- data.frame(hitter = hitters, spot = hitter_spots)
  
  hitters_2_combo <- paste0('"', hitters, '"', collapse = ',')
  
  hitter_sides_lineup <- dbGetQuery(db, glue("SELECT distinct Batter, BatterSide from pitch_data
                                           where Batter in ({hitters_2_combo})")) %>%
    plyr::rbind.fill(#read.csv('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/_other/pocket_spray/clicked_points_Tony_Livermore.csv') %>%
      #  mutate(PitchCall = 'InPlay', TaggedPitchType = 'Other'),
      #read.csv('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/_other/pocket_spray/clicked_points_Banks_Tolley.csv')%>%
      # mutate(PitchCall = 'InPlay', TaggedPitchType = 'Other'),
      read.csv('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/_other/pocket_spray/spring_hitters.csv')%>%
        mutate(PitchCall = 'InPlay', TaggedPitchType = 'Other') %>%
        mutate(hc_x = sin(Bearing * pi/180)*Distance ,
               hc_y = cos(Bearing * pi/180)*Distance) %>%
        filter(BatterTeam == team_filter)
    ) %>% 
    distinct(Batter,BatterSide)%>%
    mutate(#Batter = factor(Batter, levels = hitters),
      Spot = lineup_spots$spot[match(Batter, lineup_spots$hitter)]) %>%
    arrange(Spot)
  
  if(max(hitter_sides_lineup$Spot, na.rm= T) != nrow(hitter_sides_lineup)) {
    
    hitter_sides_lineup$Spot <- 1:nrow(hitter_sides_lineup)
    
    hitter_sides_lineup <- hitter_sides_lineup %>%
      mutate(Spot = ifelse(Batter == lag(Batter, default = ""), 0, 1),
             Spot = cumsum(Spot))
  }
  
  ballparks <- dbGetQuery(db, 'select * from ballpark_dimensions') %>%
    # filter(HomeTeamCode == home_team_code)
    filter(HomeTeamCode == 'BOOM')
  
  
  stats_prev <- dbGetQuery(db, 'select * from stats_hitting_player') %>%
    filter(Player %in% hitters) %>%
    mutate(SBA = SB + CS,.after = G) %>%
    select(Player, player_id_fl, SEASON, Team, G,H, X1B, X3B,SB, SBA)  %>%
    distinct() %>%
    filter(G > 1) %>%
    group_by(Player, player_id_fl) %>%
    summarise(Seasons = n_distinct(SEASON),#recent_season = glue::glue("{min(SEASON)} - {max(SEASON)}"),
              across(G:SBA, ~sum(.,na.rm = T)),
              .groups = 'drop')%>%
    mutate(G_Szn = G / Seasons,
           SBA_3B = (SBA+X3B),
           speed = (SBA + X3B) / G,
           Speed_Index_Z = (speed - 0.1574476) / 0.1562207,
           Speed_Plus = round(100 + 10 * Speed_Index_Z),
           color = ifelse(G_Szn <= 10, white_circle, 
                          ifelse(Speed_Plus >=100, green_circle, 
                                 ifelse(between(Speed_Plus, 95,100), yellow_circle, 
                                        red_circle)))
           
    ) %>%
    arrange(desc(Speed_Plus))
  
  # POSITIONING CARDS ----------------------------------------------------------------------
  {
    lineup <- data.frame()
    
    if_plot_list <- list()
    of_plot_list <- list()
    # i <- 12
    for(i in seq_along(hitter_sides_lineup$Batter)) {
      
      hitter<- hitter_sides_lineup$Batter[i]
      # print(hitter)
      hitter_short <- case_when(lengths(strsplit(hitter, "\\s+")) == 2 ~ word(hitter, start = 2, end = 2),
                                lengths(strsplit(hitter, "\\s+")) == 3 & grepl('Jr', hitter) ~ word(hitter, start = 2, end = 3),
                                lengths(strsplit(hitter, "\\s+")) == 3 & grepl('III|IV', hitter) ~ word(hitter, start = 2, end = 3),
                                T ~ hitter)
      cat(hitter_short, sep = '\n')
      
      lineup_spot <- hitter_sides_lineup$Spot[i]
      
      hitter_speed <- stats_prev$color[match(hitter, stats_prev$Player)]
      
      batter_side <- hitter_sides_lineup$BatterSide[i]
      
      hitter_data <- dbGetQuery(db,  glue::glue(
        'SELECT * FROM pitch_data where Batter = "{hitter}" and BatterSide = "{batter_side}" --and SEASON = 2024')   )%>%
        plyr::rbind.fill(read.csv('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/_other/pocket_spray/clicked_points_Tony_Livermore.csv') %>%
                           mutate(PitchCall = 'InPlay', TaggedPitchType = 'Other'),
                         read.csv('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/_other/pocket_spray/clicked_points_Banks_Tolley.csv')%>%
                           mutate(PitchCall = 'InPlay', TaggedPitchType = 'Other'),
                         read.csv('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/_other/pocket_spray/spring_hitters.csv')%>%
                           mutate(PitchCall = 'InPlay', TaggedPitchType = 'Other') %>%
                           mutate(hc_x = sin(Bearing * pi/180)*Distance ,
                                  hc_y = cos(Bearing * pi/180)*Distance)) %>%
        filter(Batter == hitter) %>%
        dplyr::filter(TaggedPitchType !='', BatterSide != '',TaggedPitchType !='NA',!is.na(TaggedPitchType)) %>%
        mutate(swing = ifelse(PitchCall %in% c('Foul','FoulBall','StrikeSwinging', 'CatchersInterference', 'CatchersInt', 'InPlay'),
                              1,0) ) %>%
        mutate(hit_direction_3 = 
                 case_when(
                   PitchCall == 'InPlay' & Bearing <= -15 ~ 1,
                   PitchCall == 'InPlay' & between(Bearing, -15, 15) ~ 2,
                   PitchCall == 'InPlay' & Bearing >= 15 ~ 3,
                   T ~ NA
                 ) ,
               hit_direction_5 = 
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
               
        ) 
      
      
      hitter_sides <- unique(hitter_data$BatterSide)
      
      
      title_col <- ifelse(batter_side == 'Right', 'black','red')
      
      spray_point_opp_tbl <- hitter_data %>%
        filter(PitchCall == 'InPlay', HitType != 'Bunt', !is.na(HitType) ) %>%
        dplyr::select(Date, HomeTeam,AwayTeam, `Top.Bottom`, PitchNo, PitcherThrows, Batter,
                      BatterSide,ExitSpeed,TaggedPitchType, PitchCall ,PlayResult, HitType, 
                      Angle, HitSpinRate, Direction, PlateLocSide,  Distance, Bearing, pfxx, pfxz, 
                      hit_direction_3, hit_direction_5,hit_direction_7,hit_direction_9  )%>%
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
      
      if_bip <- sum(spray_point_opp_tbl$below_arc==T, na.rm = T)
      of_bip <- sum(spray_point_opp_tbl$below_arc==F, na.rm = T)
      
      # INFIELD ONLY ----
      if(if_bip>=100) {
        
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
          geom_polygon(data = polygons, aes(x=x,y=y, group = polygon_id, fill = pct), color = "black", show.legend = FALSE, alpha = .9) +
          geom_path (data=arc_points_if, aes(x=x,y=y), size = 1)+
          scale_fill_gradient2(low = '#38589e', high = '#c22727', midpoint = .1) +
          coord_fixed()+
          labs(title = glue("{lineup_spot}. {hitter_short} ({substr(batter_side,1,1)})")) +
          xlim(-105,105)+ylim(-35,160)+
          theme_void()+
          theme(legend.position = 'bottom',
                plot.title = element_text(hjust = .5, color = title_col, size = 8, face = 'bold'))+
          new_scale_fill() +
          geom_point(x = -80, y = 15, fill = hitter_speed, size = 3, inherit.aes = F, pch = 21)+
          annotate(geom = 'text', x = 80, y = 15, label = glue("{if_bip}"), size = 3)
        if_spray_plot
        
      } else if(between(if_bip, 70,100)) {
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
          geom_polygon(data = polygons, aes(x=x,y=y, group = polygon_id, fill = pct), color = "black", show.legend = FALSE, alpha = .9) +
          geom_path (data=arc_points_if, aes(x=x,y=y), size = 1)+
          scale_fill_gradient2(low = '#38589e', high = '#c22727', midpoint = .15) +
          coord_fixed()+
          labs(title = glue("{lineup_spot}. {hitter_short} ({substr(batter_side,1,1)})")) +
          xlim(-105,105)+ylim(-35,160)+
          theme_void()+
          theme(legend.position = 'bottom',
                plot.title = element_text(hjust = .5, color = title_col, size = 8, face = 'bold'))+
          new_scale_fill() +
          geom_point(x = -80, y = 15, fill = hitter_speed, size = 3, inherit.aes = F, pch = 21)+
          annotate(geom = 'text', x = 80, y = 15, label = glue("{if_bip}"), size = 3)        
        
        
      } else if (between(if_bip, 30,70)) {
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
          geom_polygon(data = polygons, aes(x=x,y=y, group = polygon_id, fill = pct), color = "black", show.legend = FALSE, alpha = .9) +
          geom_path (data=arc_points_if, aes(x=x,y=y), size = 1)+
          scale_fill_gradient2(low = '#38589e', high = '#c22727', midpoint = .15) +
          coord_fixed()+
          labs(title = glue("{lineup_spot}. {hitter_short} ({substr(batter_side,1,1)})")) +
          xlim(-105,105)+ylim(-35,160)+
          theme_void()+
          theme(legend.position = 'bottom',
                plot.title = element_text(hjust = .5, color = title_col, size = 8, face = 'bold'))+
          new_scale_fill()+
          geom_point(x = -80, y = 15, fill = hitter_speed, size = 3, inherit.aes = F, pch = 21)+
          annotate(geom = 'text', x = 80, y = 15, label = glue("{if_bip}"), size = 3)        
      } else if (between(if_bip,0,30)) {
        
        {
          start_x <- -95
          start_y <- 80
          mid_x <- 0.45
          mid_y <- 250
          end_x <- 95
          end_y <- 80
          num_points <- 4 # Number of points along the arc
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
          num_points <- 4 # Number of points along the arc
        }
        
        arc_points_of <- calculate_arc(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points)
        
        if_spray <- spray_point_opp_tbl %>%
          filter(!is.na(hc_x))%>%
          filter(below_arc)%>%
          filter(!is.na(Bearing))
        
        
        if_spray_pct <- if_spray %>%
          arrange(Bearing) %>%
          group_by(hit_direction_3)%>%
          summarise(bip = n()) %>%
          filter(!is.na(hit_direction_3))%>%
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
          left_join(if_spray_pct, by = c('polygon_id' = 'hit_direction_3')) %>%
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
          geom_polygon(data = polygons, aes(x=x,y=y, group = polygon_id, fill = pct), color = "black", show.legend = FALSE, alpha = .9) +
          geom_path (data=arc_points_if, aes(x=x,y=y), size = 1)+
          scale_fill_gradient2(low = '#38589e', high = '#c22727', midpoint = .25) +
          coord_fixed()+
          labs(title = glue("{lineup_spot}. {hitter_short} ({substr(batter_side,1,1)})")) +
          xlim(-105,105)+ylim(-35,160)+
          theme_void()+
          theme(legend.position = 'bottom',
                plot.title = element_text(hjust = .5, color = title_col, size = 8, face = 'bold'))+
          new_scale_fill() +
          geom_point(x = -80, y = 15, fill = hitter_speed, size = 3, inherit.aes = F, pch = 21)+
          annotate(geom = 'text', x = 80, y = 15, label = glue("{if_bip}"), size = 3)        
      }
      
      if_spray_plot
      
      
      
      if_plot_list[[i]] <- if_spray_plot
      ##
      
      
      
      
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
      
      of_bip <- sum(spray_point_opp_tbl$below_arc==F, na.rm = T)
      
      # OUTFIELD ONLY ----
      if(of_bip >= 100) {
        
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
          group_by(hit_direction_9) %>%
          summarise(bip = n(),
                    hcx = median(hc_x, na.rm = T),
                    hcy = median(hc_y, na.rm = T),
                    LA = median(Angle, na.rm = T)
          ) %>%
          filter(!is.na(hit_direction_9))%>%
          mutate(pct = bip / sum(bip, na.rm = T),
                 label = round(pct * 100,1),
                 HitType = case_when(
                   LA <= 10 ~ 'GB',
                   between(LA, 10,25) ~ 'LD',
                   between(LA, 25,40) ~ 'FB',
                   LA >= 40 ~ 'PU'
                 )
          ) 
        
        polygons_of <- read.csv('_other/pocket_spray/of_spray_9.csv') %>%
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
          geom_polygon(data = polygons_of, aes(x=x,y=y, group = polygon_id, fill = pct), color = "black", show.legend = FALSE, alpha = 1) +
          # geom_path (data=arc_points_of, aes(x=x,y=y), size = 2)+
          geom_segment(data = ballparks, aes(x = Start_x , y = Start_y, xend = End_x, yend = End_y), color= 'black', size = 1) +
          geom_mlb_stadium(stadium_ids = 'cubs',
                           stadium_transform_coords = TRUE,
                           stadium_segments = c('home_plate', 'foul_lines', 'infield_outer', 'infield_inner') ,
                           size = 1,
                           color = 'black')+
          scale_fill_gradient2(low = '#38589e', high = '#c22727', midpoint = .1)+
          labs(title = glue("{lineup_spot}. {hitter_short} ({substr(batter_side,1,1)})")) +
          ylim(80,435)+
          coord_fixed()+
          theme_void()+
          theme(legend.position = 'none',
                plot.title = element_text(hjust = .5, color = title_col, size = 8, face = 'bold'),
                plot.subtitle = element_text(hjust = .5))+
          new_scale_fill() +
          geom_point(data = of_spray_pct, aes(x = hcx, y = hcy, fill = HitType), pch = 21 , color = 'black', size = 2, stroke = 1.25)+
          scale_fill_manual(values = hit_colors)+
          annotate(geom = 'text', x = 200, y = 110, label = glue("{of_bip}"), size = 3)
        
        of_spray_plot
      } else if(between(of_bip,70,100)) {
        
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
          summarise(bip = n(),
                    hcx = median(hc_x, na.rm = T),
                    hcy = median(hc_y, na.rm = T),
                    LA = median(Angle, na.rm = T)
          ) %>%
          filter(!is.na(hit_direction_7))%>%
          mutate(pct = bip / sum(bip, na.rm = T),
                 label = round(pct * 100,1),
                 HitType = case_when(
                   LA <= 10 ~ 'GB',
                   between(LA, 10,25) ~ 'LD',
                   between(LA, 25,40) ~ 'FB',
                   LA >= 40 ~ 'PU'
                 )
          ) 
        
        polygons_of <-  read.csv('_other/pocket_spray/of_spray_7.csv') %>%
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
          geom_polygon(data = polygons_of, aes(x=x,y=y, group = polygon_id, fill = pct), color = "black", show.legend = FALSE, alpha = 1) +
          # geom_path (data=arc_points_of, aes(x=x,y=y), size = 2)+
          geom_segment(data = ballparks, aes(x = Start_x , y = Start_y, xend = End_x, yend = End_y), color= 'black', size = 1) +
          geom_mlb_stadium(stadium_ids = 'cubs',
                           stadium_transform_coords = TRUE,
                           stadium_segments = c('home_plate', 'foul_lines', 'infield_outer', 'infield_inner') ,
                           size = 1,
                           color = 'black')+
          scale_fill_gradient2(low = '#38589e', high = '#c22727', midpoint = .15)+
          labs(title = glue("{lineup_spot}. {hitter_short} ({substr(batter_side,1,1)})")) +
          coord_fixed()+
          ylim(80,435)+
          theme_void()+
          theme(legend.position = 'none',
                plot.title = element_text(hjust = .5, color = title_col, size = 8, face = 'bold'),
                plot.subtitle = element_text(hjust = .5))+
          new_scale_fill() +
          geom_point(data = of_spray_pct, aes(x = hcx, y = hcy, fill = HitType), pch = 21 , color = 'black', size = 2, stroke = 1.25)+
          scale_fill_manual(values = hit_colors)+
          annotate(geom = 'text', x = 200, y = 110, label = glue("{of_bip}"), size = 3)
        
        
        of_spray_plot
        
        
      } else if(between(of_bip, 30,70)) {
        
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
          summarise(bip = n(),
                    hcx = median(hc_x, na.rm = T),
                    hcy = median(hc_y, na.rm = T),
                    LA = median(Angle, na.rm = T)
          ) %>%
          filter(!is.na(hit_direction_5))%>%
          mutate(pct = bip / sum(bip, na.rm = T),
                 label = round(pct * 100,1),
                 HitType = case_when(
                   LA <= 10 ~ 'GB',
                   between(LA, 10,25) ~ 'LD',
                   between(LA, 25,40) ~ 'FB',
                   LA >= 40 ~ 'PU'
                 )
          ) 
        
        polygons_of <-  read.csv('_other/pocket_spray/of_spray_5.csv') %>%
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
          geom_polygon(data = polygons_of, aes(x=x,y=y, group = polygon_id, fill = pct), color = "black", show.legend = FALSE, alpha = 1) +
          # geom_path (data=arc_points_of, aes(x=x,y=y), size = 2)+
          geom_segment(data = ballparks, aes(x = Start_x , y = Start_y, xend = End_x, yend = End_y), color= 'black', size = 1) +
          geom_mlb_stadium(stadium_ids = 'cubs',
                           stadium_transform_coords = TRUE,
                           stadium_segments = c('home_plate', 'foul_lines', 'infield_outer', 'infield_inner') ,
                           size = 1,
                           color = 'black')+
          scale_fill_gradient2(low = '#38589e', high = '#c22727', midpoint = .15)+
          labs(title = glue("{lineup_spot}. {hitter_short} ({substr(batter_side,1,1)})")) +
          coord_fixed()+
          ylim(80,435)+
          theme_void()+
          theme(legend.position = 'none',
                plot.title = element_text(hjust = .5, color = title_col, size = 8, face = 'bold'),
                plot.subtitle = element_text(hjust = .5))+
          new_scale_fill() +
          geom_point(data = of_spray_pct, aes(x = hcx, y = hcy, fill = HitType), pch = 21 , color = 'black', size = 2, stroke = 1.25)+
          scale_fill_manual(values = hit_colors)+
          annotate(geom = 'text', x = 200, y = 110, label = glue("{of_bip}"), size = 3)
        
      }else if (between(of_bip,0,30)){
        
        
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
          num_points <- 4 # Number of points along the arc
        }
        
        arc_points_of <- calculate_arc(start_x, start_y, mid_x, mid_y, end_x, end_y, num_points)
        
        
        of_spray <- spray_point_opp_tbl %>%
          filter(!is.na(hc_x))%>%
          # filter(!HitType %in% c('GroundBall','GB')) %>%
          filter(below_arc==F)%>%
          filter(!is.na(Bearing))
        
        
        of_spray_pct <- of_spray %>%
          arrange(Bearing) %>%
          group_by(hit_direction_3)%>%
          summarise(bip = n(),
                    hcx = median(hc_x, na.rm = T),
                    hcy = median(hc_y, na.rm = T),
                    LA = median(Angle, na.rm = T)
          ) %>%
          filter(!is.na(hit_direction_3))%>%
          mutate(pct = bip / sum(bip, na.rm = T),
                 label = round(pct * 100,1),
                 HitType = case_when(
                   LA <= 10 ~ 'GB',
                   between(LA, 10,25) ~ 'LD',
                   between(LA, 25,40) ~ 'FB',
                   LA >= 40 ~ 'PU'
                 )
          ) 
        
        polygons_of <-  read.csv('_other/pocket_spray/of_spray_3.csv') %>%
          left_join(of_spray_pct, by = c('polygon_id' = 'hit_direction_3')) %>%
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
          geom_polygon(data = polygons_of, aes(x=x,y=y, group = polygon_id, fill = pct), color = "black", show.legend = FALSE, alpha = 1) +
          # geom_path (data=arc_points_of, aes(x=x,y=y), size = 2)+
          geom_segment(data = ballparks, aes(x = Start_x , y = Start_y, xend = End_x, yend = End_y), color= 'black', size = 1) +
          geom_mlb_stadium(stadium_ids = 'cubs',
                           stadium_transform_coords = TRUE,
                           stadium_segments = c('home_plate', 'foul_lines', 'infield_outer', 'infield_inner') ,
                           size = 1,
                           color = 'black')+
          scale_fill_gradient2(low = '#38589e', high = '#c22727', midpoint = .25)+
          labs(title = glue("{lineup_spot}. {hitter_short} ({substr(batter_side,1,1)})")) +
          coord_fixed()+
          ylim(80,435)+
          theme_void()+
          theme(legend.position = 'none',
                plot.title = element_text(hjust = .5, color = title_col, size = 8, face = 'bold'),
                plot.subtitle = element_text(hjust = .5))+
          new_scale_fill() +
          geom_point(data = of_spray_pct, aes(x = hcx, y = hcy, fill = HitType), pch = 21 , color = 'black', size = 2, stroke = 1.25)+
          scale_fill_manual(values = hit_colors)+
          annotate(geom = 'text', x = 200, y = 110, label = glue("{of_bip}"), size = 3)
        
        of_spray_plot
        
      }
      
      of_plot_list[[i]] <- of_spray_plot
      
      lineup <- rbind(lineup,
                      data.frame(lineup_spot, hitter, batter_side))
      # }
      
    }
    
    
    plot_if <<- do.call(gridExtra::grid.arrange, c(if_plot_list, ncol = 3))
    
    # ggsave("C:/Users/tdmed/Downloads/IF.jpg",
    #        plot = plot_if,
    #        width = 3.65,
    #        height = 5.75,
    #        dpi = 100)
    
    
    # plot_of <- arrangeGrob(of_plot_list[[1]], of_plot_list[[2]], of_plot_list[[3]], of_plot_list[[4]],
    #                        of_plot_list[[5]], of_plot_list[[6]], of_plot_list[[7]], of_plot_list[[8]],
    #                        of_plot_list[[9]], of_plot_list[[10]], of_plot_list[[11]],  of_plot_list[[12]], ncol = 3)
    
    
    plot_of <<- do.call(gridExtra::grid.arrange, c(of_plot_list, ncol = 3))
    
    
    plot_with_legend <<- arrangeGrob(legend, plot_of, ncol = 1, heights = c(.5, 10))
    
    # grid.draw(plot_with_legend)
    # 
    # ggsave("C:/Users/tdmed/Downloads/OF.jpg",
    #        plot = plot_with_legend,
    #        width = 3.65,
    #        height = 5.75,
    #        dpi = 100)
    
  }
}

if(getwd() == "C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations") {
  setwd("C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/")
}



# unique_hitters <- dbGetQuery(db, "select distinct Batter
# hitters <- dbGetQuery(db, "select distinct Batter
# from pitch_data
# where Batter in (
# 'Andrew Fernandez',
# 'Antonio Valdez',
# 'Blake Berry',
# 'Brandon Heidal',
# 'Craig Corliss',
# 'Danny Wuestenfeld',
# 'Drew Stengren',
# 'Ian Battipaglia',
# 'Jamey Smart',
# 'Jeissy DeLaCruz',
# 'John McGuire ',
# 'Liam McArthur',
# 'Paul Toetz',
# 'Zach Weaver'
# )
# order by Batter")$Batter

opposing_hittier_pos_cards(database = db, 
                           hitters = unique_hitters,
                           home_team_code = home_team_code_series)

params <- list(
  plot_if = plot_if,
  plot_with_legend = plot_with_legend
)

# opposing_team_code <- 'JOL'
# )
# RUN ----
suppressWarnings({
  rmarkdown::render(input = "_other/pocket_spray/pocket_spray_charts.Rmd",
                    output_file = glue::glue("C:/Users/tdmed/OneDrive/_Advance/{opposing_team_code}/{opposing_team_code}_IF_OF_pocket_spray_charts.pdf"),
                    params = params)
})
