library(tidyverse)
library(ggplot2)
library(RSQLite)

db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")
hitters <- dbGetQuery(db, 'select * from stats_hitting_player_career_batted_ball  order by ExitVelo Desc')


threshold = .9
top_percent = (1- threshold)*100
batterid <- 101157
# NITRO ----
{

  
  data <- dbGetQuery(db, glue::glue('select Season, Pitcher, PitcherThrows, Batter,BatterId, TaggedPitchType, PitchCall, PlayResult, 
                   ExitSpeed, Angle, Distance, hc_x, hc_y, PlateLocHeight, PlateLocSide, bbe, hardhit, total_bases, xWOBACON, xwOBA, xBA, PitchUID
                   from pitch_data 
                   where 
                   --Batter = "hitter"
                    SEASON = 2024 
                    and
                   BatterId = {batterid}
                   and PitchCall = "InPlay"'))
  
  hitter <- data$Batter[1]
  
  data_bbe <- data %>% filter(bbe==1) %>%
    filter(!is.na(PlateLocHeight)) %>% 
    arrange(desc(ExitSpeed))%>%
    mutate(Percentile = percent_rank(ExitSpeed) * 100, .after = ExitSpeed) 
  
  tile_95 <- quantile(data_bbe$ExitSpeed[data_bbe$bbe==1], na.rm = T, threshold)
  
  data_95 <- data_bbe %>% filter(ExitSpeed >= tile_95) %>% arrange(desc(ExitSpeed))
  
  hull_indices <- chull(data_95$PlateLocSide, data_95$PlateLocHeight)
  
  data_hull <- data_bbe[hull_indices, ] %>%
    bind_rows(data_bbe[hull_indices[1], ])
  
  data_bbe_out <- data_bbe %>%
    filter(!PitchUID %in% data_95$PitchUID)
  
  avg_ev_top_95 <- round(mean(data_95$ExitSpeed, na.rm = T),1)
  slg_top_95 <- round(sum(data_95$total_bases, na.rm = T) / nrow(data_95),3)
  ba_top_95 <- round(sum(data_95$total_bases > 1, na.rm = T) / nrow(data_95),3)
  
  ggplot() +
    geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 0.75, color = "black") +
    geom_segment(data = tonybaseball::home_plate_c_pov, aes(x=x,y=y,xend=xend,yend=yend), size = 1)+
    geom_polygon(data = data_hull, aes(-PlateLocSide, PlateLocHeight), fill = '#ff8e8e', color = "darkgrey", linewidth = 1, alpha = .5) +
    geom_point(data = data_bbe_out, aes(x=-PlateLocSide, y = PlateLocHeight), size = 3, color = 'grey', alpha = .3)+
    geom_point(data = data_95, aes(x=-PlateLocSide, y = PlateLocHeight, color = PlayResult), size = 4, alpha = .5)+
    annotate(geom = 'text', x = 0, y = 4.2, label = paste("Nitro BABIP:", ba_top_95,
                                                          "\nNitro SLGBIP:", slg_top_95,
                                                          "\nNitro EV:", avg_ev_top_95))+
    annotate(geom = 'text', x = 0, y = -.2, label = paste("Seasons:", min(data_95$SEASON),"-",max(data_95$SEASON)))+
    xlim(-2,2)+
    ylim(-.5,4.5)+
    coord_fixed()+
    theme_void()+
    labs(title = paste("Nitro Zone -", hitter),
         subtitle = glue::glue("Top {top_percent}% Exit Velo Zone"))+
    theme(plot.title = element_text(hjust = .5, face = 'bold'),
          plot.subtitle = element_text(hjust = .5))
}

# Top 10% xwOBACON ----
{
  # batterid <- 100994
  
  data <- dbGetQuery(db, glue::glue('select Season, Pitcher, PitcherThrows, Batter,BatterId, TaggedPitchType, PitchCall, PlayResult, 
                   ExitSpeed, Angle, Distance, hc_x, hc_y, PlateLocHeight, PlateLocSide, bbe, hardhit, total_bases, xWOBACON, xwOBA, xBA, PitchUID
                   from pitch_data 
                   where 
                   --Batter = "hitter"
                    SEASON = 2024 
                    and
                   BatterId = {batterid}
                   and PitchCall = "InPlay"'))
  
  hitter <- data$Batter[1]
  
  data_bbe <- data %>% filter(bbe==1) %>%
    filter(!is.na(PlateLocHeight)) %>% 
    arrange(desc(xwOBA))%>%
    mutate(Percentile = percent_rank(xwOBA) * 100, .after = xwOBA) 
  
  tile_95 <- quantile(data_bbe$xwOBA[data_bbe$bbe==1], na.rm = T, threshold)
  
  data_95 <- data_bbe %>% filter(xwOBA >= tile_95) %>% arrange(desc(xwOBA))
  
  hull_indices <- chull(data_95$PlateLocSide, data_95$PlateLocHeight)
  
  data_hull <- data_bbe[hull_indices, ] %>%
    bind_rows(data_bbe[hull_indices[1], ])
  
  data_bbe_out <- data_bbe %>%
    filter(!PitchUID %in% data_95$PitchUID)
  
  avg_ev_top_95 <- round(mean(data_95$ExitSpeed, na.rm = T),1)
  slg_top_95 <- round(sum(data_95$total_bases, na.rm = T) / nrow(data_95),3)
  ba_top_95 <- round(sum(data_95$total_bases > 1, na.rm = T) / nrow(data_95),3)
  xwobacon_top_95 <- round(sum(data_95$xwOBACON, na.rm = T) / nrow(data_95),3)
  
  ggplot() +
    geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 0.75, color = "black") +
    geom_segment(data = tonybaseball::home_plate_c_pov, aes(x=x,y=y,xend=xend,yend=yend), size = 1)+
    geom_polygon(data = data_hull, aes(-PlateLocSide, PlateLocHeight), fill = '#ff8e8e', color = "darkgrey", linewidth = 1, alpha = .5) +
    geom_point(data = data_bbe_out, aes(x=-PlateLocSide, y = PlateLocHeight), size = 3, color = 'grey', alpha = .3)+
    geom_point(data = data_95, aes(x=-PlateLocSide, y = PlateLocHeight, color = PlayResult), size = 4, alpha = .5)+
    annotate(geom = 'text', x = 0, y = 4.2, label = paste("Nitro xwOBA:", xwobacon_top_95,
                                                          "\nNitro SLGBIP:", slg_top_95,
                                                          "\nNitro EV:", avg_ev_top_95))+
    annotate(geom = 'text', x = 0, y = -.2, label = paste("Seasons:", min(data_95$SEASON),"-",max(data_95$SEASON)))+
    xlim(-2,2)+
    ylim(-.5,4.5)+
    coord_fixed()+
    theme_void()+
    labs(title = paste("Nitro Zone -", hitter),
         subtitle = glue::glue("Top {top_percent}% xwOBA Zone"))+
    theme(plot.title = element_text(hjust = .5, face = 'bold'),
          plot.subtitle = element_text(hjust = .5))
}
