library(cowplot)
unique_pitch_types <- sort(unique(pitcher_data$TaggedPitchType))


league <- dbGetQuery(db, 'select PitcherThrows, BatterSide, TaggedPitchType , count(*) Total, sum(PitchofPA=1) pitch_1, sum(Strikes=2) strikes_2 , sum(swing=1) swings, sum(whiff=1) whiffs
from pitch_data
where TaggedPitchType is not NULL AND TRIM(TaggedPitchType) <>"" AND TaggedPitchType <>"Other"  AND BatterSide <>""
group by  PitcherThrows, BatterSide, TaggedPitchType') %>%
  group_by(PitcherThrows, BatterSide) %>%
  mutate(`%` = Total / sum(Total),
         `1P%` = pitch_1 / sum(pitch_1),
         `2K%` = strikes_2 / sum(strikes_2),
         `Whiff%` = whiffs / swings ) %>%
  ungroup()%>%
  mutate(across(c(9:12), ~ round(. * 100,1)))

# 
# league_2 <- dbGetQuery(db, 'select *
# from pitch_data
# where TaggedPitchType is not NULL AND TRIM(TaggedPitchType) <>"" AND TaggedPitchType <>"Other"  AND BatterSide <>"" ') %>%
#   group_by(PitcherThrows, BatterSide, TaggedPitchType) %>%
#   summarise(Total = n(),
#             pitch_1 = sum(PitchofPA==1, na.rm = T),
#             strikes_2 = sum(Strikes==2, na.rm = T),
#             swings = sum(swing, na.rm = T),
#             whiffs = sum(whiff, na.rm = T)
#   ) %>%
#   mutate(`%` = Total / sum(Total),
#          `1P%` = pitch_1 / sum(pitch_1),
#          `2K%` = strikes_2 / sum(strikes_2),
#          `Whiff%` = whiffs / swings ) %>%
#   ungroup()%>%
#   mutate(across(c(9:12), ~ round(. * 100,1)))

p_type_colors = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                  'CT' = 'orange',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black',
                  'Fastball' = 'red', 'Curveball' = 'darkgreen', 'Sinker' = '#a34700',  'Slider'='#33A8FF',
                  'Cutter' = 'orange',  'Changeup'='violet', 'Other' = 'black', 'Splitter' = 'black', 'Knuckleball' = 'black')

p_vs_l <- pitcher_data %>%
  filter(BatterSide == 'Left')%>%
  group_by(TaggedPitchType)%>%
  mutate(Usage = n(),
         FirstPitch = sum(PitchofPA==1, na.rm = T),
         two_strike = sum(Strikes==2,na.rm = T)) %>%
  ungroup()%>%
  mutate(Usage = Usage / nrow(.),
         FirstPitch_pct = FirstPitch / sum(is_pa==1, na.rm = T),
         two_strike_pct = two_strike / sum(Strikes==2, na.rm = T),
  )

p_vs_r <- pitcher_data %>%
  filter(BatterSide == 'Right') %>%
  group_by(TaggedPitchType) %>%
  mutate(Usage = n(),
         FirstPitch = sum(PitchofPA==1, na.rm = T),
         two_strike = sum(Strikes==2,na.rm = T)) %>%
  ungroup()%>%
  mutate(Usage = Usage / nrow(.),
         FirstPitch_pct = FirstPitch / sum(is_pa==1, na.rm = T),
         two_strike_pct = two_strike / sum(Strikes==2, na.rm = T),
  )
{
  plots_v_l <- list()
  
  plots_v_r <- list()
  
  whiffs_v_l <- list()
  
  whiffs_v_r <- list()
  
  first_p_l <- list()
  
  first_p_r <- list()
  
  two_k_p_l <- list()
  
  two_k_p_r <- list()
  }
# pitch <-unique_pitch_types[3]
for (pitch in unique_pitch_types) { 
  
  data_l <- p_vs_l %>%
    filter(TaggedPitchType==pitch) %>%
    filter(!is.na(PlateLocSide),!is.na(PlateLocHeight))
  
  whiffs_l <- data_l %>%
    filter(whiff==1)
  
  swings_l <- nrow(data_l %>% filter(swing==1))
  
  first_l <- data_l %>%
    filter(TaggedPitchType==pitch) %>%
    filter(PitchofPA==1)
  
  two_k_l <-  data_l %>%
    filter(Strikes==2)
  
  color = p_type_colors[pitch][[1]]
  
  if(nrow(data_l)>3){
    p <- ggplot(data_l, aes(x=-PlateLocSide, y = PlateLocHeight))+
      # stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
      stat_density2d(aes(fill = as.numeric(..level..)), geom = "polygon", bins = 6, adjust = 1, contour = T, show.legend = F) +
      # scale_fill_gradientn(colors = c("white", "#FFA4A4", "#FF5353", "#FF0000", "#940000"), values = rescale(c(0:4))) +
      scale_fill_gradient2(mid = "white", high = color, midpoint = 0) +
      theme_void()+
      annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
      xlim(c(-2, 2)) +
      ylim(c(0, 4.5)) +
      annotate(geom = 'text', x=0,y=4.25, label = paste(round(data_l$Usage[1],3)*100,"%"))+
      coord_equal()+
      geom_segment(data =  tonybaseball::home_plate_c_pov, aes(x = x, y=y, xend=xend, yend = yend), size = 1)+
      labs(title = paste(pitch))+
      theme(legend.position = 'none',
            plot.title = element_text(hjust = .5, size = 10, color = color, face = 'bold'))
    
    p
    plots_v_l[[pitch]] <- p
    
    if(nrow(whiffs_l)>3){
      p <- ggplot(whiffs_l, aes(x=-PlateLocSide, y = PlateLocHeight))+
        stat_density2d(aes(fill = as.numeric(..level..)), geom = "polygon", bins = 6, adjust = 1, contour = T, show.legend = F) +
        scale_fill_gradient2(mid = "white", high = color, midpoint = 0) +
        theme_void()+
        annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
        xlim(c(-2, 2)) +
        ylim(c(0, 4.5)) +
        annotate(geom = 'text', x=0,y=4.25, label = paste(nrow(whiffs_l), "Whiffs ||", round(nrow(whiffs_l)/swings_l, 3)*100, "%"  ))+
        coord_equal()+
        geom_segment(data =  tonybaseball::home_plate_c_pov, aes(x = x, y=y, xend=xend, yend = yend), size = 1)+
        labs(title = paste(pitch))+
        theme(legend.position = 'none',
              plot.title = element_text(hjust = .5, size = 10, color = color, face = 'bold'))
      
      
      whiffs_v_l[[pitch]] <- p
    } else {
      p <- ggplot(whiffs_l, aes(x=-PlateLocSide, y = PlateLocHeight))+
        geom_point(aes(fill = TaggedPitchType), color = 'black', pch = 21, size = 3)+
        scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                     'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black',
                                     'Fastball' = 'red', 'Curveball' = 'darkgreen', 'Sinker' = '#a34700',  'Slider'='#33A8FF',
                                     'Cutter' = 'gold',  'Changeup'='violet', 'Other' = 'black', 'Splitter' = 'black', 'Knuckleball' = 'black')) +
        theme_void()+
        annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
        xlim(c(-2, 2)) +
        ylim(c(0, 4.5)) +
        annotate(geom = 'text', x=0,y=4.25, label = paste(nrow(whiffs_l), "Whiffs ||", round(nrow(whiffs_l)/swings_l, 3)*100, "%"  ))+
        coord_equal()+
        geom_segment(data =  tonybaseball::home_plate_c_pov, aes(x = x, y=y, xend=xend, yend = yend), size = 1)+
        labs(title = paste(pitch))+
        theme(legend.position = 'none',
              plot.title = element_text(hjust = .5, size = 10, color = color, face = 'bold'))
      
      
      whiffs_v_l[[pitch]] <- p
    }
    
    if(nrow(first_l)>3){
      p <- ggplot(first_l, aes(x=-PlateLocSide, y = PlateLocHeight))+
        # stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
        stat_density2d(aes(fill = as.numeric(..level..)), geom = "polygon", bins = 6, adjust = 1, contour = T, show.legend = F) +
        # scale_fill_gradientn(colors = c("white", "#FFA4A4", "#FF5353", "#FF0000", "#940000"), values = rescale(c(0:4))) +
        scale_fill_gradient2(mid = "white", high = color, midpoint = 0) +
        theme_void()+
        annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
        xlim(c(-2, 2)) +
        ylim(c(0, 4.5)) +
        annotate(geom = 'text', x=0,y=4.25, label = paste(round(first_l$FirstPitch_pct[1],3)*100,"%"))+
        coord_equal()+
        geom_segment(data =  tonybaseball::home_plate_c_pov, aes(x = x, y=y, xend=xend, yend = yend), size = 1)+
        labs(title = paste(pitch))+
        theme(legend.position = 'none',
              plot.title = element_text(hjust = .5, size = 10, color = color, face = 'bold'))
      
      p
      first_p_l[[pitch]] <- p
    } else {
      p <- ggplot(first_l, aes(x=-PlateLocSide, y = PlateLocHeight))+
        geom_point(aes(fill = TaggedPitchType), color = 'black', pch = 21, size = 3)+
        scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                     'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black',
                                     'Fastball' = 'red', 'Curveball' = 'darkgreen', 'Sinker' = '#a34700',  'Slider'='#33A8FF',
                                     'Cutter' = 'gold',  'Changeup'='violet', 'Other' = 'black', 'Splitter' = 'black', 'Knuckleball' = 'black')) +
        theme_void()+
        annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
        xlim(c(-2, 2)) +
        ylim(c(0, 4.5)) +
        annotate(geom = 'text', x=0,y=4.25, label = paste(round(first_l$FirstPitch_pct[1],3)*100,"%"))+
        coord_equal()+
        geom_segment(data =  tonybaseball::home_plate_c_pov, aes(x = x, y=y, xend=xend, yend = yend), size = 1)+
        labs(title = paste(pitch))+
        theme(legend.position = 'none',
              plot.title = element_text(hjust = .5, size = 10, color = color, face = 'bold'))
      
      
      first_p_l[[pitch]] <- p
    }
    
    if(nrow(two_k_l)>3){
      p <- ggplot(two_k_l, aes(x=-PlateLocSide, y = PlateLocHeight))+
        # stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
        stat_density2d(aes(fill = as.numeric(..level..)), geom = "polygon", bins = 6, adjust = 1, contour = T, show.legend = F) +
        # scale_fill_gradientn(colors = c("white", "#FFA4A4", "#FF5353", "#FF0000", "#940000"), values = rescale(c(0:4))) +
        scale_fill_gradient2(mid = "white", high = color, midpoint = 0) +
        theme_void()+
        annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
        xlim(c(-2, 2)) +
        ylim(c(0, 4.5)) +
        annotate(geom = 'text', x=0,y=4.25, label = paste(round(two_k_l$two_strike_pct[1],3)*100,"%"))+
        coord_equal()+
        geom_segment(data =  tonybaseball::home_plate_c_pov, aes(x = x, y=y, xend=xend, yend = yend), size = 1)+
        labs(title = paste(pitch))+
        theme(legend.position = 'none',
              plot.title = element_text(hjust = .5, size = 10, color = color, face = 'bold'))
      
      p
      two_k_p_l[[pitch]] <- p
    } else {
      p <- ggplot(two_k_l, aes(x=-PlateLocSide, y = PlateLocHeight))+
        geom_point(aes(fill = TaggedPitchType), color = 'black', pch = 21, size = 3)+
        scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                     'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black',
                                     'Fastball' = 'red', 'Curveball' = 'darkgreen', 'Sinker' = '#a34700',  'Slider'='#33A8FF',
                                     'Cutter' = 'gold',  'Changeup'='violet', 'Other' = 'black', 'Splitter' = 'black', 'Knuckleball' = 'black')) +
        theme_void()+
        annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
        xlim(c(-2, 2)) +
        ylim(c(0, 4.5)) +
        annotate(geom = 'text', x=0,y=4.25, label = paste(round(two_k_l$two_strike_pct[1],3)*100,"%"))+
        coord_equal()+
        geom_segment(data =  tonybaseball::home_plate_c_pov, aes(x = x, y=y, xend=xend, yend = yend), size = 1)+
        labs(title = paste(pitch))+
        theme(legend.position = 'none',
              plot.title = element_text(hjust = .5, size = 10, color = color, face = 'bold'))
      
      
      two_k_p_l[[pitch]] <- p
    }
    
    p
    
    
  } else {
    p <- ggplot(data_l, aes(x=-PlateLocSide, y = PlateLocHeight))+
      geom_point(aes(fill = TaggedPitchType), color = 'black', pch = 21, size = 3)+
      scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                   'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black',
                                   'Fastball' = 'red', 'Curveball' = 'darkgreen', 'Sinker' = '#a34700',  'Slider'='#33A8FF',
                                   'Cutter' = 'gold',  'Changeup'='violet', 'Other' = 'black', 'Splitter' = 'black', 'Knuckleball' = 'black')) +
      theme_void()+
      annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
      xlim(c(-2, 2)) +
      ylim(c(0, 4.5)) +
      annotate(geom = 'text', x=0,y=4.25, label = paste(round(data_l$Usage[1],3)*100,"%"))+
      coord_equal()+
      geom_segment(data =  tonybaseball::home_plate_c_pov, aes(x = x, y=y, xend=xend, yend = yend), size = 1)+
      labs(title = paste(pitch))+
      theme(legend.position = 'none',
            plot.title = element_text(hjust = .5, size = 10, color = color, face = 'bold'))
    
    
    plots_v_l[[pitch]] <- p
    
  }
  
  data_r <- p_vs_r %>%
    filter(TaggedPitchType==pitch) %>%
    filter(!is.na(PlateLocSide),!is.na(PlateLocHeight))
  
  whiffs_r <- data_r %>%
    filter(whiff==1)
  
  swings_r <- nrow(data_r %>%
                     filter(swing==1))
  
  
  first_r <- p_vs_r %>%
    filter(TaggedPitchType==pitch)%>%
    filter(PitchofPA==1)
  
  two_k_r <-  data_r %>%
    filter(Strikes==2)
  
  
  if(nrow(data_r)>5){
    p <- ggplot(data_r, aes(x=-PlateLocSide, y = PlateLocHeight))+
      # stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
      stat_density2d(aes(fill = as.numeric(..level..)), geom = "polygon", bins = 6, adjust = 1, contour = T, show.legend = F) +
      # scale_fill_gradientn(colors = c("white", "#FFA4A4", "#FF5353", "#FF0000", "#940000"), values = rescale(c(0:4))) +
      scale_fill_gradient2(mid = "white", high = color, midpoint = 0) +
      theme_void()+
      annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
      xlim(c(-2, 2)) +
      ylim(c(0, 4.5)) +
      annotate(geom = 'text', x=0,y=4.25, label = paste(round(data_r$Usage[1],3)*100,"%"))+
      coord_equal()+
      geom_segment(data =  tonybaseball::home_plate_c_pov, aes(x = x, y=y, xend=xend, yend = yend), size = 1)+
      labs(title = paste(pitch))+
      
      theme(legend.position = 'none',
            plot.title = element_text(hjust = .5, size = 10, color = color, face = 'bold'))
    
    
    plots_v_r[[pitch]] <- p
    
    
    if(nrow(whiffs_r)>3){
      p <- ggplot(whiffs_r, aes(x=-PlateLocSide, y = PlateLocHeight))+
        stat_density2d(aes(fill = as.numeric(..level..)), geom = "polygon", bins = 6, adjust = 1, contour = T, show.legend = F) +
        scale_fill_gradient2(mid = "white", high = color, midpoint = 0) +
        theme_void()+
        annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
        xlim(c(-2, 2)) +
        ylim(c(0, 4.5)) +
        annotate(geom = 'text', x=0,y=4.25, label = paste(nrow(whiffs_r), "Whiffs ||", round(nrow(whiffs_r)/swings_r, 3)*100, "%"  ))+
        coord_equal()+
        geom_segment(data =  tonybaseball::home_plate_c_pov, aes(x = x, y=y, xend=xend, yend = yend), size = 1)+
        labs(title = paste(pitch))+
        theme(legend.position = 'none',
              plot.title = element_text(hjust = .5, size = 10, color = color, face = 'bold'))
      
      p
      whiffs_v_r[[pitch]] <- p
    } else {
      p <- ggplot(whiffs_r, aes(x=-PlateLocSide, y = PlateLocHeight))+
        geom_point(aes(fill = TaggedPitchType), color = 'black', pch = 21, size = 3)+
        scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                     'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black',
                                     'Fastball' = 'red', 'Curveball' = 'darkgreen', 'Sinker' = '#a34700',  'Slider'='#33A8FF',
                                     'Cutter' = 'gold',  'Changeup'='violet', 'Other' = 'black', 'Splitter' = 'black', 'Knuckleball' = 'black')) +
        theme_void()+
        annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
        xlim(c(-2, 2)) +
        ylim(c(0, 4.5)) +
        annotate(geom = 'text', x=0,y=4.25, label = paste(nrow(whiffs_r), "Whiffs ||", round(nrow(whiffs_r)/swings_r, 3)*100, "%"  ))+
        coord_equal()+
        geom_segment(data =  tonybaseball::home_plate_c_pov, aes(x = x, y=y, xend=xend, yend = yend), size = 1)+
        labs(title = paste(pitch))+
        theme(legend.position = 'none',
              plot.title = element_text(hjust = .5, size = 10, color = color, face = 'bold'))
      
      
      whiffs_v_r[[pitch]] <- p
    }
    if(nrow(first_r)>3){
      p <- ggplot(first_r, aes(x=-PlateLocSide, y = PlateLocHeight))+
        # stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
        stat_density2d(aes(fill = as.numeric(..level..)), geom = "polygon", bins = 6, adjust = 1, contour = T, show.legend = F) +
        # scale_fill_gradientn(colors = c("white", "#FFA4A4", "#FF5353", "#FF0000", "#940000"), values = rescale(c(0:4))) +
        scale_fill_gradient2(mid = "white", high = color, midpoint = 0) +
        theme_void()+
        annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
        xlim(c(-2, 2)) +
        ylim(c(0, 4.5)) +
        annotate(geom = 'text', x=0,y=4.25, label = paste(round(first_r$FirstPitch_pct[1],3)*100,"%"))+
        coord_equal()+
        geom_segment(data =  tonybaseball::home_plate_c_pov, aes(x = x, y=y, xend=xend, yend = yend), size = 1)+
        labs(title = paste(pitch))+
        theme(legend.position = 'none',
              plot.title = element_text(hjust = .5, size = 10, color = color, face = 'bold'))
      
      p
      first_p_r[[pitch]] <- p
    } else {
      p <- ggplot(first_r, aes(x=-PlateLocSide, y = PlateLocHeight))+
        geom_point(aes(fill = TaggedPitchType), color = 'black', pch = 21, size = 3)+
        scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                     'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black',
                                     'Fastball' = 'red', 'Curveball' = 'darkgreen', 'Sinker' = '#a34700',  'Slider'='#33A8FF',
                                     'Cutter' = 'gold',  'Changeup'='violet', 'Other' = 'black', 'Splitter' = 'black', 'Knuckleball' = 'black')) +
        theme_void()+
        annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
        xlim(c(-2, 2)) +
        ylim(c(0, 4.5)) +
        annotate(geom = 'text', x=0,y=4.25, label = paste(round(first_r$FirstPitch_pct[1],3)*100,"%"))+
        coord_equal()+
        geom_segment(data =  tonybaseball::home_plate_c_pov, aes(x = x, y=y, xend=xend, yend = yend), size = 1)+
        labs(title = paste(pitch))+
        theme(legend.position = 'none',
              plot.title = element_text(hjust = .5, size = 10, color = color, face = 'bold'))
      
      
      first_p_r[[pitch]] <- p
    }
    
    if(nrow(two_k_r)>3){
      
      p <- ggplot(two_k_r, aes(x=-PlateLocSide, y = PlateLocHeight))+
        # stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
        stat_density2d(aes(fill = as.numeric(..level..)), geom = "polygon", bins = 6, adjust = 1, contour = T, show.legend = F) +
        # scale_fill_gradientn(colors = c("white", "#FFA4A4", "#FF5353", "#FF0000", "#940000"), values = rescale(c(0:4))) +
        scale_fill_gradient2(mid = "white", high = color, midpoint = 0) +
        theme_void()+
        annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
        xlim(c(-2, 2)) +
        ylim(c(0, 4.5)) +
        annotate(geom = 'text', x=0,y=4.25, label = paste(round(two_k_r$two_strike_pct[1],3)*100,"%"))+
        coord_equal()+
        geom_segment(data =  tonybaseball::home_plate_c_pov, aes(x = x, y=y, xend=xend, yend = yend), size = 1)+
        labs(title = paste(pitch))+
        theme(legend.position = 'none',
              plot.title = element_text(hjust = .5, size = 10, color = color, face = 'bold'))
      
      p
      two_k_p_r[[pitch]] <- p
    } else {
      p <- ggplot(two_k_r, aes(x=-PlateLocSide, y = PlateLocHeight))+
        geom_point(aes(fill = TaggedPitchType), color = 'black', pch = 21, size = 3)+
        scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                     'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black',
                                     'Fastball' = 'red', 'Curveball' = 'darkgreen', 'Sinker' = '#a34700',  'Slider'='#33A8FF',
                                     'Cutter' = 'gold',  'Changeup'='violet', 'Other' = 'black', 'Splitter' = 'black', 'Knuckleball' = 'black')) +
        theme_void()+
        annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
        xlim(c(-2, 2)) +
        ylim(c(0, 4.5)) +
        annotate(geom = 'text', x=0,y=4.25, label = paste(round(two_k_r$two_strike_pct[1],3)*100,"%"))+
        coord_equal()+
        geom_segment(data =  tonybaseball::home_plate_c_pov, aes(x = x, y=y, xend=xend, yend = yend), size = 1)+
        labs(title = paste(pitch))+
        theme(legend.position = 'none',
              plot.title = element_text(hjust = .5, size = 10, color = color, face = 'bold'))
      
      
      two_k_p_r[[pitch]] <- p
    }
    
    
  } else {
    
    p <- ggplot(data_r, aes(x=-PlateLocSide, y = PlateLocHeight))+
      geom_point(aes(fill = TaggedPitchType), color = 'black', pch = 21, size = 3)+
      scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                   'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black',
                                   'Fastball' = 'red', 'Curveball' = 'darkgreen', 'Sinker' = '#a34700',  'Slider'='#33A8FF',
                                   'Cutter' = 'gold',  'Changeup'='violet', 'Other' = 'black', 'Splitter' = 'black', 'Knuckleball' = 'black')) +
      theme_void()+
      annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
      xlim(c(-2, 2)) +
      ylim(c(0, 4.5)) +
      annotate(geom = 'text', x=0,y=4.25, label = paste(round(data_r$Usage[1],3)*100,"%"))+
      coord_equal()+
      geom_segment(data =  tonybaseball::home_plate_c_pov, aes(x = x, y=y, xend=xend, yend = yend), size = 1)+
      labs(title = paste(pitch))+
      theme(legend.position = 'none',
            plot.title = element_text(hjust = .5, size = 10, color = color, face = 'bold'))
    
    
    plots_v_r[[pitch]] <- p
  }
  
}

# do.call(plot_grid, c(plots_v_l, ncol = length(plots_v_l)))
# 
# do.call(plot_grid, c(plots_v_r, ncol = length(plots_v_r)))

pitch_plot_r <- plot_grid(
  ggdraw() + draw_label("All Pitches vs RHH", fontface = 'bold', size = 12, hjust = 0.5), 
  do.call(plot_grid, c(plots_v_r, ncol = length(plots_v_r))), 
  ncol = 1, 
  rel_heights = c(0.1, 1)
)

pitch_plot_l <- plot_grid(
  ggdraw() + draw_label("All Pitches vs LHH", fontface = 'bold', size = 12, hjust = 0.5), 
  do.call(plot_grid, c(plots_v_l, ncol = length(plots_v_l))), 
  ncol = 1, 
  rel_heights = c(0.1, 1)
)

whiff_plot_l <- plot_grid(
  ggdraw() + draw_label("Whiffs vs LHH", fontface = 'bold', size = 12, hjust = 0.5), 
  do.call(plot_grid, c(whiffs_v_l, ncol = length(whiffs_v_l))), 
  ncol = 1, 
  rel_heights = c(0.1, 1)
)

whiff_plot_r <- plot_grid(
  ggdraw() + draw_label("Whiffs vs RHH", fontface = 'bold', size = 12, hjust = 0.5), 
  do.call(plot_grid, c(whiffs_v_r, ncol = length(whiffs_v_r))), 
  ncol = 1, 
  rel_heights = c(0.1, 1)
)

first_plot_r <- plot_grid(
  ggdraw() + draw_label("First Pitch vs RHH", fontface = 'bold', size = 12, hjust = 0.5), 
  do.call(plot_grid, c(first_p_r, ncol = length(first_p_r))), 
  ncol = 1, 
  rel_heights = c(0.1, 1)
)

first_plot_l <- plot_grid(
  ggdraw() + draw_label("First Pitch vs LHH", fontface = 'bold', size = 12, hjust = 0.5), 
  do.call(plot_grid, c(first_p_l, ncol = length(first_p_l))), 
  ncol = 1, 
  rel_heights = c(0.1, 1)
)

two_k_plot_l <- plot_grid(
  ggdraw() + draw_label("2 Strikes vs LHH", fontface = 'bold', size = 12, hjust = 0.5), 
  do.call(plot_grid, c(two_k_p_l, ncol = length(two_k_p_l))), 
  ncol = 1, 
  rel_heights = c(0.1, 1)
)

two_k_plot_r <- plot_grid(
  ggdraw() + draw_label("2 Strikes vs RHH", fontface = 'bold', size = 12, hjust = 0.5), 
  do.call(plot_grid, c(two_k_p_r, ncol = length(two_k_p_r))), 
  ncol = 1, 
  rel_heights = c(0.1, 1)
)


lg_p_vs_lhh <- league %>%
  filter(TaggedPitchType %in% unique_pitch_types) %>%
  filter(PitcherThrows == ifelse(pitcher_hand=='LHP', 'Left', 'Right'),
         BatterSide =='Left') %>%
  mutate(PitcherThrows = str_replace_all(PitcherThrows, c('Left' = 'LHP', 'Right' = 'RHP')),
         BatterSide = str_replace_all(BatterSide, c('Left' = 'LHH', 'Right' = 'RHH')),
         Split = paste("League",PitcherThrows,'vs',BatterSide),
         TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball','Sinker','Cutter','Curveball','Slider','Changeup','Splitter','Knuckleball','Other'))
  ) %>%
  select(-c(1,2,5:8)) %>%
  relocate(Split, .before = 1) %>%
  arrange(TaggedPitchType)

lg_p_vs_rhh <- league %>%
  filter(TaggedPitchType %in% unique_pitch_types) %>%
  filter(PitcherThrows == ifelse(pitcher_hand=='LHP', 'Left', 'Right'),
         BatterSide =='Right') %>%
  mutate(PitcherThrows = str_replace_all(PitcherThrows, c('Left' = 'LHP', 'Right' = 'RHP')),
         BatterSide = str_replace_all(BatterSide, c('Left' = 'LHH', 'Right' = 'RHH')),
         Split = paste("League",PitcherThrows,'vs',BatterSide),
         TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball','Sinker','Cutter','Curveball','Slider','Changeup','Splitter','Knuckleball','Other'))
         
  ) %>%
  select(-c(1,2,5:8)) %>%
  relocate(Split, .before = 1) %>%
  arrange(TaggedPitchType)

params <- list(
  pitcher = pitcher,
  pitcher_hand = pitcher_hand,
  pitch_plot_r = pitch_plot_r,
  pitch_plot_l = pitch_plot_l,
  whiff_plot_r = whiff_plot_r,
  whiff_plot_l = whiff_plot_l,
  first_plot_r = first_plot_r,
  first_plot_l = first_plot_l,
  two_k_plot_l = two_k_plot_l,
  two_k_plot_r = two_k_plot_r,
  team_logo = team_location,
  lg_p_vs_lhh = lg_p_vs_lhh,
  lg_p_vs_rhh = lg_p_vs_rhh
)

# suppressWarnings({
#   rmarkdown::render(input = "C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/pitcher_usage.Rmd",
#                     output_file = paste0("C:/Users/tdmed/OneDrive/R_Markdown/test",".pdf"),
#                     params = params)
# })

suppressWarnings({
  rmarkdown::render(input = "C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/adv_pitcher_usage.Rmd",
                    output_file = paste0("C:/Users/tdmed/OneDrive/R_Markdown/pitcher_reports/",team_code,"/", pitcher, "_usage_report",".pdf"),
                    params = params)
})


sequencing <- pitcher_data %>%
  group_by(GameID, Inning, PAofInning, Batter) %>%
  mutate(PreviousPitch = lag(TaggedPitchType),
         NextPitch = lead(TaggedPitchType),
         ThirdPitch = lead(NextPitch)) %>%
  ungroup() %>%
  select(Date, Inning, PAofInning, Batter, BatterSide, TaggedPitchType,PreviousPitch , NextPitch,ThirdPitch)

seq <- sequencing%>%
  group_by(BatterSide, TaggedPitchType, NextPitch) %>%
  summarise(n = n())%>%
  ungroup() %>%
  filter(!is.na(NextPitch))%>%
  group_by(BatterSide,TaggedPitchType)%>%
  mutate(pct = n / sum(n) ) %>%
  
  group_by(BatterSide, TaggedPitchType) %>%
  mutate(
    Total = sum(n), # Calculate the total sum of n
    Combined = paste0(NextPitch, " (", round(pct*100, 0), "%)") # Combine NextPitch and pct
  ) %>%
  arrange(BatterSide, TaggedPitchType, desc(n)) %>% # Arrange in descending order of n
  mutate(Rank = row_number()) %>% # Rank pitches based on n
  filter(Rank <= 3) %>% # Keep only the top 3 pitches
  select(BatterSide, TaggedPitchType, Total, Rank, Combined) %>% # Select relevant columns
  pivot_wider(names_from = Rank, values_from = Combined, names_prefix = "Next_") %>% # Reshape data
  ungroup()
