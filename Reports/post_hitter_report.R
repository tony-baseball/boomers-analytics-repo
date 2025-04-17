library(GeomMLBStadiums)
library(ggrepel)
library(patchwork)
# 
# boomers_games <- dbGetQuery(db, 'select distinct Date, GameID from pitch_data where HomeTeamCode = "BOOM" OR AwayTeamCode = "BOOM"')
# gm_id <- boomers_games$GameID[nrow(boomers_games)]
# 
# 
# # tm <- dbGetQuery(db, glue::glue('select * from pitch_data where GameID = "{gm_id}" and BatterTeam like "%Boom%"'))
# # tm <- dbGetQuery(db, glue::glue('select * from pitch_data where BatterTeam like "%Boom%"'))
# tm %>%
#   group_by(Batter) %>%
#   summarise(n())
# batter <- 'Alec Craig'
# --------
boomers_hitters <- sort(unique(yakker_day$Batter[yakker_day$BatterTeam=='Schaumburg Boomers']))

# COMMENT OUT----
# boomers_hitters <- boomers_hitters[3]

for(name in boomers_hitters) { # LOOP START ----
  batter_name <- name
  
  ## PLAYER FILTER ----
  hitter_data <- yakker_day %>%
    filter(Batter == batter_name)%>%
    tonybsbl::pitch_types_factor() %>%
    tonybsbl::pitch_types_recode() %>%
    group_by(Batter) %>%
    mutate(PA = cumsum(is_pa==1)+1, .before = 1,
           PA = ifelse(is_pa==1, PA-1,PA)) %>%
    ungroup()
  
  # sort(unique(colnames(hitter_data)))
  
  # %>% arrange(desc(Date) )%>%
  #   group_by(Batter,PitchCall,PlayResult)%>%
  #   distinct(PitchCall, PlayResult, .keep_all = TRUE)
  
  ## GAME LINE ----
  game_line <-
    hitter_data %>%
    group_by(Batter) %>%
    summarise(PA = sum(is_pa, na.rm = T),
              AB = sum(is_ab, na.rm = T),
              H = sum(is_hit, na.rm = T),
              # `1B` = sum(total_bases==1,na.rm = T),
              `2B` = sum(total_bases==2,na.rm = T),
              `3B` = sum(total_bases==3,na.rm = T),
              `HR` = sum(total_bases==4,na.rm = T),
              TB = sum(total_bases, na.rm = T),
              RBI = sum(RunsScored, na.rm = T),
              BB = sum(PlayResult=='Walk'),
              HBP = sum(grepl('HitByPitch',PlayResult)),
              SO = sum(grepl('Strikeout',PlayResult)),
              EVMax = round(max(ExitSpeed[bbe==1], na.rm = T),1),
              EVAvg = round(mean(ExitSpeed[bbe==1], na.rm = T),1)
              
    ) %>%
    ungroup() %>%
    select(-Batter)
  
  ## PA ----
  pa <-
    hitter_data %>%
    filter(is_pa==1) %>%
    dplyr::select(Inning,Pitcher,PitcherThrows,PitchCall,PlayResult,HitType=TaggedHitType, Runs=RunsScored, Pitch = TaggedPitchType, PitchSpeed=RelSpeed, EV= ExitSpeed, LA=Angle, Distance) %>%
    mutate(`PA #` = row_number(), .before = 1) %>%
    mutate(across(c('PitchSpeed', 'EV', 'LA', 'Distance'), ~ round(.,1))) %>%
    # mutate( Result = str_replace_all(Result, c('Single'='1B', 'Double' = '2B', 'Triple' = '3B', 'HomeRun' ='HR', 'Error' = 'E', 'FieldersChoice' = 'FC','CatchersInterference' = 'CI', 'Walk' = 'BB',
    #                                            'StrikeoutLooking' = 'SO-Strike','StrikeoutSwinging' = 'SO-Whiff','Strikeout' = 'SO', 'Sacrifice' = 'SAC')),) %>%
    mutate(PitchCall = str_replace_all(PitchCall, c('StrikeCalled' = 'Strike', 'BallCalled' = 'Ball', 'StrikeSwinging' = 'Whiff', 'HitByPitch' = 'HBP', 'CatchersInterference' = 'CI')),
           HitType = str_replace_all(HitType, c('Ball' = '', 'Drive' = '', 'Up' = '')),
           # Pitch = str_replace_all(Pitch, c('Fastball' = 'FB', 'Sinker' = 'SI', 'Cutter' = 'CT', 'Curveball' = 'CB', 'Slider' = 'SL', 'Changeup' = 'CH', 'Splitter' = 'SPL')),
           PlayResult = str_replace_all(PlayResult, c('Single'='1B', 'Double' = '2B', 'Triple' = '3B', 'HomeRun' ='HR', 'Error' = 'E', 'FieldersChoice' = 'FC','CatchersInterference' = 'CI', 'Walk' = 'BB',
                                                      'StrikeoutSwinging|Strikeout|StrikeoutLooking' = 'SO', 'Sacrifice' = 'SAC')),
           across(c(EV, LA, Distance), ~ round(.,1)),
           Result = str_trim(str_squish(paste(PitchCall,PlayResult))),
           Result = ifelse(Result =='Ball BB', 'BB', Result),
           Result = ifelse(Result =='Ball HitByPitch', 'HBP', Result),
           Result = ifelse(Result =='HBP HitByPitch', 'HBP', Result),
           Result = ifelse(Result =='Strike SOLooking', 'SO-Strike', Result),
           # Result = ifelse(Result =='SOLooking', 'SO-Strike', Result),
           Result = ifelse(Result =='Whiff SO', 'SO-Whiff', Result),
           Result = gsub("InPlay ", "", Result),
           Pitcher = glue::glue("{Pitcher} ({PitcherThrows})"),
           Result = ifelse(!is.na(HitType), glue::glue("{HitType} {Result}"), Result)
    ) %>%
    select(-c(PitcherThrows, PitchCall,PlayResult,HitType)) %>%
    relocate(Result, .after = Pitcher)
  
  
  
  ## BATTED BALLS ----
  batted_balls <-
    hitter_data %>%
    filter(is_pa==1) %>%
    mutate(`PA#` = row_number(), .before = 1) %>%
    filter(PitchCall=='InPlay') %>%
    dplyr::select(`PA#`,Inning,Pitcher,Result = PlayResult,HitType, Runs = RunsScored, Pitch = TaggedPitchType, EV= ExitSpeed, LA=Angle, Distance, hc_x,hc_y, HomeTeamCode) %>%
    mutate(HitType = str_replace_all(HitType, c('GroundBall' = 'GB', 'LineDrive' = 'LD', 'FlyBall' = 'FB', 'PopUp' = 'PU', 'Bunt' = 'BU')),
           Pitch = str_replace_all(Pitch, c('Fastball' = 'FB', 'Sinker' = 'SI', 'Cutter' = 'CT', 'Curveball' = 'CB', 'Slider' = 'SL', 'Changeup' = 'CH', 'Splitter' = 'SPL')),
           Result = str_replace_all(Result, c('Single'='1B', 'Double' = '2B', 'Triple' = '3B', 'HomeRun' ='HR', 'Error' = 'E', 'FieldersChoice' = 'FC','CatchersInterference' = 'CI')),
           across(c(EV, LA, Distance), ~ round(.,1))
    )
  
  batted_balls
  htc <- unique(batted_balls$HomeTeamCode)
  
  of_wall <- dbGetQuery(db, glue::glue('select * from ballpark_dimensions where HomeTeamCode = "{htc}" '))
  
  
  ## BATTED BALLS PLOT ----
  batted_balls_plot <- ggplot()+
    geom_mlb_stadium(stadium_ids = 'cubs',
                     stadium_transform_coords = TRUE,
                     stadium_segments = c('home_plate', 'foul_lines', 'infield_outer', 'infield_inner') ,
                     size = 1,
                     color = 'black')+
    
    # sportyR::geom_baseball(league = 'mlb',display_range = 'full')+
    #   geom_segment(aes(x=0,y=0,xend=-259.630564,yend = 242), size = .5, color = 'white')+
    #   geom_segment(aes(x=0,y=0,xend=258.167857,yend = 240), size = .5, color = 'white')+
    # # IN PLAY
    # '1B' = '#ff4a65', '2B' = '#785EF0', '3B' = '#FFB000',  'HR'='forestgreen',
    # 'E' = 'purple',  'FC'='lightgrey', 'SAC' = 'darkgrey',
    # # OUT 
    # 'Out' = 'black', 'SO-Strike' = 'black', 'SO-Whiff' = 'black',
    # # BALL
    # 'Ball' = 'cornflowerblue', 'BB' = 'cornflowerblue', 'HBP' = 'cornflowerblue', 
  # # STRIKE
  # 'Strike' = 'darkred', 'Whiff' = 'darkred', 'Foul' = 'darkred'))+
  # 
    geom_segment(data = of_wall, aes(x = Start_x,y=Start_y, xend = End_x, yend = End_y), size = 1, color = 'black')+
    labs(title = 'Batted Balls', subtitle = paste(unique(of_wall$Team), 'Field'))+
    geom_label_repel(data = batted_balls, aes(x = hc_x, y = hc_y, label = paste("PA",`PA#`) ), size = 3)+
    geom_point(data = batted_balls, aes(x = hc_x, y = hc_y, fill = Result), pch = 21, size = 5, color = 'black')+
    scale_fill_manual(values = c('1B' = '#ff4a65', '2B' = '#785EF0', '3B' = '#FFB000',  'HR'='forestgreen', 'Out' = 'black', 'E' = 'purple',  'FC'='lightgrey', 'SAC' = 'darkgrey')) +
    xlim(-275,275)+
    ylim(-35,450)+
    coord_equal()+
    theme_void()+
    theme(plot.title = element_text(hjust = .5, face = 'bold', color = 'black'),
          plot.subtitle = element_text(hjust = .5, face = 'bold', color = 'black'))
  batted_balls_plot
  
  
  ## ZONE DATA ----
  zone_data <- hitter_data %>%
    # filter(PitchCall=='InPlay') %>%
    dplyr::select(is_pa,PitchofPA,Inning,Pitcher,PitcherThrows,PitchCall,PlayResult,HitType, Runs = RunsScored, Pitch = TaggedPitchType, EV= ExitSpeed, LA=Angle, Distance, PlateLocSide, PlateLocHeight, HomeTeamCode) %>%
    mutate(PitchCall = str_replace_all(PitchCall, c('StrikeCalled' = 'Strike', 'BallCalled' = 'Ball', 'StrikeSwinging' = 'Whiff', 'HitByPitch' = 'HBP', 'CatchersInterference' = 'CI')),
           HitType = str_replace_all(HitType, c('GroundBall' = 'GB', 'LineDrive' = 'LD', 'FlyBall' = 'FB', 'PopUp' = 'PU', 'Bunt' = 'BU')),
           # Pitch = str_replace_all(Pitch, c('Fastball' = 'FB', 'Sinker' = 'SI', 'Cutter' = 'CT', 'Curveball' = 'CB', 'Slider' = 'SL', 'Changeup' = 'CH', 'Splitter' = 'SPL')),
           PlayResult = str_replace_all(PlayResult, c('Single'='1B', 'Double' = '2B', 'Triple' = '3B', 'HomeRun' ='HR', 'Error' = 'E', 'FieldersChoice' = 'FC','CatchersInterference' = 'CI', 'Walk' = 'BB',
                                                      'StrikeoutSwinging|Strikeout|StrikeoutLooking' = 'SO', 'Sacrifice' = 'SAC')),
           across(c(EV, LA, Distance), ~ round(.,1)),
           Result = str_trim(str_squish(paste(PitchCall,PlayResult))),
           Result = ifelse(Result =='Ball BB', 'BB', Result),
           Result = ifelse(Result =='Ball HitByPitch', 'HBP', Result),
           Result = ifelse(Result =='HBP HitByPitch', 'HBP', Result),
           Result = ifelse(Result =='Strike SOLooking', 'SO-Strike', Result),
           Result = ifelse(Result =='Whiff SO', 'SO-Whiff', Result),
           Result = gsub("InPlay ", "", Result)
    ) %>%
    filter(PitchCall !='' ) %>%
    filter(Result !='InPlay')%>%
    mutate(`PA#` = cumsum(PitchofPA==1), .before = 1) %>%
    mutate(facet = glue::glue("{`PA#`} - {sub('^\\\\S+\\\\s+', '', Pitcher)} ({PitcherThrows})"))
  
  table(zone_data$Result)
  
  
  ## PITCHES PLOT ----
  pitches_plot <- ggplot(zone_data %>%
                           mutate(Result = factor(Result, levels = c('1B','2B','3B','HR','E','FC','SAC','Ball','BB','HBP','Strike','Whiff','Foul','Out','SO-Strike','SO-Whiff'))), aes(x=-PlateLocSide,y=PlateLocHeight))+
    geom_segment(data=tonybaseball::home_plate_c_pov,aes( x=x,y=y,xend=xend,yend=yend), size = 1)+
    annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
    xlim(-2,2)+
    ylim(0,4)+
    coord_equal()+
    geom_point(aes(fill = Result, shape = Result),  size = 4)  + 
    scale_shape_manual(values = c( "1B" = 21, "2B" = 21, "3B" = 21, 'HR'=21, 'E' = 21, 'FC'=21, 'SAC' = 21 ,
                                   'Ball' = 23, 'BB' = 22, 'HBP' = 24 , 
                                   'Strike' = 23, 'Whiff' = 22, 'Foul' = 24, 
                                   'Out' = 21,  'SO-Strike' = 23, 'SO-Whiff' = 22
    )  ) +
    scale_fill_manual(values = c(
      # IN PLAY
      '1B' = '#ff4a65', '2B' = '#785EF0', '3B' = '#FFB000',  'HR'='forestgreen',
      'E' = 'purple',  'FC'='lightgrey', 'SAC' = 'darkgrey',
      # OUT 
      'Out' = 'black', 'SO-Strike' = 'black', 'SO-Whiff' = 'black',
      # BALL
      'Ball' = 'cornflowerblue', 'BB' = 'cornflowerblue', 'HBP' = 'cornflowerblue', 
      # STRIKE
      'Strike' = 'darkred', 'Whiff' = 'darkred', 'Foul' = 'darkred'))+
    theme_void()+
    geom_text(aes(label = PitchofPA), color = 'white', fontface = 'bold', size = 3)+
    # facet_wrap(~PitcherThrows)+
    # facet_wrap(~facet, ncol = 1)+
    facet_wrap(~facet, nrow = 1)+
    theme(
      strip.background = element_rect(fill = "#12294b", color = "#12294b"),
      strip.text = element_text(color = "white", size = 10),
      legend.position = 'bottom',
      legend.direction = "horizontal"
    )+ 
    guides(fill = guide_legend(nrow = 1),shape = guide_legend(nrow = 1))
  
  pitches_plot
  
  
  ## PITCH TYPE PLOT ----
  pitch_type_plot <- ggplot(zone_data, aes(x=-PlateLocSide,y=PlateLocHeight))+
    geom_segment(data=tonybaseball::home_plate_c_pov,aes( x=x,y=y,xend=xend,yend=yend), size = 1)+
    annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
    xlim(-2,2)+
    ylim(0,4)+
    coord_equal()+
    geom_point(aes(fill = Pitch), pch = 21, size = 4)+
    scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                 'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black',
                                 'Fastball' = 'red', 'Curveball' = 'darkgreen', 'Sinker' = '#a34700',  'Slider'='#33A8FF',
                                 'Cutter' = 'gold',  'Changeup'='violet', 'Other' = 'black', 'Splitter' = 'black', 'Knuckleball' = 'black')) +
    theme_void()+
    geom_text(aes(label = PitchofPA), color = 'white', fontface = 'bold', size = 3)+
    # facet_wrap(~PitcherThrows)+
    facet_wrap(~facet, nrow = 1)+
    theme(
      strip.background = element_rect(fill = "#12294b", color = "#12294b"),
      strip.text = element_text(color = "white", size = 10),
      legend.position = 'bottom',
      legend.direction = "horizontal"
    )+ 
    guides(fill = guide_legend(nrow = 1))
  
  pitch_type_plot
  
  
  
  pa_zone_list <- list()
  pa_bb_list <- list()
  pa_flex_plot_list <- list()
  ## PA TRACKER PLOT LISTS 
  for (i in unique(hitter_data$PA)) {


    pa_track <- hitter_data %>%
      filter(PA==i) %>%
      mutate(Count = glue::glue("{Balls}-{Strikes}"),
             caption = glue::glue("Inning {Inning} - PA {PA} vs. {sub('^\\\\S+\\\\s+', '', Pitcher)} ({PitcherThrows})")) %>%
      dplyr::select(`#`=PitchofPA, Inning, Pitcher, PitcherThrows, Pitch = TaggedPitchType,Count, Outs, PitchCall, PlayResult, Result, EV = ExitSpeed, LA = Angle, Dist = Distance, PlateLocSide, PlateLocHeight, hc_x, hc_y,caption) %>%
      mutate(Result = str_replace_all(Result, c('Single'='1B', 'Double' = '2B', 'Triple' = '3B', 'HomeRun' ='HR', 'Error' = 'E', 'FieldersChoice' = 'FC','CatchersInterference' = 'CI', 'Sacrifice' = 'SAC')),
             PlayResult = str_replace_all(PlayResult, c('Single'='1B', 'Double' = '2B', 'Triple' = '3B', 'HomeRun' ='HR', 'Error' = 'E', 'FieldersChoice' = 'FC','CatchersInterference' = 'CI', 'Sacrifice' = 'SAC')),
      )

    pa_flex <- pa_track %>%
      dplyr::select(-c( PlateLocSide, PlateLocHeight, hc_x, hc_y, Inning, Pitcher, PitcherThrows,caption, PlayResult, PitchCall)) %>%
      mutate(across(c(EV, LA, Dist), ~ round(.,0)))

    caption <- tail(pa_track$caption,1)

    pa_flex_plot_list[[as.character(i)]] <- flextable(pa_flex, cwidth = 'auto') %>%
      add_header_row(values = caption, colwidths = ncol(pa_flex)) %>%
      # set_table_properties(., width = .5, layout = "autofit") %>%
      autofit() %>%
      align(align = 'justify', part = 'all') %>%
      bg(bg = "#12294b", part = "header") %>%
      color(color = "white", part = "header") %>%
      bg(i = ~ Pitch == 'FB', j = c('Pitch', '#'), bg = "red") %>%
      bg(i = ~ Pitch == 'SI', j = c('Pitch', '#'), bg = "orange") %>%
      bg(i = ~ Pitch == 'CT', j = c('Pitch', '#'), bg = "#FFD700") %>%
      bg(i = ~ Pitch == 'CB', j = c('Pitch', '#'), bg = "#228b22") %>%
      bg(i = ~ Pitch == 'SL', j = c('Pitch', '#'), bg = "#33A8FF") %>%
      bg(i = ~ Pitch == 'CH', j = c('Pitch', '#'), bg = "#E398F7") %>%
      bg(i = ~ Pitch == 'SPL', j = c('Pitch', '#'), bg = "black") %>%
      bg(i = ~ Pitch == 'KN', j = c('Pitch', '#'), bg = "black") %>%
      bg(i = ~ Pitch == 'KN', j = c('Pitch', '#'), bg = "black") %>%
      bg(i = ~ grepl('1B|Single',Result), j = ~ Result, bg = "#ff4a65") %>%
      bg(i = ~ grepl('2B|Double',Result), j = ~ Result, bg = "#785EF0") %>%
      bg(i = ~ grepl('3B|Triple',Result), j = ~ Result, bg = "#FFB000") %>%
      bg(i = ~ grepl('HR|HomeRun',Result), j = ~ Result, bg = "forestgreen") %>%
      bg(i = ~ grepl('E',Result), j = ~ Result, bg = "purple") %>%
      bg(i = ~ grepl('FC',Result), j = ~ Result, bg = "lightgrey") %>%
      bg(i = ~ grepl('SAC',Result), j = ~ Result, bg = "darkgrey") %>%
      bg(i = ~ grepl('Out|K',Result), j = ~ Result, bg = "black") %>%
      color(j = c('#','Pitch'), color = 'white', part = 'body') %>%
      color(i = ~ grepl('1B|2B|3B|HR|E|Out|K|SAC|Single|Double|Triple|HomeRun',Result), j = c('Result'), color = 'white', part = 'body') %>%
      fontsize(size = 10, part = "all") %>%
      bold(bold = TRUE, part = "all")

    # # IN PLAY
    # '1B' = '#ff4a65', '2B' = '#785EF0', '3B' = '#FFB000',  'HR'='forestgreen',
    # 'E' = 'purple',  'FC'='lightgrey', 'SAC' = 'darkgrey',
    # # OUT
    # 'Out' = 'black', 'SO-Strike' = 'black', 'SO-Whiff' = 'black',
    # # BALL
    # 'Ball' = 'cornflowerblue', 'BB' = 'cornflowerblue', 'HBP' = 'cornflowerblue',
    # # STRIKE
    # 'Strike' = 'darkred', 'Whiff' = 'darkred', 'Foul' = 'darkred'))+
    #

    pa_zone_list[[as.character(i)]] <-
      pa_zone <- ggplot(pa_track, aes(x=-PlateLocSide,y=PlateLocHeight))+
      geom_segment(data=tonybaseball::home_plate_c_pov,aes( x=x,y=y,xend=xend,yend=yend), size = 1)+
      annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.0001, linewidth = 1)+
      xlim(-2,2)+
      ylim(0,4)+
      coord_equal()+
      geom_point(aes(fill = Pitch), pch = 21, size = 4)+
      scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                   'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black',
                                   'Fastball' = 'red', 'Curveball' = 'darkgreen', 'Sinker' = '#a34700',  'Slider'='#33A8FF',
                                   'Cutter' = 'gold',  'Changeup'='violet', 'Other' = 'black', 'Splitter' = 'black', 'Knuckleball' = 'black')) +
      theme_void()+
      geom_text(aes(label = `#`), color = 'white', fontface = 'bold', size = 3)+
      theme(
        strip.background = element_rect(fill = "#12294b", color = "#12294b"),
        strip.text = element_text(color = "white", size = 10),
        legend.position = 'none',
        legend.direction = "horizontal"
      )

    pa_bb_list[[as.character(i)]] <-ggplot()+
      geom_mlb_stadium(stadium_ids = 'cubs',
                       stadium_transform_coords = TRUE,
                       stadium_segments = c('home_plate', 'foul_lines', 'infield_outer', 'infield_inner') ,
                       size = 1,
                       color = 'black')+

      geom_segment(data = of_wall, aes(x = Start_x,y=Start_y, xend = End_x, yend = End_y), size = 1, color = 'black')+
      geom_point(data = pa_track %>% filter(PitchCall == 'InPlay'), aes(x = hc_x, y = hc_y, fill = PlayResult), pch = 21, size = 5, color = 'black')+
      scale_fill_manual(values = c('1B' = '#ff4a65', '2B' = '#785EF0', '3B' = '#FFB000',  'HR'='forestgreen', 'Out' = 'black',
                                   'E' = 'purple',  'FC'='lightgrey', 'SAC' = 'darkgrey')) +
      xlim(-275,275)+
      ylim(-35,450)+
      coord_equal()+
      theme_void()+
      theme(plot.title = element_text(hjust = .5, face = 'bold', color = 'black'),
            plot.subtitle = element_text(hjust = .5, face = 'bold', color = 'black'),
            legend.position = 'none')
  }
  

  # PLOT GRID ----

  # combined_layout <- NULL
  # 
  # # Loop through each set of plots and stack them
  # for (i in seq_along(pa_zone_list)) {
  #   row <- (
  #     wrap_elements(full = gen_grob(pa_flex_plot_list[[i]], fit = "fixed", just = "centre", autowidths = T)) +
  #       pa_zone_list[[i]] + pa_bb_list[[i]] +
  #       plot_layout(widths = c(.6, .3, .3))
  #     
  #   )
  # 
  #   # Combine rows using `/` operator
  #   if (is.null(combined_layout)) {
  #     combined_layout <- row
  #   } else {
  #     combined_layout <- combined_layout / row
  #   }
  # }
  # 
  # cl <- combined_layout
  # 
  # cl
  
  # params ----
  params_boom = list(
    game_line = game_line,
    pa = pa,
    batted_balls = batted_balls,
    batted_balls_plot = batted_balls_plot,
    pitch_type_plot = pitch_type_plot,
    pitches_plot = pitches_plot,
    batter_name = batter_name,
    opponent_team = unique(hitter_data$PitcherTeam),
    team_logo = team_location,
    game_date = max_b_date,
    date_of_game = file_date #,
    # pa_plot = pa_plot,
    # cl = cl
  )
  
  
  file_name <- paste(gsub(" ", "_", batter_name))
  date_of_game <- paste(file_date)
  rmd_file <-  "C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/post_HitterReport2.Rmd"
  # rmd_file <-  "C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/PostgameHitterReport2_html.rmd"
  file_save <- paste0("C:/Users/tdmed/OneDrive/_Advance/Boomers/H/", max_b_date,"_", file_name ,"_hitting_report.pdf")
  # file_save <- paste0("C:/Users/tdmed/OneDrive/_Advance/Boomers/H/", max_b_date,"_", file_name ,"_report.html")
  rmd_file
  file_save
  
  suppressWarnings({ # RUN----
    rmarkdown::render(input = rmd_file, 
                      output_file = file_save,
                      params = params_boom)
  })
  #
  Sys.sleep(2)
}

logs <- list.files("C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/", pattern = '.log', full.names = T)
for (log in logs){
  file.rename(log,
              to = paste0('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/logs/',basename(log)) )
}
