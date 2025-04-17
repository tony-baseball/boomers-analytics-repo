# ARM ANGLE INTO SQLITE DB!!!!
{
  library(plyr)
  library(dplyr)
  library(tidyverse)
  library(stringr)
  library(discordr)
  library(emojifont)
  library(RSQLite)
  library(DBI)
  library(slackr)
  library(purrr)
  library(tonybaseball)
}


db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

rosters <- dbGetQuery(db, 'select * from rosters_') %>%
  filter(!grepl('Sudden', NAME)) %>%
  mutate(height_inches = convert_to_inches(HEIGHT),
         height_inches = ifelse(
           is.na(height_inches), mean(height_inches[grepl("P",POSITION)], na.rm = T),
           height_inches
         ),
  )



arm_angle_calc <- function(data_frame) {
  
  convert_to_inches <- function(height) {
    # Create a named vector of patterns to replace with a common separator
    replacements <- c("-" = "'", "'" = "'", "''" = "'", "\"" = "", "/" = "'", "\\?" = "'", ":" = "'", "\\;" = "'")
    
    # Replace all patterns in the height string
    height <- str_replace_all(height, replacements)
    
    # Extract feet and inches
    parts <- str_split(height, "'", simplify = TRUE)
    feet <- as.numeric(parts[, 1])
    inches <- as.numeric(parts[, 2])
    
    # Convert to total inches
    total_inches <- feet * 12 + inches
    return(total_inches)
  }
  
  arm_angle_categories <- function(df) {
    bins <- c(0, 30, 60, 90, 120, 180)
    labels <- c('Overhand', 'High Three-Quarters', 'Low Three-Quarters', 'Sidearm', 'Submarine')
    
    df <- df %>%
      mutate(
        arm_angle_type = cut(arm_angle, breaks = bins, labels = labels, right = FALSE)
      )
    
    return(df)
  }
  
  
  data_frame <- data_frame %>%
    mutate(height_inches = rosters$height_inches[match(Pitcher, rosters$NAME)],
           height_inches = ifelse(is.na(height_inches), mean(rosters$height_inches[grepl("P",rosters$POSITION)], na.rm = T)
           )
    ) %>%
    mutate(
      RelSide_in = RelSide * 12,
      RelHeight_in = RelHeight * 12,
      shoulder_pos = height_in_inches * 0.70,
      Adj = RelHeight_in - shoulder_pos,
      Opp = abs(RelSide_in),
      arm_angle_rad = atan2(Opp, Adj),
      arm_angle = arm_angle_rad * (180 / pi),
      arm_angle2 = atan2(abs(RelSide * 12), ((RelHeight * 12) - (height_in_inches * 0.70))) * (180 / pi)
    ) %>%
    select(-Opp, -arm_angle_rad) 
  
  
  
  
}

t <- dbGetQuery(db, 'select * from pitch_data')

pitch <- t %>% 
  mutate(SpinVeloRatio = SpinRate/RelSpeed, .after = SpinRate) %>%
  mutate(
    arm_angle_180 = case_when(
      PitcherThrows == 'Left' ~ 180 - arm_angle,
      PitcherThrows == 'Right' ~ 180 + arm_angle,
      T ~ arm_angle
    ),
    arm_angle_savant = case_when(
      between(arm_angle, 0,90) ~ 90 - arm_angle,
      arm_angle > 90 ~ (90 - arm_angle),
      T ~ NA
    ), 
    .after = arm_angle
  )

# dbWriteTable(db, name = 'pitch_data', value = pitch, overwrite = T)
# -----------------------------------------------------------------------
library(ggplot2)
library(plotly)
library(ggarchery)
arm_angle_categories <- function(df) {
  bins <- c(0, 30, 60, 90, 120, 180)
  labels <- c('Overhand', 'High Three-Quarters', 'Low Three-Quarters', 'Sidearm', 'Submarine')
  
  df <- df %>%
    mutate(
      arm_angle_type = cut(arm_angle, breaks = bins, labels = labels, right = FALSE)
    )
  
  return(df)
}

circleFun <- function(center = c(0, 0), radius = 24, npoints = 100) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  data.frame(
    x = center[1] + radius * cos(tt),
    y = center[2] + radius * sin(tt)
  )
}

# Generate circle points
circle <- circleFun(center = c(0, 0), radius = 24)


pitch_colors = data.frame(TaggedPitchType = c("Fastball", "Sinker", "Cutter", "Curveball", 
                                              "Slider", "Changeup", "Splitter", "Knuckleball", "Other"),
                          PitchCode = c('FB', 'SI', 'CT', 'CB', 'SL', 'CH', 'SPL', 'KN', 'OT'),
                          Color = c('red', '#a34700', 'gold', 'darkgreen', 'cornflowerblue',
                                    'violet',  'black',  'black',  'black')) %>%
  filter(PitchCode %in% p_mean$TaggedPitchType)


p_lg_mean <- dbGetQuery(db, 'select PitcherThrows, TaggedPitchType, count(*) usage, avg(InducedVertBreak) InducedVertBreak, avg(HorzBreak) HorzBreak, avg(SpinAxis) SpinAxis
           from pitch_data where SEASON = 2024 AND PitcherThrows <>"" and TaggedPitchType is not NULL and TaggedPitchType <> ""
           group by PitcherThrows, TaggedPitchType') %>%
  mutate(TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball', 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup', 'Splitter', 'Knuckleball', 'Other') ),
         TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                  Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN', Splitter = 'SPL'  ) ) %>%
  group_by(PitcherThrows) %>%
  mutate(usage = round(usage / sum(usage),3)*100) %>%
  arrange(PitcherThrows, TaggedPitchType)

pitcher <- 'Dylan Stutsman'
{
  
  p <- dbGetQuery(db, 'select * from pitch_data where Pitcher = :pitcher and SEASON = 2024',
                  params = list(pitcher = pitcher)) %>%
    filter(TaggedPitchType != '' | is.na(TaggedPitchType)) %>% 
    mutate(TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball', 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup', 'Splitter', 'Knuckleball', 'Other') ),
           TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                    Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN', Splitter = 'SPL'  ) ) 
  
  p_mean <- dbGetQuery(db, 'select * from pitch_data where Pitcher = :pitcher and SEASON = 2024',
                       params = list(pitcher = pitcher)) %>%
    filter(TaggedPitchType != '' | is.na(TaggedPitchType)) %>% 
    mutate(TaggedPitchType = factor(TaggedPitchType, levels = c('Fastball', 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup', 'Splitter', 'Knuckleball', 'Other') ),
           TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                    Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN', Splitter = 'SPL'  ) ) %>%
    group_by(Pitcher, PitcherThrows, TaggedPitchType) %>%
    summarise(across(c(InducedVertBreak, HorzBreak, SpinAxis), ~ mean(.,na.rm = T)),
              usage = n()) %>%
    mutate(usage = round(usage / sum(usage),3)*100,
           scaled_usage = (usage - min(usage)) / (max(usage) - min(usage)) * (40 - 20) + 20)
  
  
  
  p_lg <- p_lg_mean %>%
    filter(PitcherThrows %in% p_mean$PitcherThrows) %>%
    filter(TaggedPitchType %in% p_mean$TaggedPitchType)
  
  p_2 <- dbGetQuery(db, 'select * from pitch_data where Pitcher = :pitcher and SEASON = 2024',
                    params = list(pitcher = pitcher)) %>%
    group_by(Pitcher, PitcherThrows) %>%
    summarise(PitcherTeam = unique(PitcherTeam),
              height_inches = mean(height_inches, na.rm = T),
              shoulder_pos = mean(shoulder_pos, na.rm = T),
              release_pos_x = median(RelSide * 12, na.rm = T),
              release_pos_z = median(RelHeight * 12, na.rm = T),
              arm_angle = median(arm_angle, na.rm = T),
              arm_angle_180 = median(arm_angle_180, na.rm = T),
              arm_angle_savant = median(arm_angle_savant, na.rm = T)
    ) %>%
    arm_angle_categories() %>%
    mutate(relx = case_when(
      release_pos_x > 20 ~ 20,
      release_pos_z > 20 ~ 20 * (release_pos_x / (release_pos_z - shoulder_pos)),
      TRUE ~ release_pos_x
    ),
    relz = case_when(
      release_pos_x > 20 ~ 20 * ((release_pos_z - shoulder_pos) / release_pos_x),
      release_pos_z > 20 ~ 20,
      TRUE ~ release_pos_z
    ),
    arm_path = 'Arm Path'
    )
  
  dput(sort(unique(pitch$TaggedPitchType)))
  
 
  
  p_c <- pitch_colors %>%
    filter(PitchCode %in% p_mean$TaggedPitchType)
  
  p_c <- setNames(as.character(p_c$Color), p_c$PitchCode)

  p_c
  
  caption <- paste(
    'Speed:', round(p$RelSpeed,1),
    "\nSpin:", round(p$SpinRate),
    "\nAxis:", round(p$SpinAxis),
    "\nSpin Eff%:", round(p$yt_Efficiency),
    "\nDate:", p$Date,
    "\nBallpark:", p$HomeTeamCode
    
  )
  
  library(plotly)
  
  ggplotly(
    ggplot(data = p, aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
      geom_polygon(data = circle, aes(x = x, y = y), fill = "#e5f3f3", color = "#e5f3f3", inherit.aes = F) +
      # DEGREE ANNOTATION
      annotate('text', x = 26, y = 1, label = '0°', size = 3)+
      annotate('text', x = -26, y = 1, label = '0°', size = 3)+
      annotate('text', x = 0, y = 26, label = '90°', size = 3)+
      # BREAK ANNOTATION
      annotate('text', x = 10, y = -1.5, label = '12"', size = 3)+
      annotate('text', x = 22, y = -1.5, label = '24"', size = 3)+
      annotate('text', x = -4.5, y = -1.5, label = '6"', size = 3)+
      annotate('text', x = -10, y = -1.5, label = '12"', size = 3)+
      annotate('text', x = -16, y = -1.5, label = '18"', size = 3)+
      annotate('text', x = -22, y = -1.5, label = '24"', size = 3)+
      annotate('text', y = 10, x = -2, label = '12"', size = 3)+
      annotate('text', y = 22, x = -2, label = '24"', size = 3)+
      annotate('text', y = -10, x = -2, label = '12"', size = 3)+
      annotate('text', y = -22, x = -2, label = '24"', size = 3)+
      geom_path(data = data.frame(
        x = 6 * cos(seq(0, 2*pi, length.out = 100)),  y = 6 * sin(seq(0, 2*pi, length.out = 100)) ), 
        aes(x = x, y = y), linetype = "dashed", color = "gray", inherit.aes = F) +
      geom_path(data = data.frame(
        x = 12 * cos(seq(0, 2*pi, length.out = 100)), y = 12 * sin(seq(0, 2*pi, length.out = 100)) ), 
        aes(x = x, y = y), linetype = "solid", color = "gray", inherit.aes = F) +
      geom_path(data = data.frame(
        x = 18 * cos(seq(0, 2*pi, length.out = 100)),  y = 18 * sin(seq(0, 2*pi, length.out = 100)) ), 
        aes(x = x, y = y), linetype = "dashed", color = "gray", inherit.aes = F) +
      geom_path(data = data.frame(
        x = 24 * sin(seq(0, 2*pi, length.out = 100)),  y = 24 * cos(seq(0, 2*pi, length.out = 100)) ), 
        aes(x = x, y = y), linetype = "solid", color = "gray", inherit.aes = F) +
      coord_fixed()+
      geom_segment(x = 0, y = -25, xend = 0, yend = 25, linewidth = .5, color = "grey55") +
      geom_segment(x = -25, y = 0, xend = 25, yend = 0, linewidth = .2, color = "grey55") +
      geom_segment(x = 25, y = 0, xend = 25, yend = 0, linewidth = .2, color = "grey55") +
      geom_segment(y=19.91,yend=21.65, x= 11.5, xend=12.5, color='grey55')+
      geom_segment(x=19.91,xend=21.65, y= 11.5, yend=12.5, color='grey55')+
      geom_segment(y=19.91,yend=21.65, x= -11.5, xend=-12.5, color='grey55')+
      geom_segment(x=-19.91,xend=-21.65, y= 11.5, yend=12.5, color='grey55')+
      annotate('text', x = 22.5, y = 13.5, label = '30°', size = 3)+
      annotate('text', x = -22.5, y = 13.5, label = '30°', size = 3)+
      annotate('text', y = 22.5, x = 13.5, label = '60°', size = 3)+
      annotate('text', y = 22.5, x = -13.5, label = '60°', size = 3)+
      theme(legend.position = "left",
            panel.background = element_blank(),
            legend.text = element_text(size = 8),
            axis.title = element_text(size = 10),
            panel.grid = element_blank(),
            plot.title = element_text(hjust = .5),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
      ) + 
      guides(color = "none")+
      scale_size_continuous(range = c(4, 9), guide = 'none') +
      labs(color = "", fill = 'Pitch',x = "", y = "", 
           title = paste0(round(p_2$arm_angle_savant),"° ", p_2$arm_angle_type) ),
    color = ~TaggedPitchType,
    
  ) %>%
    plotly::style(hoverinfo = "none", traces = 1:29)%>%
    plotly::layout(autosize = T,
                   title = list( text = paste0("Pitch Movement and Arm Angle" ),
                                 x = 0.5,xanchor = 'center') ,showlegend = TRUE)  %>%
    # INDIVIDUAL POINTS
    add_markers(data = p,
                x = ~HorzBreak,  y = ~InducedVertBreak,  color = ~TaggedPitchType, colors = p_c,
                marker = list(symbol = "circle",  opacity = 0.7, size = 8,    
                              line = list(  color = 'black',   width = .5   )   ),
                showlegend = TRUE,  legendgroup = "group1", visible = T ,
                legendgrouptitle = list(text = "Pitches", font = list(size = 10)), hovertext = caption) %>%
    # GROUP POINTS
    plotly::add_markers(data = p_mean,
      x = ~HorzBreak,  y = ~InducedVertBreak,  color = ~TaggedPitchType, colors = p_c,
      marker = list(
        symbol = "circle-dot",  opacity = 0.5, size = p_mean$scaled_usage,
        sizeref = 0.0,   sizemode = "area",   
        line = list(color = 'black', width = 2) ),  
      showlegend = TRUE,  legendgroup = "group2", visible = "legendonly", 
      legendgrouptitle = list(text = "Pitch Avg", font = list(size = 10))
      ) %>%
    # Arm Path
    add_segments(x = 0, xend = p_2$relx, y = 0, yend = p_2$relz,
                 line = list(color = 'grey', width = 7), opacity = .5, name = 'Arm',
                 legendgroup = "group4", visible = T ) %>%
    # Release Point
    add_markers(x = p_2$relx, y = p_2$relz,
                marker = list(  symbol = "diamond",  size = 15,   color = 'orange', opacity = .7,    
                                line = list(  color = 'black',   width = 1.5   )  ),
                name = "Release",
                legendgroup = "group4", visible = T)  %>%
    # LEAGUE AVERAGE
    add_markers(data = p_lg,
                x = ~HorzBreak, y = ~InducedVertBreak,
                color = ~TaggedPitchType, colors = p_c,
                marker = list(
                  symbol = "circle-x-open", opacity = 0.5, size = 35,
                  line = list(  color = 'black',   width = 2   )
                ),
                showlegend = T,  legendgroup = "group3", visible = "legendonly",
                legendgrouptitle = list(text = "League Avg", font = list(size = 10))
    ) %>%
    layout(legend = list(itemsizing = 'constant'))  %>%
    # Arm angle caption
    add_annotations(
      x = 0,  y = 28, xref = "x", yref = "y",
      text = paste0(round(p_2$arm_angle_savant),"° Arm Angle - ", gsub("Three-Quarters", "3/4", p_2$arm_angle_type)),
      showarrow = FALSE,
      font = list(size = 11, color = "#731209", face = 'bold')
    ) %>%
    # point size caption
    add_annotations(
      x = -23,  y = -20, xref = "x", yref = "y",
      text = "Pitch Avg point size\nrelative to usage",
      showarrow = FALSE,
      font = list(size = 11, color = "#731209", face = 'bold')
    ) 
  
  
  
}
