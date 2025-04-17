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
    library(RSQLite)
    library(pdftools)
    library(ggh4x)
    library(gridExtra)
    
  }))

#source('C:/Users/tdmed/OneDrive/R_Codes/Yakkertech Combine to Master.R')

file.remove(list.files(paste0("C:/Users/tdmed/OneDrive/R_Markdown/pitcher_reports/",opposing_team_code,"/"), pattern = '.pdf', full.names = T))

print("Loading CSVs...")
# db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

team_info <- # read.csv("C:/Users/tdmed/OneDrive/_FLASH/frontier team info.csv")
  dbGetQuery(db, 'SELECT * FROM teams')

all_pitchers <- #read.csv("C:/Users/tdmed/OneDrive/_FLASH/frontier_all_pitchers23.csv") %>%
  dbGetQuery(db, 'SELECT * FROM stats_pitching_player where SEASON = 2024') 

rosters<- # read.csv("C:/Users/tdmed/OneDrive/_FLASH/front_rosters24.csv")%>%
  dbGetQuery(db, 'SELECT * FROM rosters where SEASON = 2024')

print("CSVs loaded!")

{
  #---------
  
  # team_filter <- 'Schaumburg Boomers'
  
  team_code <- team_info$bats_team_code[team_info$team_FL == team_filter]
  
  team_location <- gsub(" |-","",team_info$Location[team_info$bats_team_code == team_code])
  
  folder_path <- paste0("C:/Users/tdmed/OneDrive/R_Markdown/pitcher_reports/", team_code)
  
  if (!file.exists(folder_path)) { dir.create(folder_path) }
  
  # ------------------------
  yakker <- dbGetQuery(db, 
                       #         paste0(
                       #           'SELECT *
                       # FROM pitch_data
                       # where PitcherTeam = "',team_filter,'"'
                       #         ),
                       glue::glue(
                         'SELECT * FROM pitch_data WHERE PitcherTeam = "{team_filter}" and SEASON -- >= 2024' 
                       )
  ) %>%
    filter(TaggedPitchType !='', 
           BatterSide != '',
           TaggedPitchType !='NA',
           !is.na(TaggedPitchType))  %>%
    mutate(Pitcher = gsub('A.husson', "Aaron Husson", Pitcher))
  
  unique(yakker$TaggedPitchType)
  unique(yakker$PitcherTeam)
  unique(yakker$Pitcher)
  
  sort(unique(yakker$Date))
  # set a factor to manually order the pitch types
  yakker$TaggedPitchType <- factor(yakker$TaggedPitchType, levels = c("Fastball", "Sinker", "Cutter","Curveball", "Slider", "Changeup", "Splitter", 'Knuckleball', 'Other'))
  
  
  team_roster <- rosters %>%
    filter(TEAM == team_filter,
           grepl("P", POSITION),
           STATUS != 'Inactive')
  
  roster_pitchers <- sort(unique(team_roster$NAME)) 
  roster_pitchers
  pitchers <- sort(unique(yakker$Pitcher)) 
  pitchers
  
  intersect(roster_pitchers, pitchers)
  
  team_p <- team_roster %>% filter(NAME %in% pitchers)
  
  unique_pitchers <- sort(unique(team_p$NAME))
  # unique_pitchers <- unique_pitchers[3]
  
  
  # LOOP ---------
  
  # For each pitcher in the dataset, fun through the following code
  for (pitcher in unique_pitchers) {
    suppressMessages({
      # Filter the data for the current pitcher
      pitcher_data <<- yakker[yakker$Pitcher == pitcher, ]
      
      season_stats <- all_pitchers %>%
        filter(Player == pitcher) %>%
        dplyr::select(SEASON, Player, G, GS, CG, SV, IP, ERA, R, ER, H, SO, BB, `SO%` = SO_pct, `BB%` = BB_pct, WHIP, BAA) %>%
        mutate(across(c(`SO%`, `BB%`), ~ round(.*100,1)))
      # Generate the game summary / pitch characteristics table ----
      
      season_summary_table <- 
        pitcher_data %>%
        dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                Cutter = 'CT', Splitter = 'SPL', Knuckleball = 'KN', Changeup = 'CH', Other = 'OT' ) ) %>%
        group_by('Pitch' = TaggedPitchType) %>%
        dplyr::summarize('No.' = n(),
                         'Usage' = n(),
                         '%' = n(),
                         'Velo' = round(mean(RelSpeed, na.rm = TRUE),1),
                         'VeloMax' = round(max(RelSpeed, na.rm = TRUE),1),
                         'IVB' = round(mean(InducedVertBreak, na.rm = TRUE),1),
                         'HB' = round(mean(HorzBreak, na.rm = TRUE),1), 'Tilt' = round(mean(SpinAxis, na.rm = TRUE),0),
                         'Time' = sapply(`Tilt`, function(x) if (is.na(x)){return(NA)}
                                         else if(x > 180 & x <= 360){(x/30)-6}
                                         else if(x == 180){12}
                                         else{(x/30)+6}),
                         'HH' = as.integer(Time),
                         'HH' = sapply(HH, function(x) if (is.na(x)){return(NA)}
                                       else if(x == 0){x+12}
                                       else if(x > 12){x-12}
                                       else{x+0}),
                         "MM" = formatC(round((Time%%1)*60, digits = 0), width = 2, flag = "0"),
                         'Tilt' = paste0(HH,":", MM),
                         'Spin' = round(mean(SpinRate, na.rm = TRUE),0),
                         'SpinEff%' = round(mean(yt_Efficiency, na.rm= TRUE),0),
                         'Arm°' = round(median(arm_angle_savant, na.rm = T)),
                         'VAA' = round(mean(VertApprAngle, na.rm = TRUE),1),    
                         'RelHt' = round(mean(RelHeight, na.rm = TRUE),1),
                         'RelSide' = round(mean(RelSide, na.rm = TRUE),1),
                         'Ext' = round(mean(Extension, na.rm = TRUE),1),
                         'Stuff+' = round(median(tj_stuff_plus, na.rm = T))
        ) %>%
        mutate(`%` = round(`%`/sum(`%`),3)*100) %>%
        dplyr::select(-Usage,-Time,-HH,-MM)
      
      season_summary_table
      # Generate the pitch usage table ----
      pitch_usage_table <- pitcher_data  %>%
        dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) )%>%
        group_by('Pitch' = TaggedPitchType) %>%
        dplyr::summarize('#' = n(),
                         'Use%' = n(),
                         "1P" = sum(PitchofPA == 1, na.rm = TRUE),
                         "1P%" = sum(`1P`, na.rm = TRUE),
                         '2K' = sum(Strikes == 2, na.rm = TRUE),
                         '2K%' = sum(`2K`, na.rm = TRUE),
                         'Strk%' = round(sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay", "StrikeCalled"), na.rm = TRUE)/n(),3)*100,
                         'Whiff%' = round(sum(PitchCall %in% c("StrikeSwinging"), na.rm = TRUE)/
                                            sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay"), na.rm = TRUE),3)*100 
        ) %>%
        mutate(`Use%` = round(`Use%`/sum(`Use%`),3)*100,
               `1P%` = round(`1P%`/sum(`1P%`),3)*100,
               `2K%` = round(`2K%`/sum(`2K%`),3)*100) %>% 
        dplyr::select(-`1P`, -`2K`) 
      
      
      
      #Generate the game stat table ----
      game_stats <-
        pitcher_data  %>%
        dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) )%>%
        #   group_by(Date) %>%
        summarise('IP' = round( (sum(OutsOnPlay, na.rm = TRUE))/3, 1),
                  'BF' = n_distinct(Inning, Batter, PAofInning),
                  'K' = sum(KorBB =="Strikeout"),
                  'BB' = sum(KorBB =="Walk"),
                  'HBP' = sum(PlayResult == 'HitByPitch'),
                  'BIP' = sum(PitchCall == 'InPlay') ,
                  'H' = sum(PlayResult %in% c('Single','Double','Triple','HomeRun')),
                  'XBH' = sum(PlayResult %in% c('Double','Triple','HomeRun')),
                  'R' = sum(RunsScored, na.rm = TRUE)
        )
      
      opponent <- game_stats$BatterTeam[1]
      
      
      # PITCH USAGE VS RHH ----
      usage_r <- pitcher_data  %>%
        dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Knuckleball = 'KN', Other = 'OT' ) )%>%
        filter(BatterSide == 'Right') %>%
        group_by('Pitch' = TaggedPitchType) %>%
        dplyr::summarize('No.' = n(),
                         'Usage %' = n(),
                         "1P" = sum(PitchofPA == 1),
                         "1P%" = sum(`1P`),
                         '2K' = sum(Strikes == 2),
                         '2K%' = sum(`2K`),
                         'Strike %' = round(sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay", "StrikeCalled"))/n(),3)*100,
                         'Whiff %' = round(
                           sum(whiff==1, na.rm = T)/
                             sum(swing==1, na.rm = T),3)*100 
        ) %>%
        mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100,
               `1P%` = round(`1P%`/sum(`1P%`),3)*100,
               `2K%` = round(`2K%`/sum(`2K%`),3)*100) %>% 
        dplyr::select(-`1P`, -`2K`)
      # Stats vs RHH ----
      stats_vs_r <- pitcher_data  %>%
        dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                Cutter = 'CT', Changeup = 'CH',  Splitter = 'SPL', Knuckleball = 'KN', Other = 'OT' ) )%>%
        filter(BatterSide=='Right')%>%
        dplyr::summarise('BF' = n_distinct(Inning, Batter, PAofInning),
                         'K' = sum(KorBB =="Strikeout"),
                         'BB' = sum(KorBB =="Walk"),
                         'HBP' = sum(PlayResult == 'HitByPitch'),
                         'H' = sum(PlayResult %in% c('Single','Double','Triple','HomeRun')),
                         'R' = sum(RunsScored, na.rm = TRUE),
                         '1B' = sum(PlayResult=='Single'),
                         '2B' = sum(PlayResult=='Double'),
                         '3B' = sum(PlayResult=='Triple'),
                         'HR' = sum(PlayResult=='HomeRun'),
                         'AVG' = round(H / (BF-BB-HBP),3),
                         'SLG' = round( sum( (`1B`*1)+(`2B`*2) + (`3B`*3) + (HR*4)   ) / (BF-BB-HBP)       ,3)   ) %>%
        dplyr::select(BF, K, BB, HBP, H, R,HR, AVG, SLG)
      
      # USAGE VS LHH ----
      usage_l <- pitcher_data  %>%
        dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Knuckleball = 'KN', Other = 'OT' ) )%>%
        filter(BatterSide == 'Left') %>%
        group_by('Pitch' = TaggedPitchType) %>%
        dplyr::summarize('No.' = n(),
                         'Usage %' = n(),
                         "1P" = sum(PitchofPA == 1),
                         "1P%" = sum(`1P`),
                         '2K' = sum(Strikes == 2),
                         '2K%' = sum(`2K`),
                         'Strike %' = round(sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay", "StrikeCalled"))/n(),3)*100,
                         'Whiff %' = round(
                           sum(whiff==1, na.rm = T)/
                             sum(swing==1, na.rm = T),3)*100 
        ) %>%
        mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100,
               `1P%` = round(`1P%`/sum(`1P%`),3)*100,
               `2K%` = round(`2K%`/sum(`2K%`),3)*100) %>% 
        dplyr::select(-`1P`, -`2K`)
      usage_l
      # STATS VS LHH ----
      stats_vs_l <- pitcher_data  %>%
        dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                Cutter = 'CT', Changeup = 'CH',  Splitter = 'SPL', Knuckleball = 'KN',Other = 'OT' ) )%>%
        filter(BatterSide=='Left')%>%
        dplyr::summarise('BF' = n_distinct(Inning, Batter, PAofInning),
                         'K' = sum(KorBB =="Strikeout"),
                         'BB' = sum(KorBB =="Walk"),
                         'HBP' = sum(PlayResult == 'HitByPitch'),
                         'H' = sum(PlayResult %in% c('Single','Double','Triple','HomeRun')),
                         'R' = sum(RunsScored, na.rm = TRUE),
                         '1B' = sum(PlayResult=='Single'),
                         '2B' = sum(PlayResult=='Double'),
                         '3B' = sum(PlayResult=='Triple'),
                         'HR' = sum(PlayResult=='HomeRun'),
                         'AVG' = round(H / (BF-BB-HBP),3),
                         'SLG' = round( sum( (`1B`*1)+(`2B`*2) + (`3B`*3) + (HR*4)   ) / (BF-BB-HBP)       ,3)   ) %>%
        dplyr::select(BF, K, BB, HBP, H, R,HR, AVG, SLG)#%>%
      #mutate('Whiff %' = `Whiff`/`Swings`) %>%
      #dplyr::select(-`Whiff`, -`Swings`)
      
      
      # BATTED BALL DATA ----
      batted_ball <- pitcher_data  %>%
        dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                Cutter = 'CT', Changeup = 'CH',  Splitter = 'SPL', Knuckleball = 'KN',Other = 'OT' ) )%>%
        group_by('Pitch' = TaggedPitchType) %>%
        dplyr::summarize('No.' = n(),
                         'BIP' = sum(PitchCall == 'InPlay'),
                         'XBH' = sum(PlayResult %in% c("Double","Triple","HomeRun")),
                         'Avg EV' = round(mean(ExitSpeed, na.rm= TRUE),0),
                         'EV 90+' = sum(ExitSpeed >= 90, na.rm= TRUE),
                         'EV -90' = sum(ExitSpeed < 90, na.rm= TRUE) 
        )
      
      
      # pitch movement plot ----
      
      p_2 <- pitcher_data %>%
        group_by(Pitcher, PitcherThrows) %>%
        dplyr::summarise(PitcherTeam = unique(PitcherTeam),
                         height_inches = mean(height_inches, na.rm = T),
                         shoulder_pos = mean(shoulder_pos, na.rm = T),
                         release_pos_x = median(RelSide * 12, na.rm = T),
                         release_pos_z = median(RelHeight * 12, na.rm = T),
                         arm_angle = median(arm_angle, na.rm = T),
                         arm_angle_180 = median(arm_angle_180, na.rm = T),
                         arm_angle_savant = median(arm_angle_savant, na.rm = T)
        ) %>%
        # tonybaseball::arm_angle_categories() %>%
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
        arm_path = 'Arm Path',
        # relx = ifelse(PitcherThrows %in% c('Right','RHP'), relx, ifelse(PitcherThrows %in% c('Left','LHP'), -relx, relx))
        )
      
      p_med_arm_angle <<-p_2$arm_angle_savant
      
      
      pitch_movement_plot <<- if(is.na(p_2$arm_angle_savant)) {
        ggplot(data =
                 pitcher_data %>%
                 dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                         Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Kunckleball = 'KN' ) ),
               aes(x = HorzBreak, y = InducedVertBreak)) +
          labs(title = "Pitch Movement", color = "",x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)" )  +
          xlim(-25, 25) + ylim(-25, 25) +
          geom_segment(aes(x = 0, y = -25, xend = 0, yend = 25), size = 1, color = "grey55") +
          geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), size = 1, color = "grey55") +
          coord_fixed()+
          geom_point(aes(fill = TaggedPitchType), size =3, alpha = .75, color= 'black', pch = 21) +
          # we manually set the pitch colors so that they are uniform across each plot and tables
          scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                        'CT' = 'orange',  'CH'='violet', 'SPL' = 'black', 'KN' = 'black', 'OT' = 'black')) +
          scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                       'CT' = 'orange',  'CH'='violet', 'SPL' = 'black', 'KN' = 'black', 'OT' = 'black')) +
          theme_bw() + 
          theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
                # plot.subtitle = element_text(size = 8, face = "bold", hjust = 0.5),
                legend.position = "none", 
                legend.text = element_text(size = 8), 
                axis.title = element_blank()) # axis.title = element_text(size = 6))
        
      } else {
        
        similar_arm_angle <<- dbGetQuery(db,
                                         glue::glue('SELECT * FROM pitch_data
                 WHERE PitcherThrows = "{ifelse(pitcher_data$PitcherThrows[1]=="LHP","Left","Right")}"
                 AND arm_angle_savant BETWEEN {p_med_arm_angle} - 5 and {p_med_arm_angle} + 5
                 AND SEASON >= 2023')
        ) %>%
          filter(TaggedPitchType %in% pitcher_data$TaggedPitchType) %>%
          dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                  Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Knuckleball = 'KN' ) )
        
        
        ggplot(data =
                 pitcher_data %>%
                 dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                         Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Kunckleball = 'KN' ) ),
               aes(x = HorzBreak, y = InducedVertBreak)) +
          labs(title = "Pitch Movement", color = "",x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)" )  +
          xlim(-25, 25) + ylim(-25, 25) +
          geom_segment(aes(x = 0, y = -25, xend = 0, yend = 25), size = 1, color = "grey55") +
          geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), size = 1, color = "grey55") +
          coord_fixed()+
          geom_segment(x=0, xend= p_2$relx, y= 0, yend=p_2$relz, color='grey55', linetype = 2)+
          # stat_ellipse(data = similar_arm_angle, geom = "polygon", alpha = 0.1,
          #              aes(x = HorzBreak, y = InducedVertBreak, fill = TaggedPitchType, color = TaggedPitchType, group = TaggedPitchType))  +
          geom_point(aes(fill = TaggedPitchType), size =3, alpha = .75, color= 'black', pch = 21) +
          scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                        'CT' = 'orange',  'CH'='violet', 'SPL' = 'black', 'KN' = 'black', 'OT' = 'black')) +
          scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                       'CT' = 'orange',  'CH'='violet', 'SPL' = 'black', 'KN' = 'black', 'OT' = 'black')) +
          theme_bw() + 
          theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
                legend.position = "none", 
                legend.text = element_text(size = 8), 
                axis.title = element_blank()) # axis.title = element_text(size = 6))
        
        
        
      }
      
      if(!is.na(p_2$arm_angle_savant)  & pitcher_data$PitcherThrows[1] %in% c('Right', 'RHP')) {
        
        pitch_movement_plot <- pitch_movement_plot +
          annotate('text', x = -12.5, y = 15, label= paste0(round(p_2$arm_angle_savant),"° Arm Angle\n"#, gsub("Three-Quarters","3/4",p_2$arm_angle_type)
          ))
        
      } else if (!is.na(p_2$arm_angle_savant) & pitcher_data$PitcherThrows[1] %in% c('Left', 'LHP')) {
        pitch_movement_plot <- pitch_movement_plot +
          annotate('text', x = 12.5, y = 15, label= paste0(round(p_2$arm_angle_savant),"° Arm Angle\n"#, gsub("Three-Quarters","3/4",p_2$arm_angle_type)
          ))
      }
      
      ggplotly(pitch_movement_plot)
      
      
      # Pitch velo table and plot ----
      pvp_game <- pitcher_data %>%
        group_by(Pitcher, TaggedPitchType, Inning) %>%
        summarise(Avg = mean(RelSpeed, na.rm = TRUE), Max = max(RelSpeed, na.rm = T), min = min(RelSpeed, na.rm = T)) %>%
        arrange(Inning, desc(Max)) %>%
        dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN'  ) )
      length(unique(pvp_game$Inning))
      
      pvp_game_plot <- 
        # This loop says if they pitched more than one inning, then to add geom_line(), if they only pitched one inning, then use only geom_point()
        if(length(unique(pvp_game$Inning)) >1) {
          ggplot(data = pvp_game, aes(x = Inning, y = Avg, color = TaggedPitchType) ) +
            geom_point( size = 2, alpha = .75) +
            #   scale_x_date(date_labels = "%b %d %Y", breaks = unique(velo_inn$Inning)) +
            geom_line() +
            scale_x_continuous(labels = as.numeric(pvp_game$Inning), breaks = pvp_game$Inning) +
            # xlim(min(velo_inn$Inning),max(velo_inn$Inning) ) + #ylim(0,5) +
            scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                          'CT' = 'orange',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
            labs(title = "Velo by Inning", x = "Inning", y = "Pitch Velocity (MPH)", color = " " ) + #, title = "Pitch Velocity by Inning") +
            theme_bw() + theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), axis.text = element_text(size = 8)) +
            theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_text(size = 8))
          
        } else{
          
          ggplot(data = pvp_game, aes(x = Inning, y = Avg, color = TaggedPitchType) ) +
            geom_point( size = 2, alpha = .75) +
            #   scale_x_date(date_labels = "%b %d %Y", breaks = unique(velo_inn$Inning)) +
            #  geom_line() +
            scale_x_continuous(labels = as.numeric(pvp_game$Inning), breaks = pvp_game$Inning) +
            # xlim(min(velo_inn$Inning),max(velo_inn$Inning) ) + #ylim(0,5) +
            scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                          'CT' = 'orange',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
            labs(title = "Velo by Inning", x = "Inning", y = "Pitch Velocity (MPH)", color = " " ) + #, title = "Pitch Velocity by Inning") +
            theme_bw() + theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), axis.text = element_text(size = 8)) +
            theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_text(size = 8))
        }
      
      # Pitch location plots NEW ----
      
      plp_df <- pitcher_data %>% mutate(whiff = ifelse(whiff==1,'Whiff', 'All')) %>%
        dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN'  ) )  %>%
        group_by(BatterSide,TaggedPitchType)%>%
        summarise(n = n(),across(c(PlateLocSide,PlateLocHeight), ~ mean(.,na.rm = T)))%>%
        mutate(BatterSide = paste("All Pitches vs", BatterSide))
      
      all_p_plot <-ggplot(plp_df, aes(x=-PlateLocSide,y=PlateLocHeight))+
        xlim(-1.5, 1.5) + ylim(0, 4) +
        labs(color = "", shape = '', title = paste("Pitch Locations - Batter POV")) +
        # labs(color = "", shape = '', title ='') +
        geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 0.75, color = "black") +
        geom_segment(data = tonybaseball::home_plate_c_pov, aes(x=x,y=y,xend=xend,yend=yend), size = 1)+
        geom_point(aes(fill = TaggedPitchType), size = 8 , alpha = .75, pch = 21) +
        scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700', 'SL' = '#33A8FF',
                                     'CT' = 'orange', 'CH' = 'violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) +
        facet_wrap(~BatterSide)+
        theme_bw() +
        theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))  +
        theme(legend.position = 'none' ,legend.text = element_text(size = 7), axis.title = element_blank(),
              plot.margin=unit(c(0,-.01,-0.5,-.01), "cm"))  +
        guides(size = "none")
      
      whiff_df <- pitcher_data %>% filter(whiff==1) %>%
        dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Knuckleball = 'KN'  ) ) %>%
        group_by(BatterSide,TaggedPitchType)%>%
        summarise(n = n(),across(c(PlateLocSide,PlateLocHeight), ~ mean(.,na.rm = T)))%>%
        mutate(BatterSide = paste("Whiffs vs", BatterSide))
      

      
      whiff_p_plot <- ggplot(whiff_df, aes(x=-PlateLocSide,y=PlateLocHeight))+
        xlim(-1.5, 1.5) + ylim(0, 4) +
        # labs(color = "", shape = '', title = paste("Whiff Locations - Batter POV")) +
        labs(color = "", shape = '', title = '') +
        geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 0.75, color = "black") +
        geom_segment(data = tonybaseball::home_plate_c_pov, aes(x=x,y=y,xend=xend,yend=yend), size = 1)+
        geom_point(aes(fill = TaggedPitchType), size = 8 , alpha = .75, pch = 21) +
        scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700', 'SL' = '#33A8FF',
                                     'CT' = 'orange', 'CH' = 'violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) +
        facet_wrap(~BatterSide)+
        theme_bw() +
        theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5)) +
        theme(legend.position = 'none' ,legend.text = element_text(size = 7), axis.title = element_blank(),
              plot.margin=unit(c(-0.5,-.01,0,-.01), "cm"))+
        guides(size = "none")
      
      
      plp_all <- grid.arrange(
        all_p_plot,
        whiff_p_plot,
        ncol = 1   
        
      )
      # rm(plp_all)
      # Pitch location plot vs rhh with a facet wrap on one of the created columns ----
      # plp_all <- 
      #   ggplot(pitcher_data %>% mutate(whiff = ifelse(whiff==1,'Whiff', 'All')) %>%
      #            dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
      #                                                    Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN'  ) ),
      #          aes(x = -PlateLocSide, y = PlateLocHeight)) +
      #   xlim(-1.5, 1.5) + ylim(0, 4) +
      #   labs(color = "", shape = '', title = paste("Pitch Locations - Batter POV")) +
      #   geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 0.75, color = "black") +
      #   geom_segment(data = tonybaseball::home_plate_c_pov, aes(x=x,y=y,xend=xend,yend=yend), size = 1)+
      #   geom_point(aes(color = TaggedPitchType ),size =2.5, alpha = .75) +
      #   # stat_density2d(aes(fill = as.numeric(..level..)), geom = "polygon", bins = 1, adjust = 1, contour = T, show.legend = F) +
      #   scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700', 'SL' = '#33A8FF',
      #                                 'CT' = 'orange', 'CH' = 'violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) +
      #   
      #   # scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700', 'SL' = '#33A8FF',
      #   #                              'CT' = 'orange', 'CH' = 'violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) +
      #   theme_bw() +
      #   theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5)) +
      #   theme(legend.position = 'bottom' ,legend.text = element_text(size = 7), axis.title = element_blank()) +
      #   facet_wrap(BatterSide~whiff, nrow = 2) +
      #   theme(strip.text = element_text(size = 8, face = 'bold'),
      #         axis.text.x = element_blank(),
      #         axis.text.y = element_blank(),
      #         panel.background = element_rect(fill = 'white'),
      #         panel.grid.major = element_line(colour = 'white'),
      #         panel.grid.minor = element_line(colour = 'white')
      #         
      #   )+
      #   guides(color = "none")
      # 
      # plp_all
      #
      # Pitch location plot vs lhh with a facet wrap on one of the created columns ----
      
      # plp_lhh <- ggplot(data = pitcher_data %>% filter(filter_col !='Take' & !is.na(filter_col), BatterSide == 'Left' )%>%
      #                     dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
      #                                                             Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN'  ) ), 
      #                   aes(x = -PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      #   xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Location vs LHH" ) )+
      #   geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
      #   coord_equal()+
      #   # Home Plate Outline Below
      #   geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      #   geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
      #   geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") +
      #   geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") +
      #   geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      #   geom_point(size =3, alpha = .75) +
      #   scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
      #                                 'CT' = 'orange',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
      #   theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
      #   theme(legend.position = "none", legend.text = element_text(size = 10), axis.title = element_blank())  +
      #   facet_wrap(~filter_col)+
      #   theme(strip.text = element_text(size = 11, face = 'bold'))
      # 
      
      
      
      # CREATES ANOTHER USAGE BREAKDOWN ----
      
      test <- pitcher_data%>%
        dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Knuckleball = 'KN',
                                                Other = 'OT'  ) ) %>%
        mutate(Count = paste0(Balls,"-",Strikes),
               filter_2 = ifelse(Count == "0-0", 'FirstPitch',
                                 ifelse(Strikes == 2, '2 Strikes',
                                        ifelse(Count %in% c('1-0','2-0','3-0','2-1','3-1'), 'PitcherBehind',
                                               ifelse(Count %in% c('0-1','0-2', '1-2' ), 'PitcherAhead', ''
                                               ))  ))    
        )%>%
        filter(filter_2 != '', BatterSide!='') %>%
        group_by(TaggedPitchType, filter_2, BatterSide) %>%
        summarise(P = n() ) %>%
        group_by(filter_2, BatterSide) %>%
        mutate(percentage = round(P / sum(P),3)*100) %>%
        mutate(BatterSide = gsub("Left",'LHH',BatterSide),
               BatterSide = gsub('Right','RHH',BatterSide)) 
      
      
      
      breakdown<-
        ggplot(test %>%
                 mutate(filter_2 = factor(filter_2, levels = c('FirstPitch', '2 Strikes', 'PitcherAhead', 'PitcherBehind') )), 
               aes(x = "", y = percentage, fill = TaggedPitchType)) +
        geom_col(color = "black") +
        scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                     'CT' = 'orange',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black'))+
        
        #  geom_label_repel(aes(label = percentage ), nudge_x = .5)+
        # geom_text(aes(label = paste0(percentage,"%")), position = position_stack(vjust = 0.5), fontface = 'bold', color = 'white', size = 3) +
        geom_label(aes(label = paste0(percentage,"%")), position = position_stack(vjust = 0.5), fontface = 'bold', color = 'white', size = 3) +
        #  coord_polar(theta = "y")+
        theme_void()+
        labs(title = 'Situational Usage')+
        theme(strip.text = element_text(size = 8, face = 'bold'),
              plot.title = element_text(size = 10, hjust = .5, face = 'bold'))+
        facet_wrap(BatterSide~filter_2, nrow=2) + 
        theme(legend.position="none")
      
      breakdown
      
      pitcher_hand <- pitcher_data$PitcherThrows[1]
      pitcher_hand <- ifelse(pitcher_hand =='Right', 'RHP',
                             ifelse(pitcher_hand =='Left', 'LHP',''))
      
      pitch_dist_plot <-
        ggplot(data =  
                 pitcher_data %>%
                 dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                         Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Kunckleball = 'KN' )))  +
        geom_density(aes(x = RelSpeed, fill = TaggedPitchType), alpha = .7) +
        geom_point(data = pitcher_data %>%
                     dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                             Cutter = 'CT', Changeup = 'CH', Other = 'OT' )) %>% group_by(TaggedPitchType) %>% filter(n() < 5), 
                   aes(x = RelSpeed, y = 0.03 ,fill = TaggedPitchType), color = 'black', size = 4, pch = 21, alpha = .7) +
        facet_wrap2(~TaggedPitchType, ncol = 1, strip.position = 'left', scales = 'free_y') +
        
        scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#a34700',  'SL'='#33A8FF',
                                     'CT' = 'orange',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) +
        labs(title = "Pitch Velo Distribution" ,fill = "") + # , na.rm = TRUE)+
        theme_bw() + 
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          # strip.text = element_text(color = 'black', face = 'bold'),
          strip.text = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                            colour = "grey"),
          panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
                                            colour = "grey"),
          # panel.background = element_rect(fill = '#f5f5f5'),
          # plot.background = element_rect(fill = '#f5f5f5'),
          plot.title = element_text(size = 10, face = "bold", hjust = 0.5) #,
          # aspect.ratio = 1/3
        ) 
      
      
      # PARAMETERS FOR THE R MARKDOWN FILE ----
      params <- list(
        season_stats = season_stats,
        season_summary_table = season_summary_table,
        pitch_usage_table = pitch_usage_table,
        pitch_movement_plot = pitch_movement_plot,
        pitcher = pitcher,
        game_stats = game_stats,
        usage_r = usage_r,
        stats_vs_r = stats_vs_r,
        usage_l = usage_l,
        stats_vs_l= stats_vs_l,
        date = game_stats$Date[1],
        team = pitcher_data$PitcherTeam[1],
        pvp_game_plot = pvp_game_plot,
        batted_ball = batted_ball,
        # plp_lhh = plp_lhh,
        plp_rhh = plp_all,
        breakdown =breakdown,
        pitcher_hand = pitcher_hand,
        team_logo = team_location,
        pitch_dist_plot = pitch_dist_plot
        
      )
      
      # SETS THE DATE FOR THE FILE NAME
      # file_date <- format(pitcher_data$Date[1], "%m-%d")
      
      # Knit the R Markdown file to PDF
      
    })
    ### RUN ----
    suppressWarnings({
      rmarkdown::render(input = "C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/adv_pitcher_report.Rmd",
                        #   output_file = paste0("C:/Users/tdmed/OneDrive/R_Markdown/pitcher_reports/",team_code,"/", pitcher, " report",".pdf"),
                        output_file = paste0("C:/Users/tdmed/OneDrive/R_Markdown/pitcher_reports/",team_code,"/", pitcher, " report",".pdf"),
                        params = params)
    })
    
    source('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/adv_pitcher_usage.R')
    
  } # END OF LOOP ----
  
  
  print(paste(team_filter, "Pitcher Reports Created"))
  od_adv_folder <- paste0("C:/Users/tdmed/OneDrive/_Advance/",team_code,"/P/")
  
  pdf_folder  <- paste0("C:/Users/tdmed/OneDrive/R_Markdown/pitcher_reports/",team_code,"/")
  #copy to advance folders
  file.copy(from = list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE),
            to = od_adv_folder, overwrite = TRUE)
  
  print(paste(team_filter, "Pitcher copied to OneDrive"))
  
  # Combine into 1 pdf
  pdf_files <- list.files(od_adv_folder, pattern = "\\.pdf$", full.names = TRUE)
  pdf_files_rep <- pdf_files[!grepl("usage", pdf_files, ignore.case = TRUE)]
  pdf_files_use <- pdf_files[grepl("usage", pdf_files, ignore.case = TRUE)]
  
  output_file <- paste0("C:/Users/tdmed/OneDrive/_Advance/",team_code,"/",team_code," Pitchers.pdf")
  
  pdf_combine(pdf_files_rep, paste0("C:/Users/tdmed/OneDrive/_Advance/",team_code,"/",team_code," Pitchers.pdf"))
  pdf_combine(pdf_files_use, paste0("C:/Users/tdmed/OneDrive/_Advance/",team_code,"/",team_code," Pitchers_Usage_Zones.pdf"))
  
  print(paste(team_filter, "combined and ready to be sent!"))
  
  logs<- list.files('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/', full.names = TRUE, pattern = '.log')
  
  for (log in logs){
    file.rename(log,
                to = paste0('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/logs/',basename(log)) )
  }
}

# folder_path <- 'C:/Users/tdmed/OneDrive/R_Codes/Generate Markdown Reports/'
# log_names <- 

#}
# COPY ---------
# {
#   
#   # This hcunk of code copies the r markdown files to a google drive folder. I have folders for each individual pitcher as well. The report then gets copied to that pitchers folder as long as the pitchers name is spelled correctly.
#   reports<- list.files("C:/Users/tdmed/OneDrive/R_Markdown/pitcher_reports/", pattern = ".pdf")
#   gd_folder<- "C:/Users/tdmed/OneDrive/R_Markdown/pitcher_reports/"
#   
#   all_p<- 'G:/My Drive/2023 Boomers/Boomers Reports/Pitchers'
#   
#   p<- paste0(all_p,"/",list.files(all_p))
#   p
#   daily_p_folder<-'G:/My Drive/2023 Boomers/Boomers Reports/Pitchers/_Daily/'
#   
#   reports
#   send_webhook_message("Copying reports to Google Drive!")
#   
#   file.rename(from = paste0(gd_folder,reports),
#               to =paste0(daily_p_folder,reports))
#   
#   daily_p <- list.files('G:/My Drive/2023 Boomers/Boomers Reports/Pitchers/_Daily')
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
