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
  }))

print("Loading CSVs...")

db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")
# pitcher_filter <- 'Aaron Dona'
yakker <- dbGetQuery(db, paste0(
  'SELECT * FROM pitch_data where Pitcher = "',pitcher_filter,'"' 
)
) %>%
  filter(TaggedPitchType !='', 
         BatterSide != '',
         TaggedPitchType !='NA',
         !is.na(TaggedPitchType))  %>%
  mutate(Pitcher = gsub('A.husson', "Aaron Husson", Pitcher))

team_info <- # read.csv("C:/Users/tdmed/OneDrive/_FLASH/frontier team info.csv")
  dbGetQuery(db, 'SELECT * FROM teams')

all_pitchers <- #read.csv("C:/Users/tdmed/OneDrive/_FLASH/frontier_all_pitchers23.csv") %>%
  dbGetQuery(db, 'SELECT * FROM stats_pitching_player') %>%
  mutate(NAME = gsub("JD Hammer", 'J.D. Hammer', NAME)) %>%
  mutate(NAME = gsub('Robert Klinchock', 'Rob Klinchock', NAME),
         NAME = gsub('Brain', 'Brian', NAME),
         NAME = gsub("Ryan O'Reilly", 'Ryan OReilly', NAME) )

rosters<- # read.csv("C:/Users/tdmed/OneDrive/_FLASH/front_rosters24.csv")%>%
  dbGetQuery(db, 'SELECT * FROM rosters_') %>%
  mutate(NAME = gsub("JD Hammer", 'J.D. Hammer', NAME)) %>%
  mutate(NAME = gsub('Robert Klinchock', 'Rob Klinchock', NAME),
         NAME = gsub('Brain', 'Brian', NAME),
         NAME = gsub("Ryan O'Reilly", 'Ryan OReilly', NAME))

print("CSVs loaded!")

sort(unique(yakker$Pitcher))	


#pitcher_filter <- "Logan Schmidt"
team_filter <- yakker$PitcherTeam[yakker$Pitcher==pitcher_filter][1]
team_code <- team_info$bats_team_code[team_info$team_yt_3 == team_filter]

print("Generating Pitcher report!")


folder_path <- paste0("C:/Users/tdmed/OneDrive/R_Markdown/pitcher_reports/", team_code)
folder_path
if (!file.exists(folder_path)) { dir.create(folder_path) }

yakker <- yakker %>%
  # FILTER
  filter(Pitcher == pitcher_filter,
         TaggedPitchType !='', 
         BatterSide != '',
         TaggedPitchType !='NA',
         !is.na(TaggedPitchType))  
unique(yakker$TaggedPitchType)
unique(yakker$PitcherTeam)
# set a factor to manually order the pitch types
yakker$TaggedPitchType <- factor(yakker$TaggedPitchType, levels = c("Fastball", "Sinker", "Cutter","Curveball", "Slider", "Changeup", "Splitter", 'Knuckleball', 'Other'))



team_roster <- rosters %>%
  filter(NAME == pitcher_filter,
         grepl("P", POSITION)
         # STATUS != 'Inactive'
         )

roster_pitchers <- sort(unique(team_roster$NAME)) 
roster_pitchers
pitchers <- sort(unique(yakker$Pitcher)) 
# pitchers <- 'Aaron Dona'

pitchers


team_p <- team_roster %>% filter(NAME %in% pitchers)

unique_pitchers <-team_p$NAME
# unique_pitchers <- 'Buddie Pindel'
for (pitcher in unique_pitchers) {
  # suppressMessages({
    # Filter the data for the current pitcher
    pitcher_data <- yakker[yakker$Pitcher == pitcher, ]
    
    season_stats <- all_pitchers %>%
      filter(NAME == pitcher) %>%
      select(NAME, APP, GS, CG, SV, IP, ERA, R, ER, H, SO, BB, WHIP, AVG)
    # Generate the game summary / pitch characteristics table ----
    game_summary_table <- 
      pitcher_data %>%
      dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                              Cutter = 'CT', Splitter = 'SPL', Knuckleball = 'KN', Changeup = 'CH', Other = 'OT' ) ) %>%
      group_by('Pitch' = TaggedPitchType) %>%
      dplyr::summarize('No.' = n(),
                       'Usage' = n(),
                       'Usage %' = n(),
                       'Velo' = round(mean(RelSpeed, na.rm = TRUE),1),
                       'VeloMax' = round(max(RelSpeed, na.rm = TRUE),1),
                       'Tilt' = round(mean(SpinAxis, na.rm = TRUE),0),
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
                       'Vert' = round(mean(InducedVertBreak, na.rm = TRUE),1),
                       'Horz' = round(mean(HorzBreak, na.rm = TRUE),1),
                       'VAA' = round(mean(VertApprAngle, na.rm = TRUE),1),    
                       'RelHt' = round(mean(RelHeight, na.rm = TRUE),1),
                       'RelSide' = round(mean(RelSide, na.rm = TRUE),1),
                       'Ext' = round(mean(Extension, na.rm = TRUE),1)
      ) %>%
      mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100) %>%
      dplyr::select(-Usage,-Time,-HH,-MM)
    
    
    
    
    
    
    
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
      dplyr::mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                              Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) )%>%
      #   group_by(Date) %>%
      dplyr::summarise('IP' = round( (sum(OutsOnPlay, na.rm = TRUE))/3, 1),
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
                                              Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) )%>%
      filter(BatterSide == 'Right') %>%
      group_by('Pitch' = TaggedPitchType) %>%
      dplyr::summarize('No.' = n(),
                       'Usage %' = n(),
                       "1P" = sum(PitchofPA == 1),
                       "1P%" = sum(`1P`),
                       '2K' = sum(Strikes == 2),
                       '2K%' = sum(`2K`),
                       'Strike %' = round(sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay", "StrikeCalled"))/n(),3)*100,
                       'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                                           sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay")),3)*100 
      ) %>%
      mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100,
             `1P%` = round(`1P%`/sum(`1P%`),3)*100,
             `2K%` = round(`2K%`/sum(`2K%`),3)*100) %>% 
      dplyr::select(-`1P`, -`2K`)
    # Stats vs RHH ----
    stats_vs_r <- pitcher_data  %>%
      dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                              Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) )%>%
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
                                              Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) )%>%
      filter(BatterSide == 'Left') %>%
      group_by('Pitch' = TaggedPitchType) %>%
      dplyr::summarize('No.' = n(),
                       'Usage %' = n(),
                       "1P" = sum(PitchofPA == 1),
                       "1P%" = sum(`1P`),
                       '2K' = sum(Strikes == 2),
                       '2K%' = sum(`2K`),
                       'Strike %' = round(sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay", "StrikeCalled"))/n(),3)*100,
                       'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                                           sum(PitchCall %in% c("StrikeSwinging", "Foul", "FoulTip", "InPlay")),3)*100 
      ) %>%
      mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100,
             `1P%` = round(`1P%`/sum(`1P%`),3)*100,
             `2K%` = round(`2K%`/sum(`2K%`),3)*100) %>% 
      dplyr::select(-`1P`, -`2K`)
    
    # STATS VS LHH ----
    stats_vs_l <- pitcher_data  %>%
      dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                              Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) )%>%
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
                                              Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) )%>%
      group_by('Pitch' = TaggedPitchType) %>%
      dplyr::summarize('No.' = n(),
                       'BIP' = sum(PitchCall == 'InPlay'),
                       'XBH' = sum(PlayResult %in% c("Double","Triple","HomeRun")),
                       'Avg EV' = round(mean(ExitSpeed, na.rm= TRUE),0),
                       'EV 90+' = sum(ExitSpeed >= 90, na.rm= TRUE),
                       'EV -90' = sum(ExitSpeed < 90, na.rm= TRUE) 
      )
    
    
    # Generate the pitch movement plot ----
    pitch_movement_plot <- 
      #   ggplotly(
      ggplot(data =  
               pitcher_data %>%
               dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                       Cutter = 'CT', Changeup = 'CH', Other = 'OT' ) ),
             
             
             aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
      labs(title = "Pitch Movement" ,color = "",x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)" ) + # , title = "Pitch Movement") +
      xlim(-22, 22) + ylim(-22, 22) +
      geom_segment(aes(x = 0, y = -22, xend = 0, yend = 22), size = 1, color = "grey55") +
      geom_segment(aes(x = -22, y = 0, xend = 22, yend = 0), size = 1, color = "grey55") +
      geom_point(size =4, alpha = .75) +
      # backgroundColor = styleEqual(
      #   levels = list('Slider','Other','Sinker', 'Fastball', 'Changeup', 'Curveball', 'Knuckleball', 'Cutter'),
      #   c('cornflowerblue', 'HOTpink', '#f47b20', 'red', 'violet',    'green', 'mediumorchid', 'gold' )
      scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                    'CT' = 'gold',  'CH'='violet', 'OT' = 'black')) + # , na.rm = TRUE)+
      theme_bw() + theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_text(size = 8))
    
    
    
    # Pitch velo table and plot ----
    pvp_game <- pitcher_data %>%
      group_by(Pitcher, TaggedPitchType, Inning) %>%
      dplyr::summarise(Avg = mean(RelSpeed, na.rm = TRUE), Max = max(RelSpeed, na.rm = T), min = min(RelSpeed, na.rm = T)) %>%
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
          scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                        'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
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
          scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                        'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
          labs(title = "Velo by Inning", x = "Inning", y = "Pitch Velocity (MPH)", color = " " ) + #, title = "Pitch Velocity by Inning") +
          theme_bw() + theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), axis.text = element_text(size = 8)) +
          theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_text(size = 8))
      }
    
    
    
    # Pitch location plot vs rhh with a facet wrap on one of the created columns ----
    plp_rhh <- 
      ggplot(pitcher_data %>% mutate(whiff = ifelse(whiff==1,'Whiff', 'All')) %>%
               dplyr::mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                      Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN')),
             aes(x = PlateLocSide, y = PlateLocHeight)) +
      xlim(-1.5, 1.5) + ylim(0, 4) +
      labs(color = "", shape = '', title = paste("Pitch Locations")) +
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 0.75, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") +
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") +
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_point(aes(color = TaggedPitchType ),size =2.5, alpha = .75) +
      #geom_point(aes(shape = whiff, color = TaggedPitchType ), stroke = 0.5, size = 3, alpha = 0.8 ) +
      #scale_shape_manual(values=c(10, 19)) +
      scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#f47b20', 'SL' = 'cornflowerblue',
                                    'CT' = 'gold', 'CH' = 'violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) +
      
      # scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#f47b20', 'SL' = 'cornflowerblue',
      #                              'CT' = 'gold', 'CH' = 'violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) +
      theme_bw() +
      theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5)) +
      theme(legend.position = 'bottom' ,legend.text = element_text(size = 7), axis.title = element_blank()) +
      facet_wrap(BatterSide~whiff) +
      theme(strip.text = element_text(size = 8, face = 'bold'),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.background = element_rect(fill = 'white'),
            panel.grid.major = element_line(colour = 'white'),
            panel.grid.minor = element_line(colour = 'white')
            
      )+
      guides(color = "none")
    
    plp_rhh
    #
    # Pitch location plot vs lhh with a facet wrap on one of the created columns ----
    
    plp_lhh <- ggplot(data = pitcher_data %>% filter(filter_col !='Take' & !is.na(filter_col), BatterSide == 'Left' )%>%
                        dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                                Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN'  ) ), 
                      aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Location vs LHH" ) )+
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
      # Home Plate Outline Below
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") +
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") +
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_point(size =3, alpha = .75) +
      scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                    'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
      theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none", legend.text = element_text(size = 10), axis.title = element_blank())  +
      facet_wrap(~filter_col)+
      theme(strip.text = element_text(size = 11, face = 'bold'))
    
    
    
    
    # CREATES ANOTHER USAGE BREAKDOWN ----
    
    test <- pitcher_data%>%
      dplyr:: mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                              Cutter = 'CT', Changeup = 'CH', Other = 'OT', Knuckleball = 'KN'  ) ) %>%
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
      scale_fill_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                   'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black'))+
      
      #  geom_label_repel(aes(label = percentage ), nudge_x = .5)+
      # geom_text(aes(label = paste0(percentage,"%")), position = position_stack(vjust = 0.5), fontface = 'bold', color = 'white', size = 3) +
      geom_label(aes(label = paste0(percentage,"%")), position = position_stack(vjust = 0.5), fontface = 'bold', color = 'white', size = 3) +
      #  coord_polar(theta = "y")+
      theme_void()+
      theme(strip.text = element_text(size = 8, face = 'bold'))+
      facet_wrap(BatterSide~filter_2, nrow=2) + 
      theme(legend.position="none")
    
    
    pitcher_hand <- pitcher_data$PitcherThrows[1]
    pitcher_hand <- ifelse(pitcher_hand =='Right', 'RHP',
                           ifelse(pitcher_hand =='Left', 'LHP',''))
    # SET THE PARAMETERS FOR THE R MARKDOWN FILE ----
    params <- list(
      season_stats = season_stats,
      game_summary_table = game_summary_table,
      pitch_usage_table = pitch_usage_table,
      pitch_movement_plot = pitch_movement_plot,
      pitcher = pitcher,
      game_stats = game_stats,
      usage_r = usage_r,
      stats_vs_r = stats_vs_r,
      usage_l = usage_l,
      stats_vs_l= stats_vs_l,
      date = "",
      team = "",
      pvp_game_plot = pvp_game_plot,
      batted_ball = batted_ball,
      plp_lhh = plp_lhh,
      plp_rhh = plp_rhh,
      breakdown =breakdown,
      pitcher_hand = pitcher_hand
      
    )
    
    # SETS THE DATE FOR THE FILE NAME
    # file_date <- format(pitcher_data$Date[1], "%m-%d")
    
    # Knit the R Markdown file to PDF
    
# })
  
  suppressWarnings({
    rmarkdown::render(input = "C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Opponent-Scouting-Reports-P.Rmd",
                      output_file = paste0("C:/Users/tdmed/OneDrive/R_Markdown/pitcher_reports/",team_code,"/", pitcher, " report",".pdf"),
                      params = params)
  })
}
print(paste(pitcher_filter, "Report Created"))
od_adv_folder <- paste0("C:/Users/tdmed/OneDrive/_Advance/",team_code,"/P/")

pdf_folder  <- paste0("C:/Users/tdmed/OneDrive/R_Markdown/pitcher_reports/",team_code,"/")
#copy to advance folders
file.copy(from = list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE),
          to = od_adv_folder, overwrite = TRUE)

print(paste(pitcher_filter, "copied to OneDrive"))

# log_names <- 
logs<- list.files('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/', full.names = TRUE, pattern = '.log')

for (log in logs){
  file.rename(log,
              to = paste0('C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/logs/',basename(log)) )
}
