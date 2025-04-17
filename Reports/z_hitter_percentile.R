db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")
{
  stats <- dbGetQuery(db, 'select * from stats_hitting_player where season = 2024 OR (Player = "Bren Spillane" and SEASON = 2023)')
  stats_lg <- dbGetQuery(db, 'select * from stats_hitting_league where season = 2024 ') 
  stats_bb <- dbGetQuery(db, 'select * from stats_hitting_player_batted_ball where (season = 2024 and BatterTeam not like "%LEAGUE%")
                         OR (Batter = "Bren Spillane" and SEASON = 2023)') %>%
    select(-c(3,5:27,))
  stats_bb_lg <- dbGetQuery(db, 'select * from stats_hitting_league_batted_ball where season = 2024') %>%
    select(c(3,28:59,))
  
  base_r_p <- dbGetQuery(db, 'select player_id, total_net_extra_base_sb_value from stats_baserunning_player')
  base_r_lg <- dbGetQuery(db, 'select Type, total_net_extra_base_sb_value from stats_baserunning_league where Type = "Average"')%>%
    mutate(BaseRuns_per_g = total_net_extra_base_sb_value / 96)
  
  
  league <- stats_lg %>% 
    left_join(stats_bb_lg, by = c('SEASON')) %>%
    select(Season = SEASON, PA, AB, H, HBP, BB, AVG, SO, BB,OBP, SLG, OPS, wOBA, wRC_plus, SO_pct, BB_pct, oWAR, wRAA, BattingRuns, 46:77) %>%
    mutate(BaserunningValue = base_r_lg$total_net_extra_base_sb_value,
           BaseRuns_per_reach = (BaserunningValue) * OBP ,
           BaseRuns_per_g = base_r_lg$BaseRuns_per_g)
  
  
  player <- stats %>% 
    left_join(stats_bb, by = c('player_id_fl' = 'BatterId'), relationship = "many-to-many") %>% 
    left_join(base_r_p, by = c('player_id_fl' = 'player_id'), relationship = "many-to-many") %>%
    select(Player, Team, player_id_fl, Season = SEASON, G, PA, AB, H, HBP, AVG, SO, BB,OBP, SLG, OPS, wOBA, wRC_plus, SO_pct, BB_pct, oWAR, wRAA, BaserunningValue = total_net_extra_base_sb_value, BattingRuns, 51:83) %>%
    mutate(BaseRuns_per_g = BaserunningValue / G,
           BaseRuns_per_reach = (BaserunningValue) * OBP , # = BaserunningValue / G,
           BaseRuns_plus =(BaseRuns_per_g / league$BaseRuns_per_g) * 100, 
           .after = BaserunningValue)
  
  {  
    pl_qual <- player %>%
      mutate(qual = ifelse(PA >= 1 * 95, 1, 0)) %>%
      filter(qual == 1)
    
    common_stats <- intersect(names(pl_qual), names(league))
    
    exclude_cols <- c("Season", "PA", "AB", "SO", "BB", "BU", 'wRC_plus', "Zone_pct", "oZone_pct", 
                      "ZoneSw_pct", "ZoneCon_pct","GB_pct", "FB_pct", 
                      "LD_pct", "PU_pct", "Swing_pct", 'ChaseCon_pct')
    
    common_stats <- setdiff(intersect(names(pl_qual), names(league)), exclude_cols)
    
    pl_qual_adj <- pl_qual 
    
    for(column_name in common_stats) {
      pl_qual_adj <- pl_qual_adj %>%
        mutate(
          !!column_name := .data[[column_name]] * 
            (.data[[column_name]]/ mean(league[[column_name]]))
        ) 
    }
    
    
    
    # mutate(across(
    #   all_of(common_stats) ~ . * (./league[.])
    #   )
    # )
    
    pl_rank <- pl_qual_adj %>%
      mutate(
        # higher is better
        across(c(AVG, OBP, SLG, OPS, wOBA, BB_pct, oWAR, wRC_plus,
                 wRAA, BattingRuns, xwOBA, wOBACON, xWOBACON, BABIP.y, 
                 ExitVelo, ExitVelo_max, LA, Angle_Hard, Angle_Sweet, 
                 RunValue, xBA, xSLG, xBACON, HardHit_pct, Sweetspot_pct, 
                 RV100, BaserunningValue, BaseRuns_plus), ~ percent_rank(.) * 100),
        # lower is better
        across(c(SO_pct, Whiff_pct, ZoneWhiff_pct, Chase_pct, ChaseWhiff_pct), ~  (1 - percent_rank(.)) * 100),
      )%>%
      # filter(Team =='Schaumburg Boomers')%>%
      filter(player_id_fl %in% c(101476, 100106, 101485, 101157, 100818, 100826, 100993, 100589))
    
    colnames(pl_rank)
    col_stats <- c(common_stats, 'wRC_plus', 'BaserunningValue', 'BaseRuns_plus')
    # library(plotly)
    # pl_rank_returners <- pl_rank 
    
    
    pl_pivot<- pl_rank %>%
      select(-c(PA, AB, SO, BB, BU, Season, BatterTeam, GB_pct, FB_pct, 
                LD_pct, PU_pct, Swing_pct, Zone_pct, oZone_pct, ZoneSw_pct, qual,
                ZoneCon_pct, ChaseCon_pct)) %>%
      pivot_longer(cols = starts_with(c(col_stats)),
                   names_to = "Stat",
                   values_to = "Value") 
    
    dput(colnames(pl_pivot))
    
    
    # boom_ids <- 
    
    
    
    player_data_raw <- pl_qual  %>%
      # filter(Player == player_name) %>%
      select(-c(Team, PA, AB, SO, BB, BU, Season, BatterTeam, GB_pct, FB_pct, 
                LD_pct, PU_pct, Swing_pct, Zone_pct, oZone_pct, ZoneSw_pct, qual,
                ZoneCon_pct, ChaseCon_pct)) %>%
      filter(player_id_fl %in% c(101476, 100106, 101485, 101157, 100818, 100826, 100993, 100589)) %>%
      pivot_longer(cols = starts_with(c(col_stats)),
                   names_to = "Stat",
                   values_to = "Value") %>%
      mutate(Value = ifelse(is.na(Value),0,Value))
    # filter(Team =='Schaumburg Boomers')
    
    player_data_rank <- pl_pivot 
    # %>%
    #   filter(Player == player_name) 
    
    player_data <- player_data_rank %>%
      left_join(player_data_raw, by = c('Player', 'Stat')) %>%
      rename(Percentile = Value.x,
             Value = Value.y) %>%
      mutate(Percentile = round(Percentile)) %>%
      filter(Stat %in% c('wRC_plus','oWAR','BattingRuns',#'RV100',
                         'AVG', 'OBP', 'SLG', 'OPS', 'wOBA', 'xwOBA', 'xBA', 'xSLG', 'ExitVelo',
                         'HardHit_pct', 'Angle_Sweet', 'Whiff_pct','Chase_pct', 'SO_pct', 'BB_pct', 'BaseRuns_plus', 'BaserunningValue')) %>%
      mutate(Stat = str_replace_all(Stat, c('wRC_plus' = 'wRC+','oWAR' = 'WAR', 'Angle_Sweet' = "LA Sweet-Spot%", 'HardHit_pct' = 'Hard-Hit%','RV100' = 'RunValue/100 PA',
                                            'Whiff_pct' = 'Whiff%','Chase_pct' = 'Chase%', 'SO_pct' = 'SO%', 'BB_pct' = 'BB%',
                                            'ExitVelo' = 'Avg Exit Velo', 'BaseRuns_plus' = 'BaseRuns+', 'BaserunningValue' = 'BaseRuns')),
             Stat = factor(Stat, levels = c('WAR','BattingRuns', 'BaseRuns', 'wRC+', 'RunValue/100 PA',"AVG", "OBP", "SLG", "OPS", 
                                            "wOBA",  "xwOBA", "xBA", 'xSLG',
                                            "Avg Exit Velo", "LA Sweet-Spot%",  "Hard-Hit%", "SO%", "BB%", 'BaseRuns+', "Whiff%", 
                                            "Chase%")),
             Value = round(Value, 3)) %>%
      filter(!Stat %in% c('AVG', 'OBP', 'SLG', 'OPS', 'BaseRuns+', "LA Sweet-Spot%")) %>%
      mutate(Player = gsub('Bren Spillane', 'Bren Spillane (2023)', Player),
             Value = ifelse(is.na(Value),0,Value))
    
    
p <-    ggplot(player_data, aes(x = Percentile, y = Stat, fill = Percentile , color = Percentile )) +
      geom_bar(stat = "identity",   width = .5, position = position_dodge(width = 1)) +
      geom_segment(aes(x = Percentile, xend = 100, y = Stat, yend = Stat), color = "grey", linewidth = 1) +
      geom_segment(aes(x = 50, xend = 50, y = 0, yend = 14.5), color = "white", alpha = 0.05, linewidth = 1) +
      geom_segment(aes(x = 5, xend = 5, y = 0, yend = 14.5), color = "white", alpha = 0.05, linewidth = 1) +
      geom_segment(aes(x = 95, xend = 95, y = 0, yend = 14.5), color = "white", alpha = 0.05, linewidth = 1) +
      geom_point(aes(Percentile, Stat), color= 'white', fill = 'white', size = 7, alpha = 1, shape = 21, stroke = 0.5) +
      geom_point(aes(Percentile, Stat),  size = 6, alpha = 1, shape = 21, stroke = 0.5) +
      scale_fill_gradient2(midpoint = 50, low = "#365ea6", mid = "#c7dcdc", high = "#c91f26") +
      scale_colour_gradient2(midpoint = 50, low = "#365ea6", mid = "#c7dcdc", high = "#c91f26") +
      scale_y_discrete(limits = \(Stat) rev(Stat)) +
      xlim(0, 110) +
      # ggtitle(glue::glue("{player_name} 2024 Hitter Percentile Rankings") )+
      ggtitle(glue::glue("2024 Hitter Percentile Rankings") )+
      geom_text(aes(x = Percentile, y = Stat, label = Percentile), size = 3.5, fontface = 'bold',  
                color = ifelse(player_data$Percentile > 30 & player_data$Percentile < 70, "black", "white")) +
      geom_text(aes(x = 105, y = Stat, label = paste0(Value)), size = 3, fontface = 'bold',  hjust = 0,
                color = 'black') +
      geom_text(aes(x = 5, y = "Chase%", label = "POOR"), hjust = .5, vjust = 3, size = 4, color = "#365ea6")+
      geom_text(aes(x = 5, y = "Chase%", label = "^"), hjust = .5, vjust = 2.25, size = 4, color = "#365ea6")+
      geom_text(aes(x = 50, y = "Chase%", label = "AVERAGE"), hjust = .5, vjust = 3, size = 4, color = "grey")+
      geom_text(aes(x = 50, y = "Chase%", label = "^"), hjust = .5, vjust = 2.25, size = 4, color = "grey")+
      geom_text(aes(x = 95, y = "Chase%", label = "GREAT"), hjust = .5, vjust = 3, size = 4, color = "#c91f26")+
      geom_text(aes(x = 95, y = "Chase%", label = "^"), hjust = .5, vjust = 2.25, size = 4, color = "#c91f26")+
      theme(
        # plot.background = element_rect(fill = '#Fafaf3', color = '#Fafaf3' ),
        # panel.background = element_rect(fill = '#Fafaf3', color = '#Fafaf3' ),
        plot.background = element_rect(fill = '#ffffff', color = '#ffffff' ),
        panel.background = element_rect(fill = '#ffffff', color = '#ffffff' ),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = .35, face='bold'),
        axis.text.y = element_text(hjust = 1, face='bold'),
        legend.position = "none"
      )+
      facet_wrap(~Player, nrow = 2, scales = 'free_y')+
      theme(strip.text = element_text(size = 12, face = "bold")) +
      geom_segment(x = 0, xend = 100, y = 11.5, yend = 11.5, linetype = 2, linewidth = 1, color = 'black')+
      theme(
        panel.border = element_rect(colour = "black", fill = NA, size = 1)
      )
    
    }
  #
  #
  #
}

# ggplotly(
#   # p
# ) %>%
#   layout(title = glue::glue("{player_name} Hitter Percentile Rankings\n") , plot_bgcolor = "#Fafaf3") %>% 
#   layout(paper_bgcolor='#Fafaf3') %>%  
#   layout(xaxis = list(title = ""), yaxis = list(title = ""),
#          images = list(  
#            # list(  
#            #   source =  "https://i.imgur.com/tQZCji9.png",  
#            #   xref = "paper",  
#            #   yref = "paper",  
#            #   x = 0.48,  
#            #   y = 0.05,  
#            #   sizex = 0.2,  
#            #   sizey = 0.2,  
#            #   xanchor="center",  
#            #   yanchor="center" 
#            # ),
#            list(  
#              source =  "http://silhouettesfree.com/sports/baseball/batter-silhouette-image-9.png",  
#              xref = "paper",  
#              yref = "paper",  
#              x = -0.05,  
#              y = 1.1,  
#              sizex = 0.1,  
#              sizey = 0.1,  
#              xanchor="center",  
#              yanchor="center" 
#            )  
#          ) ) %>% 
#   style(hoverinfo = 'none') %>%
#   layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) 
# 


# xwoba----
{
bin_size <- .5
player_name <- 'Christian Fedko'
hitter_data <- dbGetQuery(db, glue::glue("select * from pitch_data where Batter= '{player_name}' ")) %>%
  tonybsbl::select_common_columns() %>%
  filter(!is.na(xwOBA), !is.na(xwOBACON))%>%
  arrange((xwOBA))%>%
  mutate(
    x_bin = floor(PlateLocSide / bin_size) * bin_size,
    y_bin = floor(PlateLocHeight / bin_size) * bin_size,
    .after = BatterId
  )

hitter_mean <- hitter_data %>%
  group_by(Batter,BatterId, x_bin, y_bin) %>%
  summarise(p = n(),
            xwOBA = round(mean(xwOBA, na.rm = T),3),
            xwOBACON = round(mean(xwOBACON[bbe==1], na.rm = T),3))

# ggplot(hitter_data, aes(x=-PlateLocSide , y = PlateLocHeight))+
#   geom_point(aes( color = xwOBA), size = 5, alpha = 1) +
#   scale_color_gradient2(high = ("#c91f26"), mid = "#c7dcdc", low = ("#365ea6"), midpoint = .38) +
#   # stat_density2d(aes(fill = as.numeric(..level..)), geom = "polygon", bins = 15, adjust = 1, contour = T, show.legend = F) +
#   # scale_fill_gradient2(high = ("#c91f26"), mid = "white", low = ("#365ea6"), midpoint = .343) +
#   geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 0.75, color = "black") +
#   geom_segment(data = tonybaseball::home_plate_c_pov, aes(x=x,y=y,xend=xend,yend=yend), size = 1) +
#   ylim(0,5)+
#   xlim(-2.5,2.5)

 # geom_rect(aes(xmin = -0.708, xmax = 0.708, 
  #               ymin = people$k_zone_bot[people$player_id_fl==unique(hitter_data$BatterId)], ymax = people$k_zone_top[people$player_id_fl==unique(hitter_data$BatterId)]), 
  #           alpha = 0, size = 0.75, color = "red", inherit.aes = F) +
ggplot(hitter_mean, aes(x=-x_bin , y = y_bin))+
  stat_summary_hex(aes(z = xwOBACON), binwidth = c(0.6, 0.6), fun = mean) +
  scale_fill_gradient2(high = "#c91f26", mid = "#c7dcdc", low = "#365ea6", midpoint = .38) +
  geom_rect(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5, alpha = 0, size = 0.75, color = "black", inherit.aes = F) +
  geom_segment(data = tonybaseball::home_plate_c_pov, aes(x=x,y=y,xend=xend,yend=yend), size = 1, inherit.aes = F) +
  geom_text(data = hitter_mean, aes(x = -x_bin, y = y_bin, label = xwOBA), size = 3, color = "black") +
  ylim(0,5)+
  xlim(-2,2)+
  coord_equal()
}

