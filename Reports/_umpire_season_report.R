library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(xgboost)
library(caret)
library(stringr)
library(ggplot2)
library(hrbrthemes)

db <- dbConnect(RSQLite::SQLite(), "C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

data <- dbGetQuery(db, "SELECT PitcherThrows, BatterSide, PitchCall, PlateLocSide, PlateLocHeight, take, in_zone, Umpire, zone_hscw, called_strike_prob
  FROM pitch_data where PitchCall in ('StrikeCalled','BallCalled')") %>%
  mutate(is_strike = as.integer(PitchCall == "StrikeCalled"),
         correct = case_when(in_zone == is_strike ~ 1, in_zone != is_strike ~ 0, T ~ NA)
  )

max(data$PlateLocHeight[data$in_zone==1], na.rm = T)
min(data$PlateLocHeight[data$in_zone==1], na.rm = T)
max(data$PlateLocSide[data$in_zone==1], na.rm = T)
min(data$PlateLocSide[data$in_zone==1], na.rm = T)


ggplot(data %>% dplyr::slice(1:5000), 
       aes(x=PlateLocSide, y=PlateLocHeight, z = called_strike_prob))+
  stat_summary_hex(fun = mean, bins = 11, alpha = 1) +
  scale_fill_gradient2(midpoint = 0.5, 
                       high = tonybaseball::savant_red, 
                       low = tonybaseball::savant_blue, 
                       mid = tonybaseball::savant_mid,
                       name = "CalledStrike %")+
  geom_rect(xmin=-.708-.25, xmax = .708+.25, ymin = 1.5-.25, ymax=3.5+.25, fill=NA, color = 'black', size = 1.5, linetype = 'dotted')+
  geom_rect(xmin=-.708-.25, xmax = .708+.25, ymin = 1.5-.25, ymax=3.5+.25,fill=NA, color = 'white', size = 1, linetype = 'dotted')+
  geom_rect(xmin=-.708, xmax = .708, ymin = 1.5, ymax=3.5, fill=NA, color = 'black', size = 1.5)+
  geom_rect(xmin=-.708, xmax = .708, ymin = 1.5, ymax=3.5, fill=NA, color = 'white', size = 1)+
  geom_segment(data = tonybaseball::home_plate_p_pov, aes(x=x,y=y,xend=xend,yend=yend), inherit.aes = F,
               color='black', linewidth = 1.5) +
  geom_segment(data = tonybaseball::home_plate_p_pov, aes(x=x,y=y,xend=xend,yend=yend), inherit.aes = F,
               color='white', linewidth = 1) +
  xlim(-2,2)+
  ylim(0,4)+
  labs(title = glue::glue("League Called Strike Probability"),
       subtitle = "Pitcher POV",
       caption = glue::glue('>.5 red: high called strike probability\n<.5 blue: low called strike probability'))+
  hrbrthemes::theme_ft_rc()+
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"))+
  coord_equal()+
  facet_wrap(~BatterSide)



{
  # Filter for umpire-specific model
  umpire_name <- "Alex Reynolds"
  df_umpire <- data %>% filter(Umpire == umpire_name) %>% filter(take == 1) %>% filter(!is.na(PlateLocSide))
  
  # Design matrix for umpire model
  dummy_vars_ump <- dummyVars(~ PitcherThrows + BatterSide, data = df_umpire)
  dummies_ump <- predict(dummy_vars_ump, newdata = df_umpire)
  
  X_umpire <- as.matrix(cbind(df_umpire %>% select(PlateLocSide, PlateLocHeight), dummies_ump))
  y_umpire <- df_umpire$is_strike
  
  # Train umpire-specific model
  dtrain_umpire <- xgb.DMatrix(data = X_umpire, label = y_umpire)
  
  umpire_model <- xgboost(
    data = dtrain_umpire,
    objective = "binary:logistic",
    eval_metric = "logloss",
    nrounds = 100,
    verbose = 0
  )
  
  # Predict with both models
  df_umpire$umpire_csp <- round(predict(umpire_model, newdata = X_umpire),6)
  df_umpire$delta_csp <- round(df_umpire$umpire_csp - df_umpire$called_strike_prob,6)
  df_umpire$PitcherThrows <- str_replace_all(df_umpire$PitcherThrows, c('Right'='RHP','Left'='LHP'))
  df_umpire$BatterSide <- str_replace_all(df_umpire$BatterSide, c('Right'='RHH','Left'='LHH'))
  
  df_umpire <- df_umpire %>% 
    arrange(desc(umpire_csp)) 
  
 
  
  
  # Umpire specific expected called strikes pitcher pov
  ump_csp_p <- ggplot(df_umpire, 
                      aes(x=PlateLocSide, y=PlateLocHeight, z = umpire_csp))+
    stat_summary_hex(fun = mean, bins = 11, alpha = 1) +
    scale_fill_gradient2(midpoint = 0.5, 
                         high = tonybaseball::savant_red, 
                         low = tonybaseball::savant_blue, 
                         # mid = tonybaseball::savant_mid,
                         mid = 'white',
                         name = "CalledStrike %")+
    geom_rect(xmin=-.708-.25, xmax = .708+.25, ymin = 1.5-.25, ymax=3.5+.25, fill=NA, color = 'black', size = 1, linetype = 'dotted')+
    # geom_rect(xmin=-.708-.25, xmax = .708+.25, ymin = 1.5-.25, ymax=3.5+.25,fill=NA, color = 'white', size = .75, linetype = 'dotted')+
    geom_rect(xmin=-.708, xmax = .708, ymin = 1.5, ymax=3.5, fill=NA, color = 'black', size = 1)+
    # geom_rect(xmin=-.708, xmax = .708, ymin = 1.5, ymax=3.5, fill=NA, color = 'white', size = .75)+
    geom_segment(data = tonybaseball::home_plate_p_pov, aes(x=x,y=y,xend=xend,yend=yend), inherit.aes = F,color='black', linewidth = 1) +
    # geom_segment(data = tonybaseball::home_plate_p_pov, aes(x=x,y=y,xend=xend,yend=yend), inherit.aes = F,color='white', linewidth = .75) +
    xlim(-2,2)+
    ylim(0,4)+
    labs(title = glue::glue("{umpire_name} Called Strike Probability"),
         subtitle = "Pitcher POV",
         caption = glue::glue('>.5 red: {umpire_name} high called strike probability\n<.5 blue: {umpire_name} low called strike probability'))+
    # hrbrthemes::theme_ft_rc()+
    theme_minimal()+
    theme(legend.position = "bottom",
          legend.key.width = unit(1, "cm"))+
    coord_equal()+
    facet_wrap(~BatterSide)
  
  
  # Umpire specific expected called strikes catcher pov
  # ump_csp_c <- 
    ggplot(df_umpire, 
                      aes(x=-PlateLocSide, y=PlateLocHeight, z = umpire_csp))+
    stat_summary_hex(fun = mean, bins = 11, alpha = 1) +
    scale_fill_gradient2(midpoint = 0.5, high = tonybaseball::savant_red, 
                         low = tonybaseball::savant_blue,
                         # mid = tonybaseball::savant_mid,
                         mid ='white',
                         name = "CalledStrike %")+
    geom_rect(xmin=-.708-.25, xmax = .708+.25, ymin = 1.5-.25, ymax=3.5+.25, fill=NA, color = 'black', size = 1, linetype = 'dotted')+
    # geom_rect(xmin=-.708-.25, xmax = .708+.25, ymin = 1.5-.25, ymax=3.5+.25,fill=NA, color = 'white', size = .75, linetype = 'dotted')+
    geom_rect(xmin=-.708, xmax = .708, ymin = 1.5, ymax=3.5, fill=NA, color = 'black', size = 1)+
    # geom_rect(xmin=-.708, xmax = .708, ymin = 1.5, ymax=3.5, fill=NA, color = 'white', size = .75)+
    geom_segment(data = tonybaseball::home_plate_c_pov, aes(x=x,y=y,xend=xend,yend=yend), inherit.aes = F,color='black', linewidth = 1) +
    # geom_segment(data = tonybaseball::home_plate_c_pov, aes(x=x,y=y,xend=xend,yend=yend), inherit.aes = F,color='white', linewidth = .75) +
    xlim(-2,2)+
    ylim(0,4.5)+
    labs(title = glue::glue("{umpire_name} Called Strike Probability"),
         subtitle = "Catcher POV",
         caption = glue::glue('>.5 red: {umpire_name} high called strike probability\n<.5 blue: {umpire_name} low called strike probability'))+
    # hrbrthemes::theme_ft_rc()+
    theme_minimal()+
    theme(legend.position = "bottom",
          legend.key.width = unit(1, "cm"))+
    coord_equal()+
    facet_wrap(~BatterSide)
  
  
  # umpire delta hex bin - pitcher pov
  # ump_csp_over_lg_p <- 
    ggplot(df_umpire %>% filter(zone_hscw %in% c('shadow','chase')), 
                              aes(x = PlateLocSide, y = PlateLocHeight, z = delta_csp)) +
    stat_summary_hex(fun = mean, bins = 11, alpha = 1) +
    scale_fill_gradient2(midpoint = 0, 
                         high = tonybaseball::savant_red, 
                         low = tonybaseball::savant_blue, 
                         # mid = tonybaseball::savant_mid,
                         mid = 'white',
                         name = "+/- xCalledStrike %") +
    coord_fixed() +
    labs(
      title = glue::glue("{umpire_name} - Called Strike% Over League Avg"),
      subtitle = 'Pitcher POV',
      caption = glue::glue('Red: {umpire_name} calls more strikes than FL average in that location\nBlue: {umpire_name} calls less strikes than FL average in that location')
    ) +
    geom_rect(xmin=-.708-.25, xmax = .708+.25, ymin = 1.5-.25, ymax=3.5+.25, fill=NA, color = 'black', size = 1, linetype = 'dotted')+
    # geom_rect(xmin=-.708-.25, xmax = .708+.25, ymin = 1.5-.25, ymax=3.5+.25,fill=NA, color = 'white', size = .75, linetype = 'dotted')+
    geom_rect(xmin=-.708, xmax = .708, ymin = 1.5, ymax=3.5, fill=NA, color = 'black', size = 1)+
    # geom_rect(xmin=-.708, xmax = .708, ymin = 1.5, ymax=3.5, fill=NA, color = 'white', size = .75)+
    geom_segment(data = tonybaseball::home_plate_p_pov, aes(x=x,y=y,xend=xend,yend=yend), inherit.aes = F, color='black', linewidth = 1) +
    # geom_segment(data = tonybaseball::home_plate_p_pov, aes(x=x,y=y,xend=xend,yend=yend), inherit.aes = F, color='white', linewidth = .75) +
    xlim(-2,2)+
    ylim(0,4)+
    # hrbrthemes::theme_ft_rc()+
    theme_minimal()+
    theme(legend.position = "bottom",
          legend.key.width = unit(1, "cm"),
          legend.key.height = unit(.25,'cm'))+
    coord_equal()+
    facet_wrap(~BatterSide)
  
  # umpire delta hex bin catcher pov
  
  # ump_csp_over_lg_c <- 
    ggplot(df_umpire %>% filter(zone_hscw %in% c('shadow','chase')), 
                              aes(x = -PlateLocSide, y = PlateLocHeight, z = delta_csp)) +
    stat_summary_hex(fun = mean, bins = 11, alpha = 1) +
    scale_fill_gradient2(midpoint = 0, 
                         high = tonybaseball::savant_red, 
                         low = tonybaseball::savant_blue, 
                         # mid = tonybaseball::savant_mid,
                         mid = 'white',
                         name = "+/- xCalledStrike %") +
    coord_fixed() +
    labs(
      title = glue::glue("{umpire_name} - Called Strike% Over League Avg"),
      subtitle = 'Catcher POV',
      caption = glue::glue('Red: {umpire_name} calls more strikes than FL average umpire in that location\nBlue: {umpire_name} calls less strikes than FL average umpire in that location')
    ) +
    geom_rect(xmin=-.708-.25, xmax = .708+.25, ymin = 1.5-.25, ymax=3.5+.25, fill=NA, color = 'black', size = 1, linetype = 'dotted')+
    # geom_rect(xmin=-.708-.25, xmax = .708+.25, ymin = 1.5-.25, ymax=3.5+.25,fill=NA, color = 'white', size = .75, linetype = 'dotted')+
    geom_rect(xmin=-.708, xmax = .708, ymin = 1.5, ymax=3.5, fill=NA, color = 'black', size = 1)+
    # geom_rect(xmin=-.708, xmax = .708, ymin = 1.5, ymax=3.5, fill=NA, color = 'white', size = .75)+
    geom_segment(data = tonybaseball::home_plate_c_pov, aes(x=x,y=y,xend=xend,yend=yend), inherit.aes = F, color='black', linewidth = 1) +
    # geom_segment(data = tonybaseball::home_plate_c_pov, aes(x=x,y=y,xend=xend,yend=yend), inherit.aes = F, color='white', linewidth = .75) +
    xlim(-2,2)+
    ylim(0,4)+
    # hrbrthemes::theme_ft_rc()+
    theme_minimal()+
    theme(legend.position = "bottom",
          legend.key.width = unit(1, "cm"),
          legend.key.height = unit(.25,'cm'))+
    coord_equal()+
    facet_wrap(~BatterSide)
  
  }


lg_umpires_summary <-
  data %>%
  group_by(Umpire) %>%
  summarise(Pitches = n(),
            StrikesCalled = sum(is_strike, na.rm = T),
            BallsCalled = sum(is_strike == 0, na.rm = T),
            IZ_pitches = sum(in_zone, na.rm = T),
            OZ_pitches = sum(in_zone == 0, na.rm = T),
            Z_strikes = sum(in_zone == 1 & is_strike == 1),
            O_strikes = sum(in_zone == 0 & is_strike == 1),
            Z_balls = sum(in_zone == 1 & is_strike == 0),
            O_balls = sum(in_zone == 0 & is_strike == 0),
            .groups = 'drop',
            
  ) %>%
  mutate(
    strikes_correct = Z_strikes / IZ_pitches,
    strikes_correct = Z_strikes / StrikesCalled,
    balls_correct = O_balls / OZ_pitches,
    balls_correct = O_balls / BallsCalled,
    calls_correct = Z_strikes + O_balls,
    overall_correct = (Z_strikes + O_balls) / Pitches
  )

lg_total <- data %>%
  summarise(Pitches = n(),
            StrikesCalled = sum(is_strike, na.rm = T),
            BallsCalled = sum(is_strike == 0, na.rm = T),
            IZ_pitches = sum(in_zone, na.rm = T),
            OZ_pitches = sum(in_zone == 0, na.rm = T),
            Z_strikes = sum(in_zone == 1 & is_strike == 1),
            O_strikes = sum(in_zone == 0 & is_strike == 1),
            Z_balls = sum(in_zone == 1 & is_strike == 0),
            O_balls = sum(in_zone == 0 & is_strike == 0),
            .groups = 'drop',
  ) %>%
  mutate(
    strikes_correct = Z_strikes / IZ_pitches,
    strikes_correct = Z_strikes / StrikesCalled,
    balls_correct = O_balls / OZ_pitches,
    balls_correct = O_balls / BallsCalled,
    calls_correct = Z_strikes + O_balls,
    overall_correct = (Z_strikes + O_balls) / Pitches
  )

umpire_summary <- lg_umpires_summary %>%
  filter(Umpire == umpire_name)

library(ggplot2)
library(dplyr)

percent <- umpire_summary$overall_correct[1] * 100


plot_data <- tibble(
  part = c("filled", "empty"),
  value = c(percent, 100 - percent)
)

ggplot(plot_data, aes(x = 1, y = value, fill = part)) +
  geom_col(width = .3, color = NA) +
  coord_polar(theta = "y", start = pi*2) +  # Rotate to 12 o'clock
  scale_fill_manual(values = c("filled" = "cornflowerblue", "empty" = "gray90")) +
  xlim(0, 1.15) +
  annotate(x=.3,y=0, geom = 'text', label = glue::glue("{round(percent)}%"), color = 'black', size = 18)+
  annotate(x=0,y=0, geom = 'text', label = glue::glue("Lg:{round(lg_total$overall_correct*100)}%"), color = 'black', size = 6)+
  labs(title = 'Overall Accuracy',
       caption = glue::glue("{umpire_summary$calls_correct} of {umpire_summary$Pitches} were called correctly")
  )+
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5, size = 24, face = 'bold'),
        plot.caption = element_text(hjust = .5, size = 12))

k_percent <- umpire_summary$strikes_correct[1] * 100
plot_data_k <- tibble(
  part = c("filled", "empty"),
  value = c(k_percent, 100 - k_percent)
)
ggplot(plot_data_k, aes(x = 1, y = value, fill = part)) +
  geom_col(width = .3, color = NA) +
  coord_polar(theta = "y", start = pi*2) +  # Rotate to 12 o'clock
  scale_fill_manual(values = c("filled" = "maroon", "empty" = "gray90")) +
  xlim(.0, 1.15) +
  annotate(x=.3,y=0, geom = 'text', label = glue::glue("{round(k_percent)}%"), color = 'black', size = 18)+
  annotate(x=.0,y=0, geom = 'text', label = glue::glue("Lg:{round(lg_total$strikes_correct*100)}%"), color = 'black', size = 5)+
  labs(title = 'Called Strike Accuracy',
       caption = glue::glue("{umpire_summary$O_strikes} of {umpire_summary$StrikesCalled} Called Strikes were true balls.")
  )+
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5, size = 24, face = 'bold'),
        plot.caption = element_text(hjust = .5, size = 12))

b_percent <- umpire_summary$balls_correct[1] * 100
plot_data_b <- tibble(
  part = c("filled", "empty"),
  value = c(b_percent, 100 - b_percent)
)
ggplot(plot_data_b, aes(x = 1, y = value, fill = part)) +
  geom_col(width = .3, color = NA) +
  coord_polar(theta = "y", start = pi*2) +  # Rotate to 12 o'clock
  scale_fill_manual(values = c("filled" = "forestgreen", "empty" = "gray90")) +
  xlim(0, 1.15) +
  annotate(x=.3,y=0, geom = 'text', label = glue::glue("{round(b_percent)}%"), color = 'black', size = 18)+
  annotate(x=.0,y=0, geom = 'text', label = glue::glue("Lg:{round(lg_total$balls_correct*100)}%"), color = 'black', size = 5)+
  labs(title = 'Called Ball Accuracy',
       caption = glue::glue("{umpire_summary$Z_balls} of {umpire_summary$BallsCalled} Called Balls were true strikes.")
       )+
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5, size = 24, face = 'bold'),
        plot.caption = element_text(hjust = .5, size = 12))


