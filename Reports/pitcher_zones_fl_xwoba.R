library(ggplot2)
db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")

data <- dbGetQuery(db, 'select PitchCall, PlateLocSide, PlateLocHeight, PitchUID, GameID from pitch_data 
                   where PlateLocSide is not null')
calc_zone_hscw <- function(data){
  data <- data %>%
    mutate(zone_hscw = case_when(
      between(PlateLocSide, -.458,.458) & between(PlateLocHeight, 1.75,3.25) ~ 'heart',
      between(PlateLocSide, -.958,.958) & between(PlateLocHeight, 1.25,3.75) ~ 'shadow',
      between(PlateLocSide, -1.458,1.458) & between(PlateLocHeight, .75,4.25) ~ 'chase',
      T ~'waste'
    )
    )
  return(data)
}

data_2 <- data %>%
  calc_zone_hscw()

t <- data_2 %>%
  dplyr::slice(1:500)

heart <- data_2 %>%
  dplyr::filter(zone_hscw=='heart')

ggplot(t, aes(x=-PlateLocSide, y= PlateLocHeight, color = zone_hscw))+
  xlim(-2.5, 2.5) +
  ylim(-.5, 4.5) +
  geom_point(size=4, alpha =  .7)+# Home plate
  geom_segment(data = tonybaseball::home_plate_c_pov, 
               aes(x = x, y = y-.5, xend = xend, yend = yend-.5), inherit.aes = F)+
  # K Zone
  geom_rect(
    aes(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5),color = 'black',
    alpha = 0, size = 1, linetype = "dotted" ) +
  # Heart
  geom_rect(
    aes(xmin = -0.458, xmax = .458, ymin = 1.75, ymax = 3.25,color = 'heart'),
    alpha = 0, size = 1, linetype = "solid" ) +
  # Shadow
  geom_rect(
    aes(xmin = -.958, xmax = .958, ymin = 1.25, ymax = 3.75,color = 'shadow'),
    alpha = 0, size = 1, linetype = "solid" ) +
  # Chase
  geom_rect(
    aes(xmin = -1.458, xmax = 1.458, ymin = .75, ymax = 4.25, color = 'chase'),
    alpha = 0, size = 1, linetype = "solid" ) + # Manual color scale
  # scale_color_manual(
  #   name = "Zone Type",
  #   values = c("K Zone" = "black", "Heart" = "red", "Shadow" = "blue", 'Chase' = 'green') ) +
  # Plot limits, aspect ratio
  
  # scale_x_continuous(breaks = seq(-1.2, 1.2, by = 1), limits = c(-1.2, 1.2)) +
  # scale_y_continuous(breaks = seq(0, 4, by = 1), limits = c(0, 4))+
  coord_equal() +
  theme_void() +
  # Add a circle at (0, 2.5) with radius 1.5
  annotate("path",
           x = 1.5/12 * cos(seq(0, 2 * pi, length.out = 100)),
           y = 2.5 + 1.5/12 * sin(seq(0, 2 * pi, length.out = 100)),
           color = "red", linewidth = 1)+
  annotate("path",
           x = 1.5/12 * cos(seq(0, 2 * pi, length.out = 100)),
           y = 2.75 + 1.5/12 * sin(seq(0, 2 * pi, length.out = 100)),
           color = "blue", linewidth = 1)+
  annotate("path",
           x = 1.5/12 * cos(seq(0, 2 * pi, length.out = 100)),
           y = 2.25 + 1.5/12 * sin(seq(0, 2 * pi, length.out = 100)),
           color = "green", linewidth = 1)+
  annotate("path",
           x = .25 + 1.5/12 * cos(seq(0, 2 * pi, length.out = 100)),
           y = 2.75 + 1.5/12 * sin(seq(0, 2 * pi, length.out = 100)),
           color = "purple", linewidth = 1)+
  annotate("path",
           x = .58 + 1.5/12 * cos(seq(0, 2 * pi, length.out = 100)),
           y = 2.75 + 1.5/12 * sin(seq(0, 2 * pi, length.out = 100)),
           color = "purple", linewidth = 1)+
  annotate("path",
           x = .833 + 1.5/12 * cos(seq(0, 2 * pi, length.out = 100)),
           y = 2.75 + 1.5/12 * sin(seq(0, 2 * pi, length.out = 100)),
           color = "purple", linewidth = 1)




h <- dbGetQuery(db, 'select ExitSpeed, Angle, xwOBA from pitch_data 
                   where --season = 2024 and 
                bbe = 1 and TaggedHitType <> "Bunt"')

hit <-  h %>%
  mutate(launch_speed = round(ExitSpeed),
         launch_angle = round(Angle),
         ev_range = case_when(
           between(launch_speed, -Inf, 60)  ~ 50,
           between(launch_speed, 60, 70)  ~ 60,
           between(launch_speed, 70, 80)  ~ 70,
           between(launch_speed, 80, 90)  ~ 80,
           between(launch_speed, 90, 100)  ~ 90,
           between(launch_speed, 100, 110)  ~ 100,
           between(launch_speed, 110, Inf)  ~ 110,
         ),
         la_range = case_when(
           between(launch_angle, 0, 10)  ~ 0,
           between(launch_angle, 10, 20)  ~ 10,
           between(launch_angle, 20, 30)  ~ 20,
           between(launch_angle, 30, 40)  ~ 30,
           between(launch_angle, 40, 50)  ~ 40,
           between(launch_angle, 50, Inf)  ~ 50,
           between(launch_angle, -10, 0)  ~ -10,
           between(launch_angle, -20, -10)  ~ -20,
           between(launch_angle, -30, -20)  ~ -30,
           between(launch_angle, -Inf, -30)  ~ -40,
           
         ),
         Xcoord = ev_range / 120 *
           cos(la_range * pi / 180),
         Ycoord = ev_range / 120 *
           sin(la_range * pi / 180), .before = 1
  ) %>%
  group_by(ev_range, la_range) %>%
  summarise(n = n(),
            xwoba_sum = sum(xwOBA, na.rm = T)) %>%
  mutate(xwOBA = round(xwoba_sum/n,3),
         # Xcoord = ev_range / 120 *
         #   cos(la_range * pi / 180),
         # Ycoord = ev_range / 120 *
         #   sin(la_range * pi / 180), .before = 1,
         # la_range = ifelse(la_rnge < 0, la_range + 360, la_range)
  )%>%
  mutate(la_angle = ifelse(la_range < 0, la_range + 360, la_range),
         # la_angle = la_range + 180
  )


img <- readPNG("C:/Users/tdmed/OneDrive/Pictures/baseball-png-35353.png")  # Replace with your image path
logo <- rasterGrob(img, interpolate = TRUE)

logo_df <- data.frame(
  la_angle = 0,     # 0° launch angle
  ev_range = 50,    # inner radius (center of polar plot)
  image = "C:/Users/tdmed/OneDrive/Pictures/baseball-png-35353.png"
)



ggplot(hit, aes(x = la_angle+5, y = ev_range, fill = xwOBA)) +
  geom_tile() +
  scale_fill_gradient2(low = '#3a64af', mid = 'lightgrey', high = '#d82129', midpoint = .7) +
  coord_polar(theta = "x", start = -pi/2) +  # ← THIS puts 0° at 3 o’clock
  scale_x_continuous(
    breaks = seq(320, 360, by = 10) %>% c(seq(0, 50, by = 10)),  limits = c(0, 360),
    labels = function(x) ifelse(x >= 310, x - 360, x)   ) +
  scale_y_continuous(  breaks = seq(60, 120, by = 10), limits = c(60, 120)) +
  theme_minimal(base_size = 14) +
  theme( panel.grid = element_blank(), axis.title = element_blank() ) +
  geom_text(data = hit %>% filter (ev_range>60) ,
            aes(x=la_angle+5, y=ev_range, label = xwOBA, angle = -la_angle), fontface = 'bold', size = 4.5) +
  geom_segment(x=0, xend=0, y=60, yend=120)+
  geom_segment(x=10, xend=10, y=60, yend=120)+
  geom_segment(x=20, xend=20, y=60, yend=120)+
  geom_segment(x=30, xend=30, y=60, yend=120)+
  geom_segment(x=40, xend=40, y=60, yend=120)+
  geom_segment(x=50, xend=50, y=60, yend=120)+
  geom_segment(x=60, xend=60, y=60, yend=120)+
  geom_segment(x=-10, xend=-10, y=60, yend=120)+
  geom_segment(x=-20, xend=-20, y=60, yend=120)+
  geom_segment(x=-30, xend=-30, y=60, yend=120)+
  geom_segment(x=-40, xend=-40, y=60, yend=120)+
  geom_segment(x=-40, xend=70, y=115, yend=115)+
  geom_segment(x=-40, xend=70, y=105, yend=105)+
  geom_segment(x=-40, xend=70, y=95, yend=95)+
  geom_segment(x=-40, xend=70, y=85, yend=85)+
  geom_segment(x=-40, xend=70, y=75, yend=75)+
  geom_segment(x=-40, xend=85, y=65, yend=65)+
  annotate(x=80, y = 105, geom = 'text', label ='110+mph')+
  annotate(x=85, y = 95, geom = 'text', label ='100 mph')+
  annotate(x=90, y = 85, geom = 'text', label ='90 mph')+
  annotate(x=100, y = 75, geom = 'text', label ='80 mph') +
  annotate(x=140, y = 68, geom = 'text', label ='70- mph')


library(ggimage)
