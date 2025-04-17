suppressWarnings(suppressMessages({
  library(rvest)
  library(plyr)
  library(dplyr)
  library(xml2)
  library(XML)
  library(magrittr)
  library(lubridate)
  library(data.table)
  library(stringr)
  library(discordr)
  library(emojifont)
  library(tidyverse)
  library(slackr)
  library(RSQLite)
  # conn_obj <- create_discord_connection(webhook = 'https://discordapp.com/api/webhooks/1125539076670492732/yM9X7S4SGSJZeW9G0S-k_TTjwHHfyEHjeJJYypaTacsRgddDi8sApN2VHkal_I895MC8', 
  #                                       username = 'BoomerBot', set_default = TRUE)
  # slackr_setup(channel = "#general", username = "FLASHBot", icon_emoji = '', incoming_webhook_url = 'https://hooks.slack.com/services/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', token = 'xoxb-7165085228646-7165145332742-Sf8B6vtmbPmiZLQsFfvmNWIM', echo = FALSE)
  # 
  
  
}))

full_cal <- data.frame(Date = seq(from = as.Date('2024-05-10'), to = as.Date('2024-09-01'), by = "day"))



# db <- dbConnect(SQLite(),"C:/Users/tdmed/OneDrive/_Trackman/frontier_league.sqlite")
teams <- dbGetQuery(db, 'select * from teams')
team_filter <- c('Schaumburg Boomers', team_filter)
todays_date <- as.Date('2024-08-25')

plot_list <- list()

for(team in team_filter) {
  
  rosters <- dbGetQuery(db, glue::glue("SELECT NAME, THROWS FROM rosters_ where TEAM = '{team}' and SEASON = 2024 and POSITION like '%P%'"))
  
  bullpen_log <- full_cal %>%
    left_join(dbGetQuery(db, 'select * from bullpen_log')%>%
                mutate(Date = as.Date(Date)) %>%
                mutate(Team = gsub('Bolts','bolts', Team)) %>%
                filter(Team %in%  team), by = 'Date')   %>%
    filter(between(Date, todays_date - 3, todays_date) ) %>%
    mutate(Pitcher = gsub('Kevin Pindel', 'Buddie Pindel', Pitcher))%>%
    left_join(rosters, by = c('Pitcher' = 'NAME')) %>%
    mutate(Pitcher = glue::glue('{Pitcher} ({THROWS})'))
  
  bp_log <- bullpen_log %>%
    group_by(Pitcher, Team) %>%
    summarise(#Role = unique(P),
              Yest = sum(Pitches[Date==todays_date-1]),
              Last3 = sum(Pitches),
              diff = Last3 - Yest) %>%
    mutate(color1 = teams$color1[match(Team,teams$team_FL)],
           color2 = teams$color2[match(Team,teams$team_FL)],
           color2 = ifelse(color2 == '#ffffff', teams$color3[match(Team,teams$team_FL)], color2)
    ) 

  bp_long <- bp_log%>%
    pivot_longer(cols = c(Yest, diff), names_to = "Metric", values_to = "Pitches") %>%
    mutate(FillColor = ifelse(Metric == "Yest", color1, color2)) %>%
    group_by(Pitcher) %>%
    mutate(total_p = sum(Pitches),
           p_fact = as.factor(total_p)) %>%
    ungroup() %>%
    arrange(desc(total_p)) %>%
    filter(!is.na(Pitcher))
  
  color1<- unique(bp_log$color1[!is.na(bp_log$Team)])
  color2<- unique(bp_log$color2[!is.na(bp_log$Team)])
  # plotly::ggplotly(
  # COLORS
  plot_list[[team]] <-
    ggplot(bp_long, aes(y = reorder(Pitcher, desc(total_p)), x = Pitches, fill = Metric)) +
    geom_bar(stat = "identity", position = "stack", color = 'white', width = .75) +
    labs(title = "Bullpen Usage in Last 3 Days",
         y = "Pitcher",
         x = "Pitches",
         fill = "") +
    scale_fill_manual(values = c("Yest" = color1, "diff" = color2), 
                      labels = c("Yest" = "Yesterday", "diff" = "Last 3 Days Total")) +
    geom_text(data = bp_long %>% filter(Metric == 'Yest' & Pitches > 0),
              aes(label = Pitches, x = Pitches / 2),
              color = "white", size = 5, fontface = 'bold') +
    geom_text(data = bp_long %>% filter(Metric == 'diff' & Pitches > 0),
              aes(label = total_p, x = total_p - (Pitches / 2)),
              color = "white", size = 5, fontface = 'bold') +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12, face = 'bold'),
          axis.text.y = element_text(size = 12, face = 'bold', margin = margin(r=-9)),
          plot.title = element_text(hjust = 0.25, color = 'black', face = 'bold'),
          plot.subtitle = element_text(hjust = 0.25, color = 'black', face = 'bold'),
          panel.grid.major.y = element_blank(),
          legend.position = 'top',
          legend.key.size = unit(.3, 'cm'),
          legend.margin=margin(c(0,0,-12,0)),
          legend.text = element_text(size = 8, color = 'black', face = 'bold'),
          axis.text = element_text(color = 'black', face = 'bold'),
          axis.title = element_text(color = 'black', face = 'bold'),
          panel.border = element_blank(),  
          strip.text = element_text(colour = 'black', size = 11),
          axis.ticks.y = element_blank(),
    ) + 
    facet_wrap(~Team, nrow = 2, scales = 'free_x')
  

}
cur_date_time <- paste("As of",  format(as.POSIXct(Sys.time(), format = "%Y-%m-%d %H:%M:%S", tz = "CST6CDT"), "%Y-%m-%d at %I:%M:%S %p CT"))

params_bp <- list(
  plot_list = plot_list,
  away_team_loc = opposing_team_location,
  opposing_team = team,
  cur_date_time = cur_date_time
)
plot_list[1]
plot_list[2]


file_name <- paste0(opposing_team_code,"_Bullpen")
# date_of_game <- paste(date_of_game)
rmd_file <-  "C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/adv_bullpen_report.Rmd"
file_save <- paste0("C:/Users/tdmed/OneDrive/_Advance/",opposing_team_code,"/", file_name ,"_report.pdf")
rmd_file
file_save

suppressWarnings({ # RUN----
  rmarkdown::render(input = rmd_file, 
                    output_file = file_save,
                    params = params_bp)
})


file.copy(file_save, 
          to = paste0("C:/Users/tdmed/OneDrive/_Advance/",opposing_team_code))

  # ggsave(plot = ggplot2::last_plot(), filename = paste0('C:/Users/tdmed/OneDrive/_Advance/Bullpen ',team,'.png'),
  #        width = 3.75, height = 2.5, dpi = 300)
  
# SKIP ----
# for(team in team_filter) {
#   
#   bullpen_log <- dbGetQuery(db, 'select * from bullpen_log') %>%
#     mutate(Team = gsub('Bolts','bolts', Team)) %>%
#     filter(Team %in%  team,
#            P != 'SP') %>%
#     mutate(Date = as.Date(Date)) %>%
#     filter(between(Date, Sys.Date() - 3, Sys.Date()) ) %>%
#     group_by(Pitcher, Team) %>%
#     summarise(Role = unique(P),
#               Yest = sum(Pitches[Date==Sys.Date()-1]),
#               Last3 = sum(Pitches),
#               diff = Last3 - Yest) %>%
#     mutate(color1 = teams$color1[match(Team,teams$team_FL)],
#            color2 = teams$color2[match(Team,teams$team_FL)],
#            color2 = ifelse(color2 == '#ffffff', teams$color3[match(Team,teams$team_FL)], color2)
#     ) 
#   # %>%
#   #   filter(Team == 'Schaumburg Boomers') 
#   
#   bp_long <- bullpen_log%>%
#     pivot_longer(cols = c(Yest, diff), names_to = "Metric", values_to = "Pitches") %>%
#     mutate(FillColor = ifelse(Metric == "Yest", color1, color2)) %>%
#     group_by(Pitcher) %>%
#     mutate(total_p = sum(Pitches)) %>%
#     ungroup()
#   
#   color1<- unique(bullpen_log$color1)
#   color2<- unique(bullpen_log$color2)
#   # plotly::ggplotly(
#   # COLORS
#   plot <- ggplot(bp_long, aes(y = Pitcher, x = Pitches, fill = Metric)) +
#     geom_bar(stat = "identity", position = "stack", color = 'white') +
#     labs(title = "Bullpen Usage in Last 3 Days",
#          x = "Pitcher",
#          y = "Pitches",
#          fill = "") +
#     scale_fill_manual(values = c("Yest" = color1, "diff" = color2), 
#                       labels = c("Yest" = "Yesterday", "diff" = "Last 3 Days")) +
#     geom_text(data = bp_long %>% filter(Metric == 'Yest' & Pitches > 0),
#               aes(label = Pitches, x = Pitches / 2),
#               color = "white", size = 5, fontface = 'bold') +
#     geom_text(data = bp_long %>% filter(Metric == 'diff' & Pitches > 0),
#               aes(label = total_p, x = total_p - (Pitches / 2)),
#               color = "white", size = 5, fontface = 'bold') +
#     theme_minimal() +
#     theme(axis.text.x = element_text(size = 12, face = 'bold'),
#           axis.text.y = element_text(size = 12, face = 'bold', margin = margin(r=-9)),
#           plot.title = element_text(hjust = 0.5, face = 'bold'),
#           plot.subtitle = element_text(hjust = 0.5,  face = 'bold'),
#           panel.background = element_rect(fill = 'white', color = NA),
#           plot.background = element_rect(fill = 'white', color = NA),
#           panel.grid.major.y = element_blank(),
#           legend.position = 'top',
#           legend.key.size = unit(.3, 'cm'),
#           legend.margin=margin(c(0,0,-12,0)),
#           legend.text = element_text(size = 8,  face = 'bold'),
#           axis.text = element_text( face = 'bold'),
#           axis.title = element_text( face = 'bold'),
#           panel.border = element_blank(),  
#           strip.text = element_text( size = 11),
#           axis.ticks.y = element_blank(),
#     ) + 
#     facet_wrap(~Team, nrow = 2, scales = 'free_x')
#   
#   plot
#   
#   ggsave(plot = last_plot(), filename = paste0('C:/Users/tdmed/OneDrive/_Advance/Bullpen ',team,'_.png'),
#          width = 3.75, height = 2.5, dpi = 300)
# }
# # -----------------------
# bullpen_log <- dbGetQuery(db, 'select * from bullpen_log') %>%
#   mutate(Team = gsub('Bolts','bolts', Team)) %>%
#   filter(Team %in% c('Schaumburg Boomers', team_filter),
#          P != 'SP') %>%
#   mutate(Date = as.Date(Date)) %>%
#   filter(between(Date, Sys.Date() - 3, Sys.Date()) ) %>%
#   group_by(Pitcher, Team) %>%
#   summarise(Role = unique(P),
#             Yest = sum(Pitches[Date==Sys.Date()-1]),
#             Last3 = sum(Pitches),
#             diff = Last3 - Yest) %>%
#   mutate(color1 = teams$color1[match(Team,teams$team_FL)],
#          color2 = teams$color2[match(Team,teams$team_FL)],
#          color2 = ifelse(color2 == '#ffffff', teams$color3[match(Team,teams$team_FL)], color2)
#   ) 
# # %>%
# #   filter(Team == 'Schaumburg Boomers') 
# 
# bp_long <- bullpen_log%>%
#   pivot_longer(cols = c(Yest, diff), names_to = "Metric", values_to = "Pitches") %>%
#   mutate(FillColor = ifelse(Metric == "Yest", color1, color2)) %>%
#   group_by(Pitcher) %>%
#   mutate(total_p = sum(Pitches)) %>%
#   ungroup()
# 
# color1<- unique(bullpen_log$color1)
# color2<- unique(bullpen_log$color2)
# # plotly::ggplotly(
# # COLORS
# ggplot(bp_long, aes(x = Pitcher, y = Pitches, fill = Metric)) +
#   geom_bar(stat = "identity", position = "stack", color = 'white') +
#   labs(title = "Bullpen Usage in Last 3 Days",
#        x = "Pitcher",
#        y = "Pitches",
#        # fill = "Metric") +
#   scale_fill_manual(values = c("Yest" = color1, "diff" = color2), 
#                     labels = c("Yest" = "Yesterday", "diff" = "Last 3 Days")) +
#   geom_text(data = bp_long %>% filter(Metric == 'Yest' & Pitches > 0),
#             aes(label = Pitches, y = Pitches / 2),
#             color = "white", size = 5, fontface = 'bold') +
#   geom_text(data = bp_long %>% filter(Metric == 'diff' & Pitches > 0),
#             aes(label = total_p, y = total_p - (Pitches / 2)),
#             color = "white", size = 5, fontface = 'bold') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
#         plot.title = element_text(hjust = 0.5, color = 'white', face = 'bold'),
#         plot.subtitle = element_text(hjust = 0.5, color = 'white', face = 'bold'),
#         panel.background = element_rect(fill = color1, color = NA),  
#         plot.background = element_rect(fill = color1, color = NA),  
#         panel.grid.major.x = element_blank(),
#         legend.position = 'right',
#         legend.text = element_text(color = 'white', face = 'bold'),
#         axis.text = element_text(color = 'white', face = 'bold'),
#         axis.title = element_text(color = 'white', face = 'bold'),
#         panel.border = element_blank(),  
#         # plot.margin = margin(5, 5, 5, 5)
#   ) + 
#   facet_wrap(~Team, nrow = 2, scales = 'free_x')
# 
# 
# # SIMPLE
# ggplot(bp_long, aes(x = Pitcher, y = Pitches, fill = Metric)) +
#   geom_bar(stat = "identity", position = "stack", color = 'white') +
#   labs(title = "Bullpen Usage in Last 3 Days",
#        x = "Pitcher",
#        y = "Pitches",
#        fill = "Metric") +
#   scale_fill_manual(values = c("Yest" = color1, "diff" = color2), 
#                     labels = c("Yest" = "Yesterday", "diff" = "Last 3 Days")) +
#   geom_text(data = bp_long %>% filter(Metric == 'Yest' & Pitches > 0),
#             aes(label = Pitches, y = Pitches / 2),
#             color = "white", size = 5, fontface = 'bold') +
#   geom_text(data = bp_long %>% filter(Metric == 'diff' & Pitches > 0),
#             aes(label = total_p, y = total_p - (Pitches / 2)),
#             color = "white", size = 5, fontface = 'bold') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
#         plot.title = element_text(hjust = 0.5,  face = 'bold'),
#         plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
#         # panel.background = element_rect(fill = '#f47b29', color = NA),  
#         # plot.background = element_rect(fill = '#f47b20', color = NA),  
#         panel.grid.major.x = element_blank(),
#         legend.position = 'right',
#         legend.text = element_text(face = 'bold'),
#         axis.text = element_text( face = 'bold'),
#         axis.title = element_text(face = 'bold'),
#         panel.border = element_blank(),  
#         # plot.margin = margin(5, 5, 5, 5)
#   ) + 
#   facet_wrap(~Team, nrow = 2, scales = 'free_x')
# 
# 
# 
# 
# 
# 
# 
# 
# # )
# gradient_background <- function(grad_colors = c("white", "#f47b29"), n = 100) {
#   gradient <- colorRampPalette(grad_colors)(n)
#   n <- length(gradient)
#   rectGrob(
#     x = unit(seq(1/n/2, 1, length.out = n), "npc"),
#     y = unit(rep(1/2, n), "npc"),
#     width = unit(1/n, "npc"),
#     height = unit(1, "npc"),
#     just = "centre",
#     gp = gpar(col = NA, fill = gradient)
#   )
# }
# library(grid)
# ggplot(bp_long, aes(x = Pitcher, y = Pitches, fill = Metric)) +
#   annotation_custom(gradient_background(), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
#   geom_bar(stat = "identity", position = "stack", color = 'white') +
#   scale_fill_manual(values = c("Yest" = color1, "diff" = color2), 
#                     labels = c("Yest" = "Yesterday", "diff" = "Last 3 Days")) +
#   labs(title = "Bullpen Usage in Last 3 Days",
#        x = "Pitcher",
#        y = "Pitches",
#        fill = "Metric") +
#   geom_text(data = bp_long %>% filter(Metric == 'Yest' & Pitches > 0),
#             aes(label = Pitches, y = Pitches / 2),
#             color = "white", size = 5, fontface = 'bold') +
#   geom_text(data = bp_long %>% filter(Metric == 'Last3' & Pitches > 0),
#             aes(label = Pitches, y = Pitches - (Pitches / 2)),
#             color = "white", size = 5, fontface = 'bold') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5, color = 'white'),
#         plot.subtitle = element_text(hjust = 0.5, color = 'white'),
#         panel.background = element_rect(fill = NA),
#         plot.background = element_rect(fill = NA),
#         panel.grid.major.x = element_blank(),
#         legend.position = 'right',
#         legend.text = element_text(color = 'white'),
#         axis.text = element_text(color = 'white'),
#         axis.title = element_text(color = 'white')) +
#   facet_wrap(~Team, nrow = 2, scales = 'free_x')
