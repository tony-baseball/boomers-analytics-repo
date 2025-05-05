library(shiny)
library(plotly)
library(base64enc)
library(png)
library(grid)

clicked_points <- data.frame(
  Batter = character(),
  HitType = character(),
  hc_x = numeric(),
  hc_y = numeric(),
  Distance = numeric(),
  Bearing = numeric()
  
)

ui <- fluidPage(
  fluidRow(align = 'center', column(6,
                                    plotOutput('hit_plot', height = "800px", width = "200%", click = "hit_click")
  )),
  fluidRow(
    column(3, textInput(inputId = 'hitter', label = 'Hitter')),
    column(3, selectInput(inputId = 'hittype', label = 'HitType', choices = c('','FlyBall', 'LineDrive', 'GroundBall', 'PopUp', 'Bunt'))),
    column(3, verbatimTextOutput("hit_info"))
  )
)
add_image <- function(img_grob, center_x = 0, center_y = 0, width = 100, height = 100) {
  annotation_custom(
    img_grob,
    xmin = center_x - width/2,
    xmax = center_x + width/2,
    ymin = center_y - height/2,
    ymax = center_y + height/2
  )
}
server <- function(input, output, session) {
  # img_path <- "C:/Users/tdmed/Downloads/livermore.png"
  img_path <- "C:/Users/tdmed/OneDrive/_Github/boomers-fl-automations/Reports/_other/clicked_points_downeast.png"
  img <- png::readPNG(img_path)
  
  img_grob <- rasterGrob(img,
                         width  = unit(1, "npc"),
                         height = unit(1, "npc"))
  
  output$hit_plot <- renderPlot({ 
    
    scale <- .6
    ggplot() +
      # annotation_custom(img_grob, -270, 270, -70, 400) +
      add_image(img_grob, center_x = 2.5, center_y = 180, width = 870*scale, height = 736*scale)+
      # annotation_custom(img_grob, -350, 270, -20, 440) +
      geom_mlb_stadium(
        stadium_ids = 'braves',
        stadium_transform_coords = TRUE,
        stadium_segments = c('home_plate', 'foul_lines', 'infield_outer', 'infield_inner', 'outfield_outer'),
        size = 1,
        color = 'white'
      ) +
      
      scale_y_continuous(
        breaks = seq(0, 420, by = 10),
        limits = c(-50, 420),
        expand = c(0, 0)
      )+
      coord_fixed()+
      geom_hline(yintercept = 385, color ='white', size = 1)+
      geom_point(aes(x = 0, y = 385), size = 5, fill = 'grey', pch = 21)
  })
  
  observeEvent(input$hit_click, {
    click_info <- input$hit_click
    
    # Append to global dataframe
    clicked_points <<- rbind(clicked_points, data.frame(
      Batter = input$hitter,
      HitType = input$hittype,
      hc_x = round(click_info$x, 3),
      hc_y = round(click_info$y, 3)
    ) %>% mutate(
      Distance = sqrt(hc_x^2 + hc_y^2),
      Bearing = atan2(hc_x, hc_y) * 180 / pi
    )
    )
    
    # Show text
    output$hit_info <- renderText({
      paste("x:", round(click_info$x, 3), "y:", round(click_info$y, 3))
    })
  })
  
  onStop(function() {
    hitter <- gsub(' ', '_', isolate(input$hitter))  # <-- isolate() instead of reactive({})
    write.csv(clicked_points, glue::glue("clicked_points_{hitter}.csv"), row.names = FALSE)
  })
}
tonybaseball::tm_transform_file
shinyApp(ui, server)
