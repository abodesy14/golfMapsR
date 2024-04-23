{
library(shiny)
library(shinyWidgets)
library(shinybusy)
library(tidyverse)
library(dplyr)
}

server <- function(input, output, session) {
  
  output$map <- renderPlot({
    map_subset <- subset(geojson_df, course_name %in% input$course)
  
    ggplot() +
      geom_sf(data = map_subset, aes(fill = color), color = "black") + 
      #geom_sf(data = points, color = "black", size = 3, shape = 20) +   # Points
      # regex to display hole number on green
      geom_text(data = filter(map_subset, grepl("green", polygon_name)), 
                aes(x = st_coordinates(centroid)[, 1], 
                    y = st_coordinates(centroid)[, 2], 
                    label = gsub(".*_hole_(\\d+)_.*", "\\1", polygon_name)), 
                size = 3, color = "black", fontface = "bold", hjust = 0.5, vjust = 0.5) +
      scale_fill_identity(name = "Color", guide = "legend", labels = levels(map_subset$polygon_type)) + # specify legend fill
      theme_minimal() + # remove background and grid lines
      theme(axis.title.x = element_blank(), # remove x-axis label
            axis.title.y = element_blank(), # remove y-axis label
            axis.text.x = element_blank(), # remove x-axis text
            axis.text.y = element_blank(), # remove y-axis text
            panel.grid.major = element_blank(), # remove major grid lines
            panel.grid.minor = element_blank()) + # remove minor grid lines
      theme(legend.position = "none") +
      labs(title = paste0(map_subset$course_name, " | ", map_subset$city, " , ", map_subset$state))
  
  })
}

shinyApp(ui, server)