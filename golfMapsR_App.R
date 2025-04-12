# load libraries
{
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(stringr)  
  library(purrr)
  library(shiny)
  library(shinyWidgets)
  library(shinybusy)
  library(shinythemes)
  library(gt)
  library(gtExtras)
  library(base64enc)
  library(magick)
}

geojson_df <- readRDS("data/geojson_df.rds")

# ui
ui <- fluidPage(
  add_busy_spinner(spin = "fading-circle"),
  theme = shinythemes::shinytheme("simplex"), 
  navbarPage(
    "golfMapsR",
    fluidRow(
      column(width = 3, 
             selectInput("course", "Choose Course:", 
                         choices = unique(geojson_df$course_name_concat), 
                         selected = "Erin Hills - Hartford, WI", 
                         multiple = FALSE))
    ),
    tabPanel("Course Viewer", 
             fluidRow(
               column(width = 9, 
                      plotOutput("map", width = "100%", height = "90vh"))
             )
    ),
    tabPanel("Hole by Hole",
             fluidRow(
               column(width = 12, 
                      gt_output("hole_by_hole"))
             )
    )
  )
)

# server
server <- function(input, output, session) {
  
  output$map <- renderPlot({
    map_subset <- geojson_df %>% subset(course_name_concat == input$course)
    
    # generate ggplot map
    ggplot() +
      # plot all polygons first
      geom_sf(data = map_subset, aes(fill = color), color = "black") +
      # overlay greens on top
      geom_sf(data = filter(map_subset, grepl("_green$", polygon_name)), 
              aes(geometry = geometry), fill = "#86D14A", color = "black") +
      # add hole numbers for greens
      geom_text(data = filter(map_subset, grepl("_green$", polygon_name)), 
                aes(x = st_coordinates(st_centroid(geometry))[,1], 
                    y = st_coordinates(st_centroid(geometry))[,2], 
                    label = hole_num), 
                size = 3, color = "black", fontface = "bold", hjust = 0.5, vjust = 0.5) +
      scale_fill_identity() +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        plot.title = element_text(size = 16, family = "Big Caslon"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
        theme(legend.position = "none") + 
      labs(title = paste0(map_subset$course_name, " | ", map_subset$city, ", ",  map_subset$state))
  })
  
  
  output$hole_by_hole <- render_gt({
    hbh_subset <- geojson_df %>% filter(course_name_concat == input$course)
    
    # fix any invalid geometries
    hbh_subset <- st_make_valid(hbh_subset)
    
    # initialize the gt data frame
    gt_data <- data.frame(
      course_name = character(),
      hole_num = numeric(),
      plot_path = character(),
      bunker_count = numeric(),
      green_sq_ft = numeric(),
      distance_to_green_yards = numeric(),
      stringsAsFactors = FALSE
    )
    
    # loop through each unique hole
    unique_holes <- unique(hbh_subset$hole_num)
    for (hole in unique_holes) {
      # subset data for the current hole
      geojson_df_hole <- hbh_subset %>% filter(hole_num == hole)
      
      # count bunkers and calculate green size
      bunker_count <- sum(grepl("bunker", geojson_df_hole$course_element, ignore.case = TRUE))
      green_sq_ft <- prettyNum(round(sum(geojson_df_hole$total_sq_ft[grepl("green", geojson_df_hole$course_element, ignore.case = TRUE)])), big.mark = ",")
      distance_to_green_yards <- round(max(geojson_df_hole$distance_to_green_yards, na.rm = TRUE))
      
      # plot the hole layout
      plot <- ggplot() +
        # draw all course elements
        geom_sf(data = geojson_df_hole, aes(fill = color), color = "black") +
        # overlay greens
        geom_sf(
          data = geojson_df_hole %>% filter(grepl("green", course_element, ignore.case = TRUE)),
          aes(geometry = geometry),
          fill = "#86D14A", color = "black"
        ) +
        scale_fill_identity() +
        theme_void()
      
      # save plot to a temporary file
      filename <- tempfile(fileext = ".png")
      ggsave(filename, plot, width = 3, height = 3, units = "in")
      
      # encode the plot as a base64 string
      plot_path <- base64encode(readBin(filename, "raw", file.info(filename)$size))
      
      # add to df to be referenced in gt table
      gt_data <- rbind(gt_data, data.frame(
        course_name = geojson_df_hole$course_name[1],
        hole_num = hole,
        plot_path = plot_path,
        bunker_count = bunker_count,
        green_sq_ft = green_sq_ft,
        distance_to_green_yards = distance_to_green_yards,
        stringsAsFactors = FALSE
      ))
    }
    
    # convert hole_num to numeric for sorting
    gt_data$hole_num <- as.integer(gt_data$hole_num)
    
    # sort df so gt table can plot holes chronologically
    gt_data <- gt_data %>% arrange(hole_num)
 
    # create and render gt table
    gt_table <- gt_data %>%
      gt() %>%
      tab_header(
        title = md(paste0("**", input$course, "**")),
        subtitle = md(paste0(
        "Avg. Green Size: **", prettyNum(round(mean(as.numeric(gsub(",", "", gt_data$green_sq_ft)), na.rm = TRUE)), big.mark = ","), " sq ft**",
        " | ",
        "Bunkers: **", round(sum(as.numeric(gsub(",", "", gt_data$bunker_count)), na.rm = TRUE)), "**",
        " | ",
        "Trace Date: **", unique(hbh_subset$trace_date), "**",
        "<br>",
        "<i>Est. Distance: The straight-line yards from the furthest tee to the green for each hole.</i>"))
        ) %>%
      text_transform(
        locations = cells_body(columns = c(plot_path)),
        fn = function(x) {
          lapply(x, function(img) {
            html(sprintf('<img src="data:image/png;base64,%s" height="100px">', img))
          })
        }
      ) %>%
      cols_label(
        course_name = "Course",
        hole_num = "Hole #",
        plot_path = "Hole Layout",
        bunker_count = "Bunkers",
        green_sq_ft = "Est. Green Size (Sq Ft)",
        distance_to_green_yards = "Est. Distance"
      ) %>%
      tab_style(
        style = cell_text(align = "center"), 
        locations = cells_body(columns = c(hole_num, plot_path, bunker_count, green_sq_ft, distance_to_green_yards))
      )
    gt_table
  })
}

shinyApp(ui, server)
