# load libraries
{
  library(sf)
  library(tidyverse)
  library(ggplot2)
  library(stringr)  
  library(shiny)
  library(shinyWidgets)
  library(shinybusy)
  library(tidyverse)
  library(gt)
  library(gtExtras)
  library(base64enc)
  library(magick)
}

# list files in kml data folder
# shinyapps.io
kml_files <- list.files("data/kml", pattern = "\\kml$", full.names = TRUE, recursive = TRUE)
# local testing
#kml_files <- list.files("/Users/adambeaudet/Github/golfMapsR/data/kml", pattern = "\\kml$", full.names = TRUE, recursive = TRUE)

# read and combine files  
# sometimes unwanted polygons from osm get included that aren't true specific hole elements that we want to exclude
kml_df <- bind_rows(lapply(kml_files, st_read)) %>%
  rename(polygon_name = Name)%>%
  filter(!(polygon_name %in% c("Practice green", "Practice Area", "Driving Range", "", "Putting Green")))

geojson_df <- st_as_sf(kml_df, "POLYGON")

# read in mapped course db for course name, city, state, etc.
# shinyapps.io
course_db <- read.csv("data/mapped_course_list/mapped_courses.csv")
# local testing
#course_db <- read.csv("/Users/adambeaudet/Github/golfMapsR/data/mapped_course_list/mapped_courses.csv")

# extracting course name from polygon name
geojson_df$course_name <- str_match(geojson_df$polygon_name, "^(.+)_hole")[,2]  # extract substring before "_hole"

# join and light data cleaning
geojson_df <- left_join(geojson_df, course_db, by = "course_name")
geojson_df$course_name <- gsub("_", " ", geojson_df$course_name)       
geojson_df$course_name <- str_to_title(geojson_df$course_name)

geojson_df <- st_transform(geojson_df, "+proj=utm +zone=15 +datum=WGS84")

# supply colors to each polygon type and retrieve course element from mapped polygon name
geojson_df <- geojson_df %>%
  mutate(color = case_when(
    grepl("_tee$", polygon_name) ~ "#57B740",
    grepl("_bunker$", polygon_name) ~ "#EDE6D3",
    grepl("_water$", polygon_name) ~ "#2243b6",
    grepl("_fairway$", polygon_name) ~ "#57B740",
    grepl("_green$", polygon_name) ~ "#86D14A",
    grepl("_hazard$", polygon_name) ~ "#094d1d")) %>% 
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(course_element = case_when(
    grepl("_tee$", polygon_name) ~ 'tee',
    grepl("_fairway$", polygon_name) ~ 'fairway',
    grepl("_bunker$", polygon_name) ~ 'bunker',
    grepl("_water$", polygon_name) ~ 'water',
    grepl("_hazard$", polygon_name) ~ 'hazard',
    grepl("_green$", polygon_name) ~ 'green'))

# format polygon color as factor
geojson_df$color <- as.factor(geojson_df$color)

# create hole column
geojson_df$hole_num <- gsub(".*_hole_(\\d+)_.*", "\\1", geojson_df$polygon_name)
geojson_df$course_name_concat <- paste0(geojson_df$course_name, " - ", geojson_df$city, ", ", geojson_df$state)
geojson_df$course_hole_concat <- paste0(geojson_df$course_name, " | ", geojson_df$hole_num)

# area/sq ft calculation (while retaining sf structure)
geojson_df$area <- st_area(geojson_df)  # calculate area in square meters
geojson_df$total_sq_ft <- as.numeric(geojson_df$area) * 10.7639  # convert to square feet

# filter out NA values and empty/whitespace strings explicitly
valid_courses <- unique(geojson_df$course_name_concat)
valid_courses <- valid_courses[!is.na(valid_courses) & valid_courses != "" & valid_courses != "NA - NA, NA"]

# initialize the distance column (straight line distance, aka "as the crow flies")
geojson_df$distance_to_green_yards <- NA

# loop through each course/hole combination
for (hole in unique(geojson_df$course_hole_concat)) {
  
  # filter polygons for hole elements and green
  hole_elements <- filter(geojson_df, course_hole_concat == hole & !grepl("_green$", polygon_name))
  green_polygon <- filter(geojson_df, course_hole_concat == hole & grepl("_green$", polygon_name))$geometry
  
  if (length(green_polygon) > 0) {
    # calculate green centroid
    green_centroid <- st_centroid(green_polygon)
    
    # calculate hole distances ('as the crow flies' straight line distance from furthest point to green)
    distances <- st_distance(st_centroid(hole_elements$geometry), green_centroid)
    
    if (length(distances) > 0) {
      geojson_df$distance_to_green_yards[geojson_df$course_hole_concat == hole & !grepl("_green$", geojson_df$polygon_name)] <- distances / 0.9144
    }
  }
}


# ui
ui <- fluidPage(
  add_busy_spinner(spin = "fading-circle"),
  theme = shinythemes::shinytheme("simplex"), 
  navbarPage(
    "golfMapsR",
    fluidRow(
      column(width = 3, 
             selectInput("course", "Choose Course:", 
                         choices = valid_courses, 
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
                      gt_output("course_scorecard"))
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
  
  
  output$course_scorecard <- render_gt({
    scorecard_subset <- geojson_df %>% filter(course_name_concat == input$course)
    
    # fix any invalid geometries
    scorecard_subset <- st_make_valid(scorecard_subset)
    
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
    unique_holes <- unique(scorecard_subset$hole_num)
    for (hole in unique_holes) {
      # subset data for the current hole
      geojson_df_hole <- scorecard_subset %>% filter(hole_num == hole)
      
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
      
      # add to gt_data
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
        "Trace Date: **", unique(scorecard_subset$trace_date), "**",
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