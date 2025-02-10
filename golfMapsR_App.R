# load libraries
{
  library(sf)
  library(tidyverse)
  library(ggplot2)
  library(stringr)  
  library(shiny)
  library(shinyWidgets)
  library(shinybusy)
  library(shinythemes)
  library(gt)
  library(gtExtras)
  library(base64enc)
  library(magick)
  library(RSQLite)
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
course_db$course_name <- gsub("_", " ", course_db$course_name_raw)       
course_db$course_name <- str_to_title(course_db$course_name)
course_db$course_name_concat <- paste0(course_db$course_name, " - ", course_db$city, ", ", course_db$state)
course_db$logo_url <- paste0("https://raw.githubusercontent.com/abodesy14/golfMapsR/refs/heads/main/images/logos/", 
                             course_db$course_name_raw,
                             ".png")


# local testing
#course_db <- read.csv("/Users/adambeaudet/Github/golfMapsR/data/mapped_course_list/mapped_courses.csv")

# extracting course name from polygon name
geojson_df$course_name_raw <- str_match(geojson_df$polygon_name, "^(.+)_hole")[,2]  # extract substring before "_hole"

# join and position maps to point north
geojson_df <- left_join(geojson_df, course_db, by = "course_name_raw")
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
geojson_df$course_hole_concat <- paste0(geojson_df$course_name, " | ", geojson_df$hole_num)

# area/sq ft calculation (while retaining sf structure)
geojson_df$area <- st_area(geojson_df)
geojson_df$total_sq_ft <- as.numeric(geojson_df$area) * 10.7639

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

###################
# bring in api data

# connect to course metadata db
conn <- dbConnect(RSQLite::SQLite(), "data/course_metadata/course_metadata.db")

# read table and de-dupe
query <- "WITH source as (SELECT 
                            *,
                            ROW_NUMBER() OVER (PARTITION BY course_id, club_name, tee_name, hole, par ORDER BY processed_ts DESC) as rn 
                          FROM course_metadata), 

                          qualify as (
                            SELECT 
                              * 
                            FROM source 
                            WHERE rn = 1
                          ) 

                          SELECT 
                              *
                          FROM qualify"

course_metadata <- dbGetQuery(conn, query)

# drop column with dplyr since sqlite has no .exclude like snowflake does
course_metadata <- course_metadata %>%
  select(-rn)

# read in course data not returned by api - logged manually
courses_missing_from_api <- read.csv("data/course_metadata/manual_uploads.csv")

# bind data from api and manual uploads
course_metadata <- rbind(course_metadata, courses_missing_from_api) %>%
  select(course_id, club_name, tee_name, hole, par, yardage, handicap, total_par, total_yardage)

# subset to just mapped courses
# eventually, could make course_db sqlite table instead of csv so we can directly join
# could read in significantly less data
course_metadata_mapped <- inner_join(course_metadata, course_db, by = c("course_id" = "api_id"))

###################

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
                      gt_output("hole_by_hole"))
             )
    ),
    
    tabPanel("Scorecard",
             fluidRow(
               column(width = 12, 
                      gt_output("scorecard"))
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
  
  
  output$scorecard <- render_gt({
    
    scorecard_subset <- geojson_df %>% filter(course_name_concat == input$course)
    
    # fix any invalid geometries
    scorecard_subset <- st_make_valid(scorecard_subset)
    
    # DRY this part up from previous tab eventually
    # using same function as hole by hole tab
    generate_hole_plot <- function(geometry, color, filename) {
      plot <- ggplot() +
        geom_sf(data = geojson_df_hole, aes(fill = color), color = "black") +
        geom_sf(
          data = geojson_df_hole %>% filter(grepl("green", course_element, ignore.case = TRUE)),
          aes(geometry = geometry),
          fill = "#86D14A", color = "black"
        ) +
        scale_fill_identity() +
        theme_void()
      ggsave(filename, plot, width = 3, height = 3, units = "in")
    }
    
    # function to encode image to base64
    encode_image_base64 <- function(filepath) {
      img <- readBin(filepath, "raw", file.info(filepath)$size)
      base64 <- base64encode(img)
      return(base64)
    }
    
    # initialize df for gt table
    gt_data <- data.frame(
      course_name = character(),
      hole_num = numeric(),
      course_hole_concat = character(),
      plot_path = character(),
      stringsAsFactors = FALSE
    )
    
    # loop through unique hole numbers
    unique_holes <- unique(scorecard_subset$course_hole_concat)
    for (hole in unique_holes) {
      # Filter data for the current hole
      geojson_df_hole <- scorecard_subset %>%
        filter(course_hole_concat == hole)
      
      # generate plot and encode to base64
      filename <- tempfile(fileext = ".png")
      generate_hole_plot(geojson_df_hole$geometry, geojson_df_hole$color, filename)
      plot_path <- encode_image_base64(filename)
      
      # add row for the current hole to the gt_data data frame
      gt_data <- rbind(gt_data, data.frame(
        course_hole_concat = geojson_df_hole$course_hole_concat[1],
        course_name = geojson_df_hole$course_name[1],
        hole_num = geojson_df_hole$hole_num[1],
        plot_path = plot_path,
        stringsAsFactors = FALSE
      ))
    }
      
    # create wide df to mimic real scorecard
      wide_data <- gt_data %>%
        mutate(tee_name = "") %>%
        mutate(total_yardage = "") %>%
        rename(club_name = course_name) %>%
        select(club_name, tee_name, total_yardage, hole_num, plot_path) %>%
        pivot_wider(
          names_from = hole_num,
          values_from = plot_path,
          names_prefix = "hole_"
        )
      
      hole_columns <- paste0("hole_", 1:18)

      selected_course_metadata <- course_metadata_mapped %>% filter(course_name_concat == input$course)

      # start building scorecard
      # different data types, need to union together yardage, handicap, par, and hole plots
      selected_course_wide <- selected_course_metadata %>%
        select(club_name, hole, yardage, tee_name, total_yardage) %>%
        pivot_wider(
          names_from = hole,
          values_from = yardage,
          names_prefix = "hole_"
        ) %>%
        arrange(desc(total_yardage))
      
      # get longest tees
      largest_tee <- selected_course_metadata %>%
        group_by(tee_name) %>%
        slice(1) %>%
        arrange(desc(total_yardage)) %>%
        pull(tee_name) %>%
        .[1]
      
      # get longest tees and use handicaps from those
      hdcp_wide <- selected_course_metadata %>%
        filter(tee_name == largest_tee) %>%
        select(club_name, hole, tee_name, handicap, total_yardage) %>%
        pivot_wider(
          names_from = hole,
          values_from = handicap,
          names_prefix = "hole_") %>%
        mutate(tee_name = paste0("Handicap (", tee_name, ")"))
      
      hdcp_wide$total_yardage <- ""
      
      par_wide <- selected_course_metadata %>%
        filter(tee_name == largest_tee) %>%
        select(club_name, hole, par, tee_name, total_yardage) %>%
        pivot_wider(
          names_from = hole,
          values_from = par,
          names_prefix = "hole_"
        ) %>%
        mutate(tee_name = paste0("Par (", tee_name, ")"))
      
      par_wide$total_yardage <- ""
      
      # union together scorecard components
      bind_yardage_hdcp_par <- rbind(wide_data, selected_course_wide, hdcp_wide, par_wide)
    
      bind_yardage_hdcp_par %>%
        select(tee_name, total_yardage, all_of(hole_columns)) %>%
        gt() %>%
        tab_header(
          title = md(paste0("**", input$course, "**", " ", html(paste0('<img src="', unique(scorecard_subset$logo_url), '" height="40px">'))))
        ) %>%
        text_transform(
          locations = cells_body(columns = all_of(hole_columns)),
          fn = function(x) {
            # find base64 images and reduce size if found
            lapply(x, function(value) {
              if (grepl("^iVBOR", value)) {
                html(sprintf('<img src="data:image/png;base64,%s" height="40px" width="40px">', value))
              } else {
                value
              }
            })
          }
        ) %>%
        cols_label(
          club_name = "Course Name",
          .list = setNames(as.list(paste(1:18)), hole_columns)
        ) %>%
        cols_width(
          tee_name ~ px(90),
          total_yardage ~ px(62),
          everything() ~ px(55)
        ) %>%
        tab_style(
          style = cell_text(align = "center"), 
          locations = cells_body(columns = c(total_yardage, hole_1:hole_18))
        ) %>%
        tab_style(
          style = cell_text(align = "left"), 
          locations = cells_body(columns = c(tee_name))
        ) %>%
        tab_style(
          style = cell_text(size = px(12)),
          locations = cells_body()
        ) %>%
        tab_style(
          style = cell_text(size = px(12), align = "center", weight = "bold"),
          locations = cells_column_labels()
        ) %>%
        # borders to look more like real scorecard
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom", "left", "right"),
        color = "gray",
        weight = px(1),
        style = "solid"
      ),
      locations = cells_body()
    ) %>%
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        color = "gray",
        weight = px(1),
        style = "solid"
      ),
      locations = cells_column_labels()
    ) %>%
    cols_label(
      tee_name = "Tee",
      total_yardage = "Yards"
    )
  })
  
}

shinyApp(ui, server)