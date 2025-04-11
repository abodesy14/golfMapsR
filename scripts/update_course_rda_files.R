# Load necessary libraries
library(sf)
library(tidyverse)
library(stringr)

# Function to load and process KML files into RDA format for course polygons
update_course_rda_files <- function() {
  # List KML files in the directory
  kml_files <- list.files("data/kml", pattern = "\\.kml$", full.names = TRUE, recursive = TRUE)

  # Read and combine all KML files
  kml_df <- bind_rows(lapply(kml_files, st_read)) %>%
    rename(polygon_name = Name) %>%
    filter(!(polygon_name %in% c("Practice green", "Practice Area", "Driving Range", "", "Putting Green")))

  # Convert to sf object (geometry)
  geojson_df <- st_as_sf(kml_df, "POLYGON")

  # Read the course database
  course_db <- read.csv("data/mapped_course_list/mapped_courses.csv")
  course_db$course_name <- gsub("_", " ", course_db$course_name_raw)
  course_db$course_name <- str_to_title(course_db$course_name)
  course_db$course_name_concat <- paste0(course_db$course_name, " - ", course_db$city, ", ", course_db$state)
  course_db$logo_url <- paste0(
    "https://raw.githubusercontent.com/abodesy14/golfMapsR/refs/heads/main/images/logos/",
    course_db$course_name_raw, ".png"
  )

  # Extract course_name_raw from polygon name
  geojson_df$course_name_raw <- str_match(geojson_df$polygon_name, "^(.+)_hole")[, 2]

  # Join with course DB (this now brings in api_id too!)
  geojson_df <- left_join(geojson_df, course_db, by = "course_name_raw")
  geojson_df <- st_transform(geojson_df, "+proj=utm +zone=15 +datum=WGS84")

  # Add polygon info
  geojson_df <- geojson_df %>%
    mutate(color = case_when(
      grepl("_tee$", polygon_name) ~ "#57B740",
      grepl("_bunker$", polygon_name) ~ "#EDE6D3",
      grepl("_water$", polygon_name) ~ "#2243b6",
      grepl("_fairway$", polygon_name) ~ "#57B740",
      grepl("_green$", polygon_name) ~ "#86D14A",
      grepl("_hazard$", polygon_name) ~ "#094d1d"
    )) %>%
    mutate(centroid = st_centroid(geometry)) %>%
    mutate(course_element = case_when(
      grepl("_tee$", polygon_name) ~ "tee",
      grepl("_fairway$", polygon_name) ~ "fairway",
      grepl("_bunker$", polygon_name) ~ "bunker",
      grepl("_water$", polygon_name) ~ "water",
      grepl("_hazard$", polygon_name) ~ "hazard",
      grepl("_green$", polygon_name) ~ "green"
    )) %>%
    mutate(
      hole_num = gsub(".*_hole_(\\d+)_.*", "\\1", polygon_name),
      course_hole_concat = paste0(course_name, " | ", hole_num),
      area = st_area(geometry),
      total_sq_ft = as.numeric(area) * 10.7639
    )

  # ➕ NEW: Add distance from each polygon to its green centroid
  geojson_df$distance_to_green_yards <- NA_real_

  for (hole in unique(geojson_df$course_hole_concat)) {
    # Get green geometry for the hole
    green_geom <- geojson_df %>%
      filter(course_hole_concat == hole, grepl("_green$", polygon_name)) %>%
      pull(geometry)

    if (length(green_geom) > 0) {
      green_centroid <- st_centroid(green_geom)
      hole_polys <- geojson_df$course_hole_concat == hole

      geojson_df$distance_to_green_yards[hole_polys] <- 
        as.numeric(st_distance(geojson_df$geometry[hole_polys], green_centroid)) / 0.9144
    }
  }

  # Filter valid courses
  valid_courses <- geojson_df %>%
    filter(!is.na(api_id), !is.na(course_name_concat), course_name_concat != "NA - NA, NA") %>%
    pull(api_id) %>%
    unique()

  # Save each course separately using api_id
  for (cid in valid_courses) {
    course_data <- geojson_df %>% filter(api_id == cid)
    save(course_data, file = paste0("data/rda/", cid, "_data.rda"))
  }

  message("✅ RDA files have been written by api_id")
}

# Run the function
update_course_rda_files()
