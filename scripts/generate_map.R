# load libraries
{
library(sf)
library(tidyverse)
library(ggplot2)
library(stringr)
}

# set course name to singular course for plotting
# only necessary outside of Shiny environment
set_course_name <- "Erin Hills"

# list files in geojson folder
geojson_files <- list.files("data/geojson", pattern = "geojson", full.names = TRUE)

# read and combine files
geojson_df <- bind_rows(lapply(geojson_files, st_read)) %>%
    rename(polygon_name = name)

# read in mapped courses file
course_db <- read.csv("data/mapped_course_list/mapped_courses.csv")    

# extracting course name from polygon name
geojson_df$course_name <- str_match(geojson_df$polygon_name, "^(.+)_hole")[,2]  # extract substring before "_hole"

# join in course db file to get additional course information
geojson_df <- left_join(geojson_df, course_db, by = "course_name")

geojson_df$course_name <- gsub("_", " ", geojson_df$course_name)        
geojson_df$course_name <- str_to_title(geojson_df$course_name)

# reproject to a suitable CRS if necessary
# changing zone number rotates map
geojson_df <- st_transform(geojson_df, "+proj=utm +zone=44 +datum=WGS84")

# supply colors to each polygon type
geojson_df <- geojson_df %>%
  mutate(color = case_when(
    grepl("tee", polygon_name) ~ "#57B740",
    grepl("bunker", polygon_name) ~ "#EDE6D3",
    grepl("water", polygon_name) ~ "#2243b6",
    grepl("fairway", polygon_name) ~ "#57B740",
    grepl("green", polygon_name) ~ "#86D14A",
    grepl("hazard", polygon_name) ~ "#094d1d"
  ),
  polygon_type = case_when(
    grepl("tee", polygon_name) ~ "Tee",
    grepl("fairway", polygon_name) ~ "Fairway",
    grepl("green", polygon_name) ~ "Green",
    grepl("bunker", polygon_name) ~ "Bunker",
    grepl("water", polygon_name) ~ "Water",
    grepl("hazard", polygon_name) ~ "Hazard")) %>%
  mutate(centroid = st_centroid(geometry))

# regex to retrieve hole number from polygon name
geojson_df$hole_num <- gsub(".*_hole_(\\d+)_.*", "\\1", geojson_df$polygon_name)

# format polygon color and hole number as factor
geojson_df$color <- as.factor(geojson_df$color)
geojson_df$hole_num <- as.factor(geojson_df$hole_num)

# filter data frame to set variable
geojson_df <- geojson_df %>%
    filter(course_name == set_course_name)

# generate ggplot map
ggplot() +
  geom_sf(data = geojson_df, aes(fill = color), color = "black") + 
  #geom_sf(data = points, color = "black", size = 3, shape = 20) +   # Points
  geom_text(data = filter(geojson_df, grepl("green", polygon_name)), 
            aes(x = st_coordinates(centroid)[, 1], 
                y = st_coordinates(centroid)[, 2], 
                label = hole_num), 
            size = 3, color = "black", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  scale_fill_identity(name = "Color", guide = "legend", labels = levels(geojson_df$polygon_type)) + # Specify legend fill manually
  theme_minimal() + # Remove background and gridlines
  theme(axis.title.x = element_blank(), # Remove x-axis label
      axis.title.y = element_blank(), # Remove y-axis label
     axis.text.x = element_blank(), # Remove x-axis text
    axis.text.y = element_blank(), # Remove y-axis text
    plot.title = element_text(size = 16, family = "Big Caslon"),
   panel.grid.major = element_blank(), # Remove major gridlines
  panel.grid.minor = element_blank()) + # Remove minor gridlines
  theme(legend.position = "none") +
  labs(title = paste0(geojson_df$course_name, " | ", geojson_df$city, " , ", geojson_df$state))
