# load libraries
{
library(sf)
library(tidyverse)
library(ggplot2)
library(stringr)
library(geojsonsf)
}

# set course name to singular course for plotting
# only necessary outside of Shiny environment
set_course_name <- "Erin Hills"

# list files in kml folder
kml_files <- list.files("data/kml", pattern = "kml", full.names = TRUE)

# read and combine files
kml_df <- bind_rows(lapply(kml_files, st_read)) %>%
    rename(polygon_name = Name) %>%
    select(polygon_name, geometry)

# convert from kml to geojson
geojson_df <- st_as_sf(kml_df, "POLYGON") 

# read in mapped courses file
course_db <- read.csv("data/mapped_course_list/mapped_courses.csv")    

# extracting course name from polygon name
# extract substring before "_hole"
geojson_df$course_name <- str_match(geojson_df$polygon_name, "^(.+)_hole")[,2]  

# join in course db file to get additional course information
geojson_df <- left_join(geojson_df, course_db, by = "course_name")

geojson_df$course_name <- gsub("_", " ", geojson_df$course_name)        
geojson_df$course_name <- str_to_title(geojson_df$course_name)

# define a CRS for Lambert Conformal Conic projection so map always points due north
crs <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# transform data to CRS
geojson_df <- st_transform(geojson_df, crs)

# supply colors to each polygon type
geojson_df <- geojson_df %>%
  mutate(color = case_when(
    grepl("_tee$", polygon_name) ~ "#57B740",
    grepl("_bunker$", polygon_name) ~ "#EDE6D3",
    grepl("_water$", polygon_name) ~ "#2243b6",
    grepl("_fairway$", polygon_name) ~ "#57B740",
    grepl("_green$", polygon_name) ~ "#86D14A",
    grepl("_hazard$", polygon_name) ~ "#094d1d")) %>%
  mutate(centroid = st_centroid(geometry)) 

# regex to retrieve hole number from polygon name
geojson_df$hole_num <- gsub(".*_hole_(\\d+)_.*", "\\1", geojson_df$polygon_name)

# format polygon color and hole number as factor
geojson_df$color <- as.factor(geojson_df$color)
geojson_df$hole_num <- as.factor(geojson_df$hole_num)

# filter data frame to set variable
geojson_df <- geojson_df %>%
    filter(course_name == set_course_name)

# Area Calculation + Analysis (with retaining sf structure)
geojson_df$area <- st_area(geojson_df)  # calculate area in square meters
geojson_df$total_sq_ft <- as.numeric(geojson_df$area) * 10.7639  # convert to square feet

# generate ggplot map
ggplot() +
  # plot all polygons first
  geom_sf(data = geojson_df, aes(fill = color), color = "black") +
  # overlay greens on top
  geom_sf(data = filter(geojson_df, grepl("_green$", polygon_name)), 
          aes(geometry = geometry), fill = "#86D14A", color = "black") +
  # add hole numbers on greens
  geom_text(data = filter(geojson_df, grepl("_green$", polygon_name)), 
            aes(x = st_coordinates(st_centroid(geometry))[,1], 
                y = st_coordinates(st_centroid(geometry))[,2], 
                label = hole_num), 
            size = 3, color = "black", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  scale_fill_identity() +
  theme_minimal() +
  theme(axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    plot.title = element_text(size = 16),
    panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank()) + 
  theme(legend.position = "none") +
  labs(title = paste0(geojson_df$course_name, " | ", geojson_df$city, " , ", geojson_df$state))