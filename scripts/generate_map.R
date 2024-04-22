# load libraries
{
library(sf)
library(tidyverse)
library(ggplot2)
library(stringr)
}

# read in geojson file(s)
hole <- st_read("data/geojson/fieldstone.geojson") %>%
    rename(polygon_name = name)

course_db <- read.csv("data/mapped_course_list/mapped_courses.csv")    


# Extracting the desired substring before "hole" and removing underscores
hole$course_name <- str_match(hole$polygon_name, "^(.+)_hole")[,2]  # extract substring before "_hole"

# join in course db file to get additional course information
hole <- left_join(hole, course_db, by = "course_name")

hole$course_name <- gsub("_", " ", hole$course_name) # replace underscores with spaces           
hole$course_name <- str_to_title(hole$course_name)

# reproject to a suitable CRS if necessary
# changing zone number rotates map
hole <- st_transform(hole, "+proj=utm +zone=44 +datum=WGS84")

# supply colors to each polygon type
hole <- hole %>%
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
    grepl("water", polygon_name) ~ "Water")) %>%
  mutate(centroid = st_centroid(geometry))

# format polygon color as factor
hole$color <- as.factor(hole$color)

# generate ggplot map
ggplot() +
  geom_sf(data = hole, aes(fill = color), color = "black") + 
  #geom_sf(data = points, color = "black", size = 3, shape = 20) +   # Points
  geom_text(data = filter(hole, grepl("green", polygon_name)), 
            aes(x = st_coordinates(centroid)[, 1], 
                y = st_coordinates(centroid)[, 2], 
                label = gsub(".*_hole_(\\d+)_.*", "\\1", polygon_name)), 
            size = 3, color = "black", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  scale_fill_identity(name = "Color", guide = "legend", labels = levels(hole$polygon_type)) + # Specify legend fill manually
  theme_minimal() + # Remove background and gridlines
  theme(axis.title.x = element_blank(), # Remove x-axis label
      axis.title.y = element_blank(), # Remove y-axis label
     axis.text.x = element_blank(), # Remove x-axis text
    axis.text.y = element_blank(), # Remove y-axis text
   panel.grid.major = element_blank(), # Remove major gridlines
  panel.grid.minor = element_blank()) + # Remove minor gridlines
  theme(legend.position = "none") +
  labs(title = paste0(hole$course_name, " | ", hole$city, " - ", hole$state))

view(hole)
