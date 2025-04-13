# load libraries
source("R/get_mapped_courses.R")
source("R/get_polygon_data.R")

library(tidyverse)

# course metadata
course_db <- read.csv("data/mapped_course_list/mapped_courses.csv")

# valid ids from mapped courses
mapped_courses <- get_mapped_courses()
api_ids <- unique(mapped_courses$api_id)

# file paths
rda_paths <- paste0("data/rda/", api_ids, "_data.rda")

# check which exist
valid_api_ids <- api_ids[file.exists(rda_paths)]

missing_api_ids <- setdiff(api_ids, valid_api_ids)
if (length(missing_api_ids) > 0) {
  message("Missing RDA files for the following api_ids: ", paste(missing_api_ids, collapse = ", "))
}

# bind geojson df 
geojson_df <- purrr::map_dfr(valid_api_ids, get_polygon_data)

# save as rds
saveRDS(geojson_df, "data/geojson_df.rds")
message("Saved geojson_df to data/geojson_df.rds")
