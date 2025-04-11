# Load required libraries and your functions
source("R/get_plotted_courses.R")
source("R/get_polygon_data.R")

library(tidyverse)

# Load course metadata
course_db <- read.csv("data/mapped_course_list/mapped_courses.csv")

# Get list of valid api_ids from plotted courses
plotted_courses <- get_plotted_courses()
api_ids <- unique(plotted_courses$api_id)

# Define file paths for each api_id
rda_paths <- paste0("data/rda/", api_ids, "_data.rda")

# Check which ones exist
valid_api_ids <- api_ids[file.exists(rda_paths)]

# Log missing ones (optional)
missing_api_ids <- setdiff(api_ids, valid_api_ids)
if (length(missing_api_ids) > 0) {
  message("🚫 Missing RDA files for the following api_ids: ", paste(missing_api_ids, collapse = ", "))
}

# Load and bind all valid course polygon data
geojson_df <- purrr::map_dfr(valid_api_ids, get_polygon_data)

# Save to .rds
saveRDS(geojson_df, "data/geojson_df.rds")
message("✅ Saved geojson_df to data/geojson_df.rds")
