#' Get polygon data for a course
#'
#' @param api_id The ID of the course
#' @return An sf object with course polygons. Grain of table is one row per hole element.
#' @export

get_polygon_data <- function(api_id) {
  course_file <- paste0("data/rda/", api_id, "_data.rda")
  
  if (file.exists(course_file)) {
    load(course_file)
    
    return(course_data)
  } else {
    stop(paste("No data found for course with ID:", api_id))
  }
}
