#' Get polygon data for a course, optionally filtered by hole number(s)
#'
#' @param api_id The ID of the course
#' @param hole_num Optional. A single number, vector (i.e., c(1, 3)), or range (i.e., 1:18)
#'
#' @return An sf object with course polygons filtered by hole
#' @export
get_polygon_data <- function(api_id, hole_num = NULL) {
  course_file <- paste0("data/rda/", api_id, "_data.rda")
  
  if (!file.exists(course_file)) {
    stop(paste("No data found for course with ID:", api_id))
  }

  load(course_file)

  if (!is.null(hole_num)) {
    hole_num <- as.character(hole_num)
    course_data <- dplyr::filter(course_data, hole_num %in% hole_num)
  }

  return(course_data)
}
