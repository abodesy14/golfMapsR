#' Get polygon data for a course
#'
#' @param api_id The ID of the course
#'
#' @return An sf object with golf course polygons (tee boxes, fairways, bunkers, hazards, greens, etc.)
#' @export
get_polygon_data <- function(api_id) {
  course_file <- system.file("extdata", paste0(api_id, "_data.rda"), package = "golfMapsR")
  
  if (course_file == "") {
    stop(paste("No data found for course with ID:", api_id))
  }

  env <- new.env()
  load(course_file, envir = env)
  course_data <- env$course_data

  return(course_data)
}
