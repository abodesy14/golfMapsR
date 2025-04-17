#' Get Mapped Courses
#'
#' Returns a data frame of all mapped golf courses
#'
#' @return A data.frame with columns api_id, course_name_raw, city, state, trace_date, and logo_url
#' @export
#' @importFrom magrittr %>%
get_mapped_courses <- function() {
  course_db %>%
    dplyr::select(api_id, course_name_raw, city, state, trace_date,
                  logo_url)
}
