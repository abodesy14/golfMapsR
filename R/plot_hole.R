#' Plot a Single Golf Course Hole
#'
#' Generates a ggplot visualization of a specific hole of a golf course
#' using polygon data, based on the course's API ID and hole number.
#'
#' @param api_id Numeric or character. The API ID of the golf course.
#' @param hole_number Integer from 1 to 18. The hole number to plot.
#'
#' @return A ggplot object displaying the layout of the selected hole.
#' @export
plot_hole <- function(api_id, hole_number) {
  geojson_df <- get_polygon_data(api_id)
  
  if (!hole_number %in% 1:18) {
    stop("Invalid hole_number. Must be an integer from 1 to 18.")
  }

  # filter polygons related to selected hole
  hole_tag <- paste0("_", hole_number, "_")
  hole_df <- geojson_df %>% filter(grepl(hole_tag, polygon_name))

  # plot single hole for single course
  ggplot(hole_df) +
    geom_sf(aes(fill = color), color = "black") + 
    scale_fill_identity() + 
    theme_minimal() + 
    theme(
      axis.title.x = element_blank(), 
      axis.title.y = element_blank(),
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(), 
      plot.title = element_text(size = 16),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      legend.position = "none"
    ) +
    labs(title = paste0(hole_df$course_name[1], " | Hole ", hole_number, " | ", hole_df$city[1], ", ", hole_df$state[1]))
}
