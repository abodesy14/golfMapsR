#' Plot a golf course map by API ID
#'
#' @param api_id The course API ID (numeric or character)
#' @param hole_num Optional. A hole number or vector of hole numbers to filter (e.g., 1, c(1,3,7), 1:18)
#'
#' @return A ggplot object showing the course layout
#' @export
plot_course <- function(api_id, hole_num = NULL) {
  course_data <- get_polygon_data(api_id)

  # Filter if hole_num is specified
  if (!is.null(hole_num)) {
    course_data <- dplyr::filter(course_data, hole_num %in% !!hole_num)
  }

  # base map with all course elements
  p <- ggplot() +
    geom_sf(data = course_data, aes(fill = color), color = "black") +

    # overlay greens to top
    geom_sf(data = dplyr::filter(course_data, grepl("_green$", polygon_name)),
            aes(geometry = geometry), fill = "#86D14A", color = "black") +

    # hole numbers centered on greens
    geom_text(data = dplyr::filter(course_data, grepl("_green$", polygon_name)),
              aes(x = sf::st_coordinates(sf::st_centroid(geometry))[, 1],
                  y = sf::st_coordinates(sf::st_centroid(geometry))[, 2],
                  label = hole_num),
              size = 3, color = "black", fontface = "bold", hjust = 0.5, vjust = 0.5) +
    scale_fill_identity() +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 16, family = "Big Caslon"),
      legend.position = "none"
    ) +
    labs(title = paste0(course_data$course_name[1], " | ",
                        course_data$city[1], ", ",
                        course_data$state[1]))

  return(p)
}
