#' Plot a golf course map by API ID
#'
#' @param api_id The course API ID (numeric or character)
#' @param hole_num Optional. A hole number or vector of hole numbers to filter (i.e., 1, c(1,3,7), 1:18)
#'
#' @return A ggplot object showing the course layout
#' @export
plot_course <- function(api_id, hole_num = NULL) {
  # load sf to avoid vec_size error
  library(sf)

  course_data <- get_polygon_data(api_id)

  if (!is.null(hole_num)) {
    course_data <- dplyr::filter(course_data, hole_num %in% !!hole_num)
  }

  # extract green polygons and calculate label position
  green_data <- dplyr::filter(course_data, grepl("_green$", polygon_name)) %>%
    dplyr::mutate(
      centroid = st_centroid(geometry),
      coords = st_coordinates(centroid),
      x = coords[, 1],
      y = coords[, 2]
    )

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = course_data,
                     ggplot2::aes(fill = color), color = "black") +

    ggplot2::geom_sf(data = dplyr::filter(course_data, grepl("_green$", polygon_name)),
                     ggplot2::aes(geometry = geometry), fill = "#86D14A", color = "black") +

    ggplot2::geom_text(data = green_data,
                       ggplot2::aes(x = x, y = y, label = hole_num),
                       size = 3, color = "black", fontface = "bold",
                       hjust = 0.5, vjust = 0.5) +

    ggplot2::scale_fill_identity() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 16, family = "Big Caslon"),
      legend.position = "none"
    ) +
    ggplot2::labs(
      title = paste0(course_data$course_name[1], " | ",
                     course_data$city[1], ", ",
                     course_data$state[1])
    )

  return(p)
}
