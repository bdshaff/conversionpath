#' Plot Touch-point Frequency
#'
#' @param path_data path_data
#' @return plotly plot

plot_touchpoint_frequency <- function(path_data, remove_repeating = FALSE) {

  path_list = extract_path_list(path_data, remove_repeating)
  touchpoint_frequency <- sort(table(unlist(path_list)))
  touchpoint_frequency <- data.frame(touchpoint_frequency)
  names(touchpoint_frequency) <- c("touchpoint", "frequency")

  plotly::ggplotly(
      ggplot2::ggplot(touchpoint_frequency, ggplot2::aes(touchpoint, frequency)) +
      ggplot2::geom_col() +
      ggplot2::theme_minimal() +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Touchpoint Frequency")
  )
}
