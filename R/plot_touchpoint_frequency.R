#' Plot Touchpoint Frequency
#'
#' @param path_list path_list
#' @return plotly plot

plot_touchpoint_frequency <- function(path_list) {
  touchpoint_frequency <- sort(table(unlist(path_list)))
  touchpoint_frequency <- data.frame(touchpoint_frequency)
  names(touchpoint_frequency) <- c("touchpoint", "frequency")

  plotly::ggplotly(
    touchpoint_frequency %>%
      ggplot2::ggplot(ggplot2::aes(touchpoint, frequency)) +
      ggplot2::geom_col() +
      ggplot2::theme_minimal() +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Touchpoint Frequency")
  )
}
