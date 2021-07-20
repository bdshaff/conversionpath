#' Plot Path Lengths
#'
#' @param path_list path_list
#' @return plotly plot

plot_path_lengths <- function(path_list) {
  path_lengths <- data.frame(path_length = purrr::map_int(path_list, ~ length(.x)))

  plotly::ggplotly(
    path_lengths %>%
      ggplot2::ggplot(ggplot2::aes(path_length)) +
      ggplot2::geom_histogram(alpha = 0.8, binwidth = 1) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Path Length Histogram")
  )
}
