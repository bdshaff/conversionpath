#' Plot Path Lengths
#'
#' @param path_data path_data
#' @return plotly plot

plot_path_lengths <- function(path_data, remove_repeating = FALSE) {
  path_data$path_length <- path_length(path_data$path, remove_repeating)

  plotly::ggplotly(
    ggplot2::ggplot(path_data, ggplot2::aes(path_length)) +
      ggplot2::geom_histogram(alpha = 0.8, binwidth = 1) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Path Length Histogram")
  )
}
