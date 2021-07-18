#' Plot Path Lengths
#'
#' @param path_list path_list
#' @return plotly plot

plot_path_lengths <- function(path_list) {
  path_lengths <- data.frame(path_length = map_int(path_list, ~ length(.x)))

  ggplotly(
    path_lengths %>%
      ggplot(aes(path_length)) +
      geom_histogram(alpha = 0.8, binwidth = 1) +
      theme_minimal() +
      labs(title = "Path Length Histogram")
  )
}
