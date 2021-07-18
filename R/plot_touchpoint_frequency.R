#' Plot Touchpoint Frequency
#'
#' @param path_list path_list
#' @return plotly plot

plot_touchpoint_frequency = function(path_list){
  touchpoint_frequency = sort(table(unlist(path_list)))
  touchpoint_frequency = data.frame(touchpoint_frequency)
  names(touchpoint_frequency) = c("touchpoint","frequency")

  ggplotly(
    touchpoint_frequency %>%
      ggplot(aes(touchpoint, frequency)) +
      geom_col() +
      theme_minimal() +
      coord_flip() +
      labs(title = "Touchpoint Frequency")
  )
}
