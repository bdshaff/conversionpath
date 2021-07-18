#' Plot Markov Chain Transition Matrix
#'
#' @param transition_matrix transition_matrix
#' @return plotly plot

plot_transition_matrix <- function(transition_matrix) {
  M <- transition_matrix
  plot_ly(
    x = colnames(M[-c(1, 2, 3), -c(1, 2, 3)]),
    y = row.names(M[-c(1, 2, 3), -c(1, 2, 3)]),
    z = M[-c(1, 2, 3), -c(1, 2, 3)],
    type = "heatmap",
    colors = pals::brewer.divbin(3)
  )
}
