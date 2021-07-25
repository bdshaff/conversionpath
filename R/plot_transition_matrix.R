#' Plot Markov Chain Transition Matrix
#'
#' @param transition_matrix transition_matrix
#' @return plotly plot

plot_transition_matrix <- function(transition_matrix, full = FALSE) {

  if(!is.proper_transition_matrix(transition_matrix)){
    stop("Need a proper transition matrix")
  }

  M <- transition_matrix

  if(full){

    cnames = colnames(M)
    rnames = row.names(M)
    vals = M

    p = plotly::plot_ly(
      x = colnames(M),
      y = row.names(M),
      z = M,
      type = "heatmap",
      colors = pals::brewer.divbin(3)
    )

    p = plotly::add_annotations(p,
                                y = rep(rnames, ncol(vals)),
                                x = rep(rnames, each = ncol(vals)),
                                text = round(as.vector(vals),2),
                                showarrow = FALSE)

  }else{

    cnames = colnames(M[-c(1, 2, 3), -c(1, 2, 3)])
    rnames = row.names(M[-c(1, 2, 3), -c(1, 2, 3)])
    vals = M[-c(1, 2, 3), -c(1, 2, 3)]
    p = plotly::plot_ly(
      x = cnames,
      y = rnames,
      z = vals,
      type = "heatmap",
      colors = pals::brewer.divbin(3)
    )

    p = plotly::add_annotations(p,
                                y = rep(rnames, ncol(vals)),
                                x = rep(rnames, each = ncol(vals)),
                                text = round(as.vector(vals),2),
                                showarrow = FALSE)

  }
  return(p)
}
