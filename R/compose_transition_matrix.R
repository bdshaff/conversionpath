#' Compose a Random Markov Chain Transition Matrix
#'
#' @param unique_tchp_type unique_tchp_type
#' @return a transition matrix

compose_transition_matrix = function(unique_tchp_type){
  n_unique_tchp_type = length(unique_tchp_type)

  rfill = runif(n_unique_tchp_type + 3 * n_unique_tchp_type + 3)
  Mat = matrix(rfill, nrow = n_unique_tchp_type + 3, ncol = n_unique_tchp_type + 3)
  colnames(Mat) = c("start","conv","drop",unique_tchp_type)
  row.names(Mat) = c("start","conv","drop",unique_tchp_type)

  diag(Mat) = 0

  Mat["conv",] = 0
  Mat["drop",] = 0
  Mat["conv","conv"] = 1
  Mat["drop","drop"] = 1
  Mat["start",c("start","conv","drop")] = 0
  Mat[,"start"] = 0

  Mat = Mat/rowSums(Mat)

  return(Mat)
}
