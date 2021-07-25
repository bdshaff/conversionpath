#' Compose a Random Markov Chain Transition Matrix
#'
#' This function allows you to generate a random markov chain transition matrix following the form
#' required for the conversion path analysis i.e. Square matrix with two absorbing states (conv, drop),
#' a (start) state that can't be reached from any other state, and a user defined set of unique states
#' representing possible touch points on path to conversion.
#'
#' @param unique_tchp_type a character vector of unique possible touch-points in path to conversion
#' @return a transition matrix

compose_transition_matrix = function(unique_tchp_type){

  if(!is.vector(unique_tchp_type, mode = "character")){
    stop("unique_tchp_type must be a character vector")
  }

  if(length(unique(unique_tchp_type)) < 2){
    stop("must supply at least two distinct touchpoints")
  }

  if(!(length(unique(unique_tchp_type)) == length(unique_tchp_type))){
    stop("all supplied touchpoints must be distinct")
  }

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
