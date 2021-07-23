#' Test Format of Transition Matrix
#'
#' Test if a markov chain transition matrix follows the form
#' required for the conversion path analysis i.e. Square matrix with two absorbing states (conv, drop),
#' a (start) state that can't be reached from any other state.
#'
#' @param transition_matrix a transiton matrix to be tested
#' @return a transition matrix

is.proper_transition_matrix = function(transition_matrix){
  M = transition_matrix
  test = TRUE

  if(!is.matrix(M)){
    test = FALSE
    warning("not a proper transition matrix")
  }

  if(nrow(M) != ncol(M)){
    test = FALSE
    warning("not a proper transition matrix")
  }

  if(sum(rowSums(M)) != nrow(M)){
    test = FALSE
    warning("not a proper transition matrix")
  }

  if(sum(colnames(M) != rownames(M)) > 0){
    test = FALSE
    warning("not a proper transition matrix")
  }

  if(colnames(M)[1] != "start"){
    test = FALSE
    warning("not a proper transition matrix")
  }

  if(colnames(M)[2] != "conv"){
    test = FALSE
    warning("not a proper transition matrix")
  }

  if(colnames(M)[3] != "drop"){
    test = FALSE
    warning("not a proper transition matrix")
  }

  if(length(unique(colnames(M))) != ncol(M)){
    test = FALSE
    warning("not a proper transition matrix")
  }

  if(sum(M[,"start"] != 0) > 0){
    test = FALSE
    warning("not a proper transition matrix")
  }

  if(M["conv","conv"] != 1){
    test = FALSE
    warning("not a proper transition matrix")
  }

  if(M["drop","drop"] != 1){
    test = FALSE
    warning("not a proper transition matrix")
  }

  if(sum(diag(M[4:ncol(M),4:ncol(M)]) != 0) > 0){
    test = FALSE
    warning("not a proper transition matrix")
  }
  return(test)
}
