#' Simulate Path List From Transition Matrix
#'
#' From a given transition matrix simulate a list of paths
#'
#' @param transition_matrix transition_matrix
#' @param lambda mean of a Poisson(lambda) distribution to sample path length from
#' @param num_sim number of simulations paths to generate
#' @return list

simulate_path_list <- function(transition_matrix, num_sim, lambda = 5) {

  if(!is.proper_transition_matrix(transition_matrix)){
    stop("Need a proper transition matrix")
  }

  if(!is.numeric(num_sim)){
    stop("num_sim must be an integer and > 1")
  }

  if(num_sim < 2){
    stop("num_sim must be an integer and > 1")
  }

  if(!is.numeric(lambda)){
    stop("lambda must be an integer and > 1")
  }

  if(lambda < 2){
    stop("lambda must be an integer and > 1")
  }

  path_lengths = rpois(num_sim, lambda - 2) + 2
  path_list = purrr::map(path_lengths, ~simulate_path(transition_matrix, num_steps = .x)$path)
  path_list = purrr::map(path_list, ~remove_rep_tchp(.x))

  return(path_list)
}
