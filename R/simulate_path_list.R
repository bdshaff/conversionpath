#' Simulate Path List From Transition Matrix
#'
#' @param transition_matrix transition_matrix
#' @param lambda lambda
#' @param num_sim num_sim
#' @return list

simulate_path_list <- function(transition_matrix, num_sim, lambda = 5) {

  path_lengths = rpois(num_sim, lambda - 2) + 2
  path_list = map(path_lengths, ~simulate_path(transition_matrix, num_steps = .x)$path)
  path_list = map(path_list, ~remove_rep_tchp(.x))

  return(path_list)
}
