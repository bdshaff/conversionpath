#' Simulate Path From Transition Matrix
#'
#' @param transition_matrix transition_matrix
#' @param num_steps num_steps
#' @param num_sim num_sim
#' @return data.frame

simulate_path_table <- function(transition_matrix, num_steps, num_sim) {
  sim_tb <-
    map(1:num_sim, ~ simulate_path(transition_matrix, num_steps)$target_table) %>%
    bind_rows(.id = "path_id")
  return(sim_tb)
}
