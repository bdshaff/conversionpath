#' Simulate Path Table From Transition Matrix
#'
#' @param transition_matrix transition_matrix
#' @param num_steps num_steps
#' @param num_sim num_sim
#' @return data.frame

simulate_path_table <- function(transition_matrix, num_steps, num_sim) {

  if(!is.proper_transition_matrix(transition_matrix)){
    stop("Need a proper transition matrix")
  }

  if(!is.numeric(num_steps)){
    stop("num_steps must be an integer and > 1")
  }

  if(num_steps < 2){
    stop("num_steps must be an integer and > 1")
  }

  if(!is.numeric(num_sim)){
    stop("num_sim must be an integer and > 1")
  }

  if(num_sim < 2){
    stop("num_sim must be an integer and > 1")
  }

  sim_tb <- purrr::map(1:num_sim, ~ simulate_path(transition_matrix, num_steps)$target_table)
  sim_tb <- dplyr::bind_rows(sim_tb, .id = "path_id")
  return(sim_tb)
}
