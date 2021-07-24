#' Derive a Proper Transition Matrix from MC Model
#'
#' @param path_list mc_model
#' @return a transition matrix
#'
transition_matrix_from_markov_model = function(mc_model){

  trans_mat =
    mc_model$transition_matrix %>%
    pivot_wider(names_from = channel_to, values_from = transition_probability) %>%
    relocate(`(null)`, .before = `1`) %>%
    relocate(`(conversion)`, .before = `(null)`) %>%
    mutate(start = 0, .before = `(conversion)`)

  trans_mat = as.matrix(trans_mat[,-1])
  trans_mat = rbind(
    trans_mat[1,],
    rbind(rep(0, ncol(trans_mat)),rep(0, ncol(trans_mat))),
    trans_mat[2:nrow(trans_mat),])

  trans_mat[is.na(trans_mat)] = 0

  colnames(trans_mat) = c("start","conv","drop", mc_model$result$channel_name)
  row.names(trans_mat) = colnames(trans_mat)

  trans_mat["conv","conv"] = 1
  trans_mat["drop","drop"] = 1

  return(trans_mat)

}
