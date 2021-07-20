#' Simulate one Path From Transition Matrix
#'
#' @param transition_matrix transition_matrix
#' @param num_steps num_steps
#' @return data.frame


simulate_path <- function(transition_matrix, num_steps = 10) {
  path <- vector(mode = "integer", length = num_steps)

  MM <- transition_matrix
  p0 <- as.numeric(MM["start", ])
  path[1] <- which(rmultinom(1, 1, p0) == 1)

  # MM = MM[-1,]

  for (i in 1:(num_steps - 1)) {
    p <- MM[path[i], ]
    sn <- which(rmultinom(1, 1, p) == 1)
    path[i + 1] <- sn
  }

  nodes <- data.frame(name = colnames(MM), path = 1:ncol(MM))

  step <- 1:num_steps
  pd <- data.frame(step, path)
  pd <- dplyr::left_join(pd, nodes)
  pd <- dplyr::mutate(pd, step_name = paste0(step, "-", name))
  pd <- dplyr::filter(pd, path != 0)

  source <- pd$step_name[1:(nrow(pd) - 1)]
  target <- pd$step_name[-1]

  df <- data.frame(source, target)

  npath <- pd$name

  return(list(path = npath, target_table = df))
}
