#' Extract List of Paths from Data
#'
#' @param path_data path_data
#' @param remove_repeating remove_repeating
#' @return a list of paths

extract_path_list <- function(path_data, remove_repeating = FALSE) {
  P <- map(path_data$path, ~ str_split(.x, ">")) %>%
    flatten() %>%
    map(~ str_trim(.x))

  if (remove_repeating) {
    cat("Removing touch-points that are repeated within paths.\n")
    P <- map(P, ~ remove_rep_tchp(.x))
  }

  return(P)
}
