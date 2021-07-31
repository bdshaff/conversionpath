#' Number of Unique Touch-points Per Paths
#'
#' @param paths paths
#' @return numeric vector

nupt = function(paths){
  split_paths = stringr::str_split(paths, ">")
  split_paths <- purrr::map(split_paths, ~stringr::str_trim(.x))
  purrr::map_int(split_paths, ~length(unique(.x)))
}
