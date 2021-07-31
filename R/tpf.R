#' Touch-point Frequency per Path
#'
#' @param paths paths
#' @param touchpoint touchpoint
#' @return numeric vector
tpf = function(paths, touchpoint){
  split_paths = stringr::str_split(paths, ">")
  split_paths <- purrr::map(split_paths, ~stringr::str_trim(.x))
  purrr::map_int(split_paths, ~sum(.x == touchpoint))
}
