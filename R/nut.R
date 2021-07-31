#' Number of Unique Touch-points Across Paths
#'
#' @param paths paths
#' @return numeric vector

nut = function(paths){
  all_touchpoints = stringr::str_trim(unlist(stringr::str_split(paths, ">")))
  length(unique(all_touchpoints))
}
