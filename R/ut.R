#' Unique Touch-points Across Paths
#'
#' @param paths paths
#' @return numeric vector

ut = function(paths){
  all_touchpoints = stringr::str_trim(unlist(stringr::str_split(paths, ">")))
  return(unique(all_touchpoints))
}
