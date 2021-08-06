#' Touch-point Frequency Across Paths
#'
#' @param paths paths
#' @param touchpoint touchpoint
#' @return numeric vector

tf = function(paths, touchpoint){
  all_touchpoints = stringr::str_trim(unlist(stringr::str_split(paths, ">")))
  sum(touchpoint == all_touchpoints, na.rm = TRUE)
}
