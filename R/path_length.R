#' Compute Path Length
#'
#' Remove repeated touch-points in a path if they are adjacent to one another.
#'
#' @param path vector of paths
#' @param remove_repeating Should repeated touchpoints be removed first?
#' @return vector of cleaned paths


path_length = function(path, remove_repeating = FALSE){
  if(remove_repeating){
    path = remove_repeated(path)
    split_path = stringr::str_split(path, ">")
    purrr::map_int(split_path , ~ length(.x))
  }else{
    split_path = stringr::str_split(path, ">")
    purrr::map_int(split_path , ~ length(.x))
  }
}
