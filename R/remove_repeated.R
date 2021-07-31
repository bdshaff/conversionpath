#' Remove Repeated Touch-points
#'
#' Remove repeated touch-points in a path if they are adjacent to one another.
#'
#' @param path vector of paths
#' @return vector of cleaned paths

remove_repeated = function(path){
  if(length(path) == 1){
    split_path = stringr::str_split(path, ">")
    cp = remove_rep_tchp(split_path[[1]])
    purrr::map_chr(cp, ~stringr::str_c(.x, collapse = " > "))
  }else{
    split_paths = stringr::str_split(path, ">")
    cp = purrr::map(split_paths, ~remove_rep_tchp(.x))
    purrr::map_chr(cp, ~stringr::str_c(.x, collapse = " > "))
  }
}
