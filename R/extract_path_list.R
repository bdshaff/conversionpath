#' Extract List of Paths from Data
#'
#' Given a data.frame with paths in a column `path`, generate a list.
#'
#' @param path_data path_data
#' @param remove_repeating remove_repeating
#' @return a list of paths

extract_path_list <- function(path_data, remove_repeating = FALSE) {

  if(!is.data.frame(path_data)){
    stop("path_data must be a data.frame")
  }

  if(is.null(path_data$path) | !is.character(path_data$path)){
    stop("path_data must contain column named path of type character")
  }

  if(is.null(path_data$conv_count) | !is.numeric(path_data$conv_count)){
    stop("path_data must contain column named conv_count of type numeric")
  }

  if(is.null(path_data$drop_count) | !is.numeric(path_data$drop_count)){
    stop("path_data must contain column named drop_count of type numeric")
  }

  path_list <- purrr::map(path_data$path, ~  stringr::str_split(.x, ">"))
  path_list <- purrr::flatten(path_list)
  path_list <- purrr::map(path_list, ~ stringr::str_trim(.x))

  if(remove_repeating) {
    cat("Removing touch-points that are repeated within paths.\n")
    path_list <- purrr::map(path_list, ~ remove_rep_tchp(.x))
  }

  return(path_list)
}
