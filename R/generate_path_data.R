#' Generate Path Data from Path List
#'
#' @param path_list path_list
#' @return data.frame

generate_path_data = function(path_list){

  if(!is.list(path_list)){
    stop("path_list must be a list")
  }

  conv_vec = purrr::map(path_list, ~ dplyr::if_else(.x[length(.x)] == "conv","conv","drop")) %>% unlist()
  path_vec = purrr::map(path_list, ~stringr::str_c(.x[-length(.x)], collapse = " > ")) %>% unlist()

  pd =
    data.frame(path = path_vec, conv = conv_vec) %>%
    dplyr::group_by(path) %>%
    dplyr::summarise(conv_count = sum(conv == "conv"),
              drop_count = sum(conv == "drop")) %>%
    dplyr::arrange(-conv_count)

  return(pd)
}
