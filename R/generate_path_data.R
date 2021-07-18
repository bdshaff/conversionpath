#' Generate Path Data from Path List
#'
#' @param path_list path_list
#' @return data.frame

generate_path_data = function(path_list){
  conv_vec = map(path_list, ~if_else(.x[length(.x)] == "conv","conv","drop")) %>% unlist()
  path_vec = map(path_list, ~str_c(.x[-length(.x)], collapse = " > ")) %>% unlist()

  pd =
    data.frame(path = path_vec, conv = conv_vec) %>%
    group_by(path) %>%
    summarise(conv_count = sum(conv == "conv"),
              drop_count = sum(conv == "drop")) %>%
    arrange(-conv_count)

  return(pd)
}
