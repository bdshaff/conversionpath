#' Augment TPF
#'
#' @param data data
#' @param touchpoints touchpoints
#' @return data


augment_tpf = function(data, touchpoints = NULL){

  paths = data$path

  if(is.null(touchpoints)){
    touchpoints = ut(paths)
  }

  tpfs = purrr::map(touchpoints, ~tpf(paths, .x))
  names(tpfs) = janitor::make_clean_names(touchpoints)

  tpf_df = dplyr::bind_rows(tpfs)

  return(dplyr::bind_cols(data, tpf_df))

}
