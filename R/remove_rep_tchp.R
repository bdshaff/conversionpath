#' Remove Repeated Touchpoints
#'
#' @param long_path long_path
#' @return short path

remove_rep_tchp <- function(long_path) {
  long_path <- long_path %>% str_trim()
  from <- long_path[-length(long_path)]
  to <- long_path[-1]
  short_path <- long_path[-which(from == to)]

  sfrom <- short_path[-length(short_path)]
  sto <- short_path[-1]
  return(short_path)
}
