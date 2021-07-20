#' Remove Repeated Touchpoints
#'
#' @param long_path long_path
#' @return short path

remove_rep_tchp <- function(long_path) {
  long_path <- stringr::str_trim(long_path)
  from <- long_path[-length(long_path)]
  to <- long_path[-1]
  if(sum(from == to) > 0){
    short_path <- long_path[-which(from == to)]
  }else{
    short_path <- long_path
  }
  return(short_path)
}
