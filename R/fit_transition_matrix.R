#' Compute MLE Markov Chain Transition Matrix
#'
#' @param path_list path_list
#' @param conv_count conv_count
#' @param drop_count drop_count
#' @return a transition matrix


fit_transition_matrix <- function(path_list, conv_count, drop_count) {

  if(!is.vector(conv_count, mode = "numeric")){
    stop("conv_count must be a numeric vector")
  }

  if(!is.vector(drop_count, mode = "numeric")){
    stop("conv_count must be a numeric vector")
  }

  if(!(length(conv_count) == length(path_list)) | !(length(drop_count) == length(path_list))){
    stop("all arguments must be of the same length")
  }

  total_count <- conv_count + drop_count
  split_paths <- path_list

  # get the state space from the data
  unique_tchp_type <- sort(unique(stringr::str_trim(unlist(split_paths))))
  # get thee number of possible states
  n_unique_tchp_type <- length(unique_tchp_type)

  # init a transition matrix to fill in
  Mat <- matrix(0, nrow = n_unique_tchp_type + 2, ncol = n_unique_tchp_type + 2)
  colnames(Mat) <- c("conv", "drop", unique_tchp_type)
  row.names(Mat) <- c("conv", "drop", unique_tchp_type)

  # for each possible state - touchpoint index
  for (tpi in 1:n_unique_tchp_type) {
    tp <- unique_tchp_type[tpi]
    print(paste0("Start: ", tp))

    # get the index of where the touch-point occurs across all paths
    split_paths_trimmed <- purrr::map(split_paths, ~ stringr::str_trim(.x))
    index_list <- purrr::map(split_paths_trimmed, ~ which(.x == tp))

    # get the counts of transitions to the next touch-point by adding 1 to the index list
    tp_count <- purrr::map2(split_paths, index_list, ~ .x[.y + 1])

    tp_count_total <- unlist(purrr::map2(tp_count, total_count, ~ table(.x) * .y))

    tb_count_table <- data.frame(name = names(tp_count_total), value = tp_count_total)
    tb_count_table <- dplyr::group_by(tb_count_table, name)
    tb_count_table <- dplyr::summarise(tb_count_table , val = sum(value))

    total_conv_count <- sum(conv_count[unlist(purrr::map(split_paths_trimmed, ~ .x[length(.x)] == tp))])
    total_drop_count <- sum(drop_count[unlist(purrr:: map(split_paths_trimmed, ~ .x[length(.x)] == tp))])

    ## CHECK contents of the table
    tb_count_table <-
      rbind(
        data.frame(
          name = c("conv", "drop"),
          val = c(total_conv_count, total_drop_count)
        ),
        tb_count_table
      )

    # change to probabilities
    tp_prob <- tb_count_table$val / sum(tb_count_table$val)
    # set the names
    tp_prob_names <- tb_count_table$name


    # fill in the matrix with the estimated probabilities
    for (i in 1:length(tp_prob)) {
      # print(paste(print(tp), tp_prob_names[i], tp_prob[tp_prob_names[i]]))
      Mat[tp, tp_prob_names[i]] <- tp_prob[i]
    }
    print(paste0("END: ", tp))
  }

  # set conversion as a sinking node
  Mat["conv", "conv"] <- 1
  Mat["drop", "drop"] <- 1

  start_count <- table(unlist(purrr:: map(split_paths, ~ stringr::str_trim(.x[1]))))
  start_prob <- start_count / sum(start_count)
  start_prob <- c(start_prob, conv = 0, drop = 0)
  start_prob <- start_prob[colnames(Mat)]
  start_prob[is.na(start_prob)] <- 0
  names(start_prob) <- colnames(Mat)

  # Mat = Mat[-which(rownames(Mat) == "conversion"),]
  M <- rbind(start = start_prob[colnames(Mat)], Mat)
  M <- cbind(start = 0, M)

  return(M)
}
