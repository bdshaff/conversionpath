#' Simulate Paths and Generate Conversion Flow Diagram
#'
#' Given a transition matrix run a simulation to generate `num_sim` number of paths with `num_steps`.
#' This function will return an aggregated table of transition points as well as the conversion flow diagram.
#'
#' @param transition_matrix transition_matrix
#' @param num_steps num_steps
#' @param num_sim num_sim
#' @return a list


conversion_flow <- function(transition_matrix, num_steps, num_sim) {
  sim_tb <- simulate_path_table(transition_matrix, num_steps, num_sim)

  sim_tb_agg <- dplyr::group_by(sim_tb, source, target)
  sim_tb_agg <- dplyr::summarise(sim_tb_agg, volume = dplyr::n())
  sim_tb_agg <- dplyr::filter(sim_tb_agg, volume > 0)

  nodes <- unique(data.frame(name = c(unique(sim_tb_agg$source), unique(sim_tb_agg$target))))
  nodes <- dplyr::mutate(nodes, id = 1:dplyr::n() - 1)
  nodes <- dplyr::mutate(nodes,
    step = as.factor(as.numeric(as.factor(stringr::str_sub(name, 1, 1)))),
    group = stringr::str_extract(name, "\\-.*")
  )

  links <- dplyr::ungroup(sim_tb_agg)
  links <- dplyr::left_join(links, nodes, by = c("source" = "name"))
  links <- dplyr::rename(links, src = id)
  links <- dplyr::left_join(links, nodes, by = c("target" = "name"))
  links <- dplyr::rename(links, tar = id)
  links <- as.data.frame(dplyr::select(links, source = src, target = tar, volume))

  p <- plotly::plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = nodes$name,
      color = pals::alphabet()[as.numeric(as.factor(nodes$group))],
      pad = 10,
      thickness = 30,
      line = list(
        color = "black",
        width = 0
      )
    ),
    link = list(
      source = links$source,
      target = links$target,
      value = links$volume
      # color = links$source
    )
  )
  p <-
    plotly::layout(p,
      title = "Conversion Flow Diagram",
      font = list(
        size = 10
      )
    )

  return(list(
    conversion_flow_diagram = p,
    nodes = nodes,
    links = links,
    simulated_path_table = sim_tb
  ))
}
