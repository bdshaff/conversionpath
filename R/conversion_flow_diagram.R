#' Simulate Paths and Generate Conversion Flow Diagram
#'
#' @param transition_matrix transition_matrix
#' @param num_steps num_steps
#' @param num_sim num_sim
#' @return a list


conversion_flow_diagram <- function(transition_matrix, num_steps, num_sim) {
  sim_tb <- simulate_path_table(transition_matrix, num_steps, num_sim)

  sim_tb_agg <-
    sim_tb %>%
    group_by(source, target) %>%
    summarise(volume = n()) %>%
    filter(volume > 0)

  nodes <-
    data.frame(name = c(unique(sim_tb_agg$source), unique(sim_tb_agg$target))) %>%
    unique() %>%
    mutate(id = 1:n() - 1) %>%
    mutate(
      step = as.factor(as.numeric(as.factor(str_sub(name, 1, 1)))),
      group = str_extract(name, "\\-.*")
    )

  links <-
    sim_tb_agg %>%
    ungroup() %>%
    left_join(nodes, by = c("source" = "name")) %>%
    rename(src = id) %>%
    left_join(nodes, by = c("target" = "name")) %>%
    rename(tar = id) %>%
    select(source = src, target = tar, volume) %>%
    as.data.frame()

  p <- plot_ly(
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
  ) %>%
    layout(
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
