simulate_path_table = function(M, num_steps, num_sim){
  sim_tb = 
    map(1:num_sim, ~simulate_path(M, num_steps)$target_table) %>% 
    bind_rows(.id = "path_id")
  return(sim_tb)
}