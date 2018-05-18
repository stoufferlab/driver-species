example_ntw_structural <- function(){
  en_controllable <- list(
    edges = dplyr::data_frame(from = c("x_1", "x_2", "u_1"), 
                              to   = c("x_2", "x_3", "x_1"),
                              control_type = c("a", "a", "b")), 
    vertex = dplyr::data_frame(name = c("x_1", "x_2", "x_3", "u_1"), 
                               control_type = c("a", "a", "a", "b"))
  )
  
  en_inaccessible <- list(
    edges = dplyr::data_frame(from = c("x_1", "x_3", "u_1"), 
                              to   = c("x_2", "x_2", "x_1"),
                              control_type = c("a", "a", "b")), 
    vertex = dplyr::data_frame(name = c("x_1", "x_2", "x_3", "u_1"),
                               control_type = c("a", "a", "a", "b"))
  )
  
  en_dilation <- list(
    edges = dplyr::data_frame(from = c("x_2", "x_2", "u_1"), 
                              to   = c("x_1", "x_3", "x_2"),
                              control_type = c("a", "a", "b")),
    vertex = dplyr::data_frame(name = c("x_1", "x_2", "x_3", "u_1"), 
                               control_type = c("a", "a", "a", "b"))
  )
  
  en_inaccessible_ok <- add_vertex_edge(en_inaccessible, "u_2", "b", "u_2", "x_3", "b")
  en_dilation_ok <- add_vertex_edge(en_dilation, "u_2", "b", "u_2", "x_3", "b")
  
  list(en_controllable, en_inaccessible, en_inaccessible_ok, en_dilation, en_dilation_ok) %>%
    purrr::map(~ igraph::graph_from_data_frame(.$edges, directed = T, vertices = .$vertex))
  
}


example_ntw_directions <- function(){
  en_vis <- list(
    edges = dplyr::data_frame(from = c("p_1", "p_2", "p_2", "p_3", "p_3"), 
                              to = c("a_1", "a_1", "a_2", "a_2", "a_1"), 
                              weight = c(1, 2, 3, 1, 1)), 
    vertex = dplyr::data_frame(name = c("p_1", "p_2", "p_3", "a_1", "a_2"), 
                               type = c("p", "p", "p", "a", "a"))
  )
  
  en_bip <- igraph::graph_from_data_frame(en_vis$edges, directed = F, vertices = en_vis$vertex) 
  en_dir <- as_directed_network(en_bip, direction = "asymmetry", ties = "both", higher_level = "a")
  # plot(en_dir)
  # length(all_matchings(en_dir))
  
  list(en_bip, en_dir)
}

example_ntw_matchings_chain <- function(){
  list(
    edges = dplyr::data_frame(from = c("x_1", "x_2", "x_3"), 
                              to =   c("x_2", "x_3", "x_4"), 
                              control_type = c(rep("a", 3))), 
    vertex = dplyr::data_frame(name = c("x_1", "x_2", "x_3", "x_4"), 
                               control_type = c(rep("a", 4)))
  ) %$%
    igraph::graph_from_data_frame(edges, directed = T, vertices = vertex)
}


example_ntw_matchings_star <- function(){
  list(
    edges = dplyr::data_frame(from = c("x_1", "x_1", "x_1"), 
                              to   = c("x_2", "x_3", "x_4" )), 
    vertex = dplyr::data_frame(name = c("x_1", "x_2", "x_3", "x_4"))
  ) %$%
    igraph::graph_from_data_frame(edges, directed = T, vertices = vertex) 
}

matchings_example_networks <- function(x){
  x %>%
    all_matchings() %>%
    purrr::map(add_control_nodes)
}
