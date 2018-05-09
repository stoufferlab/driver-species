example_ntw_structural <- function(){
  en_controllable <- list(
    edges = dplyr::data_frame(from = c("x_1", "x_2", "u_1"), 
                              to   = c("x_2", "x_3", "x_1"),
                              type = c("a", "a", "b")), 
    vertex = dplyr::data_frame(name = c("x_1", "x_2", "x_3", "u_1"), 
                               type = c("a", "a", "a", "b"))
  )
  
  en_inaccessible <- list(
    edges = dplyr::data_frame(from = c("x_1", "x_3", "u_1"), 
                              to   = c("x_2", "x_2", "x_1"),
                              type = c("a", "a", "b")), 
    vertex = dplyr::data_frame(name = c("x_1", "x_2", "x_3", "u_1"),
                               type = c("a", "a", "a", "b"))
  )
  
  en_dilation <- list(
    edges = dplyr::data_frame(from = c("x_2", "x_2", "u_1"), 
                              to   = c("x_1", "x_3", "x_2"),
                              type = c("a", "a", "b")),
    vertex = dplyr::data_frame(name = c("x_1", "x_2", "x_3", "u_1"), 
                               type = c("a", "a", "a", "b"))
  )
  
  en_inaccessible_ok <- add_vertex_edge(en_inaccessible, "u_2", "b", "u_2", "x_3", "b")
  en_dilation_ok <- add_vertex_edge(en_dilation, "u_2", "b", "u_2", "x_3", "b")
  
  list(en_controllable, en_inaccessible, en_inaccessible_ok, en_dilation, en_dilation_ok) %>%
    purrr::map(~ igraph::graph_from_data_frame(.$edges, directed = T, vertices = .$vertex))
  
}
