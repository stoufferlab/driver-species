#' takes a directed graph and convert it into a bipartite representation with
#' duplicated nodes
#' 
#' @param x an igraph directed network
#' @param weighted wether the output should include weights or not
#' @return an igraph object with the bipartite representation. Vertex names correspond to the original with and added ..f for origin nodes and ..t for receiving nodes. Attribute legacy types includes the original types from the original bipartite representation
#' 
#' @examples
alternative_bipartite <- function(x, weighted = FALSE){
  
  if(weighted) {
    attribute <- "weight"
  } else {
    attribute <- NULL
  }
  
  # error checking to be added
  # 1. check that x is bipartite (has type attribute)
  # 2. check that if weighted=TRUE the weight attibute actually exists
  
  igraph::as_adjacency_matrix(x, attr = attribute, edges = F, sparse = F) %>%
    `rownames<-`(paste0(rownames(.), "..f")) %>%
    `colnames<-`(paste0(colnames(.), "..t")) %>%
    igraph::graph_from_incidence_matrix(weighted = attribute) %>%
    igraph::set_vertex_attr("legacy.type", value = igraph::V(x)$type)
}

maximum_matching <- function(x, weighted = FALSE){
  
  x_bip <- alternative_bipartite(x, weighted = weighted)
  
  m <- igraph::max_bipartite_match(x_bip)
  
  igraph::V(x)$matched <- m %$%
    matching %>%
    extract(igraph::V(x_bip)$type) %>%
    is.na(.) %>%
    not()
  
   matched_edges_bip <- m %$%
    matching %>%
    extract(igraph::V(x_bip)$type) %>% {
      cbind(from = ., 
            to = names(.))
    } %>% 
     dplyr::as_data_frame() %>%
     dplyr::filter(!is.na(from)) %>%
     as.matrix() %>%
     t()
   
   igraph::E(x_bip)$matched <- FALSE
   igraph::E(x_bip)[igraph::get.edge.ids(x_bip, matched_edges_bip, error = F)]$matched <- TRUE
   
   matched_edges <- matched_edges_bip %>% stringi::stri_sub(., 1, -4)
   
   igraph::E(x)$matched <- FALSE
   igraph::E(x)[igraph::get.edge.ids(x, matched_edges, error = F)]$matched <- TRUE
   
   x %<>%
     igraph::set_graph_attr("matching_size", m$matching_size) %>%
     igraph::set_graph_attr("bipartite_representation", x_bip)
   
   return(x)
}

get_input_graph <- function(x){
  
  if(!"matching_size" %in% graph_attr_names(x)){
    x %<>% maximum_matching()
  }
  
  driver <- igraph::V(x)[!igraph::V(x)$matched]
  non_driver <- igraph::V(x)[igraph::V(x)$matched]
  
  input_graph <- . %>%
    purrr::map(~get_control_adjacent(x, .)) %>%
    purrr::map(make_graph_from_vertex) %>%
    do.call(union_input_graphs, .)
  
 
  ig <- input_graph(driver) %>%
    igraph::set_vertex_attr("input_node", value = TRUE) %>%
    union_input_graphs(input_graph(non_driver) %>% 
                         set_vertex_attr("input_node", value = F), 
                       delete_graph_attr = F) 
  
  set_graph_attr(x, "input_graph", ig)
}

get_control_adjacent <- function(x, d){
  
  d_bip <- paste0(d$name, "..t")
  
  adjacent_bip <- igraph::V(x$bipartite_representation)[distances(x$bipartite_representation, d_bip) == 2] %>%
    purrr::map_lgl(~is_adjacent(x$bipartite_representation, d_bip, .)) %>%
    `names<-`(stringi::stri_sub(names(.), 1, -4))

  igraph::V(x)[c(d$name, names(adjacent_bip[adjacent_bip]))]
}


is_adjacent <- function(xb, d_bip, pab){
  xb %>%
    igraph::shortest_paths(d_bip, pab, output = "epath") %$%
    epath %>%
    extract2(1)%>% 
    {xor(.$matched[1], .$matched[2])}
}

make_graph_from_vertex <- function(v){
  igraph::make_full_graph(length(v)) %>%
    igraph::set_vertex_attr("name", value = v$name) %>%
    igraph::set_vertex_attr("type", value = v$type) 
}

union_input_graphs <- function(..., delete_graph_attr = TRUE){
  nets <- list(...)
  
  at <- igraph::vertex_attr_names(nets[[1]])
  at <- at[at != "name"]
  n_nets <- length(nets)
  
  joint_network <- nets %>%
    do.call(igraph::union, .)
  
  for (i in 1:length(at)){
    values <- 1:n_nets %>%
      purrr::map(~ coalece_attribute(joint_network, at[i], .)) %>%
      do.call(dplyr::coalesce, .)
    for(j in 1:n_nets){
      if(delete_graph_attr){
        joint_network %<>%
          igraph::delete_graph_attr(paste("name", j, sep = "_")) %>%
          igraph::delete_graph_attr(paste("loops", j, sep = "_"))
      }
      joint_network %<>%
        igraph::delete_vertex_attr(paste(at[i], j, sep = "_"))
    }
    joint_network %<>%
      set_vertex_attr(at[i], value = values)
  }
  joint_network
}

coalece_attribute <- function(x, y, n){
  vertex_attr(x, name = paste(y, n, sep = "_"))
}

control_capacity <- function(x){
  
  if(!"input_graph" %in% graph_attr_names(x)){
    x %<>% get_input_graph()
  }
  
  comp <- induced_subgraph(x$input_graph, V(x$input_graph)[V(x$input_graph)$input_node]) %>%
    components() 
  
  n_control_configurations <- prod(comp$csize)
  cc <- 1 / comp$csize

  V(x$input_graph)$control_capacity <- 0
  V(x$input_graph)[V(x$input_graph)$input_node]$control_capacity <- rep(cc, rle(comp$membership)$lengths)
  
  V(x)[V(x$input_graph)$name]$control_capacity <- V(x$input_graph)$control_capacity
  x
}
