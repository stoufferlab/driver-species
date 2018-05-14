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
     igraph::set_graph_attr("bipartite_representation", x_bip) %>%
     `class<-`(c(class(.), "matched_graph"))
   
   return(x)
}

