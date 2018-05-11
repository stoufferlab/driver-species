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
