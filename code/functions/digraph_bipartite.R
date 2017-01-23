#' 
#' 
#' takes a directed graph and convert it into a bipartite representation with
#' duplicated nodes
#' 
#' Before using the function for production, ensure that the edge order is the
#' same in the input and the output network. That will make things easier when
#' mapping back the matchings to the original directed network
#' 
#' @param net an igraph object
#' @param type ignore for unweighted networks
#' @return the bipartite representation as an igraph object
#' @export
#' 
#' @examples
digraph_bipartite <- function(net, type = "weight"){
	edges <- dplyr::data_frame(di_name = igraph::V(net)$name,
														 name = paste(di_name, "fr", sep = "."),
														 guild = igraph::V(net)$type,
														 type = "from") %>%
		rbind(dplyr::data_frame(di_name = igraph::V(net)$name,
														name = paste(di_name, "to", sep = "."),
														guild = igraph::V(net)$type,
														type = "to"))
	adj_matrix <- igraph::as_adjacency_matrix(net, sparse = F, attr = "weight")
	
	if(type == "z-bi"){
		adj_matrix %<>% t()
		colnames(adj_matrix) <- dplyr::filter(edges, type == "from") %$% name
		rownames(adj_matrix) <- dplyr::filter(edges, type == "to") %$% name
	} else {
		rownames(adj_matrix) <- dplyr::filter(edges, type == "from") %$% name
		colnames(adj_matrix) <- dplyr::filter(edges, type == "to") %$% name
	}
	igraph::graph_from_incidence_matrix(adj_matrix, weight = TRUE, 
																					 directed = F, mode = "out") %>% 
		igraph::set_vertex_attr(name = "guild", value = edges$guild) %>%
		igraph::set_vertex_attr(name = "di_name", value = edges$di_name)
}