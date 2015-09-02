#' Title
#'
#' @param net 
#' @param type
#' @param matchings
#' @param side 
#' @param output 
#'
#' @return
#' @export
#'
#' @examples
get_matched_vertex <- function (net, 
																choosen_type,
																matchings,
																side = c("AB", "BC"), 
																output = c("matrix", "data.frame")){
	
	# get only the nodes in one side of the bipartite graph
	target_vertices <- igraph::V(net)[type == choosen_type]
	
	# cycle trough each matching
	m <- plyr::adply(matchings, 1, function(x){
		# make it a directed graph going from one side to the other
		vertex <- bipartite_digraph(net, side[1]) %>% 
			igraph::tail_of(x) %>% as.numeric()
		# find the vertices that were matched
		y <- as.numeric(target_vertices) %in% vertex
		names(y) <- target_vertices$name
		y
	})
	
	# if you want a data frame you'll get it
	if(output[1] == "data.frame") {
		m <- m %>%
			tidyr::gather(v, matched, -1) %>%
			dplyr::rename(matching = X1)
	}
	
	return(m)
		
}