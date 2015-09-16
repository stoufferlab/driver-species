n_unmatched_vertex <- function(net){
	# transform the network into the bipartite format we need
	m_net <- net %>%
		digraph_bipartite() %>%
		igraph::add_layout_(igraph::as_bipartite())
	
	# calculate a maximum matching (so we know the size)
	matching <- igraph::max_bipartite_match(m_net)
	matching$matching[is.na(matching$matching) 
																	 & grepl("to", names(matching$matching))] %>%
		length()
}