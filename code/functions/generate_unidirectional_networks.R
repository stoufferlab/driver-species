# generate alternative networks
generate_unidirectional_network <- function(x, 
																						n,
																						upper_double_links,
																						lower_double_links) {
	upper_double_links %<>%
		dplyr::mutate(delete = as.logical(x))
	lower_double_links %<>%
		dplyr::mutate(delete = !x)
	rbind(upper_double_links, lower_double_links) %>%
		dplyr::filter(delete) %$% value %>%
		igraph::delete_edges(graph = n, edges = .)
}

# takes a network and returns a list with all the unidirectional networks
all_unidirected_networks <- function(x, type, keep, weight.type, scale) {
	dir <- x %>%
		keep_largest_component() %>%
		bipartite_digraph(type, keep, weight.type, scale)
	m <- igraph::as_adjacency_matrix(dir, sparse = F) 
	bi_links <- (m * t(m)) 
	
	# bi_links %>% lattice::levelplot()
	# adjacency matrix with the edge id
	edge_number <- igraph::as_adjacency_matrix(dir, sparse = F, edges = T) 
	
	# get the repeated links in the upper and the lower part
	upper_double_links <- edge_number %>% 
		multiply_by(bi_links) %>% 
		multiply_by(upper.tri(.)) %>% 
		reshape2::melt() %>% 
		dplyr::filter(value != 0)
	lower_double_links <- edge_number %>% 
		multiply_by(bi_links) %>% 
		multiply_by(lower.tri(.)) %>% 
		t() %>%  # transpose so that the order is the same as in the upper part
		reshape2::melt() %>% 
		dplyr::filter(value != 0)
	
	# count the number of repeated links
	n_combinations <- bi_links %>% sum() %>% divide_by(2)
	
	if(n_combinations == 0){
		all_net_versions <- list(A = dir)
	} else {
		# generate combinations for the edges
		combinations <- gtools::permutations(2, 
																				 n_combinations, v = 0:1, 
																				 repeats.allowed = T)
		# generate the alternative networks
		all_net_versions <- combinations %>%
			plyr::alply(1, generate_unidirectional_network, 
									dir, upper_double_links, lower_double_links)
		names(all_net_versions) <- LETTERS[1:length(all_net_versions)]
	}
	return(all_net_versions)
}
