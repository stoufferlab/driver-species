# Calculate number of matched species in a bipartite network
n_matched <- function(x, type = "weight", keep = "all", output = "matching_size", ...){
	
	x %>% keep_largest_component() %>%
		bipartite_digraph(type, keep, ...) %>%
		digraph_bipartite() %>%
		igraph::max_bipartite_match() %>%
		magrittr::extract2(output)
	
}
