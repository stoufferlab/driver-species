# correct network to acount for possible biases originated by sample sizes per
# plant species. This is express visitation as a proportion of visits by all
# visitors to a plant species

standarise_network <- function(n){
	
	std <- n %>% 
		igraph::as_incidence_matrix(types = igraph::V(.)$type == "pol", 
																attr = "weight") %>%
		apply(1, function(x) x/sum(x)) %>% t() %>%
		igraph::graph_from_incidence_matrix(weighted = T) 
	
	new_type <- igraph::V(std)$type %>% as.factor() %>% 
		`levels<-`(c("pla", "pol")) %>% as.character()
	
	igraph::V(std)$type <- new_type
	
	return(std)
}