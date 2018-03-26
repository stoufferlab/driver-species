generate_example_network <- function(){
	sp_names_fancy <- c(TeX("$p_1$"), TeX("$p_2$"), TeX("$p_3$"), TeX("$a_1$"), TeX("$a_2$"))
	sp_names_fancy_b <- c(TeX("$\\mathbf{p_1}$"), TeX("$\\mathbf{p_2}$"), TeX("$\\mathbf{p_3}$"), TeX("$\\mathbf{a_1}$"), TeX("$\\mathbf{a_2}$"))
	
	sp_names <- c("p1", "p2", "p3", "a1", "a2")
	
	adj_matrix <- c(0,0,0,1,0,
									0,0,0,2,5,
									0,0,0,0,0,
									0,0,1,0,0,
									0,0,1,0,0) %>%
		matrix(nrow = 5, byrow = T)
	
	example_net <- adj_matrix %>% 
		graph_from_adjacency_matrix(mode = "undirected", weighted = TRUE) 
	igraph::V(example_net)$type <- c(F, F, F, T, T)
	igraph::V(example_net)$name <- sp_names
	example_net
}

