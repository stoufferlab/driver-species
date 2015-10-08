netBIt <- net[[1]] %>%
	keep_largest_component() %>%
	bipartite_digraph(type = "z-bi", keep = "all")

netBI <- net[[1]] %>%
	keep_largest_component() %>%
	bipartite_digraph(type = "z-bi", keep = "all") %>%
	igraph::delete_vertices("p_1")

netABt <- net[[1]] %>%
	keep_largest_component() %>%
	bipartite_digraph(type = "AB", keep = "A")

netAB <- net[[1]] %>%
	keep_largest_component() %>%
	bipartite_digraph(type = "AB", keep = "A") %>%
	igraph::delete_vertices("p_1")

netBAt <- net[[1]] %>%
	keep_largest_component() %>%
	bipartite_digraph(type = "BA", keep = "B") 

netBA <- net[[1]] %>%
	keep_largest_component() %>%
	bipartite_digraph(type = "BA", keep = "B") %>%
	igraph::delete_vertices("p_1")

a <- . %T>% igraph::plot.igraph(vertex.size = 10, edge.arrow.size = 0.2, vertex.color = V(.)$type == "pla") %>% igraph::add_layout_(igraph::as_bipartite(V(.)$type == "pla")) %T>%
	igraph::plot.igraph(vertex.size = 10, edge.arrow.size = 0.2) %>% digraph_bipartite() %T>% igraph::plot.igraph(vertex.size = 10, edge.arrow.size = 0.2)   %>%
	igraph::add_layout_(igraph::as_bipartite()) %>%
	igraph::max_bipartite_match()

35-a(netBIt)$matching_size
35-a(netABt)$matching_size
a(netBA)$matching_size

# calculate a maximum matching (so we know the size)
matching$matching[is.na(matching$matching) 
									& grepl("to", names(matching$matching))] %>%
	length()

library(igraph)
net[[1]] %>%
	add_layout_(as_bipartite(V(.)$type=="pla")) %>% plot.igraph()

matching <- igraph::max_bipartite_match(m_net)

# actually calculate all the maximum matchings for the network
netABtb <- netABt %>% digraph_bipartite()
netBAtb <- netBAt %>% digraph_bipartite() 
netBItb <- netBIt %>% digraph_bipartite() 

b <- netBItb %>%
igraph::make_line_graph() %>%
	igraph::complementer() %>% 
	# 	igraph::count_max_cliques(min = matching$matching_size, 
	# 														max = matching$matching_size) %>%
	igraph::max_cliques(min = 16, 
											max = 16,
											file = "mama")

E(netBAtb)$sd <- FALSE
E(netBAtb)[b[[1]]]
b[[1]]
