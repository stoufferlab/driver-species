folder <- "./data/redundancy/edge/"

link_redundancy <- list.files(folder, full.names = TRUE) %>%
	plyr::ldply(function(x, name){
		y <- read.csv(x) 
		info <- basename(x) %>% 
			stringr::str_sub(end = -5) %>% 
			stringr::str_split("_") %>% 
			unlist()
		y %>%
			dplyr::mutate(net = info[[1]],
										type = info[2],
										keep = info[3])
	}) 

net <- list.files("./data/networks/", full.names = TRUE) %>%
	lapply(readRDS)
names(net) <- list.files("./data/networks/", full.names = FALSE) %>%
	stringr::str_split("\\.") %>% lapply(`[`, 1) %>% unlist ()


mnet <- "MIQ1OP"
mtype <- "weight"
mkeep <- "A"

rob <- link_redundancy %>%
	dplyr::filter(net == mnet, type == mtype , keep == mkeep) %$%
	robustness

m_net <- net[[mnet]] %>%
	# select only the largest connected component of the network
	keep_largest_component() %>%
	# transform the network into the bipartite format we need
	bipartite_digraph(type = mtype , keep = mkeep)

igraph::plot.igraph(m_net, edge.arrow.size = .2, edge.color = as.integer(rob), vertex.size = 10)

