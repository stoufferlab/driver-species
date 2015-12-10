library(ggplot2)
library(magrittr)
# load utility functions
nn <- list.files("./code/functions", full.names = TRUE) %>% lapply(source)

net <- list.files("./data/networks/", full.names = TRUE) %>%
	lapply(readRDS)
names(net) <- list.files("./data/networks/", full.names = FALSE) %>%
	stringr::str_split("\\.") %>% lapply(`[`, 1) %>% unlist ()

space <- expand.grid(net = 1:length(net),
										 keep = c("A", "B"),
										 perm = 1:999)

rand_directions <- plyr::ddply(space, c("net", "keep", "perm"), function(x){
	
	m_net <- net[[x$net]] %>%
		# select only the largest connected component of the network
		keep_largest_component() %>%
		# transform the network into the bipartite format we need
		bipartite_digraph(type = "weight", keep = x$keep)
	
	n <- igraph::tail_of(m_net, igraph::E(m_net)) %>% names() %>%
		cbind(igraph::head_of(m_net, igraph::E(m_net)) %>% names())
	
	nr <- n %>%
		apply(1, sample) %>% c()

	rn <- igraph::make_directed_graph(nr)
	
	igraph::E(rn)$weight <- 1
	igraph::V(rn)$type <- grepl("i", names(igraph::V(rn)))
	igraph::V(rn)$type <- plyr::mapvalues(igraph::V(rn)$type, 
																			 c(TRUE, FALSE), c("pol", "pla"))
	
	data.frame(overlap = sum(c(t(n)) == nr)/ 2 / nrow(n),
						 n_driver = rn %>% n_unmatched_vertex())
}, .progress = "text")

write.csv(rand_directions, "./data/random_dir_n_driver.csv", row.names = FALSE)