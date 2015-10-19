# This script detects which nodes and which links are critical or redundant for
# controling the whole network dynamics
setwd("~/github/controlling-pollination-networks/")

# which network
i <- as.integer(commandArgs(trailingOnly = TRUE)[1])

# load libraries
library(magrittr)
# load utility functions
a <- list.files("./code/functions", full.names = TRUE) %>% lapply(source)

# load networks
net <- list.files("./data/networks/", full.names = TRUE) %>%
	lapply(readRDS)
names(net) <- list.files("./data/networks/", full.names = FALSE) %>%
	stringr::str_split("\\.") %>% lapply(`[`, 1) %>% unlist ()

# generate parameter spaces
suppressWarnings({
	suppressMessages({
		space <- expand.grid(type = c("z-bi", "weight", "AB", "BA"), 
												 net = 1:length(net)) %>%
			dplyr::inner_join(data.frame(type = c("z-bi", "AB", "BA", "weight", "weight"),
																	 keep = c("all", "A", "B", "A", "B")))
	})
})

# select the corresponding one for this computation
p <- space[i,]

message("Critical Elements Index ", i, " Working on net ", p$net, "; type ", p$type)

# get baseline network
m_net <- net[[p$net]] %>%
	keep_largest_component() %>%
	bipartite_digraph(type = p$type, keep = p$keep)
igraph::E(m_net)$weight <- 1

# get baseline number of unmatched vertices
base_n_unm <- n_unmatched_vertex(m_net)

# delete each vertex in turn
node_redundancy <- plyr::ldply(igraph::V(m_net), function(x){
	data.frame(n = x, 
						 n_unm = m_net %>% 
						 	igraph::delete_vertices(x) %>%
						 	n_unmatched_vertex())
}) %>%
	dplyr::select(-`.id`) %>%
	dplyr::mutate(robustness = categ_node(n_unm, base_n_unm),
								d_driver = n_unm - base_n_unm) %>%
	dplyr::select(-n_unm)

# delete each edge in turn
link_redundancy <- plyr::ldply(igraph::E(m_net), function(x){
	data.frame(l = x, 	
						 n_unm = m_net %>% igraph::delete_edges(x) %>%
						 	n_unmatched_vertex())
}) %>%
	dplyr::mutate(robustness = categ_link(n_unm, base_n_unm),
								d_driver = n_unm - base_n_unm) %>%
	dplyr::select(-n_unm)
	
# set the filename where the matchings will be saved
name <- paste(names(net)[p$net], p$type, p$keep, sep = "_")
# save the node robustness
filename <- paste0("./data/redundancy/node/", name, ".dat")
write.csv(node_redundancy, file = filename, row.names = FALSE)
# save the link robustness
filename <- paste0("./data/redundancy/edge/", name, ".dat")
write.csv(link_redundancy, file = filename, row.names = FALSE)

