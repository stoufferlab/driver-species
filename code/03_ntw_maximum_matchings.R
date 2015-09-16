# This script calculates all the possible maximum matchings for each of the
# networks. It is designed to run in the cluster
setwd("~/github/controlling-pollination-networks/")

# which network
i <- as.integer(commandArgs(trailingOnly = TRUE)[1])
# load libraries
library(magrittr)
# load functions
list.files("./code/functions", full.names = TRUE) %>% lapply(source)
# load networksnano
net <- readRDS("./data/bartomeus/networks.dat")
# generate parameter space
space <- expand.grid(type = c("bi", "weight", "AB", "BA"), 
										 net = 1:length(net)) %>%
	dplyr::inner_join(data.frame(type = c("bi", "AB", "BA", "weight", "weight"),
															 keep = c("all", "A", "B", "A", "B")))
# select the corresponding one for this computation
p <- space[i,]
m_net <- net[[p$net]] %>%
	# select only the largest connected component of the network
	keep_largest_component() %>%
	# transform the network into the bipartite format we need
	bipartite_digraph(type = p$type, keep = p$keep) %>%
	digraph_bipartite() %>%
	igraph::add_layout_(igraph::as_bipartite())
# calculate a maximum matching (so we know the size)
matching <- igraph::max_bipartite_match(m_net)

# set the filename where the matchings will be saved
filename <- paste(names(net)[p$net], p$type, p$keep, sep = "_")
filename <- paste0("./data/bartomeus/maximum_matchings/", filename, ".dat")

# actually calculate all the maximum matchings for the network
igraph::make_line_graph(m_net) %>%
	igraph::complementer() %>% 
	igraph::count_max_cliques(min = matching$matching_size, 
														max = matching$matching_size)
	igraph::max_cliques(min = matching$matching_size, 
											max = matching$matching_size, 
											file = filename)