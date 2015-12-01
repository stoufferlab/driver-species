# This script calculates all the possible maximum matchings for each of the
# networks. It is designed to run in the cluster
setwd("~/github/driver-species/")

# which network
i <- as.integer(commandArgs(trailingOnly = TRUE)[1])
# load libraries
library(magrittr)
# load functions
a <- list.files("./code/functions", full.names = TRUE) %>% lapply(source)
# load networksnano
net <- list.files("./data/networks/", full.names = TRUE) %>%
	lapply(readRDS)
names(net) <- list.files("./data/networks/", full.names = FALSE) %>%
	stringr::str_split("\\.") %>% lapply(`[`, 1) %>% unlist ()
# generate parameter space
suppressWarnings({
	suppressMessages({
		space <- expand.grid(type = c("z-bi", "weight", "AB", "BA"), 
												 net = as.character(1:length(net))) %>%
			dplyr::inner_join(data.frame(type = c("z-bi", "AB", "BA", "weight", "weight"),
																	 keep = c("all", "A", "B", "A", "B"))) %>%
			dplyr::inner_join(plyr::ldply(net, function(x) length(igraph::E(x))) %>%
													dplyr::add_rownames() %>%
													dplyr::rename(net = rowname, 
																				name = .id,
																				n_int = V1)) %>%
			dplyr::arrange(type, n_int)
	})
})
# select the corresponding one for this computation
p <- space[i,]
message("Max Matching Index ", i, " Working on net ", p$net, "; type ", p$type)
m_net <- net[[as.numeric(p$net)]] %>%
	# select only the largest connected component of the network
	keep_largest_component() %>%
	# transform the network into the bipartite format we need
	bipartite_digraph(type = p$type, keep = p$keep) %>%
	digraph_bipartite() %>%
	igraph::add_layout_(igraph::as_bipartite())

igraph::E(m_net)$weight <- 1
# calculate a maximum matching (so we know the size)
matching <- igraph::max_bipartite_match(m_net)
# this igraph function is wrong

# set the filename where the matchings will be saved
filename <- paste(names(net)[as.numeric(p$net)], p$type, p$keep, sep = "_")
filename <- paste0("./data/maximum_matchings/", filename, ".dat")

igraph::make_line_graph(m_net) %>%
	igraph::complementer() %>% 
	# 	igraph::count_max_cliques(min = matching$matching_size, 
	# 														max = matching$matching_size) %>%
	igraph::max_cliques(min = matching$matching_size, 
											max = matching$matching_size, 
											file = filename)
