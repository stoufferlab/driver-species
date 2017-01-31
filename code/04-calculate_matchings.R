library(magrittr)
library(foreach)
library(doMC) 
registerDoMC(cores = as.numeric(commandArgs(trailingOnly = T)[2]))
net_number <- as.integer(commandArgs(trailingOnly = T)[1])
tmpdir <- as.character(commandArgs(trailingOnly = T)[3])
message("gonna save stuff in ", tmpdir)
setwd("~/github/driver-species/")

message("loading functions")

"./code/functions" %>% 
	list.files(full.names = T) %>%
	plyr::l_ply(source)
  
message("reading networks")

# read networks 
net <- "./data/processed/networks" %>% 
	read_networks()

# read metadata
meta <- readr::read_csv("data/ntw_info.csv") %>% dplyr::tbl_df()

# Calculate maximum matchings 
matched <- net %>%
	plyr::ldply(n_matched) %>%
	`names<-`(c("net_name", "n_matched"))

# Order by size
ordered_net_names <- dplyr::inner_join(meta, matched) %>% 
  dplyr::mutate(ncomb = choose(n_pla + n_pol, n_matched)) %>%
  dplyr::arrange(ncomb) %$% net_name

onet <- net[ordered_net_names]

# Parameters
scale <- FALSE
weight.type <- "asymmetry"
keep <- "all"
type <- "weight"

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

if(net_number != 0){
	subset_onet <- onet[net_number]
}

# calculate matchings
all_matchings <- subset_onet %>%
	plyr::llply(all_unidirected_networks, type, keep, weigh.type, scale) %>%
	plyr::llply(function(x){
		x %>% 
			plyr::llply(function(y) {
				matched_frequency(n = NULL, dir = y, type = type, 
									keep = keep, seq(0, 1, by = 0.1),
									# temporary directory goes as a input argument
									tmpdir = tmpdir)
									}) 
	})

# save data
all_matchings %>%
	names() %>%
	plyr::l_ply(function(x){
		all_matchings[[x]] %>%
			names() %>%
			plyr::l_ply(function(y){
				saveRDS(all_matchings[[x]][[y]], 
								file = paste0("./data/processed/matching_frequency/",
															weight.type, "/", "scaled_", scale, "/",
															x,"_", y, ".rds"), 
								ascii = TRUE, compress = F)
			})
	})

