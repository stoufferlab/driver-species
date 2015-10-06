setwd("~/github/controlling-pollination-networks/")
library(magrittr)
library(foreach)
library(doMC)
registerDoMC(cores = 4)
# load functions
a <- list.files("./code/functions", full.names = TRUE) %>% lapply(source)

folder <- "./data/maximum_matchings"
filenames <- list.files(folder)

plyr::ldply(filenames, function(x){
	
 n_lines <- file.path(folder, x) %>% paste("wc -l", .) %>%
 	system(intern = T) %>%
 	stringr::str_split(" ") %>% 
 	unlist() %>%
 	magrittr::extract(1) %>%
 	as.numeric()
 
 m_size <- scan(file.path(folder, x), nlines = 1) %>% length()
	
 net <- x %>% stringr::str_split("_") %>% unlist() %>% extract(1)
 type <- x %>% stringr::str_split("_") %>% unlist() %>% extract(2)
 keep <- x %>% stringr::str_split("_") %>% unlist() %>% extract(3) %>%
 	stringr::str_split("\\.") %>% unlist() %>% extract(1)
 net <- "./data/networks" %>% file.path(paste0(net, ".dat")) %>%
 	readRDS() %>%
 	# select only the largest connected component of the network
 	keep_largest_component()
 net_bi <- net %>%
 	# transform the network into the bipartite format we need
 	bipartite_digraph(type = type, keep = keep) %>%
 	digraph_bipartite() %>%
 	igraph::add_layout_(igraph::as_bipartite())
 
 
 
 freq <- seq(1, n_lines, by = 1000) %>%
 	plyr::adply(1, function(y) {
 		 f <- scan(file.path(folder, x), nlines = 1000, skip = y-1) %>% 
 			matrix(ncol = m_size, byrow = T) %>%
 			apply(1, function(z){
 				matched <- igraph::ends(net_bi, igraph::E(net_bi)[z + 1])[, 2]
 				!(igraph::V(net_bi)[igraph::V(net_bi)$type]$name %in% matched)
 			}) %>%
 			apply(1, sum)
 		 
 		 dplyr::data_frame(node = igraph::V(net_bi)[igraph::V(net_bi)$type]$name,
 		 									freq = f) %>%
 		 	dplyr::rowwise() %>%
 		 	dplyr::mutate(node = stringr::str_split(node, "\\.") %>% 
 		 									unlist() %>% magrittr::extract(1))
 	}, .parallel = T)
 

 freq %>%
 	dplyr::group_by(node) %>%
 	dplyr::summarise(freq = sum (freq)) %>% 
 	dplyr::mutate(name = unlist(stringr::str_split(x, "_"))[1],
 								type = unlist(stringr::str_split(x, "_"))[2]) 

}, .progress = "text") %>%
	write.csv(file = "./data/node_max-matching.dat", row.names = F)
