setwd("~/github/controlling-pollination-networks/")
library(magrittr)

i <- as.integer(commandArgs(trailingOnly = T)[1])

# load functions
a <- list.files("./code/functions", full.names = TRUE) %>% lapply(source)

folder <- "./data/maximum_matchings"
filenames <- list.files(folder)

if(!file.exists("./data/maximum_matchings_space.dat")){
	space <- plyr::ldply(filenames, function(x){
		n_lines <- file.path(folder, x) %>% paste("wc -l", .) %>%
			system(intern = T) %>%
			stringr::str_split(" ") %>% 
			unlist() %>%
			magrittr::extract(1) %>%
			as.numeric()
		
		data.frame(file = x, line = seq(1, n_lines, by = 100000)) 
	}, .progress = "text") 
	
	space <- space %>% dplyr::group_by(file) %>% dplyr::mutate(m = max(line)) %>% 
		dplyr::group_by() %>%	dplyr::arrange(m, line)
	
	write.csv(space, file = file.path("./data/maximum_matchings_space.dat", filename), row.names = F)
} else {
	space <- read.csv("./data/maximum_matchings_space.dat")
}

p <- space[i, ]

m_size <- scan(file.path(folder, p$file), nlines = 1) %>% length()

net <- p$file %>% stringr::str_split("_") %>% unlist() %>% extract(1)
type <- p$file %>% stringr::str_split("_") %>% unlist() %>% extract(2)
keep <- p$file %>% stringr::str_split("_") %>% unlist() %>% extract(3) %>%
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

f <- scan(file.path(folder, p$file), nlines = 100000, skip = p$line-1) %>% 
	matrix(ncol = m_size, byrow = T) %>%
	apply(1, function(z){
		matched <- igraph::ends(net_bi, igraph::E(net_bi)[z + 1])[, 2]
		!(igraph::V(net_bi)[igraph::V(net_bi)$type]$name %in% matched)
	}) %>%
	apply(1, sum)

out <- dplyr::data_frame(node = igraph::V(net_bi)[igraph::V(net_bi)$type]$name,
									freq = f) %>%
	dplyr::rowwise() %>%
	dplyr::mutate(node = stringr::str_split(node, "\\.") %>% 
									unlist() %>% magrittr::extract(1))

filename <- p$file %>% stringr::str_split("\\.") %>% unlist() %>% magrittr::extract(1) %>%
	paste(p$line, sep = "_")

write.csv(out, file = file.path("./data/maximum_matchings_summary", filename), row.names = F)
