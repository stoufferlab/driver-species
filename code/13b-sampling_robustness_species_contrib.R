library(magrittr)
library(foreach)
library(doMC) 
setwd("~/github/driver-species/")
registerDoMC(cores = 8)

# establish the parameters for this run
this_task <- as.numeric(commandArgs(trailingOnly = T)[1])
task_index <- expand.grid(completeness = seq(1, 0.5, by = -0.01),
						replicate = 1:10) %>%
	dplyr::filter(!(completeness == 1 & replicate != 1)) %>%
	dplyr::mutate(task = 1:nrow(.))
this_task <- dplyr::filter(task_index, task == this_task)

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

# parameters for the computation 
scale <- F
weight.type <- "asymmetry"

1:length(onet) %>%
	plyr::mlply(function(x){
		
		set.seed(this_task$replicate * this_task$completeness * 100)
		n_edges_to_remove <- round(length(igraph::E(onet[[x]])) * (1 - this_task$completeness[1]))
		edges_to_remove <- sample(igraph::E(onet[[x]]), n_edges_to_remove, prob = 1/igraph::E(onet[[x]])$weight)
		subsampled_network <- igraph::delete_edges(onet[[x]], edges_to_remove)
		
		o <- matched_frequency(subsampled_network, 
													 prop = seq(0, 1, by = 0.1),
													 weight.type = as.character(weight.type),
													 scale = scale, 
													 tmpdir = "/data/efc29/tmp/") 
		saveRDS(o, 
						file = paste0("./data/processed", 
													"/sampling_robustness/",
													weight.type, "/", "scaled_", scale, "/",
													names(onet)[x], 
													"_sampling_", this_task$completeness, 
													"_replicate_", this_task$replicate, ".rds"), 
						ascii = TRUE, compress = F)
		return(o)
	}, .progress = "text")
