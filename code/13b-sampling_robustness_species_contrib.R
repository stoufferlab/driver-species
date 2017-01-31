library(magrittr)
library(foreach)
library(doMC) 
setwd("~/github/driver-species/")

# establish the parameters for this run
this_task <- as.numeric(commandArgs(trailingOnly = T)[1])
registerDoMC(cores = as.numeric(commandArgs(trailingOnly = T)[2]))
tmpdir <- as.character(commandArgs(trailingOnly = T)[3])
overwrite <- as.logical(commandArgs(trailingOnly = T)[4])

task_index <- expand.grid(network = 1:24,
													completeness = seq(1, 0.5, by = -0.01),
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
scale <- FALSE
weight.type <- "asymmetry"
keep <- "all"
type <- "weight"

subset_onet <- onet[[this_task$network]]
# set seed
set.seed(this_task$network * this_task$replicate * this_task$completeness * 100)

# generate a subsampled network
n_edges_to_remove <- round(length(igraph::E(subset_onet)) * 
																		(1 - this_task$completeness[1]))
edges_to_remove <- sample(igraph::E(subset_onet), n_edges_to_remove, 
																		prob = 1/igraph::E(subset_onet)$weight)
subsampled_network <- igraph::delete_edges(subset_onet, edges_to_remove)

# get all the possible directed networks out of that one
directed_networks <- all_unidirected_networks(subsampled_network, type, keep, 
																							weight.type, scale)

directed_networks %>%
	names() %>%
	plyr::l_ply(function(x){
		# check if it exists
		file_name <- paste0("./data/processed", 
												"/sampling_robustness/",
												weight.type, "/", "scaled_", scale, "/",
												names(onet)[this_task$network], 
												"_sampling_", this_task$completeness, 
												"_replicate_", this_task$replicate, "_", x, ".rds")
		if(!overwrite & file.exists(file_name)) {
			return(NULL)
		} else {
			matched_frequency(n = NULL, dir = directed_networks[[x]], type = type, 
												keep = keep, seq(0, 1, by = 0.1),
												# temporary directory goes as a input argument
												tmpdir = tmpdir
												) %>%
				saveRDS(file = file_name, 
								ascii = TRUE, compress = F)
		}
	})

