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

if(net_number != 0){
	subset_onet <- onet[net_number]
}

# calculate matchings
all_matchings <- subset_onet %>%
	plyr::llply(all_unidirected_networks, type, keep, weight.type, scale) %>%
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

