# generate randomisations of the weighted networks and calculate the number of
# matched species

# ARGUMENTS: 
#			1: the number of cores to be used
#			2: the number of randomisations per matrix 
 

library(magrittr)
library(foreach) 
library(doMC)
registerDoMC(cores = as.numeric(commandArgs(trailingOnly = T)[1]))
setwd("~/github/driver-species/")
nperm <- commandArgs(trailingOnly = T)[2]

"./code/V2.0/functions" %>% 
	list.files(full.names = T) %>%
	plyr::l_ply(source)

# read networks
net <- "./data/V2.0/networks" %>%
	read_networks()

null_methods <- c(
	"quasiswap_count",
	# "swsh_samp",
	# "swsh_samp_r",
	# "swsh_samp_c",
	"swsh_both",
	# "swsh_both_r",
	# "swsh_both_c",
	"r00_ind",
	# "r0_ind",
	# "c0_ind",
	"r00_samp",
	# "r0_samp",
	# "c0_samp",
	"r00_both"
	# "r0_both",
	# "c0_both"
) 

# for each network
plyr::ldply(net, function(n){	
	
	# for each method
	plyr::ldply(null_methods, function(y){
		
		vertex_type <- igraph::V(n)$type
		# generate the random matrix
		random_matrices <- n %>%
			igraph::as_incidence_matrix(types = vertex_type == "pol", 
																	attr = "weight") %>%
			vegan::nullmodel(method = y) %>%
			simulate(nperm)
		
		# for each type of assumption
		list(weight = "weight", z_bi = "z-bi") %>%
		  plyr::ldply(function(z){
		    # for each random matrix
		    plyr::adply(random_matrices, 3, function (x){ 
		      plyr::try_default({
		        rn <- x %>%
		          # create network
		          igraph::graph_from_incidence_matrix(weighted = TRUE)
		        igraph::V(rn)$type <- vertex_type
		        # calculate number of matched species
		        n_matched(rn, type = z, weight.type = "asymmetry", scale = F)
		      }, NULL)
		    }, .parallel = T)
		  }) %>% 
		  magrittr::set_colnames(c("type", "sim", "n_matched")) %>%
		  dplyr::mutate(method = y)
	})
}, .progress = "text") %>%
	saveRDS(file = "./data/V2.0/random_weights.rds", ascii = T, compress = F)
