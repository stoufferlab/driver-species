# calculate properties of species in the network. number of simulations is used
# only to calculate a species' contribution to nestedness ARGUMENTS: 1: the
# number of cores to be used 2: the number of randomisations per matrix

library(magrittr)
library(foreach)
library(doMC)
registerDoMC(cores = as.numeric(commandArgs(trailingOnly = T)[1]))
setwd("~/github/driver-species/")
nsimul <- commandArgs(trailingOnly = T)[2]

"./code/functions" %>% 
  list.files(full.names = T) %>%
  plyr::l_ply(source)

# read networks
net <- "./data/V2.0/networks" %>%
  read_networks()

# read metadata
meta <- readr::read_csv("data/ntw_info.csv") %>% dplyr::tbl_df()

# degree - count 
# strength - sum of dependences
# contribution to nestedness
# centrality

plyr::ldply(net, function(n){
  # calculate most metrics using igraph
  d1 <- dplyr::data_frame(species = igraph::V(n)$name ,
                          # degree
                          degree = n %>% igraph::degree(igraph::V(.)),
                          # visitation strength
                          v_strength = n %>% igraph::strength(igraph::V(.)),
                          # dependency strength
                          d_strength = n %>% 
                            igraph::as_adjacency_matrix(attr = "weight", 
                                                        sparse = F) %>% 
                            apply(1, function(x)  x / sum(x)) %>% t() %>%
                            apply(2, sum),
                          # eigenvector centrality
                          eig_cen = n %>% igraph::evcent() %$% vector)
  
  # calculate nested contriibution using my version of nested contribution/bipart
  d2 <- n %>% igraph::as_incidence_matrix(types = igraph::V(.)$type == "pol") %>%
    nested_contribution(nsimul = nsimul) %>% 
    plyr::ldply(function(x) dplyr::add_rownames(as.data.frame(x))) %>%
    dplyr::select_("rowname", "nestedcontribution") %>% 
    dplyr::rename(species = rowname)
   
  dplyr::inner_join(d1, d2)
}, .parallel = TRUE) %>%
  dplyr::rename_("net_name" = ".id") %>%
  dplyr::tbl_df() %>%
  saveRDS(file = "./data/V2.0/species_properties.rds", 
          ascii = TRUE, compress = FALSE)

