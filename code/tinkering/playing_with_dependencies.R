library(magrittr)
library(foreach)
library(doMC) 
registerDoMC(cores = as.numeric(commandArgs(trailingOnly = T)[1]))
setwd("~/github/driver-species/")

"./code/V2.0/functions" %>% 
  list.files(full.names = T) %>%
  plyr::l_ply(source)

# read networks
net <- "./data/V2.0/networks" %>%
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

library(igraph)
library(bipartite)

n <- onet[[3]] %>% keep_largest_component()

matched_frequency(n)
matched_frequency(n, type = "z-bi")


bipartite_digraph(n) %>% p() digraph_bipartite() %T>% p %>% max_bipartite_match()
bipartite_digraph(net, type = "z-bi") %>% p  %>% digraph_bipartite() %T>% p %>% max_bipartite_match()

