# random directions

# ARGUMENTS: 
#			1: the number of cores to be used
#			2: the number of randomisations per matrix 

library(magrittr)
setwd("~/github/driver-species/") 

"./code/functions" %>% 
  list.files(full.names = T) %>%
  plyr::l_ply(source)

# read networks
net <- "./data/processed/networks" %>%
  read_networks()

# read metadata
meta <- readr::read_csv("data/ntw_info.csv") %>% dplyr::tbl_df()

types <- data.frame(type = c("z-bi", "weight", "AB", "BA"), 
            keep = "all")

# for each network
plyr::ldply(net, function(n){
  plyr::ddply(types, "type", function(x){
    n_matched(n, type = x$type, keep = x$keep)
  })
}, .progress = "text") %>%
  `names<-`(c("net_name", "type", "n_matched")) %>%
  saveRDS(file = "./data/processed/n_matched_all_types.rds", ascii = T, compress = F)
