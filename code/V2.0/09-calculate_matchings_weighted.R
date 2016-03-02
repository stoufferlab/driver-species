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
  plyr::ldply(n_matched, type = "z-bi") %>%
  `names<-`(c("net_name", "n_matched"))

# calculate the frequency of species matchings
m_freq <- 1:length(net) %>%
  plyr::mlply(function(x) matched_frequency(net[[x]], matched$n_matched[x], type = "z-bi"),
              .progress = "text")

names(m_freq) <- names(net)

# save data
saveRDS(matched, file = "./data/V2.0/n_matched_bi.rds", ascii = T, compress = F)
saveRDS(m_freq, file = "./data/V2.0/matching_frequency_bi.rds", ascii = T, compress = F)
