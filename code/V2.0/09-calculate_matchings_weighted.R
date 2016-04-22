# calculate the nominal minimum matching and all the maximum matchings on the
# bidirectional networks using the weighted attriute of the dependency links

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

ordered_net_names <- dplyr::inner_join(meta, matched) %>% 
  dplyr::mutate(ncomb = choose(n_pla + n_pol, n_matched)) %>%
  dplyr::arrange(ncomb) %$% net_name

onet <- net[ordered_net_names]

# calculate the frequency of species matchings
m_freq <- 1:length(onet) %>%
  plyr::mlply(function(x) {
    print(names(onet)[x])
    o <- matched_frequency(onet[[x]],
                           prop = seq(0, 1, by = 0.1),
                           type = "z-bi")
    saveRDS(o, 
            file = paste0("./data/V2.0/matching_frequency_bi/", 
                          names(onet)[x], ".rds"), 
            ascii = TRUE, compress = F)
    return(o)
  }, .progress = "text")

names(m_freq) <- names(onet)

# save data
saveRDS(matched, file = "./data/V2.0/n_matched_bi.rds", ascii = T, compress = F)
saveRDS(m_freq, file = "./data/V2.0/matching_frequency_bi.rds", ascii = T, compress = F)
