library(magrittr)
library(foreach)
library(doMC) 
registerDoMC(cores = as.numeric(commandArgs(trailingOnly = T)[1]))
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

# calculate the frequency of species matchings
expand.grid(scale = c(F
											#, T
											), weight.type = c("asymmetry", "max_dep")) %>%
  plyr::d_ply(c("scale", "weight.type"), function(y) {
    print(y$scale)
    print(y$weight.type)
    
    1:length(onet) %>%
      plyr::mlply(function(x){
        print(names(onet)[x])
        o <- matched_frequency(onet[[x]], 
                               prop = seq(0, 1, by = 0.1),
                               weight.type = as.character(y$weight.type),
                               scale = y$scale) 
        saveRDS(o, 
                file = paste0("./data/processed/matching_frequency/",
                              y$weight.type, "/", "scaled_", y$scale, "/",
                              names(onet)[x], ".rds"), 
                ascii = TRUE, compress = F)
        return(o)
      }, .progress = "text")
  })
