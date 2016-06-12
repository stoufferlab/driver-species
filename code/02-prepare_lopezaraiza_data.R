library(magrittr)

setwd("~/github/driver-species/")

# read lopezaraiza data
# files with a correspond to abbundances
# files with b correspond to interactions
# files with exp are experimental plots (uninvaded) and ctl are control (invaded plots)

folder <- "./data/raw/networks_lopezaraiza/"
net_names <- list.files(folder) %>%
  grep("b", ., value = T) %>% 
  stringr::str_split(" ") %>%
  lapply(function(x) paste(x[1], x[2], sep = "_")) %>%
  unlist()

# organise plants and pollinator names and set weight and visit frequency
ntw <- list.files(folder, full.names = T) %>%
  grep("b", ., value = T) %>%
  lapply(read.table, sep = "\t") %>%
  `names<-`(net_names) %>%
  plyr::ldply(.id = "Site") %>%
  dplyr::rename(pla = V1, pol = V2) %>%
  dplyr::mutate(pla = as.character(pla),
                pol = as.character(pol)) %>%
  dplyr::group_by(Site, pla, pol) %>%
  dplyr::summarise(weight = sum(V6)) %>%
  dplyr::group_by()

# convert into an igraph object
net <- plyr::dlply(ntw, "Site", function(x){
  edg <- x %>% 
    dplyr::select(-Site)
  ver <- edg %>%
    dplyr::select(-weight) %>%
    tidyr::gather(type, name, pla, pol) %>%
    dplyr::distinct() %>%
    dplyr::select(name, type)
  
  igraph::graph_from_data_frame(edg, directed = F, vertices = ver)
})

# save each network
folder <- "./data/processed/networks/"
plyr::l_ply(names(net), function(x){
  filename <- paste0(x, ".rds")
  saveRDS(net[[x]], paste0(folder, filename), ascii = T, compress = F)
})