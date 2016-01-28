library(magrittr)

setwd("~/github/driver-species/")

# read bartomeus data
data <- read.csv("./data/Bartomeus_Ntw_nceas.txt",sep="\t") %>%
	dplyr::tbl_df()

# organise plants and pollinator names and set weight and visit frequency
ntw <- data %>%
	dplyr::mutate(pla = paste0("p_", Plant_id),
								pol = paste0("i_", Insect_id)) %>%
	dplyr::group_by(Site, pla, pol) %>%
	dplyr::summarise(weight = sum(Number_of_visits_per6min)) %>%
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
folder <- "./data/V2.0/networks/"
plyr::l_ply(names(net), function(x){
	filename <- paste0(x, ".rds")
	saveRDS(net[[x]], paste0(folder, filename), ascii = T, compress = F)
})