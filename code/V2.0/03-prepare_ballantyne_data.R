library(magrittr)

setwd("~/github/driver-species/")

# read data
data <- read.csv("./data/Ballantyne-2015-raw_data.csv") %>%
	dplyr::tbl_df() %>%
	dplyr::mutate(pla = paste0("p_", as.numeric(plant_species)),
								pol = paste0("i_", as.numeric(visitor_group)))

data %<>% 
	dplyr::group_by(pla, pol)

visitation <- data %>% 
	dplyr::summarise(weight = n())

deposition <- data %>% 
	dplyr::summarise(weight = sum(conspecific_pollen_deposited_.before_control_values_removed., 
																 na.rm = T))

efficacy <- data %>% 
	dplyr::summarise(weight = sum(conspecific_pollen_deposited, 
																 na.rm = T))

importance <- data %>%
	dplyr::summarise(weight = sum(conspecific_pollen_deposited, 
																 na.rm = T) * n())

ntw <- list(bal_vis = visitation,
						bal_eff = efficacy,
						bal_imp = importance)

net <- plyr::llply(ntw, function(x){
	edg <- x 
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

	