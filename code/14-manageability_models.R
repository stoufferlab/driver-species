library(magrittr)

net <- "../data/processed/networks" %>%
	read_networks()
meta <- readr::read_csv("../data/ntw_info.csv") %>% dplyr::tbl_df() %>%
	dplyr::rename(count = method)
n_matched <- readRDS("../data/processed/n_matched.rds")
a_m <- readRDS("../data/processed/n_matched_all_types.rds") 

spp_lc <- net %>%
	plyr::ldply(function(x){
		keep_largest_component(x) %>%
			igraph::V() %>%
			length()
	}) %>%
	`names<-`(c("net_name", "n_spp_lc"))

n_links <- net %>%
	plyr::ldply(function(x){
		keep_largest_component(x) %>%
			igraph::E() %>%
			length()
	}) %>%
	`names<-`(c("net_name", "n_links"))


n_driver <- a_m %>%
	dplyr::inner_join(meta) %>%
	dplyr::inner_join(n_links) %>%
	dplyr::inner_join(spp_lc) %>%
	dplyr::filter(count == "visitation") %>%
	dplyr::mutate(n_sp = n_pla + n_pol,
								n_dr = n_sp - n_matched,
								n_dr_lc = n_spp_lc - n_matched, 
								n_dr_p = n_dr / n_sp)

src_bi_w <- n_driver %>%
	dplyr::filter(type == "z-bi" | type == "weight",
								study != "ballantyne") %>%
	dplyr::select(net_name, type, n_dr_p) %>%
	tidyr::spread(type, n_dr_p) %$%
	cor.test(weight, `z-bi`, method = "spearman")

n_driver %<>%
	dplyr::filter(!is.na(inv))

n_driver_wb <- dplyr::filter(n_driver, 
														 type == "z-bi" | type == "weight")

library(glmulti)

models_n <- glmulti(cbind(n_dr, n_matched) ~ I(n_pla/n_pol) + study + inv + type + I(n_links/(n_pla*n_pol)), data = n_driver_wb, family = "binomial", level = 1)

saveRDS(n_driver, "../data/processed/n_driver.rds", ascii = T, compress = F)
saveRDS(models_n, "../data/processed/models_manageability.rds")
saveRDS(src_bi_w, "../data/processed/manageability_bi_vs_weight.rds", ascii = T, compress = F)
