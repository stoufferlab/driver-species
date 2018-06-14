get_controllability_superiorness <- function(x){
  x %>% {
    dplyr::data_frame(sp_name = igraph::vertex_attr(., "name"), 
                      control_capacity = igraph::vertex_attr(., "control_capacity"), 
                      superior = igraph::vertex_attr(., "superior"))
  }
}
# drake::loadd(matched_networks)
# x <- matched_networks[[1]]

join_sl_characteristics <- function(sigma_phi_df, species_coovariates_df, species_empirical_coov, structural_stability){
  dplyr::inner_join(sigma_phi_df, species_coovariates_df, by = c("net_name", "sp_name")) %>%
    dplyr::inner_join(species_empirical_coov, by = c("net_name", "sp_name")) %>%
    dplyr::inner_join(structural_stability, by = c("net_name", "sp_name"))
}
# drake::loadd(sigma_phi_df, species_coovariates_df)

species_level_characteristics_correlation <- function(sl_characteristics, metadata, vars, method = "pearson"){
  vars <- c("net_name", "sp_name", vars)
  
  y <- sl_characteristics %>%
    filter_networks_df(metadata) %>% 
    dplyr::select(!! vars) 
  
  z <- y %>%
    split(.$net_name) %>%
    purrr::map(corr_df, method = method) %>%
    simplify2array()
  
  list(mean = apply(z, 1:2, mean),
       sd = apply(z, 1:2, sd))
}

corr_df <- function(x, method){
  x %>%
    dplyr::select_if(is.numeric) %>%
    # dplyr::select_at(dplyr::vars(-dplyr::contains(".directed"))) %>% 
    as.matrix() %>%
    cor(method = method) }
