# drake::loadd(structural_rho_sensitivity, metadata)
get_structural_sensitivity_correlation <- function(sensitivity_range, structural_rho_sensitivity, chosen_rho = 0.005, metadata){
  separator <- "---"
  purrr::cross(sensitivity_range) %>%
    purrr::map2_df(structural_rho_sensitivity, 
                   ~dplyr::mutate(.y, rho = .x[[1]], delta  = .x[[2]])) %>% 
    dplyr::select(net_name, sp_name, feasibility, rho, delta) %>%
    filter_networks_df(metadata) %>%
    split(list(.$net_name, .$delta), sep = separator) %>%
    purrr::map_dfr(function(x){
      x %>%
        tidyr::spread(rho, feasibility) %>%
        dplyr::select_if(is.numeric) %>%
        dplyr::select(-delta) %>%
        cor(.[, as.character(chosen_rho)], ., method = "spearman") %>%
        extract(1, ) %>%
        dplyr::data_frame(cor = ., rho = names(.))
    }, .id = "net_delta") %>%
    tidyr::separate(col = "net_delta", into = c("net_name", "delta"), sep = separator) %>%
    dplyr::mutate(delta = as.numeric(delta))
}

# drake::loadd(sigma_phi_df, species_coovariates_df, species_empirical_coov,structural_rho_sensitivity, sensitivity_rho, sensitivity_range, metadata)
# calculate the difference between critical and redundant species for several values of rho
get_rho_feasibility <- function(sigma_phi_df, species_coovariates_df, species_empirical_coov,structural_rho_sensitivity, sensitivity_range){
  vals <- purrr::cross(sensitivity_range) %>%
    purrr::map_dfr(~dplyr::data_frame(rho = .[[1]], delta = .[[2]]))
  structural_rho_sensitivity %>%
    purrr::map(~ join_sl_characteristics(sigma_phi_df, species_coovariates_df, species_empirical_coov, .)) %>%
    purrr::map(get_t_test_feasibility, metadata) %>%
    # purrr::map(~ .$estimate) %>%
    purrr::map_dfr(~dplyr::data_frame(critical = .$estimate[1], 
                                      redundant = .$estimate[2], 
                                      conf_low = .$conf.int[1], 
                                      conf_high = .$conf.int[2], 
                                      p_value = .$p.value)) %>%
    dplyr::bind_cols(vals) %>%
    dplyr::mutate(dif = critical-redundant)
}

# get t test for a data frame with the adequate rho
get_t_test_feasibility <- function(sl_characteristics, metadata){
  st_df <- sl_characteristics %>%
    filter_networks_df(metadata) %>%
    dplyr::inner_join(metadata, by = "net_name") %>%
    dplyr::group_by(net_name) %>%
    dplyr::filter(is.finite(feasibility)) %>%
    dplyr::mutate(feasibility = scale(feasibility), 
                  critical = dplyr::case_when(
                    control_capacity == 1 ~ "critical", 
                    # control_capacity == 0 ~ "redundant",
                    TRUE ~ "redundant"
                  ), 
                  value = feasibility) %>%
    dplyr::filter(!is.na(critical)) 
  
  means <- st_df %>%
    dplyr::group_by(critical) %>%
    dplyr::summarise_if(is.numeric, mean, na.rm = T)
  
  value <- 1
  my.labs <- list(bquote(paste("critical species (", phi==.(value), ")")), 
                  bquote(paste("redundant (", phi<.(value), ")")))
  
  st_df  %$%
    t.test(feasibility ~ critical, conf.int = T)
}
