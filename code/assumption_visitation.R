get_visitation_importance_agreement <- function(sl_characteristics, metadata){
  
  c("control_capacity", "superior") %>%
    set_names(., .) %>%
    purrr::map(function(var){
      sl_characteristics %>%
        filter_networks_df(metadata, desired_ntw = "ballantyne") %>%
        dplyr::filter(net_name != "bal_bin") %>%
        dplyr::select_("net_name", "sp_name", var) %>%
        tidyr::spread(net_name, var) %>%
        dplyr::select_if(is.numeric) %>%
        cor(method = "spearman") %>%
        as.data.frame()
    })
}
