assemble_controllabillity_df <- function(controllability, network_properties, metadata){
  controllability %>%
    filter_networks_df(metadata) %>%
    dplyr::inner_join(metadata, by = "net_name") %>%
    dplyr::inner_join(network_properties, by = "net_name") %>%
    dplyr::rename(n_pla = n_pla.x, 
                  n_pol = n_pol.x, 
                  web_asymmetry = web.asymmetry, 
                  interaction_strength_asymmetry = interaction.strength.asymmetry, 
                  weighted_NODF = weighted.NODF)
}
# drake::loadd(controllability, network_properties, metadata)

fit_controllability_models <- function(controllability_model_data){
  vars = c("n_sp", "connectance", "web_asymmetry", "interaction_strength_asymmetry", "weighted_NODF", "inv", "study")
  # extra = " + (1 | study)"
  suppressWarnings({
    models <- get_model_formulas("n_D ~ ", vars) %>% 
      `names<-`(., .) %>%
      purrr::map(~glm(as.formula(.), data = controllability_model_data, family = "binomial"))
  })
  models
}
# drake::loadd(controllability_model_data)

get_model_formulas <- function(prefix, vars, suffix = "", add_1 = T){
  # all combinations
  length(vars):0 %>%
    purrr::map(function(x){
      f <- combn(vars, x) %>%
        purrr::array_branch(2) %>%
        purrr::map(paste, collapse = " + ")
      if(add_1) {f <- f %>% purrr::modify_if(. == "", function(x) "1")}
      f
    }) %>%
    purrr::flatten() %>%
    purrr::map(~ paste(prefix, ., suffix)) 
}

get_var_importance <- function(model_list){
  params <- summary(model_list[[1]])$aliased %>% names() %>% extract(-1)
  params %>% `names<-`(., .) %>%
    as.list() %>%
    purrr::map(~ AICcmodavg::importance(model_list, .)) %>%
    purrr::map_dfr(as.data.frame)
}
# drake::loadd(controllability_model)
