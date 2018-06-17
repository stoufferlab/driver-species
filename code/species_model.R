# drake::loadd(sl_characteristics, metadata)
# response <- "control_capacity"
fit_species_models <- function(sl_characteristics, metadata, response){
  df <- sl_characteristics %>% 
    filter_networks_df(metadata) %>%
    dplyr::inner_join(metadata, by = "net_name") %>%
    dplyr::mutate_at(c("control_capacity", "superior"), as.character) %>%
    dplyr::mutate_if(is.numeric, scale) %>%
    dplyr::mutate_at(c("control_capacity", "superior"), as.numeric)
  
  # testing random effects
  random_vars <- c("(nestedcontribution | guild)", "(species.strength | guild)", "(interaction.push.pull | guild)",  "(1 | site)")
  fixed_vars <- c("species.strength", "nestedcontribution", "interaction.push.pull", "degree")
  prefix_random <- paste0(response, " ~ ", paste(fixed_vars, collapse = " + "), " + ")
  formulas_random <- get_model_formulas(prefix_random, random_vars) %>% 
    set_names(., .) 
  models_random <- foreach(i=1:(length(formulas_random)-1)) %dopar% {
    lme4::glmer(formulas_random[[i]], data = df, family = "binomial")
  } 
  no_random_model <- glm(formulas_random[[length(formulas_random)]], data = df, family = "binomial")
  # View(AICcmodavg::aictab(models_random, modnames = unlist(formulas_random[-length(formulas_random)])))
  # View(AICcmodavg::aictab(models_random))
  # suppressWarnings(get_var_importance(models_random))
  
  if(response == "superior"){
    prefix_fixed <- paste0(response, " ~ ")
    suffix_fixed <- "1" %>%
      paste("+", .)
    formulas_fixed <- get_model_formulas(prefix_fixed, fixed_vars, suffix_fixed) %>% 
      set_names(., .) 
    models_fixed <- foreach(i=1:(length(formulas_fixed))) %dopar% {
      glm(formulas_fixed[[i]], data = df, family = "binomial")
    }
  }
  
  if(response == "control_capacity"){
    prefix_fixed <- paste0(response, " ~ ")
    suffix_fixed <- get_best_random(models_random, random_vars)[[1]] %>%
      paste("+", .)
    formulas_fixed <- get_model_formulas(prefix_fixed, fixed_vars, suffix_fixed) %>% 
      set_names(., .) 
    models_fixed <- foreach(i=1:(length(formulas_fixed))) %dopar% {
      lme4::glmer(formulas_fixed[[i]], data = df, family = "binomial")
    }
  }
  # View(AICcmodavg::aictab(models_fixed, modnames = unlist(formulas_fixed)))
  
  list(fixed = models_fixed, 
       random = models_random, 
       no_random_model = no_random_model, 
       df = df)
}

get_best_random <- function(model_list, random_vars){
  best_mod <- model_list %>%
    AICcmodavg::aictab(modnames = 1:length(model_list)) %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(include = dplyr::lag(Cum.Wt) < 0.95, 
                  include = dplyr::if_else(is.na(include) | include, T, F)) %>%
    dplyr::filter(include) %$%
    Modnames
  
  model_list[best_mod] %>%
    purrr::map(~ as.character(.@call)[2]) %>%
    purrr::map(~stringr::str_detect(., stringr::fixed(random_vars))) %>%
    purrr::map(~random_vars[.]) %>%
    purrr::map(paste, collapse = " + ")
}

# returns the best model by AICc from a list of models
get_best_model <- function(model_list, criterium = AICcmodavg::AICc){
  best_model_index <- model_list %>%
    purrr::map(criterium) %>%
    unlist() %>%
    which.min()
  
  model_list[[best_model_index]]
}

# get the number of fixed and grouping (random) terms
get_num_effects <- function(model){
  model %>%
    broom::tidy() %>%
    split(.$group) %>%
    purrr::map(nrow)
}

# get importance of fixed effects from a model list
get_var_importance <- function(model_list){
  var_info <- model_list %>%
    purrr::map_df(broom::tidy, effects = "fixed", .id = "model_index")
  
  suppressWarnings({
    mod_info <- model_list %>%
      AICcmodavg::aictab() %>%
      dplyr::as_data_frame() %>%
      tibble::rownames_to_column(var = "model_index")
  })
  
  dplyr::inner_join(var_info, mod_info, by = "model_index") %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(importance = sum(AICcWt)) %>%
    dplyr::arrange(dplyr::desc(importance))
}
