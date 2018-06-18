# drake::loadd(metrics_subsampled, metadata)

# get a data frame with the correlations per proportion
extract_correlations_subsamples <- function(metrics_subsampled, var){
  metrics_subsampled %>% 
    dplyr::arrange(net_name, prop) %>% 
    dplyr::group_by(net_name) %>%
    dplyr::mutate(prop_int = n_int / orig_int) %>%
    dplyr::group_by() %>%
    split(.$net_name) %>%
    purrr::map_dfr(function(x){
      # message(x$net_name[1])
      x %>%
        dplyr::mutate(group = interaction(prop, prop_int, sep = "_")) %>%
        dplyr::select_("net_name", "group", "sp_name", var) %>%
        tidyr::spread("group", var) %>%
        dplyr::select_if(is.numeric) %>%
        cor(.[, ncol(.)], ., use = "pairwise.complete.obs", method = "spearman") %>%
        extract(1, ) %>%
        dplyr::data_frame(cor = ., group = names(.)) %>%
        tidyr::separate(col = group, c("prop", "prop_int"), sep = "_") %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("prop")), as.numeric)
    }, .id = "net_name") 
  
}


# drake::loadd(networks)
# get control metrics for subsamples of the empirical networks
subsample_nets <- function(networks, from = 0, to = 0.2, by = 0.1, this_method, bias = "none"){
  metric_list <- foreach(i=1:length(networks)) %dopar% {
    cat(paste(names(networks)[i], "\n"))
    metrics_subsampled_nets(networks[[i]], from, to, by, this_method, bias)
  } %>%
    set_names(names(networks)) 
  
  list(
    controllability = metric_list %>%
      purrr::map_dfr(~ .$controllability, .id = "net_name"),
    species_metrics = metric_list %>%
      purrr::map_dfr(~ .$species_metrics, .id = "net_name")
    ) 
}

# get control metrics and subsample a net 
metrics_subsampled_nets <- function(x, from, to, by, this_method, bias){
  subsampled_networks <- seq(from, to, by) %>%
    set_names(., .) %>%
    purrr::map(~remove_interaction_prop(x, ., bias)) 
  
  matched_networks <- subsampled_networks %>%
    purrr::map(as_directed_network, direction = "asymmetry", ties = "both", higher_level = "pol") %>%
    purrr::map(control_capacity_empirical_nets, l = aggregation_option_list, .method = this_method)
  
  interactions <- subsampled_networks %>%
    purrr::map_dfr(~dplyr::data_frame(n_int = sum(igraph::edge_attr(., "weight"))), .id = "prop") %>%
    dplyr::mutate(orig_int = sum(igraph::edge_attr(x, "weight")))

  cont <- controllability_emp(matched_networks) %>%
    dplyr::rename(prop = net_name) %>%
    dplyr::inner_join(interactions, by = "prop")
  
  cc_sup <- matched_networks %>%
  purrr::map_dfr(get_controllability_superiorness, .id = "prop") %>%
    dplyr::inner_join(interactions, by = "prop")
  
  list(controllability = cont, species_metrics = cc_sup)
}

# remove a proportion of interactions from a network
remove_interaction_prop <- function(x, prop, bias = "none"){
  int_to_delete <-igraph::edge_attr(x, "weight") %>%
    sum() %>%
    multiply_by(prop) %>%
    round()
  
  if(int_to_delete == 0) return(x)
  
  smaller_network <- x
  for(i in 1:int_to_delete){
    smaller_network %<>% remove_interaction(bias = bias)
  }
  smaller_network
}

# randomly remove ONE interaction 
remove_interaction <- function(x, bias = c("none", "weaker")){
  
  if(bias[1] == "weaker") {
    prob <- 1/igraph::edge_attr(x, "weight")
  } else {
    prob <- NULL
  }
  
  interaction_to_reduce <- 1:length(igraph::edge_attr(x, "weight")) %>% 
    sample(1, replace = FALSE,  prob)
  weight_to_reduce <- igraph::edge_attr(x, "weight", interaction_to_reduce)
  
  igraph::set_edge_attr(x, "weight", interaction_to_reduce, weight_to_reduce - 1) %>% 
    delete_edge_if_no_weight() %>%
    delete_vertex_if_unconnected()
}

# remove link if it has zero weight
delete_edge_if_no_weight <- function(x){
  x %>%
    igraph::edge_attr("weight") %>% 
    equals(0) %>%
    which() %>%
    igraph::delete_edges(x, .) 
}

# remove node if unconnected
delete_vertex_if_unconnected <- function(x){
  x %>%
    igraph::degree() %>%
    equals(0) %>%
    which() %>%
    igraph::delete_vertices(x, .)
}
