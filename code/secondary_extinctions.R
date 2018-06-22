# calculate secondary extinctions for a network, an extinction sequence, and a guild
secondary_extinctions <- function(x, sequence, guild = c("pla", "pol", "both")){
  
  if(guild == "both"){
    allowed_species <- igraph::V(x)$name
  } else if (guild == "pla"){
    allowed_species <- igraph::V(x)$name[igraph::V(x)$type == "pla"]
  } else if (guild == "pol"){
    allowed_species <- igraph::V(x)$name[igraph::V(x)$type == "pol"]
  }
  
  to_remove <- sequence[min(which(sequence %in% allowed_species))]
  
  x_removed <- x %>%
    igraph::delete_vertices(to_remove) %>% {
      sn <- .
      secondary_extinctions <- igraph::V(sn)[igraph::degree(sn) == 0]
      igraph::delete_vertices(sn, secondary_extinctions)
    } 
  
  if(length(igraph::V(x_removed)) == 0){
    further <- list()
  } else {
    further <- secondary_extinctions(x_removed, sequence, guild)
  }
  
  further %>% rlist::list.prepend(x_removed) %>% return()
}

# extract some properties of a network in a data frame
net_properties_df <- function(y, i) {
 
   dplyr::data_frame(primary_extinctions = i - 1L, 
                    n_species = length(igraph::V(y)), 
                    n_comp = as.integer(igraph::count_components(y)))
}

# get extinction df for a network and a sequence (all guilds)
secondary_all_modes <- function(x, sequence){
  list(pla = "pla", pol = "pol", both = "both") %>%
    purrr::map(~secondary_extinctions(x, sequence, guild = .)) %>%
    purrr::map(rlist::list.prepend, x) %>%
    purrr::map_dfr(function(x){
      purrr::imap_dfr(x, net_properties_df)
    }, .id = "guild") 
}

all_secondary_extinctions <- function(networks, sl_characteristics){
  
  sequence_names <- sl_characteristics %>% 
    dplyr::select_if(is.numeric) %>%
    names
  
  require(foreach)
  
  foreach(i = 1:length(networks), .combine = rbind) %dopar% {
    
    sequence_names %>%
      set_names(., .) %>%
      purrr::map_dfr(function(x, net){
        sequence <- sl_characteristics %>% 
          dplyr::filter(net_name == names(networks)[i]) %>%
          dplyr::arrange_at(x) %>%
          extract2("sp_name") %>% rev()
        
        secondary_all_modes(net, sequence)
      }, networks[[i]], .id = "metric") %>% 
      dplyr::mutate(net_name = names(networks)[i])
  }
}

# calculate data frame withp proportions extinct
standardize_secondary_extinctions <- function(secondary_ext, controllability, metadata){
  
  secondary_ext %>%
    dplyr::inner_join(controllability, by = "net_name") %>%
    filter_networks_df(metadata) %>%
    dplyr::mutate(prim_prop = primary_extinctions / n, 
                  seco_prop = (n - n_species) / n)
}

# drake::loadd(sl_characteristics, networks)
# x <- networks[[1]]
# sequence <- sl_characteristics %>% dplyr::filter(net_name == "BAT1CA") %>% dplyr::arrange(desc(control_capacity)) %$% sp_name
