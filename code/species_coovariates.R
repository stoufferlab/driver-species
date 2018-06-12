# calulate a bunch of species properties for a network, returns a data frame 
get_species_coov <- function(n, metrics){
  bipartite_metrics <- metrics[! metrics %in% c("nested_contribution", "page_rank", "eigen")]
  
  bipart <- n %>% 
    igraph::as_incidence_matrix(attr = "weight", 
                                types = igraph::V(.)$type == "pol") %>%
    bipartite::specieslevel(index = bipartite_metrics) %>%
    purrr::map_dfr(tibble::rownames_to_column, var = "sp_name") 
  
  if ("nested_contribution" %in% metrics){
    bipart %<>% 
      dplyr::inner_join(nestedness_contribution(n), by = "sp_name")
  }
  
  dir_list <- list(directed = TRUE, nondirected = FALSE) 
  
  if("page_rank" %in% metrics){
    pr <- dir_list %>%
      purrr::map(~ igraph::page_rank(n, directed = .)) %>%
      centrality_as_df("page_rank")
    bipart %<>% 
      dplyr::inner_join(pr, by = "sp_name")
  }
  
  if ("eigen" %in% metrics){
    ei <- dir_list %>%
      purrr::map(~ igraph::eigen_centrality(n, directed = .)) %>%
      centrality_as_df("eigen")
    bipart %<>%
      dplyr::inner_join(ei, by = "sp_name")
  }
  
  bipart
}


centrality_as_df <- function(x, label){
  x %>%
    purrr::map(~ .$vector) %>%
    purrr::map_dfr(~ dplyr::data_frame(sp_name = names(.), val = .), .id = "directed")%>%
    tidyr::spread(directed, val) %>%
    dplyr::rename_if(is.numeric, dplyr::funs(paste(label, ., sep =".")))
}

# drake::loadd(directed_networks)
# n <- directed_networks[[1]]

# delta <- 0
# rho <- 0.01
# interaction_matrix <- interaction_matrix_mutualism

plants_or_pols <- function(x){
  dplyr::data_frame(sp_name = igraph::V(x)$name, 
                    guild = igraph::V(x)$type) %>%
    dplyr::mutate(invasive = dplyr::if_else(sp_name %in% c("p_4", "p_25", "Impatiens glandulifera"), TRUE, FALSE))
}

