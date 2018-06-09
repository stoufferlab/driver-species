# calulate a bunch of species properties for a network, returns a data frame 
get_species_coov <- function(n){
  bipart <- n %>% 
    igraph::as_incidence_matrix(attr = "weight", 
                                types = igraph::V(.)$type == "pol") %>%
    bipartite::specieslevel(index = c("degree", "normalised degree", "species strength", "betweenness", "closeness")) %>%
    purrr::map_dfr(tibble::rownames_to_column, var = "sp_name") 
  
  
  dir_list <- list(directed = TRUE, nondirected = FALSE) 
  pr <- dir_list %>%
    purrr::map(~ igraph::page_rank(n, directed = .)) %>%
    centrality_as_df("page_rank")
  ei <- dir_list %>%
    purrr::map(~ igraph::eigen_centrality(n, directed = .)) %>%
    centrality_as_df("eigen")
  
  dplyr::inner_join(pr, ei, by = "sp_name")  %>%
    dplyr::inner_join(bipart, by = "sp_name")
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
