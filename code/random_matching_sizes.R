random_matching_sizes <- function(x, n = 99){
  if (igraph::is_directed(x)){
    1:n %>%
      purrr::map(~ randomise_directions(x)) %>%
      purrr::map(function(z){
        z %>% split_bilinks() %>%
          purrr::map(maximum_matching) %>%
          merge_bilinks(list(list("graph", "matching_size", mean)))
      }) %>%
      purrr::map(igraph::graph_attr, "matching_size") %>%
      unlist()
    
  } else { 

    x %>% 
      igraph::as_incidence_matrix(types = igraph::V(.)$type == "pol", attr = "weight") %>% {
        m <- .
        bipartite::nullmodel(m, method = "swap.web", N = 99) %>%
          purrr::map(`colnames<-`, paste0("pol_", 1:ncol(m))) %>%
          purrr::map(`rownames<-`, paste0("pla_", 1:nrow(m)))
      } %>%
      purrr::map(igraph::graph_from_incidence_matrix, weighted = T)  %>%
      purrr::map(as_directed_network, higher_level = TRUE) %>%
      purrr::map(function(z){
        z %>% split_bilinks() %>%
          purrr::map(maximum_matching) %>%
          merge_bilinks(list(list("graph", "matching_size", mean)))
      }) %>%
      purrr::map(igraph::graph_attr, "matching_size") %>%
      unlist()
  }
}

# randomise directions of a graph - DO NOT TOUCH BIDIRECTIONAL LINKS
randomise_directions <- function(x){
  igraph::as_data_frame(x) %>%
    dplyr::mutate(dir = paste(from, to), 
                  dir_inv = paste(to, from), 
                  bi = dir %in% dir_inv, 
                  swap = runif(nrow(.)) >= 0.5, 
                  swap = dplyr::if_else(bi, FALSE, swap),
                  from2 = from,
                  from = dplyr::if_else(swap, to, from), 
                  to = dplyr::if_else(swap, from2, to)) %>%
    dplyr::select(-swap, -from2, -dir, -dir_inv, -bi) %>%
    igraph::graph_from_data_frame(vertices = igraph::as_data_frame(x, "vertices"))
}

# x is a list of networks
random_matching_sizes_emp <- function(x, n = 99){
  require(foreach)
  
  # binary <- x %>% 
  #   purrr::map(~ igraph::E(.)$weight) %>% 
  #   purrr::map(~ . == 1) %>% 
  #   purrr::map(all)
  
  foreach(i = 1:length(x)) %do% {
    # if(!binary[[i]]){
      m <- R.utils::withTimeout({
        m <- random_matching_sizes(x[[i]], n)
      }, timeout = 60, onTimeout = "silent")
      if(is.null(m)) m <- NA
    # } else {
    #   m <- NA
    # }
    cat("Finished network ", names(x)[i], "\n")
    m
  } %>%
    `names<-`(names(x))
}

# put randomisation results into a data frame
organise_randomisations <- function(controllability, random_interactions, random_directions){
  controllability
  
  ri <- random_interactions %>%
    purrr::imap_dfr(~dplyr::data_frame(net_name = .y, matching_size = .x, randomisation = "interactions"))
  rd <- random_directions %>%
    purrr::imap_dfr(~dplyr::data_frame(net_name = .y, matching_size = .x, randomisation = "directions"))
  
  dplyr::bind_rows(ri, rd) %>%
    dplyr::inner_join(dplyr::select(controllability, net_name, n), by = "net_name") %>%
    dplyr::mutate(D = n - matching_size, 
                  n_D = D / n) %>%
    dplyr::select(net_name, randomisation, n_D) %>%
    dplyr::inner_join(dplyr::select(controllability, net_name, D, n_D), by = "net_name")
  }
# drake::loadd(controllability, random_interactions, random_directions)
