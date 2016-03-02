# random directions

# ARGUMENTS: 
#			1: the number of cores to be used
#			2: the number of randomisations per matrix 


library(magrittr)
library(foreach)
library(doMC)
registerDoMC(cores = as.numeric(commandArgs(trailingOnly = T)[1]))
setwd("~/github/driver-species/")
nperm <- commandArgs(trailingOnly = T)[2]

"./code/V2.0/functions" %>% 
  list.files(full.names = T) %>%
  plyr::l_ply(source)

# read networks
net <- "./data/V2.0/networks" %>%
  read_networks()

# read metadata
meta <- readr::read_csv("data/ntw_info.csv") %>% dplyr::tbl_df() %>%
  dplyr::rename(count = method)

# read matching results
matched <- readRDS("./data/V2.0/n_matched.rds")

null_methods <- c(
  "r0") 

# for each network  
plyr::ldply(net, function(n){
  n %<>% keep_largest_component()
  
  edges <- n %>% 
    igraph::ends(igraph::E(.))
  
  # get directed graph 
  dir_n <- n %>% 
    keep_largest_component() %>%
    bipartite_digraph(type = "weight", keep = "all")
  
  # get edge list of directed graph
  dir_edges <- dir_n %>%
    igraph::ends(., igraph::E(.)) 
  
  # get dependencies
  dependencies <- n %>% 
    keep_largest_component() %>%
    igraph::as_adjacency_matrix(sparse = 0, attr = "weight") %>%
    apply(1, function(x){x / sum(x)}) %>% t()
  
  # extract matrix of directions
  dir_matrix <- c(function(x) x, rev) %>% 
    plyr::laply(function(z) {
      dir_edges %>%
        apply(1, function(x){
          apply(edges, 1, identical, z(x))
        }) %>%
        apply(1, any)
    }) %>% t()
  
  # for each method
  plyr::ldply(null_methods, function(y){
    # radomise directions
    random_dir <- dir_matrix %>%
      vegan::nullmodel(method = y) %>%
      simulate(nperm, seed = 1)
    
    # create baseline matrix
    m <- matrix(0, ncol = length(igraph::V(n)), nrow = length(igraph::V(n)),
                dimnames = list(igraph::V(n)$name, igraph::V(n)$name))
    
    # for each randomisation
    plyr::adply(random_dir, 3, function(r){
      pla_to_pol <- edges[as.logical(r[,1]),] %>%
        plyr::aaply(1, function(x){
          m[x[1], x[2]] <- 1
          return(m)
        }) %>%
        apply(c(2,3), sum)
      
      pol_to_pla <- edges[as.logical(r[,2]),] %>%
        plyr::aaply(1, function(x){
          m[x[2], x[1]] <- 1
          return(m)
        }) %>%
        apply(c(2,3), sum)
      
      # create new adjacency matrix
      rn <- (pla_to_pol + pol_to_pla) * dependencies
      rn <- igraph::graph_from_adjacency_matrix(rn, mode = "directed", weighted = TRUE)
      igraph::V(rn)$type <- igraph::V(dir_n)$type
      
      rn %>%
        digraph_bipartite() %>%
        igraph::max_bipartite_match() %>%
        magrittr::extract2("matching_size")
    }, .parallel = TRUE) %>% 
      magrittr::set_colnames(c("sim", "n_matched")) %>%
      dplyr::mutate(method = y)
  })
}, .progress = "text") %>%
  saveRDS(file = "./data/V2.0/random_directions.rds", ascii = T, compress = F)






