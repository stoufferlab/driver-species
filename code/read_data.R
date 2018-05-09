## Functions that read the raw data and convert them to igraph objects
## Fernando Cagua April 2018

#' Process Bartomeus networks
#'
#' @param x file with bartomeus data
#'
#' @return list of igraph objects
#' 
preprocess_bartomeus <- function(x){
  
  # read bartomeus data
  data <- read.csv(x ,sep="\t") %>%
    dplyr::tbl_df()
  
  # organise plants and pollinator names and set weight and visit frequency
  ntw <- data %>%
    dplyr::mutate(pla = paste0("p_", Plant_id),
                  pol = paste0("i_", Insect_id)) %>%
    dplyr::group_by(Site, pla, pol) %>%
    dplyr::summarise(weight = sum(Number_of_visits_per6min)) %>%
    dplyr::group_by()
  
  # convert into an igraph object
  plyr::dlply(ntw, "Site", function(x){
    edg <- x %>% 
      dplyr::select(-Site)
    ver <- edg %>%
      dplyr::select(-weight) %>%
      tidyr::gather(type, name, pla, pol) %>%
      dplyr::distinct() %>%
      dplyr::select(name, type)
    
    igraph::graph_from_data_frame(edg, directed = F, vertices = ver)
  })
  
}

#' Process Lopezaraiza networks
#'
#' @param ... each filename points to a network. Filenames must include the path
#'
#' @return list of igraph objects
#'
preprocess_lopezaraiza <- function(...){
  
  files <- list(...) %>% unlist
  net_names <- files %>%
    basename %>%
    grep("b", ., value = T) %>% 
    stringr::str_split(" ") %>%
    lapply(function(x) paste(x[1], x[2], sep = "_")) %>%
    unlist()
  
  # organise plants and pollinator names and set weight and visit frequency
  ntw <- files %>%
    grep("b", ., value = T) %>%
    lapply(read.table, sep = "\t") %>%
    `names<-`(net_names) %>%
    plyr::ldply(.id = "Site") %>%
    dplyr::rename(pla = V1, pol = V2) %>%
    dplyr::mutate(pla = as.character(pla),
                  pol = as.character(pol)) %>%
    dplyr::group_by(Site, pla, pol) %>%
    dplyr::summarise(weight = sum(V6)) %>%
    dplyr::group_by()
  
  # convert into an igraph object
  plyr::dlply(ntw, "Site", function(x){
    edg <- x %>% 
      dplyr::select(-Site)
    ver <- edg %>%
      dplyr::select(-weight) %>%
      tidyr::gather(type, name, pla, pol) %>%
      dplyr::distinct() %>%
      dplyr::select(name, type)
    
    igraph::graph_from_data_frame(edg, directed = F, vertices = ver)
  })
}

#' Process Ballantyne networks
#'
#' @param x file with bartomeus data
#'
#' @return list of igraph objects
#' 
preprocess_ballantyne <- function(x) {
  
  # read data
  data <- read.csv(x) %>%
    dplyr::tbl_df() %>%
    dplyr::mutate(pla = paste0("p_", as.numeric(plant_species)),
                  pol = paste0("i_", as.numeric(visitor_group)))
  
  data <- data %>% 
    dplyr::group_by(pla, pol)
  
  visitation <- data %>% 
    dplyr::summarise(weight = n())
  
  deposition <- data %>% 
    dplyr::summarise(weight = sum(conspecific_pollen_deposited_.before_control_values_removed., 
                                  na.rm = T))
  
  efficacy <- data %>% 
    dplyr::summarise(weight = round(mean(conspecific_pollen_deposited, 
                                         na.rm = T)))
  
  importance <- data %>%
    dplyr::summarise(weight = mean(conspecific_pollen_deposited, 
                                   na.rm = T) * n())
  unweighted <- importance %>%
    dplyr::mutate(weight = 1)
  
  ntw <- list(bal_vis = visitation,
              bal_eff = efficacy,
              bal_imp = importance,
              bal_bin = unweighted)
  
  plyr::llply(ntw, function(x){
    edg <- x 
    ver <- edg %>%
      dplyr::select(-weight) %>%
      tidyr::gather(type, name, pla, pol) %>%
      dplyr::distinct() %>%
      dplyr::select(name, type)
    
    igraph::graph_from_data_frame(edg, directed = F, vertices = ver)
  })
}

#' Read metadata file and rename method column as count
#' 
#' Metadata file was manually generated and contains iformation about each network
#'
#' @param x metadata file location
#'
#' @return a data frame
#'
read_metadata <- function(x){
  x %>% 
    readr::read_csv() %>% 
    dplyr::tbl_df() %>%
    dplyr::rename(count = method)
}
