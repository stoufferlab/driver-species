#' Split a network with bidirectional links 
#' 
#' into multiple ones with unidirectional links
#'
#' @param x igraph network with bidirectional links
#'
#' @return a list of networks with unidirectional links
#' 
split_bilinks <- function(x) {
  
  # if it has no bidirectional links just return the same network 
  if(igraph::reciprocity(x) == 0) {
    x %<>% 
      igraph::set_graph_attr(name = "original_network", 
                             value = x) %>%
      list()
    return(x)
  }
  
  xm <- x[, sparse = F, edges = T]
  # find bidirectional links
  simmetry <- (xm * t(xm)) != 0  
  up <- xm[upper.tri(simmetry) & simmetry]
  lo <- xm[lower.tri(simmetry) & simmetry]
  
  purrr::map2(up, lo, function(x,y){c(x,y)}) %>%
    # generate combinations
    do.call(expand.grid, args = .) %>%
    as.matrix() %>%
    purrr::array_branch(1) %>%
    # generate unidirectional networks
    purrr::map(~ igraph::delete_edges(x, .)) %>%
    purrr::map(igraph::set_graph_attr, 
               name = "original_network", 
               value = x)
}

#' Aggregate back nets split by split_bilinks
#'
#' @param x a list of splitted networks with split_blinks
#' @param l a list of lists with the attr_opt for aggregate_attribute
#'
#' @return a network with aggregated arguments
#'
merge_bilinks <- function(x, l){
  # get original network
  on <- igraph::graph_attr(x[[1]], "original_network")
  for(i in 1:length(l)){
    on %<>% 
      aggregate_attribute(x, l[[i]])
  }
  return(on)
}

#' Aggregate an attribute over a list of networks
#'
#' @param y The network to which to add the aggregated attribute
#' @param x The list of networks  
#' @param attr_opt Named list with the aggregation options. type can be either "vertex", "edge" or "graph". name is the name of the attribute to aggregate and fun the aggregation function
#'
#' @return x with with the aggregated attribute
#'
aggregate_attribute <- function(y = NULL, x, attr_opt){
  if(is.null(y)) y <- x[[1]]
  if(is.null(names(attr_opt))) names(attr_opt) <- c("type", "name", "agg_fun")
  
  get_fun <- getFromNamespace(paste0(attr_opt$type, "_attr"), ns = "igraph")
  set_fun <- getFromNamespace(paste0("set_", attr_opt$type, "_attr"), ns = "igraph")
  
  x %>%
    purrr::map(get_fun, attr_opt$name) %>%
    purrr::pmap_dbl(purrr::lift_vd(attr_opt$agg_fun)) %>%
    set_fun(y, name = attr_opt$name, value = .)
}

# default list of aggregation options for this study
aggregation_option_list <- list(
  list("vertex", "control_capacity", mean), 
  list("graph", "n_control_configurations", sum), 
  list("vertex", "superior", mean), 
  list("graph", "matching_size", mean)
)


#' Calculate control capacity of empirical directed networks
#'
#' @param x list of directed networks
#' @param l aggregation option list
#'
#' @return a list of directed networks with control properties calculated
#'
control_capacity_empirical_nets <- function(x, l){
  x %>%
    split_bilinks() %>%
    purrr::map(control_capacity) %>%
    merge_bilinks(l)
}
