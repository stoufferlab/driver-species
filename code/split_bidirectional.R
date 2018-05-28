split_bidirectional <- function(x) {
  
  if(igraph::reciprocity(x) == 0) return(list(x))
  
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
    purrr::map(~ igraph::delete_edges(x, .))
}

# example_ntw_bidirection_triple() %>%
#   split_bidirectional() %>%
#   purrr::map(control_capacity) %T>% {
#     x <- .
#     x %>%
#     # purrr::map() %>%
#       purrr::map(igraph::plot.igraph) 
#     x
#   } %>%
#   purrr::map(vertex_attr, "control_capacity") %>%
#   purrr::pmap(lift_vd(mean))
# 
# directed_networks %>%
#   purrr::map(split_bidirectional) %>%
#   purrr::map(~ purrr::map(., control_capacity))
  