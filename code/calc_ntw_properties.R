#' Calculate network properties
#'
#' @param x a list of networks
#' @param properties a vector with strings that define the properties that want to be calulated. Must be one or more of the properties described on \link[bipartite]{networklevel}
#'
#' @return a data frame with net_names and properties 
#'
calc_ntws_properties <- function(x, properties = c("connectance", 
																									 "web asymmetry", 
																									 "ISA", 
																									 "weighted NODF")){
	net_prop <- x %>%
		plyr::ldply(calc_ntw_properties, properties)
	
	names(net_prop)[1] <- "net_name"
	
	return(net_prop)
}

#' Calculate network properties
#'
#' @param x a network
#' @param properties same as properties for calc_ntws_properties
#'
#' @return a single row data frame 
#'
calc_ntw_properties <- function(x, properties){
	keep_largest_component(x)
	n_pla <- sum(igraph::V(x)$type == "pla")
	n_pol <- sum(igraph::V(x)$type == "pol")
	bipartite::networklevel(igraph::as_incidence_matrix(x, 
																											types = igraph::V(x)$type == "pla",
																											attr = "weight"),
													index = properties) %>%
		as.list() %>% as.data.frame() %>%
		dplyr::mutate(n_sp = length(igraph::V(x))) %>%
		dplyr::mutate(n_pla = n_pla,
									n_pol = n_pol)
}

#' Keep largest component in a network
#'
#' @param net 
#'
#' @return
#' @export
#'
#' @examples
keep_largest_component <- function(net){
	co <- igraph::components(net)
	largest_co <- which(co$csize == max(co$csize))
	igraph::delete_vertices(net, co$membership != largest_co)
}