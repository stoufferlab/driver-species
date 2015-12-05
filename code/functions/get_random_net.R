#' Title
#'
#' @param net 
#' @param method
#' @param nperm
#' @param type 
#' @param keep
#' 
#'  Assumes that the network has cateories "pla" and "pol 
#'
#' @return
#' @export
#'
#' @examples
#' 
# function to obtain the number of control nodes for a particular randomisation scheme
get_random_net <- function (net, method, nperm) {
	
	random_mat <- igraph::as_incidence_matrix(net, types = igraph::V(net)$type == "pla") %>%
		vegan::nullmodel(method) %>%
		stats::simulate(nperm)
	
	plyr::alply(random_mat, 3, igraph::graph_from_incidence_matrix, weighted = TRUE)

}