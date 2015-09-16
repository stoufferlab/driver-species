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