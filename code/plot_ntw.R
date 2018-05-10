#' Make a vertex name a mathematica expression
#'
#' @param x an igraph network with vertex names
#'
#' @return an igraph network
#'
fancify_vertex_name <- function(x){
  igraph::V(x)$name_fancy <- paste0("$", igraph::V(x)$name, "$") %>%
    latex2exp::TeX()
  x
}

#' Add a property for vertex or graphs
#'
#' @param x an igraph network
#' @param element either "vertex" or "edge"
#' @param ... named argument. The name of the argument correspond to the new attribute and 
#'
#' @return
#'
add_property <- function(x, element, attr_name, ...){
  if(element == "vertex"){
    sel <- igraph::set_vertex_attr
    elements <- igraph::V(x)
  } else {
    sel <- igraph::set_edge_attr
    elements <- igraph::E(x)
  }
  type <- elements$type
  if(is.null(type)) type <- 1:length(elements)
  e <- environment()
  pattern <- list(...) %>% purrr::map(as.formula, e)
  df <- dplyr::data_frame(type = type) %>%
    dplyr::mutate(attr = dplyr::case_when(!!! pattern))
  sel(x, name = attr_name, value = df$attr) 

}

add_vertex_edge <- function(x, vertex_name, vertex_type, edges_from, edges_to, edges_type){
  x$edges <- x$edges %>%
    rbind(dplyr::data_frame(from = edges_from, to = edges_to, type = edges_type))
  x$vertex <- x$vertex %>%
    rbind(dplyr::data_frame(name = vertex_name, type = vertex_type))
  x
}

#' Plot a network using several aesthetic parameters
#'
#' @param x network
#' @param ... parameters for plot.igraph
#'
#'
plot_examples <- function(x, ...){
  igraph::plot.igraph(x, 
                      vertex.label = igraph::V(x)$name_fancy, 
                      vertex.label.cex = igraph::V(x)$label.cex,
                      vertex.label.color = igraph::V(x)$label.color,
                      vertex.size = igraph::V(x)$size,
                      vertex.color = igraph::V(x)$color,
                      # vertex.label.family = fam,
                      # vertex.label.color = l_c,
                      edge.width = igraph::E(x)$width,
                      edge.label = igraph::E(x)$label,
                      edge.label.x = igraph::E(x)$label.x,
                      edge.label.color = igraph::E(x)$label.color,
                      edge.label.font = igraph::E(x)$label.font,
                      # edge.label.family = fam,
                      ...)
}

#' Rescale igraph layout
#' 
#' Use with rescale=FALSE in igraph::plot
#'
#' @param layout the layout -a matrix-
#' @param xlim a vector of length two with the minimum and maximum x value
#' @param ylim a vector of length two with the minimum and maximum y value
#'
#' @return the updated layout
#'
rescale_layout <- function(layout, xlim, ylim){
  layout[, 1] <- range02(layout[, 1], xlim[1], xlim[2])
  layout[, 2] <- range02(layout[, 2], ylim[1], ylim[2])
  layout
}

#' Rescale range of a vector
#' 
#'
#' @param x vector to be rescaled
#' @param newMin new minimum
#' @param newMax new maximum
#'
#' @return the updated layout
#'
range02 <- function(x, newMin, newMax){ 
  (x - min(x))/(max(x)-min(x)) * (newMax - newMin) + newMin 
}
