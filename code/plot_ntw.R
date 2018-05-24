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
add_property <- function(x, element, attr_name = "type", attr_base, ...){
  if(element == "vertex"){
    sel <- igraph::set_vertex_attr
    elements <- igraph::V(x)
    get_attr <- igraph::vertex_attr
  } else {
    sel <- igraph::set_edge_attr
    elements <- igraph::E(x)
    get_attr <- igraph::edge_attr
  }
  type <- get_attr(x, attr_base)
  if(is.null(type)) type <- 1:length(elements)
  e <- environment()
  pattern <- list(...) %>% purrr::map(as.formula, e)
  df <- dplyr::data_frame(type = type) %>%
    dplyr::mutate(attr = dplyr::case_when(!!! pattern))
  sel(x, name = attr_name, value = df$attr) 

}

add_vertex_edge <- function(x, vertex_name, vertex_type, edges_from, edges_to, edges_type){
  x$edges <- x$edges %>%
    rbind(dplyr::data_frame(from = edges_from, to = edges_to, control_type = edges_type))
  x$vertex <- x$vertex %>%
    rbind(dplyr::data_frame(name = vertex_name, control_type = vertex_type))
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
                      vertex.frame.color = igraph::V(x)$frame.color,
                      vertex.shape = igraph::V(x)$shape,
                      # vertex.label.family = fam,
                      # vertex.label.color = l_c,
                      edge.width = igraph::E(x)$width,
                      edge.lty = igraph::E(x)$lty,
                      edge.color = igraph::E(x)$color,
                      edge.label = igraph::E(x)$label,
                      edge.label.x = igraph::E(x)$label.x,
                      edge.label.color = igraph::E(x)$label.color,
                      edge.label.font = igraph::E(x)$label.font,
                      edge.arrow.size = igraph::E(x)$arrow.size,
                      edge.arrow.width = igraph::E(x)$arrow.width,
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
  if(min(x) == max(x)) return(x)
  (x - min(x))/(max(x)-min(x)) * (newMax - newMin) + newMin 
}

standalone_text <- function(text, x = 0.5, y = 0.5, adj = c(0.5,0.5), font = 1, cex = 0.9, srt = 0){
  plot.new()
  text(x, y, text, adj = adj, font = font, cex = cex, srt = srt)
}

standalone_vline <- function(lty = 1, lwd = 0.5){
  plot.new()
  segments(0.5,0,0.5,1, lty = lty, lwd = lwd)
}

standalone_hline <- function(lty = 1, lwd = 0.5){
  plot.new()
  segments(0,0.5,1,0.5, lty = lty, lwd = lwd)
}

my_pallete <- function(){
  list(dark_orange = "#e66101", 
    light_orange = "#fdb863", 
    light_purple = "#b2abd2", 
    dark_purple = "#5e3c99", 
    light = "#f7f7f7")
}

get_color <- function(x = T){
  dplyr::case_when(
    x == "matched" ~ my_pallete()$dark_purple,
    x == "unmatched" ~ my_pallete()$light_purple,
    x == "control" ~ my_pallete()$dark_orange,
    x == "base" ~ my_pallete()$light, 
    x == "dark" ~ "grey10",
    x == "bg" ~ "grey95",
    x == "bg_dark" ~ "grey90",
    TRUE ~ "white"
  )
}

plot_example_ntw <- function(x, ...){
  plot_examples(x, rescale = F, 
                frame = F, 
                margin = rep(0, 4), 
                edge.arrow.size = 0.4,
                edge.label.family = "sans", 
                vertex.label.family = "sans", ...)
}

# common formatting
ntw_format_theme <- function(x){
  x %>%
    fancify_vertex_name() %>%
    add_property(element = "edge", attr_name = "color", attr_base = "type", 'TRUE ~ "black"') %>%
    add_property(element = "edge", attr_name = "label.color", attr_base = "type",'TRUE ~ "black"') %>%
    add_property(element = "edge", attr_name = "label.font", attr_base = "type",'TRUE ~ 2') %>%
    add_property(element = "vertex", attr_name = "color",attr_base = "type", "TRUE ~ get_color('base')") %>%
    add_property(element = "vertex", attr_name = "frame.color", attr_base = "type","TRUE ~ 'black'") %>%
    add_property(element = "vertex", attr_name = "size",attr_base = "type", "TRUE ~ 55") %>%
    add_property(element = "vertex", attr_name = "label.cex", attr_base = "type","TRUE ~ 1") %>%
    add_property(element = "vertex", attr_name = "label.color", attr_base = "type","TRUE ~ 'black'") %>%
    add_property(element = "edge",attr_name = "arrow.size", attr_base = "control_type", "TRUE ~ 0.4") %>%
    add_property(element = "edge",attr_name = "arrow.width", attr_base = "control_type", "TRUE ~ 0.7") 
  
}
ntw_control_network_theme <- . %>%
  add_property(element = "edge", attr_name = "color", attr_base ="control_type", 'type== "a" ~ "black"', 'TRUE ~ get_color("control")') %>%
  add_property(element = "edge",attr_name = "lty", attr_base = "control_type", "type == 'a' ~ 1", "TRUE ~ 1") %>%
  add_property(element = "vertex", attr_name = "frame.color", attr_base = "control_type", "type == 'a' ~ 'black'", "TRUE ~ get_color('bg')") %>%
  add_property(element = "vertex", attr_name = "color", attr_base = "control_type", "type == 'b' ~ get_color('bg')", "TRUE ~ get_color('base')") %>%
  add_property(element = "vertex", attr_name = "size",attr_base = "control_type",  "type == 'a' ~ 55", "TRUE ~ 45")

ntw_matched_theme <- function(x, named = F){
  x %<>%  
    add_property(element = "vertex", attr_name = "color", attr_base = "matched", 'type ~ get_color("matched")', '!type ~ get_color("unmatched")', 'TRUE ~ get_color("bg")') %>%
    add_property(element = "edge", attr_name = "color", attr_base = "matched", 'type ~ get_color("matched")', '!type ~ get_color("unmatched")', 'TRUE ~ get_color("control")') %>%
    add_property(element = "vertex", attr_name = "size",attr_base = "type", "TRUE ~ 25") %>%
    add_property(element = "vertex", attr_name = "label.color", attr_base = "matched", 'type ~ "white"', '!type ~ "black"', 'TRUE ~ "black"')
  
  if(!named) x %<>% add_property(element = "vertex", attr_name = "name_fancy", attr_base = "type", 'TRUE ~ NA') 
  return(x)
} 

ntw_dir_base_theme <- . %>%
  add_property(element = "vertex", attr_name = "name_fancy", attr_base = "type", 'TRUE ~ NA')


get_bipartite_layout <- function(x, attri){
  types <- igraph::vertex_attr(x, attri) == dplyr::first(igraph::vertex_attr(x, attri))
  igraph::layout_as_bipartite(x, types)
}

direction_layout <- function(){
  matrix(c(1,0,
           2,0,
           3,0,
           1.5, 1,
           2.5, 1), nrow = 5, ncol = 2, byrow = T)
} 

fig_sizes <- function(){
  list(
    one_column_width = 3.4,
    two_column_width = 7
  )
}
