make_fig_all_matchings <- function(en_direction, pdf_out = NULL){
  n0 <- maximum_matching(en_direction[[2]])
  n1 <- igraph::graph_attr(n0, "bipartite_representation")
  n2 <- igraph::make_line_graph(n1) 
  n3 <- igraph::complementer(n2)
  cliques <- igraph::max_cliques(n3, 
                                 min = igraph::graph_attr(n0, "matching_size"), 
                                 max = igraph::graph_attr(n0, "matching_size"))

  n1 %<>% igraph::vertex_attr("name") %>% 
    stringr::str_replace("..f", "^+") %>%
    stringr::str_replace("..t", "^-") %>%
    igraph::set_vertex_attr(n1, "name", value = .) %>%
    igraph::set_vertex_attr("type", value = "a") %>%
    ntw_format_theme() %>%
    add_property(element = "vertex", attr_name = "size",attr_base = "type", "TRUE ~ 35")
  
  line_names <- paste(igraph::ends(n1, 1:5)[,1], igraph::ends(n1, 1:5)[,2], sep = "-")
  
  n2 %<>%
    igraph::set_vertex_attr("name", value = line_names) %>%
    ntw_format_theme() %>%
    add_property(element = "vertex", attr_name = "size",attr_base = "type", "TRUE ~ 55") %>%
    add_property(element = "vertex", attr_name = "color", attr_base = "type", "TRUE ~ 'white'") %>%
    add_property(element = "vertex", attr_name = "frame.color", attr_base = "type", "TRUE ~ 'white'") %>%
    add_property(element = "vertex", attr_name = "shape", attr_base = "type", "TRUE ~ 'circle'")
  
  n3 %<>%
    igraph::set_vertex_attr("name", value = line_names) %>%
    ntw_format_theme() %>%
    add_property(element = "vertex", attr_name = "size",attr_base = "type", "TRUE ~ 55") %>%
    add_property(element = "vertex", attr_name = "color", attr_base = "type", "TRUE ~ 'white'") %>%
    add_property(element = "vertex", attr_name = "frame.color", attr_base = "type", "TRUE ~ 'white'") %>%
    add_property(element = "vertex", attr_name = "shape", attr_base = "type", "TRUE ~ 'circle'")
  
  n4 <- cliques %>%
    purrr::map(combn, 2) %>%
    purrr::map(as.vector) %>%
    purrr::map(~ igraph::get.edge.ids(n3, .)) %>%
    purrr::map(~ igraph::set_edge_attr(n3, "clique", ., T)) %>%
    purrr::map(~ add_property(., element = "edge", attr_name = "color", attr_base = "clique", "type == T ~ get_color('matched')", "TRUE ~ get_color('unmatched')")) %>%
    purrr::map(igraph::set_vertex_attr, name ="size", value = 55)
  
  heading_height <- 0.25
  fig_height <- 1.35
  heights <- c(heading_height, fig_height)
  
  total_width <- fig_sizes()$two_column_width
  x <- total_width/5
  widths <- c(x, x, x, x, x)
  
  pdf(pdf_out, width = sum(widths), height = sum(heights), paper='special')
  if(is.null(pdf_out)) dev.control("enable")
  
  c(20,21,22,23,23,
    10,11,12,13,14) %>%
    dplyr::dense_rank() %>%
    matrix(ncol = 5, byrow = T) %>%
    layout(widths = widths, heights = heights)
  par(mar = rep(0,4), bg = "white", xpd = NA)
  
  # alternative bipartite
  l <- c(rep(1:5, 2), rep(2:1, each = 5)) %>% matrix(ncol = 2) %>%
    rescale_layout(xlim = c(-1, 1)* 0.8, ylim = c(-1, 1)* 0.5)
  plot_example_ntw(n1, layout = l)
  # line graph
  ll <- igraph::layout_in_circle(n2)%>%
    rescale_layout(xlim = c(-1, 1) * 0.75, ylim = c(-1, 1) * 0.75)
  plot_example_ntw(n2, layout = ll)
  # complementer
  plot_example_ntw(n3, layout = ll)
  # cliques
  plot_example_ntw(n4[[1]], layout = ll)
  plot_example_ntw(n4[[2]], layout = ll)
  
  # titles
  standalone_text("P: alternative bipartite\nrepresentation", font = 2)
  standalone_text("L(P): line graph of P", font = 2)
  standalone_text("H: complement of L(P)", font = 2)
  standalone_text("max. cliques in H", font = 2)
  
  
  p <- recordPlot()
  dev.off()
  return(list(
    plot = p,
    width = sum(widths),
    height = sum(heights)
  ))
}

# drake::loadd(en_direction)
# replayPlot(make_fig_all_matchings(en_direction, "test.pdf")$plot)
