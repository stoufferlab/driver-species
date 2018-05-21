make_fig_maximum_matching <- function(en_direction, pdf_out = NULL){
  
  n1 <- en_direction[[2]]
  n3 <- en_direction[[2]] %>%
    maximum_matching()
  n2 <- igraph::graph_attr(n3, "bipartite_representation")
  
  n1 %<>%
    ntw_format_theme()
  
  n2 %<>% igraph::vertex_attr("name") %>% 
    stringr::str_replace("..f", "^+") %>%
    stringr::str_replace("..t", "^-") %>%
    igraph::set_vertex_attr(n2, "name", value = .) %>%
    igraph::set_vertex_attr("type", value = "a") %>%
    ntw_format_theme() %>%  
    add_property(element = "edge", attr_name = "color", attr_base = "matched", 'type ~ get_color("matched")', '!type ~ get_color("unmatched")', 'TRUE ~ get_color("control")') %>%
    add_property(element = "vertex", attr_name = "size",attr_base = "type", "TRUE ~ 55")

  n3 %<>%
    ntw_format_theme() %>%
    ntw_matched_theme(named = T) %>%
    add_property(element = "vertex", attr_name = "size",attr_base = "type", "TRUE ~ 55")
 
  heading_height <- 0.25
  fig_height <- 0.75
  legend_height <- 0.2
  
  total_width <- fig_sizes()$two_column_width
  
  heights <- c(heading_height,
               fig_height,
               legend_height)
  widths <- c(total_width/3, total_width/3, total_width/3)
  
  # FIG DEVICE HERE
  pdf(pdf_out, width = sum(widths), height = sum(heights), paper='special')
  if(is.null(pdf_out)) dev.control("enable")
  
  c(01,02,03,
    10,11,12,
    80,80,80) %>%
    dplyr::dense_rank() %>%
    matrix(ncol = 3, byrow = T) %>%
    layout(widths = widths, heights = heights)
  par(mar = rep(0,4), bg = "white", xpd = NA)
  
  # titles
  standalone_text("directed network", font = 2)
  standalone_text("alternative bipartite representation", font = 2)
  standalone_text("matching in original network", font = 2)
  # directed network
  ld <- direction_layout() %>%
    rescale_layout(xlim = c(-1, 1)* 1.25, ylim = c(-1, 1)* 0.5)
  plot_example_ntw(n1, layout = ld)
  # alternative bipartite
  l <- c(rep(1:5, 2), rep(2:1, each = 5)) %>% matrix(ncol = 2) %>%
    rescale_layout(xlim = c(-1, 1)* 1.75, ylim = c(-1, 1)* 0.65)
  plot_example_ntw(n2, layout = l)
  # matching
  plot_example_ntw(n3, layout = ld)
  # legend
  plot.new()
  x_y <- c(1:4, rep(1,4)) %>% matrix(ncol = 2) %>%
    rescale_layout(xlim = c(0.25, 0.75), ylim = c(0.5, 0.5)) %>%
    as.data.frame()
  x_just <- 0.5
  y_just <- 1
  legend(x_y[1, ],legend = c("matched link"), horiz = TRUE,
         lty = 1, 
         col = get_color("matched"),
         lwd = 1.5, cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")
  legend(x_y[2,], legend = c("unmatched link"), horiz = TRUE,
         lty = 1, 
         col = get_color("unmatched"),
         lwd = 1.5, cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")
  legend(x_y[3,], legend = c("matched node"), horiz = TRUE,
         pch = 21, 
         pt.bg = c(get_color("matched")),
         col =  c(get_color("matched")),
         pt.cex = 1.5,
         cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")
  legend(x_y[4,], legend = c("unmatched node (driver)"), horiz = TRUE,
         pch = 21, 
         pt.bg = c(get_color("unmatched")),
         col =  c(get_color("unmatched")),
         pt.cex = 1.5,
         cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")
  
  p <- recordPlot()
  
  dev.off()
  
  return(list(
    plot = p,
    width = sum(widths),
    height = sum(heights)
  ))
    
}

# drake::loadd(en_direction)
# replayPlot(make_fig_maximum_matching(en_direction, "test.pdf")$plot)

