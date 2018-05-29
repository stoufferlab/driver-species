get_input_graph_step_nets <- function(x){
  n1 <- x %>%
    control_capacity() %>%
    ntw_format_theme()
  
  n2 <- n1 %>%
    ntw_format_theme() %>%
    ntw_matched_theme(named = T) %>%
    add_property(element = "vertex", attr_name = "size",attr_base = "type", "TRUE ~ 55")
  
  n3 <- n1$input_graph %>%
    ntw_input_graph_theme()
  list(n1, n2, n3)
}

make_fig_input_graph <- function(en_direction, en_star, en_chain, pdf_out = NULL){
  
  n1 <- get_input_graph_step_nets(en_direction[[2]])
  n2 <- get_input_graph_step_nets(en_star)
  n3 <- get_input_graph_step_nets(en_chain)
  
  heading_height <- 0.25
  fig_height <- 0.75
  sep_height <- 0.05
  legend_height <- 0.2
  heights <- c(heading_height, fig_height, sep_height, fig_height, sep_height, fig_height, legend_height)
  
  total_width <- fig_sizes()$two_column_width
  x <- total_width/3
  widths <- c(x, x, x)
  
  pdf(pdf_out, width = sum(widths), height = sum(heights), paper='special')
  if(is.null(pdf_out)) dev.control("enable")
  
  c(80,81,82,
    10,11,12,
    90,90,90,
    20,21,22,
    91,91,91,
    30,31,32,
    70,70,70) %>%
    dplyr::dense_rank() %>%
    matrix(ncol = 3, byrow = T) %>%
    layout(widths = widths, heights = heights)
  par(mar = rep(0,4), bg = "white", xpd = NA)
  
  ## DIRECTION NETWORK
  # directed network
  l <- direction_layout() %>%
    rescale_layout(xlim = c(-1, 1)* 1.25, ylim = c(-1, 1)* 0.5)
  plot_example_ntw(n1[[1]], layout = l)
  # matchings
  plot_example_ntw(n1[[2]], layout = l)
  # input graph
  ls <- igraph::layout_as_tree(n1[[3]])%>%
    rescale_layout(xlim = c(-1, 1)* 1.25, ylim = c(-1, 1)* 0.5)
  plot_example_ntw(n1[[3]], layout = ls)
  ## STAR NEYWORK
  # directed network
  l <- matrix(c(3,2,2,2,
                2,3,2,1), 4,2)[, c(2,1)] %>%
    rescale_layout(xlim = c(-1, 1)* 1.25, ylim = c(-1, 1)* 0.5)
  plot_example_ntw(n2[[1]], layout = l)
  # matching
  plot_example_ntw(n2[[2]], layout = l)
  # input graph
  ls <- igraph::layout_as_tree(n2[[3]])%>%
    rescale_layout(xlim = c(-1, 1)* 1.25, ylim = c(-1, 1)* 0.5)
  plot_example_ntw(n2[[3]], layout = ls)
  ## CHAIN
  # dir network
  l <- igraph::layout_as_tree(n3[[3]])%>% # use layout of input graph with adjustment
    rescale_layout(xlim = c(-1, 1)* 1.75, ylim = c(-1, 1)* 0.5)
  plot_example_ntw(n3[[1]], layout = l)
  # matching
  plot_example_ntw(n3[[2]], layout = l)
  # input graph
  ls <- l %>% rescale_layout(xlim = c(-1, 1)* 1.25, ylim = c(-1, 1)* 0.5)
  plot_example_ntw(n3[[3]], layout = ls)
  # legends
  plot.new()
  x_y <- c(1:5, rep(1,5)) %>% matrix(ncol = 2) %>%
    rescale_layout(xlim = c(0.15, 0.85), ylim = c(0.5, 0.5)) %>%
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
  legend(x_y[4,], legend = c("unmatched node"), horiz = TRUE,
         pch = 21, 
         pt.bg = c(get_color("unmatched")),
         col =  c(get_color("unmatched")),
         pt.cex = 1.5,
         cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")
  legend(x_y[5,], legend = c("possible input node"), horiz = TRUE,
         pch = 21, 
         pt.bg = c(get_color("dark")),
         col =  c(get_color("dark")),
         pt.cex = 1.5,
         cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")

  # titles
  standalone_text("directed network", font = 2)
  standalone_text("maximum matching", font = 2)
  standalone_text("input graph", font = 2)
  
  # sep lines
  standalone_hline(lty = 2)
  standalone_hline(lty = 2)
  p <- recordPlot()
  dev.off()
  return(list(
    plot = p,
    width = sum(widths),
    height = sum(heights)
  ))  
}

# drake::loadd(en_direction, en_star, en_chain)
# replayPlot(make_fig_input_graph(en_direction, en_star, en_chain, "test.pdf")$plot)
