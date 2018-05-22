make_small_network <- function(networks, pdf_out = NULL){
  node_size <- 40
  n1 <- networks$SA_exp
  v_names <- dplyr::data_frame(full_name = igraph::V(n1)$name) %>%
    dplyr::mutate(abb_name = letters[1:nrow(.)],
                  full_name = stringr::str_replace(full_name, "[0-9]", ""),
                  full_name = stringr::str_replace(full_name, "\\?rutitarse", "rufitarse"))
  n1 %<>% igraph::set_vertex_attr("name", value = v_names$abb_name)  %>%
    ntw_format_theme() %>%
    igraph::set_vertex_attr("size", value = node_size) %>%
    igraph::set_edge_attr("label", value = igraph::E(.)$weight) %>%
    igraph::set_edge_attr("color", value = "grey")
  n2 <- as_directed_network(n1, higher_level = "pol")  %>%
    ntw_format_theme() %>%
    igraph::set_vertex_attr("size", value = node_size)
  n3 <- maximum_matching(n2) %>%
    ntw_matched_theme(named = T) %>%
    igraph::set_vertex_attr("size", value = node_size)
  
  heading_height <- 0.25
  fig_height <- 1
  legend_height <- 0.15
  table_height <- 1
  heights <- c(heading_height, fig_height,
               heading_height, fig_height,
               heading_height, fig_height, legend_height, 
               table_height)
  
  total_width <- fig_sizes()$two_column_width
  x <- total_width/2
  widths <- c(x, x)
  
  pdf(pdf_out, width = sum(widths), height = sum(heights), paper='special')
  if(is.null(pdf_out)) dev.control("enable")
  
  c(20,20,
    10,10,
    21,21,
    11,11,
    22,22,
    12,12,
    80,80,
    90,90) %>%
    dplyr::dense_rank() %>%
    matrix(ncol = 2, byrow = T) %>%
  layout(widths = widths, heights = heights)
  par(mar = rep(0,4), bg = "white", xpd = NA)
  
  l <- igraph::layout_as_bipartite(n1, igraph::V(n1)$type == "pla") %>%
    rescale_layout(xlim = c(-1, 1)* 6, ylim = c(-0.8, 0.9)* 0.75)
  plot_example_ntw(n1, layout = l)
  plot_example_ntw(n2, layout = l)
  plot_example_ntw(n3, layout = l)
  
  # labels
  standalone_text("pollination network", font = 2)
  standalone_text("directed network", font = 2)
  standalone_text("a maximum matching", font = 2)
  # legend
  plot.new()
  x_y <- c(1:4, rep(1,4)) %>% matrix(ncol = 2) %>%
    rescale_layout(xlim = c(0.25, 0.75), ylim = c(0.5, 0.5)) %>%
    as.data.frame()
  x_just <- 0.5
  y_just <- 0.9
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
  # table
  plot.new()
  vals <- c(0, 0.21, 0.42, 0.63) + 0.07
  sep <- 0.025
  legend(vals[1], 0.85, legend = v_names$abb_name[1:5], horiz = F, bty = "n", text.font = 2, cex = 0.9)
  legend(vals[1] + sep, 0.85, legend = v_names$full_name[1:5], horiz = F, bty = "n", text.font = 3, cex = 0.9)
  legend(vals[2], 0.85, legend = v_names$abb_name[6:10], horiz = F, bty = "n", text.font = 2, cex = 0.9)
  legend(vals[2] + sep, 0.85, legend = v_names$full_name[6:10], horiz = F, bty = "n", text.font = 3, cex = 0.9)
  legend(vals[3], 0.85, legend = v_names$abb_name[11:15], horiz = F, bty = "n", text.font = 2, cex = 0.9)
  legend(vals[3] + sep, 0.85, legend = v_names$full_name[11:15], horiz = F, bty = "n", text.font = 3, cex = 0.9)
  legend(vals[4], 0.85, legend = v_names$abb_name[16:19], horiz = F, bty = "n", text.font = 2, cex = 0.9)
  legend(vals[4] + sep, 0.85, legend = v_names$full_name[16:19], horiz = F, bty = "n", text.font = 3, cex = 0.9)
  p <- recordPlot()
  dev.off()
  return(list(
    plot = p,
    width = sum(widths),
    height = sum(heights)
  ))
}

# drake::loadd(networks)
# replayPlot(make_small_network(networks, "test.pdf")$plot)
