make_fig_structural_control <- function(en_direction, en_structural, pdf_out = NULL){
  
  # graph attributes
  formatted_en_direction <- en_direction  %>%
    purrr::map(ntw_format_theme)
  
  formatted_en_direction[[1]] %<>%
    igraph::set_edge_attr("width", value = igraph::E(.)$weight)
  formatted_en_direction[[2]] %<>%
    igraph::set_edge_attr("width", value = 1)
  
  # format networks for plotting
  formatted_en_structural <- en_structural %>%
    purrr::map(ntw_format_theme) %>%
    purrr::map(ntw_control_network_theme)
  
  adjust_darker_bg <- . %>%
    add_property("vertex", "color", "control_type", "type == 'b' ~ get_color('bg_dark')", "TRUE ~ get_color('bg')") %>%
    add_property("vertex", "frame.color", "control_type", "type == 'b' ~ get_color('bg_dark')", "TRUE ~ 'black'")
  
  formatted_en_structural[[3]] %<>% adjust_darker_bg()
  formatted_en_structural[[5]] %<>% adjust_darker_bg()
  
  box_col <- get_color("bg") 
  box_col_2 <- get_color("bg_dark")
  
  heading_height <- 0.20
  heading_thin_height <- 0.15
  div_height <- 0.05
  inner_margin_height <- 0.05
  plot_1_height <- 0.75
  plot_2_height <- 0.7
  legend_height <- 0.25
  heights <- c(heading_height, plot_1_height, 
               div_height, 
               heading_thin_height,
               plot_2_height,
               div_height, plot_2_height, 
               legend_height)
  d <- 30
  x <- 3.1
  margin_h <- (fig_sizes()$one_column_width-x)
  widths <- c(margin_h, x/3-x/2/d, x/d, x/2-x/3-x/d, x/d, x/2-x/3-x/d/2, x/3,0.001)
  
  pdf(pdf_out, width = sum(widths), height = sum(heights), paper='special') 
  if(is.null(pdf_out)) dev.control("enable")
  
  c(  01,10,10,10,12,11,11,91,
      90,13,13,13,12,14,14,91, 
      80,80,80,80,80,80,80,80, 
      22,20,20,20,28,21,21,93,
      22,24,24,24,28,25,25,94,
      94,81,81,81,81,81,81,81,
      23,26,26,26,28,27,27,93,
      30,30,30,30,30,30,30,30) %>%
    dplyr::dense_rank() %>%
    matrix(ncol = 8, byrow = T) %>%
    layout(heights = heights,
           widths = widths)
  par(mar = rep(0,4), bg = "white", xpd = NA)
  
  plot.new()
  
  rect(grconvertX(margin_h, "inches", "user"),
       grconvertY(sum(heights) - plot_1_height - heading_height, "inches", "user"),
       grconvertX(3.5, "inches", "user"),
       grconvertY(sum(heights) , "inches", "user"),
       col = box_col, 
       border = NA)
  
  rect(grconvertX(margin_h, "inches", "user"),
       grconvertY(legend_height, "inches", "user"),
       grconvertX(3.5, "inches", "user"),
       grconvertY(plot_2_height * 2 + div_height + heading_thin_height + legend_height, "inches", "user"),
       col = box_col, 
       border = NA)

  # rect(grconvertX(margin_h, "inches", "user"),
  #      grconvertY(plot_2_height  + legend_height +  heading_thin_height, "inches", "user"),
  #      grconvertX(3.5, "inches", "user"),
  #      grconvertY(plot_2_height * 2 + div_height + heading_thin_height + legend_height, "inches", "user"),
  #      col = box_col_2,
  #      border = NA)
  
  rect(grconvertX(margin_h, "inches", "user"),
       grconvertY(legend_height, "inches", "user"),
       grconvertX(3.5, "inches", "user"),
       grconvertY(plot_2_height + div_height/2+ legend_height, "inches", "user"),
       col = box_col_2,
       border = NA)

 
  # 02
  # standalone_text("(b)", y = 0.75)
  # plot.new()
  ## FIGURE A
  # 10
  standalone_text("Visitation network", y = 0.25, adj = c(0.5,0), font = 2)
  text(0, 1, "(a)", adj = c(0.15,1.5), font = 1)
  
  # 11
  standalone_text("Direction of control", y = 0.25, adj = c(0.5,0), font = 2)
  
  # 12
  # standalone_vline(lty = 2)
  plot.new()
  # 13-14
  for(i in 1:length(formatted_en_direction)){
    l <- rescale_layout(direction_layout(), xlim = c(-1, 1) * 1.6, ylim = c(-0.55, 0.7))
    plot_example_ntw(formatted_en_direction[[i]], layout = l)
  }
  ## FIGURE B
  # 20
  standalone_text("Dilation", y = 0.5, adj = c(0.5,1), font = 1)
  text(0, 1, "(b)", adj = c(0.15,1.5), font = 1)
  # 21
  standalone_text("Inaccessible node", y = 0.5, adj = c(0.5,1), font = 1)
  # 22
  standalone_text("Not\ncontrollable", srt = 90, font = 2)
  # 23
  standalone_text("Controllable", srt = 90, font = 2)
  # 24-27
  for (i in c(4,2,5,3)){
    # if(i == 3)   text(-2.26, 1, "(b)", adj = c(0,1.3), font = 1)

    types <- igraph::V(formatted_en_structural[[i]])$control_type == dplyr::first(igraph::V(formatted_en_structural[[i]])$control_type)
    l <- igraph::layout_as_bipartite(formatted_en_structural[[i]], types) %>%
      rescale_layout(xlim = c(-1, 1) * 1.55, ylim = c(-0.6, 0.65))
    plot_example_ntw(formatted_en_structural[[i]], layout = l)
  }
  # 28
  # standalone_vline(lty = 2)
  plot.new()
  
  ## DIV LINES
  # 80
  # standalone_hline()
  
  # 82
  # standalone_hline(lty = 2)
  # 83
  # standalone_hline(lty = 2)
  plot.new()
  legend(0.5, 0.5,legend = c("Control input"), horiz = TRUE,
         lty = 1, 
         col = get_color("control"),
         lwd = 1.5, cex = 0.9, xjust=0.5, yjust=0.5, bty = "n")
  plot.new()
  # standalone_hline(lty = 1)
  p <- recordPlot()
  
  dev.off()
  
  return(list(
    plot = p,
    width = sum(widths),
    height = sum(heights)
  ))
}

# drake::loadd(en_structural, en_direction)
# replayPlot(make_fig_structural_control(en_direction, en_structural, "test.pdf")$plot)
