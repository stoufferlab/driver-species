fig_graphical_abstract <- function(en_direction, pdf_out = NULL){

  
  box_col <- "#b2abd2"
  
  # Prepare visitation network and directed network
  formatted_en_direction <- en_direction  %>%
    purrr::map(ntw_format_theme)
  # visitation
  formatted_en_direction[[1]] %<>%
    igraph::set_edge_attr(name = "width", value = igraph::E(.)$weight)
  # direction
  formatted_en_direction[[2]] %<>%
    igraph::set_edge_attr(name = "width", value = 1)
  
  # Prepare controlled networks
  en_dir_matchings <- en_direction[[2]] %>%
    matchings_example_networks() %>%
    purrr::map(ntw_format_theme) %>%
    purrr::map(ntw_control_network_theme) %>%
    purrr::map(ntw_matched_theme) %>%
    purrr::map(~ add_property(., element = "vertex", attr_name = "color", attr_base = "matched", 'type ~ my_pallete()$dark_orange', '!type ~ my_pallete()$light_orange', 'TRUE ~ my_pallete()$light_purple')) %>%
    purrr::map(~ add_property(., element = "vertex", attr_name = "frame.color", attr_base = "control_type", "type == 'a' ~ 'black'", "TRUE ~ my_pallete()$light_purple")) %>%
    purrr::map(~ add_property(., element = "edge", attr_name = "color", attr_base = "matched", 'type ~ my_pallete()$dark_orange', '!type ~ my_pallete()$dark_orange', 'TRUE ~ my_pallete()$dark_purple')) %>%
    purrr::map(~ add_property(., "vertex", "size", "type", "TRUE ~ 32.5"))
  
  # Setup graphical parameters
  dummy_area_height <- 0.0001
  heading_height <- 0.20
  plot_1_height <- 0.75
  div_height <- 0.05
  plot_2_height <- 1.25
  legend_height <- 0.4
  heights <- c(dummy_area_height, 
               heading_height, 
               plot_1_height, 
               div_height, 
               heading_height,
               plot_2_height,
               legend_height)
  # x <- fig_sizes()$two_column_width/2
  x <- sum(heights)
  widths <- c(x/2, x/2)
  png(pdf_out, width = sum(widths), height = sum(heights), units = "in", res = 320)
  if(is.null(pdf_out)) dev.control("enable")
  c(00,00,
    01,02,
    11,12,
    90,90,
    03,03,
    21,22,
    80,80) %>%
    dplyr::dense_rank() %>%
    matrix(ncol = 2, byrow = T) %>%
    layout(heights = heights,
           widths = widths)
  par(mar = rep(0,4), bg = "white", xpd = NA)
  # Start plotting

  plot.new()
    # Background rectangles first
  rect(grconvertX(0, "inches", "user"),
       grconvertY(sum(heights) - plot_1_height - heading_height - dummy_area_height, "inches", "user"),
       grconvertX(x*2, "inches", "user"),
       grconvertY(sum(heights) , "inches", "user"),
       col = box_col, 
       border = NA)
  
  rect(grconvertX(0, "inches", "user"),
       grconvertY(legend_height, "inches", "user"),
       grconvertX(x*2, "inches", "user"),
       grconvertY(plot_2_height + legend_height + heading_height, "inches", "user"),
       col = box_col, 
       border = NA)
  
  # Next titles
  standalone_text("Visitation network", y = 0.25, adj = c(0.5,0), font = 2, cex = 1.1)
  standalone_text("Direction of control", y = 0.25, adj = c(0.5,0), font = 2, cex = 1.1)
  standalone_text("Maximum matchings / Control configurations", y = 0.25, adj = c(0.5,0), font = 2, cex = 1.1)
  
  for (i in 1:length(formatted_en_direction)) {
    l <- rescale_layout(direction_layout(), xlim = c(-1, 1) * 1.6, ylim = c(-0.55, 0.7))
    plot_example_ntw(formatted_en_direction[[i]], layout = l)
  }
  
  for (i in 1:2) {
    l <- direction_layout() %>% rbind(c(2, -1), c(c(1.5, 2.5)[-i], 2))  %>%
      rescale_layout(xlim = c(-1, 1)*0.95, ylim = c(-1,1) * 1.1)
    plot_example_ntw(en_dir_matchings[[i]], layout = l)
  }
  
  
  plot.new()
  
  ## LEGEND
  
  x_y <- rbind(c(0.15, 1),
               c(0.5, 1), 
               c(0.85, 1), 
               c(1/3-0.025, 0.6), 
               c(2/3+0.025, 0.6)) %>% as.data.frame()
  x_just <- 0.5
  y_just <- 1.1
  legend(x_y[1, ],legend = c("matched link"), horiz = TRUE,
         lty = 1, 
         col = my_pallete()$dark_orange,
         lwd = 1.5, cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")
  legend(x_y[2,], legend = c("unmatched link"), horiz = TRUE,
         lty = 1, 
         col = my_pallete()$light_orange,
         lwd = 1.5, cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")
  legend(x_y[3,], legend = c("control input"), horiz = TRUE,
         lty = 1, 
         col = my_pallete()$light_purple,
         lwd = 1.5, cex = 0.9, xjust=0.5, yjust=y_just, bty = "n")
  
  legend(x_y[4,], legend = c("matched node"), horiz = TRUE,
         pch = 21, 
         pt.bg = c(my_pallete()$dark_orange),
         col =  c(my_pallete()$dark_orange),
         pt.cex = 1.5,
         cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")
  legend(x_y[5,], legend = c("unmatched node"), horiz = TRUE,
         pch = 21, 
         pt.bg = c(my_pallete()$light_orange),
         col =  c(my_pallete()$light_orange),
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

fig_graphical_abstract(en_direction, pdf_out = "mama.png")
