make_fig_control_configurations <- function(en_chain, en_star, en_direction, pdf_out = NULL){

    en_chain %<>%
    ntw_format_theme() %>%
    ntw_dir_base_theme() %>%
    add_property("vertex", "size", "type", "TRUE ~ 25")
  
  en_chain_matchings <- en_chain %>%
    matchings_example_networks() %>%
    purrr::map(ntw_format_theme) %>%
    purrr::map(ntw_control_network_theme) %>%
    purrr::map(ntw_matched_theme)
  
  en_star %<>%
    ntw_format_theme() %>%
    ntw_dir_base_theme() %>%
    add_property("vertex", "size", "type", "TRUE ~ 12")
  
  en_star_matchings <- en_star %>%
    matchings_example_networks() %>%
    purrr::map(ntw_format_theme) %>%
    purrr::map(ntw_control_network_theme) %>%
    purrr::map(ntw_matched_theme)
  
  en_dir_m <- en_direction[[2]] %>%
    ntw_format_theme() %>%
    ntw_dir_base_theme() %>%
    add_property("vertex", "size", "type", "TRUE ~ 17")
  
  en_dir_matchings <- en_direction[[2]] %>%
    matchings_example_networks() %>%
    purrr::map(ntw_format_theme) %>%
    purrr::map(ntw_control_network_theme) %>%
    purrr::map(ntw_matched_theme) %>%
    purrr::map(~ add_property(., "vertex", "size", "type", "TRUE ~ 17"))
  
  
  box_col <- get_color("bg") 
  box_col_2 <- get_color("bg_dark")
  heading_height <- 0.25
  div_height <- 0.05
  inner_margin_height <- 0.05
  plot_1_height <- 0.5
  plot_2_height <- 1
  plot_3_height <- 0.75
  legend_height <- 0.4
  
  heights <- c(heading_height, 
               plot_1_height,
               div_height, 
               inner_margin_height,
               plot_2_height/2, plot_2_height/2, 
               inner_margin_height,
               div_height,
               plot_3_height, 
               legend_height)
  d <- 30
  x <- 3.4
  widths <- c(0.05, x/3-x/2/d, x/d, x/2-x/3-x/d, x/d, x/2-x/3-x/d/2, x/3, 0.05)
  
  pdf(pdf_out, width = sum(widths), height = sum(heights), paper='special')
  if(is.null(pdf_out)) dev.control("enable")
  
  c(00,31,99,32,32,32,32,94, 
    03,34,33,35,35,35,35,94,
    84,84,82,82,82,82,82,82,
    92,92,45,90,90,90,90,90,
    04,40,45,41,41,41,42,97,
    96,40,45,43,43,43,43,97,
    93,93,45,91,91,91,91,91,
    83,83,83,83,83,83,83,83,
    05,50,53,51,51,51,52,98,
    61,61,61,61,61,61,61,61) %>%
    dplyr::dense_rank() %>%
    matrix(ncol = 8, byrow = T) %>%
    layout(heights = heights,
           widths = widths)
  par(mar = rep(0,4), bg = "white", xpd = NA)
  
  plot.new()
  
  rect(grconvertX(0, "inches", "user"),
       grconvertY(sum(heights) - heading_height - plot_1_height, "inches", "user"),
       grconvertX(3.5, "inches", "user"),
       grconvertY(sum(heights) - heading_height, "inches", "user"),
       col = box_col, 
       border = NA)
  
  rect(grconvertX(0, "inches", "user"),
       grconvertY(plot_3_height + div_height + legend_height, "inches", "user"),
       grconvertX(3.5, "inches", "user"),
       grconvertY(plot_3_height + div_height + plot_2_height  + legend_height + inner_margin_height * 2, "inches", "user"),
       col = box_col, 
       border = NA)
  
  rect(grconvertX(0, "inches", "user"),
       grconvertY(legend_height, "inches", "user"),
       grconvertX(3.5, "inches", "user"),
       grconvertY(plot_3_height + legend_height, "inches", "user"),
       col = box_col, 
       border = NA)
  
  # darker rects
  
  darker_rects_width <- sum(0.05, x/3-x/2/d, x/d/2)
  
  rect(grconvertX(0, "inches", "user"),
       grconvertY(sum(heights) - heading_height - plot_1_height, "inches", "user"),
       grconvertX(darker_rects_width, "inches", "user"),
       grconvertY(sum(heights) - heading_height, "inches", "user"),
       col = box_col_2, 
       border = NA)
  
  rect(grconvertX(0, "inches", "user"),
       grconvertY(plot_3_height + div_height + legend_height, "inches", "user"),
       grconvertX(darker_rects_width, "inches", "user"),
       grconvertY(plot_3_height + div_height + plot_2_height  + legend_height + inner_margin_height * 2, "inches", "user"),
       col = box_col_2, 
       border = NA)
  
  rect(grconvertX(0, "inches", "user"),
       grconvertY(legend_height, "inches", "user"),
       grconvertX(darker_rects_width, "inches", "user"),
       grconvertY(plot_3_height + legend_height, "inches", "user"),
       col = box_col_2, 
       border = NA)
  
  ## LABELS
  # 03
  # standalone_text("(a)", y = 0.75)
  plot.new()
  # 04
  # standalone_text("(b)", y = 0.75)
  plot.new()
  # 05
  # standalone_text("(c)", y = 0.75)
  plot.new()
  ## FIGURE C
  # 31
  standalone_text("directed network", y = 0.5, adj = c(0.5,0), font = 2)
  # 32
  standalone_text("control configurations", y = 0.5, adj = c(0.5,0), font = 2)
  # 33
  # standalone_vline(lty = 2)
  plot.new()
  # 34-35
  l <- matrix(c(1,2,3,4,
                0,0,0,0), 4,2) %>% 
    rescale_layout(xlim = c(-1, 1)* 1.5, ylim = c(1, 1))
  plot_example_ntw(en_chain, layout = l)
  l <- matrix(c(1,2,3,4,0,
                0,0,0,0,0), 5,2) %>%
    rescale_layout(xlim = c(-1, 1)* 2, ylim = c(-0.6, 0.6))
  plot_example_ntw(en_chain_matchings[[1]], layout = l)
  ## FIGURE D
  # 40
  l <- matrix(c(1,2,2,2,
                2,3,2,1), 4,2) %>%
    rescale_layout(xlim = c(-1, 1)* 0.3, ylim = c(-1, 1)* 0.75/2)
  plot_example_ntw(en_star, layout = l)
  # 41-44
  for(i in 1:3){
    l <- matrix(c(1,2,2,2,0,3,3,
                  2,3,2,1, 2,(3:1)[-i]), 7,2)  %>%
      rescale_layout(xlim = c(-1, 1) * 1.8, ylim = c(-1,1) * 0.75)
    plot_example_ntw(en_star_matchings[[i]], layout = l)
  }
  # 45
  # standalone_vline(lty = 2)
  plot.new()
  ## FIGURE E
  # 50
  l <- direction_layout() %>%
    rescale_layout(xlim = c(-1, 1)* 0.9, ylim = c(-1, 1)* 0.34)
  plot_example_ntw(en_dir_m, layout = l)
  # 51-52
  for(i in 1:2){
    l <- direction_layout() %>% rbind(c(2, -1), c(c(1.5, 2.5)[-i], 2))  %>%
      rescale_layout(xlim = c(-1, 1)*0.9, ylim = c(-1,1) *1)
    plot_example_ntw(en_dir_matchings[[i]], layout = l)
  }
  # 53
  # standalone_vline(lty = 2)
  plot.new()
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
         col = get_color("matched"),
         lwd = 1.5, cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")
  legend(x_y[2,], legend = c("unmatched link"), horiz = TRUE,
         lty = 1, 
         col = get_color("unmatched"),
         lwd = 1.5, cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")
  legend(x_y[3,], legend = c("control input"), horiz = TRUE,
         lty = 1, 
         col = get_color("control"),
         lwd = 1.5, cex = 0.9, xjust=0.5, yjust=y_just, bty = "n")
  
  legend(x_y[4,], legend = c("matched node"), horiz = TRUE,
         pch = 21, 
         pt.bg = c(get_color("matched")),
         col =  c(get_color("matched")),
         pt.cex = 1.5,
         cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")
  legend(x_y[5,], legend = c("unmatched node (driver)"), horiz = TRUE,
         pch = 21, 
         pt.bg = c(get_color("unmatched")),
         col =  c(get_color("unmatched")),
         pt.cex = 1.5,
         cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")
  
  # plot.new()
  ## DIV LINES
  # 80
  # standalone_hline()
  # 81
  # standalone_hline()
  # 82
  # standalone_hline(lty = 2)
  # 83
  # standalone_hline(lty = 2)
  
  
  
  p <- recordPlot()
  
  dev.off()
  
  return(list(
    plot = p,
    width = sum(widths),
    height = sum(heights)
  ))
}
# replayPlot(make_fig_control_configurations(en_chain, en_star, en_direction, "test.pdf")$plot)

