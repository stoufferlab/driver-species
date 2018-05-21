make_fig_control_configurations <- function(en_chain, en_star, en_direction, pdf_out = NULL){

    en_chain %<>%
    ntw_format_theme() %>%
    ntw_dir_base_theme() %>%
    add_property("vertex", "size", "type", "TRUE ~ 35")
  
  en_chain_matchings <- en_chain %>%
    matchings_example_networks() %>%
    purrr::map(ntw_format_theme) %>%
    purrr::map(ntw_control_network_theme) %>%
    purrr::map(ntw_matched_theme) %>%
    purrr::map(~ add_property(., "vertex", "size", "type", "TRUE ~ 35"))
  
  en_star %<>%
    ntw_format_theme() %>%
    ntw_dir_base_theme() %>%
    add_property("vertex", "size", "type", "TRUE ~ 17")
  
  en_star_matchings <- en_star %>%
    matchings_example_networks() %>%
    purrr::map(ntw_format_theme) %>%
    purrr::map(ntw_control_network_theme) %>%
    purrr::map(ntw_matched_theme) %>%
    purrr::map(~ add_property(., "vertex", "size", "type", "TRUE ~ 17"))
  
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
  desc_height <- 0.15
  plot_1_height <- 0.75/2
  plot_2_height <- 0.75
  plot_3_height <- 0.75
  legend_height <- 0.4
  
  heights <- c(heading_height, 
               plot_1_height,
               desc_height,
               div_height,
               plot_2_height, 
               desc_height,
               div_height,
               plot_3_height, 
               desc_height,
               legend_height)
  d <- 30
  x <- 3.5
  widths <- c(x/3, 2*x/3/6, 2*x/3/6, 2*x/3/6, 2*x/3/6, 2*x/3/6, 2*x/3/6)
  
  pdf(pdf_out, width = sum(widths), height = sum(heights), paper='special')
  if(is.null(pdf_out)) dev.control("enable")
  
  c(01,02,02,02,02,02,02,
    30,31,31,31,31,31,31,
    41,42,42,42,42,42,42,
    90,90,90,90,90,90,90,
    32,33,33,34,34,35,35,
    43,44,44,44,44,44,44,
    91,91,91,91,91,91,91,
    36,37,37,37,38,38,38,
    45,46,46,46,46,46,46,
    80,80,80,80,80,80,80) %>%
    dplyr::dense_rank() %>%
    matrix(ncol = 7, byrow = T) %>%
    layout(heights = heights,
           widths = widths)
  par(mar = rep(0,4), bg = "white", xpd = NA)
  
  # 31
  standalone_text("directed network", y = 0.5, adj = c(0.5,0), font = 2)  
  rect(grconvertX(0, "inches", "user"),
       grconvertY(sum(heights) - heading_height - plot_1_height - desc_height, "inches", "user"),
       grconvertX(3.5, "inches", "user"),
       grconvertY(sum(heights) - heading_height, "inches", "user"),
       col = box_col, 
       border = NA)
  
  rect(grconvertX(0, "inches", "user"),
       grconvertY(plot_3_height + div_height + legend_height + desc_height, "inches", "user"),
       grconvertX(3.5, "inches", "user"),
       grconvertY(plot_3_height + div_height + plot_2_height  + legend_height + desc_height * 2, "inches", "user"),
       col = box_col, 
       border = NA)
  
  rect(grconvertX(0, "inches", "user"),
       grconvertY(legend_height, "inches", "user"),
       grconvertX(3.5, "inches", "user"),
       grconvertY(plot_3_height + legend_height + desc_height, "inches", "user"),
       col = box_col, 
       border = NA)
  
  # darker rects
  
  darker_rects_width <- sum(0.05, x/3-x/2/d, x/d/2)

  rect(grconvertX(0, "inches", "user"),
       grconvertY(sum(heights) - heading_height - plot_1_height - desc_height, "inches", "user"),
       grconvertX(darker_rects_width, "inches", "user"),
       grconvertY(sum(heights) - heading_height, "inches", "user"),
       col = box_col_2,
       border = NA)

  rect(grconvertX(0, "inches", "user"),
       grconvertY(plot_3_height + div_height + legend_height + desc_height, "inches", "user"),
       grconvertX(darker_rects_width, "inches", "user"),
       grconvertY(plot_3_height + div_height + plot_2_height  + legend_height + desc_height * 2, "inches", "user"),
       col = box_col_2,
       border = NA)

  rect(grconvertX(0, "inches", "user"),
       grconvertY(legend_height, "inches", "user"),
       grconvertX(darker_rects_width, "inches", "user"),
       grconvertY(plot_3_height + legend_height + desc_height, "inches", "user"),
       col = box_col_2,
       border = NA)

  # 32
  standalone_text("control configurations", y = 0.5, adj = c(0.5,0), font = 2)
  # 33
  # standalone_vline(lty = 2)
  # plot.new()
  # 34-35
  l <- matrix(c(1,2,3,4,
                0,0,0,0), 4,2) %>% 
    rescale_layout(xlim = c(-1, 1)* 2, ylim = c(1, 1))
  plot_example_ntw(en_chain, layout = l)
  l <- matrix(c(1,2,3,4,0,
                0,0,0,0,0), 5,2) %>%
    rescale_layout(xlim = c(-1, 1)* 2.5, ylim = c(-0.6, 0.6))
  plot_example_ntw(en_chain_matchings[[1]], layout = l)
  ## FIGURE D
  # 40
  l <- matrix(c(3,2,2,2,
                2,3,2,1), 4,2)[, c(2,1)] %>%
    rescale_layout(xlim = c(-1, 1)* 0.55, ylim = c(-1, 1)* 0.75/2)
  plot_example_ntw(en_star, layout = l)
  # 41-44
  for(i in 1:3){
    l <- matrix(c(3,2,2,2,4,1,1,
                  2,3,2,1, 2,(3:1)[-i]), 7,2)[, c(2,1)]   %>%
      rescale_layout(xlim = c(-1, 1) * 0.55, ylim = c(-1,1) * 1)
    plot_example_ntw(en_star_matchings[[i]], layout = l, margin = rep(0.5,4))
  }
  # 45
  # standalone_vline(lty = 2)
  # plot.new()
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
  ## TEXT DESCRIPTIONS
  y_desc <- 0.7
  standalone_text("N = 4", y = y_desc)
  
  standalone_text(latex2exp::TeX("$matching\\,size = 3 \\;\\;\\; D = 1 \\;\\;\\; n_d = 0.25$"), y = y_desc)
  
  standalone_text("N = 4", y = y_desc)
  
  standalone_text(latex2exp::TeX("$matching\\,size = 1 \\;\\;\\; D = 3 \\;\\;\\; n_d = 0.75$"), y = y_desc)
  
  standalone_text("N = 5", y = y_desc)
  
  standalone_text(latex2exp::TeX("$matching\\,size = 3 \\;\\;\\; D = 2 \\;\\;\\; n_d = 0.40$"), y = y_desc)
  
  # plot.new()
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
# 
# library(magrittr)
# drake::loadd(en_chain, en_star, en_direction)
# replayPlot(make_fig_control_configurations(en_chain, en_star, en_direction, NULL)$plot)

