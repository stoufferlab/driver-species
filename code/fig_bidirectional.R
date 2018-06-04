ntw_desc <- function(x){
  latex2exp::TeX(c(
    paste0("$D = ", length(igraph::V(x)) - igraph::graph_attr(x, "matching_size"), "$"),
    paste0("$n_D = ", round(1 - igraph::graph_attr(x, "matching_size") / length(igraph::V(x)), 2), "$"),
    paste0("$No.\\,control\\,config. = ", igraph::graph_attr(x, "n_control_configurations"), "$")
  ))
}

make_fig_bidirectional <- function(en_chain, pdf_out = NULL){
  n1 <- en_chain %>%
    igraph::add_edges(c(2,1,4,3)) %>%
    ntw_format_theme(named = F) %>%
    add_property(element = "vertex", attr_name = "size",attr_base = "type", "TRUE ~ 55")
  
  
  n2 <- n1 %>% 
    split_bilinks() %>%
    purrr::map(control_capacity) %>%
    purrr::map(ntw_format_theme, named = F) %>%
    # purrr::map(ntw_matched_theme, named = F) %>%
    purrr::map(~ add_property(., element = "vertex", attr_name = "size",attr_base = "type", "TRUE ~ 55"))
  
  agg_n1 <- n2 %>% 
    merge_bilinks(aggregation_option_list)
    
  heading_height <- 0.25
  fig_height <- 0.5
  text_height <- 0.35
  sep_height <- 0.05
  legend_height <- 0.2
  heights <- c(heading_height, fig_height, text_height)
  
  total_width <- fig_sizes()$two_column_width
  sep_width <- 0.05
  x <- (total_width - sep_width)/5
  widths <- c(x, sep_width, x, x, x, x)
  
  pdf(pdf_out, width = sum(widths), height = sum(heights), paper='special')
  if(is.null(pdf_out)) dev.control("enable")
  
  c(01,90,02,02,02,02,
    21,90,23,25,27,29,
    22,90,24,26,28,30) %>%
    dplyr::dense_rank() %>%
    matrix(ncol = 6, byrow = T) %>%
    layout(widths = widths, heights = heights)
  par(mar = rep(0,4), bg = "white", xpd = NA)
  
  # titles
  standalone_text("directed network\nwith reciprocal links", font = 2)
  standalone_text("non-reciprocal network realisations", font = 2)
  # nets
  # dir network
  l <- c(1, 0, 
         2, 0, 
         3, 0, 
         4, 0) %>% matrix(ncol = 2, byrow = T) %>%
    rescale_layout(xlim = c(-1, 1) * 2.25, ylim = c(-1, 1)* 0.5)
  plot_example_ntw(n1, layout = l)
  # dir network info
  ntw_desc(agg_n1) %>%
    standalone_text(y = c(0.9, 0.55, 0.2))
  # unipartite nets
  for(i in 1:length(n2)){
    l %<>%
      rescale_layout(xlim = c(-1, 1) * 1.75, ylim = c(-1, 1)* 0.5)
    plot_example_ntw(n2[[i]], layout = l)
    plot.new()
    text(0.5, c(0.9, 0.55, 0.2), ntw_desc(n2[[i]]), font = 1, cex = 0.9, srt = 0)
  }
  # legend
  # plot.new()
  # x_y <- c(1:4, rep(1,4)) %>% matrix(ncol = 2) %>%
  #   rescale_layout(xlim = c(0.15, 0.85), ylim = c(0.5, 0.5)) %>%
  #   as.data.frame()
  # x_just <- 0.5
  # y_just <- 0.9
  # legend(x_y[1, ],legend = c("matched link"), horiz = TRUE,
  #        lty = 1, 
  #        col = get_color("matched"),
  #        lwd = 1.5, cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")
  # legend(x_y[2,], legend = c("unmatched link"), horiz = TRUE,
  #        lty = 1, 
  #        col = get_color("unmatched"),
  #        lwd = 1.5, cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")
  # legend(x_y[3,], legend = c("matched node"), horiz = TRUE,
  #        pch = 21, 
  #        pt.bg = c(get_color("matched")),
  #        col =  c(get_color("matched")),
  #        pt.cex = 1.5,
  #        cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")
  # legend(x_y[4,], legend = c("unmatched node (driver)"), horiz = TRUE,
  #        pch = 21, 
  #        pt.bg = c(get_color("unmatched")),
  #        col =  c(get_color("unmatched")),
  #        pt.cex = 1.5,
  #        cex = 0.9, xjust=x_just, yjust=y_just, bty = "n")
  # separator
  standalone_vline(lty = 2)
  p <- recordPlot()
  dev.off()
  return(list(
    plot = p,
    width = sum(widths),
    height = sum(heights)
  ))  
}

# drake::loadd(en_chain)
# replayPlot(make_fig_bidirectional(en_chain, "test.pdf")$plot)
