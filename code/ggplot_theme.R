base_ggplot_theme <- function(){
  theme_bw() +
    theme(text = element_text(family = "Helvetica"),
          title = element_text(size = 7, hjust = 0),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7),
          axis.text = element_text(size = 7),
          axis.title = element_text(size = 8, hjust = 0.5), 
          strip.text = element_text(size = 8, hjust = 0), 
          strip.background = element_blank(),
          plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), "mm"),
          panel.grid = element_blank())
}
