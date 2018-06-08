make_fig_emp_contollability <- function(controllability, metadata){
  require(ggplot2)
  d <- controllability %>% 
    filter_networks_df(metadata) 
  
  d %>%
    ggplot(aes(x = n_D)) +
    stat_density(geom= "line", size = 0.5) +
    geom_vline(xintercept = mean(d$n_D), linetype = 2, size = 0.25) +
    xlab(expression(paste("", n[D], "", " - relative size of the minimum driver node set"))) +
    base_ggplot_theme()
}

# drake::loadd(controllability, metadata)
