make_fig_emp_contollability <- function(controllability, randomisations_df, metadata){
  require(ggplot2)
  d <- controllability %>% 
    filter_networks_df(metadata) 
  
  r <- randomisations_df %>%
    filter_networks_df(metadata) %>%
    dplyr::mutate(delta_n_D = n_D.x - n_D.y) 
  
  p1 <- d %>%
    ggplot(aes(x = n_D)) +
    stat_density(geom= "line", size = 0.5) +
    geom_vline(xintercept = median(d$n_D), linetype = 2, size = 0.25) +
    xlab(expression(paste(n[D]))) +
    base_ggplot_theme() +
    labs(title = expression(paste("(a) Relative size of the minimum driver node set (", n[D], ")")))

  
  means <- r %>%
    dplyr::group_by(randomisation) %>%
    dplyr::summarise(delta_n_D = median(delta_n_D, na.rm = T))
  
  p2 <- r %>% 
    ggplot(aes(x = delta_n_D, color = randomisation)) +
    stat_density(geom = "line", position = position_identity(), size = 0.5) +
    geom_vline(data = means, aes(xintercept = delta_n_D, color = randomisation), size = 0.25, linetype = 2, show.legend = F) +
    scale_color_manual(values = c(get_color("matched"), get_color("unmatched")), 
                       name = "", 
                       guide = guide_legend(reverse = T), labels = c("random directions", "random interactions")) +
    base_ggplot_theme() +
    theme(legend.position = c(0.82,0.88), legend.background = element_rect(fill = "NA"), legend.key.size = unit(0.15, "in")) +
    labs(title = expression(paste("(b) Difference between random and empirical ", n[D])), 
         x = expression(paste(Delta, n[D])))
         
 list(p1, p2)
}

 # drake::loadd(controllability, metadata, randomisations_df)

