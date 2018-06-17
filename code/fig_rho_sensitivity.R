make_fig_rho_sensitivity <- function(structural_rho_correlation, rho_feasibility){
  require(ggplot2)
  
  structural_rho_correlation %<>%
    dplyr::mutate(rho = as.numeric(rho))
  
  medians <- structural_rho_correlation %>%
    dplyr::group_by(rho) %>%
    dplyr::summarise(cor = median(cor)) %>%
    dplyr::group_by()
  
  p1 <- medians %>%
    ggplot(aes(x = rho, y = cor)) +
    geom_line(data = structural_rho_correlation, 
              aes(x = rho, y = cor, group = net_name),
              alpha = 0.25, 
              colour = my_pallete()$light_purple) +
    geom_line(aes(group = 1), colour = my_pallete()$dark_purple) + 
    base_ggplot_theme() + 
    coord_cartesian(ylim = c(-1, 1)) +
    labs(title = "(a) Species contribution to coexistence", 
         x = latex2exp::TeX("interspecific competition ($\\rho$)"), 
         y = "Spearman correlation")
  
  p2 <- rho_feasibility %>%
    ggplot(aes(x = rho)) +
    geom_line(aes(y = dif), colour = my_pallete()$dark_purple) +
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high), 
                fill = my_pallete()$light_purple, 
                alpha = 0.25) +
    base_ggplot_theme() +
    labs(title = "(b) Diff. crirical - redundant species", 
         x = latex2exp::TeX("interspecific competition ($\\rho$)"), 
         y = "structural feasibility (scaled)")
  
  list(p1, p2)

}

