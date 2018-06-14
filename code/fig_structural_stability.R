make_fig_structural_stability <- function(sl_characteristics, metadata){
  require(ggplot2)
  p1 <- sl_characteristics %>%
    filter_networks_df(metadata) %>%
    dplyr::inner_join(metadata, by = "net_name") %>%
    dplyr::group_by(net_name) %>%
    dplyr::filter(is.finite(feasibility), 
                  guild == "pla") %>%
    dplyr::mutate(feasibility1 = dplyr::percent_rank(feasibility),
                  feasibility2 = scale(feasibility), 
                  control_capacity = control_capacity) %>%
    ggplot(aes(x = feasibility2 , y = control_capacity)) +
    # scale_y_log10() +
    # geom_point() +
    geom_smooth(aes(group = net_name), 
                method = "glm", 
                se = F, 
                size =0.25, 
                method.args = list(family = "binomial"), 
                colour = "grey", 
                alpha = 0.25) +
    geom_smooth(method = "glm", 
                se = F, 
                size =0.5, 
                method.args = list(family = "binomial"), 
                colour = "black") +
    base_ggplot_theme() +
    labs(title = "structural stability of species removal", 
         x = latex2exp::TeX("scaled feasibility ($\\Omega$)"), 
         y = latex2exp::TeX("control capacity ($\\phi$)"))
  
  list(p1)

}
