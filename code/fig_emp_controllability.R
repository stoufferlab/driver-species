make_fig_emp_contollability <- function(controllability, randomisations_df, metadata, controllability_models, controllability_model_data, network_properties){
  require(ggplot2)
  d <- controllability %>% 
    filter_networks_df(metadata) %>%
    dplyr::inner_join(metadata, by = "net_name") 

  medians_d <- d %>%
    dplyr::group_by(inv) %>%
    dplyr::summarise(n_D = median(n_D, na.rm = T))
  
  
  p1 <- d %>%
    ggplot(aes(x = n_D, colour = inv)) +
    stat_density(geom= "line", size = 0.5, position = position_identity()) +
    geom_vline(data = medians_d, aes(xintercept = n_D, colour = inv),  linetype = 2, size = 0.25, show.legend = FALSE) +
    scale_color_manual(values = c(my_pallete()$light_orange, my_pallete()$dark_orange), 
                       name = "", 
                       guide = guide_legend(reverse = F), labels = c("invaded", "uninvaded")) +
    xlab(expression(paste(n[D]))) +
    base_ggplot_theme() +
    theme(legend.position = c(1, 1.15),
          legend.justification = c(1,1),
          legend.background = element_rect(fill = "NA"), legend.key.size = unit(0.15, "in")) +
    labs(title = expression(paste("(a) Relative size of the minimum driver node set (", n[D], ")")))
  r <- randomisations_df %>%
    filter_networks_df(metadata) %>%
    dplyr::mutate(delta_n_D = n_D.x - n_D.y) 
  
  means <- r %>%
    dplyr::group_by(randomisation) %>%
    dplyr::summarise(delta_n_D = median(delta_n_D, na.rm = T))
  
  cont_model <- controllability_models[[which.min(controllability_models %>% purrr::map(AICcmodavg::AICc))]]
  
  original_web_asymmetry <- network_properties %>%
    dplyr::select(net_name, web.asymmetry) 
  
  df_cc <- controllability_model_data %>%
    modelr::add_predictions(cont_model) %>% 
    dplyr::inner_join(original_web_asymmetry, by = "net_name") %>%
    dplyr::mutate(web.asymmetry = scale(web.asymmetry)) %>%
    dplyr::mutate(web_asymmetry_o = unscale(web.asymmetry))
  
  p3 <- df_cc %>%
    ggplot(aes(x = web_asymmetry_o, y = pred)) +
    geom_point(shape = 21, size = 1, colour = "grey50", alpha = 0.75) +
    geom_smooth(method = "lm", 
                se = T, 
                colour = "black", 
                size = 0.5) +
    scale_y_continuous(labels = function(x) round(plogis(x), 2)) +
    labs(title = latex2exp::TeX("(b) Effect of network asymmetry on $n_D$"), 
         x = "network asymmetry", 
         y = latex2exp::TeX("$n_D$")) +
    base_ggplot_theme()

  p2 <- r %>% 
    ggplot(aes(x = delta_n_D, color = randomisation)) +
    stat_density(geom = "line", position = position_identity(), size = 0.5) +
    geom_vline(data = means, aes(xintercept = delta_n_D, color = randomisation), size = 0.25, linetype = 2, show.legend = F) +
    scale_color_manual(values = c(my_pallete()$dark_purple,  my_pallete()$light_purple), 
                       name = "", 
                       guide = guide_legend(reverse = T), labels = c("random directions", "random interactions")) +
    base_ggplot_theme() +
    theme(legend.position = c(1,1.15), 
          legend.justification = c(1,1),
          legend.background = element_rect(fill = "NA"), legend.key.size = unit(0.15, "in")) +
    labs(title = expression(paste("(c) Difference between random and empirical ", n[D])), 
         x = expression(paste(Delta, n[D])))
         
 list(p1, p3, p2)
}

# drake::loadd(controllability, metadata, randomisations_df)
