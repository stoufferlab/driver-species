make_fig_structural_stability <- function(sl_characteristics, metadata){
  require(ggplot2)

  st_df <- sl_characteristics %>%
    filter_networks_df(metadata) %>%
    dplyr::inner_join(metadata, by = "net_name") %>%
    dplyr::group_by(net_name) %>%
    dplyr::filter(is.finite(feasibility)) %>%
    dplyr::mutate(feasibility = scale(feasibility), 
                  critical = dplyr::case_when(
                    control_capacity == 1 ~ "critical", 
                    # control_capacity == 0 ~ "redundant",
                    TRUE ~ "redundant"
                  ), 
                  value = feasibility) %>%
    dplyr::filter(!is.na(critical)) 
    
  means <- st_df %>%
    dplyr::group_by(critical) %>%
    dplyr::summarise_if(is.numeric, mean, na.rm = T)
  
  value <- 1
  my.labs <- list(bquote(paste("critical species (", phi==.(value), ")")), 
                  bquote(paste("redundant (", phi<.(value), ")")))
  
  p1 <- st_df %>%
    ggplot(aes(x = value, colour = critical)) +
    stat_density(geom = "line", position = "identity", 
                 size = 0.5) +
    geom_vline(data = means, 
               aes(xintercept = value, colour = critical), 
               linetype = 2, 
               size = 0.25, 
               show.legend = F) +
    scale_color_manual(values = c(my_pallete()$dark_orange, 
                                  my_pallete()$light_orange), 
                       name = "", 
                       labels = my.labs) + 
    base_ggplot_theme() +
    theme(legend.position = c(0, 1.15),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = "NA"), 
          legend.key.size = unit(0.15, "in")) +
    labs(title = "structural stability of species removal",
         x = latex2exp::TeX("scaled feasibility ($\\Omega$)"))
  
  st_df  %$%
    t.test(feasibility ~ critical, alternative = "less", conf.int = T)
  
  list(p1)

}



