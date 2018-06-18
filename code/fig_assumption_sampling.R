# drake::loadd(metrics_subsampled, metadata)
make_fig_assumption_subsampling <- function(metrics_subsampled, metadata){
  require(ggplot2)
 
  p1 <- metrics_subsampled %>%
    extract2("controllability") %>%
    filter_networks_df(metadata) %>%
    dplyr::arrange(net_name, prop) %>%
    dplyr::group_by(net_name) %>%
    dplyr::mutate(prop_int = n_int / orig_int, 
                  n_D_diff = n_D - dplyr::first(n_D)) %>% 
    ggplot(aes(x = prop_int, y = n_D_diff)) +
    geom_point(shape = 21, colour = my_pallete()$light_purple, size = 1) +
    geom_smooth(aes( group =net_name), 
                method = "lm",
                formula = y ~ 0 + I(-x -1), 
                colour = my_pallete()$light_purple, 
                fill = my_pallete()$light_purple, 
                size = 0.25,
                alpha  = 0.1,
                se = T) +
    geom_smooth(method = "lm",
                formula = y ~ 0 + I(-x -1), 
                colour = my_pallete()$dark_purple, 
                size = 0.5,
                alpha  = 0.1,
                se = F) +
    geom_hline(yintercept = 0, linetype = 2, size = 0.25) +
    base_ggplot_theme() +
    scale_x_reverse(labels = scales::percent) +
    labs(x = "proportion of interactions", 
         y = latex2exp::TeX("$\\Delta n_D$"))
  
  p2 <- c("superior", "control_capacity") %>%
    set_names(., .) %>%
    purrr::map_dfr(~ extract_correlations_subsamples(metrics_subsampled$species_metrics, .), .id = "metric") %>%
    filter_networks_df(metadata) %>% 
    dplyr::mutate(metric = dplyr::case_when(
      metric == "control_capacity" ~ "control capacity", 
      TRUE ~ "superior probability"
    )) %>%
    ggplot(aes(x = prop_int, y = cor - 1)) +
    geom_point(shape = 21, colour = my_pallete()$light_purple, size = 1) +
    geom_smooth(aes( group =net_name), 
                method = "lm",
                formula = I(y) ~ 0 + I(-x -1), 
                colour = my_pallete()$light_purple, 
                fill = my_pallete()$light_purple, 
                size = 0.25,
                alpha  = 0.1,
                se = T) +
    geom_smooth(method = "lm",
                formula = I(y) ~ 0 + I(-x -1), 
                colour = my_pallete()$dark_purple, 
                size = 0.5,
                alpha  = 0.1,
                se = F) +
    geom_hline(yintercept = 0, linetype = 2, size = 0.25) +
    base_ggplot_theme() +
    scale_x_reverse(labels = scales::percent) +
    scale_y_continuous(labels = function(x) round(x+1, 1)) +
    labs(x = "proportion of interactions", 
         y = "spearman correlation") + 
    facet_wrap(~metric, ncol = 2)

  one_less_trans <- function(){
    scales::trans_new("one_less", function(x) x, function(x) x, 
                      breaks = scales::extended_breaks(), )
  }
  
  p3 <- metrics_subsampled %>%
    extract2("species_metrics") %>%
    dplyr::group_by(net_name, sp_name) %>%
    dplyr::mutate(critical = prop == 0 & control_capacity == 1, 
                  critical = any(critical)) %>% 
    dplyr::filter(critical)%>%
    dplyr::arrange(net_name, prop) %>%
    dplyr::group_by(net_name) %>%
    dplyr::mutate(prop_int = n_int / orig_int) %>% 
    ggplot(aes(x = prop_int, y = control_capacity - 1)) +
    geom_point(shape = 21, colour = my_pallete()$light_purple, size = 1) +
    geom_smooth(aes( group =net_name), 
                method = "lm",
                formula = I(y) ~ 0 + I(-x -1), 
                colour = my_pallete()$light_purple, 
                fill = my_pallete()$light_purple, 
                size = 0.25,
                alpha  = 0.1,
                se = T) +
    geom_smooth(method = "lm",
                formula = I(y) ~ 0 + I(-x -1), 
                colour = my_pallete()$dark_purple, 
                size = 0.5,
                alpha  = 0.1,
                se = F) +
    geom_hline(yintercept = 0, linetype = 2, size = 0.25) +
    base_ggplot_theme() +
    scale_x_reverse(labels = scales::percent) +
    scale_y_continuous(labels = function(x) x+1) +
    labs(title = "control capacity of critical species", 
         x = "proportion of interactions", 
         y = latex2exp::TeX("control capacity $\\phi$")) 
  
 list(p1, p2, p3)

}