make_fig_asymmetry_dist <- function(directed_networks){
  require(ggplot2)
  directed_networks %>%
    purrr::map(igraph::edge_attr, "weight") %>%
    purrr::map_dfr(~tibble::data_frame(weight = .), .id = "net_name") %>%
    dplyr::mutate(color = dplyr::case_when(
      weight < 0.1 ~ "smaller", 
      weight <= 0.5 ~ "small", 
      TRUE ~ "large"
    )) %>%
    dplyr::group_by(color) %>%
    dplyr::mutate(n = n()) %>%
    ggplot(aes(weight)) +
    # stat_ecdf() 
    base_ggplot_theme() +
    geom_histogram(aes(fill = color), breaks = seq(0,1, by = 0.05), fill = my_pallete()$light_grey) +
    scale_fill_manual(values = c(my_pallete()$light_purple, my_pallete()$light_purple, my_pallete()$dark_purple)) +
    theme(legend.position = "none", 
          plot.subtitle = element_text(size = 8), 
          plot.title = element_text(size = 9)) +
    labs(x = "interaction asymmetry")
}
