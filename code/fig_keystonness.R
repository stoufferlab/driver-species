make_fig_correlation <- function(sl_char_corr){
  require(ggplot2)
  order <- sl_char_corr[[1]] %>% 
    as.dist() %>%
    hclust() %>% {
      l <- . 
    l$labels[l$order]
  }
  
  plot_df <- sl_char_corr[[1]][order, order] %>% {
    l <- .
    l[upper.tri(l, diag = T)] <- NA
    l
  } %>%
    reshape2::melt() %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(Var1 = factor(Var1, levels = rev(order)), 
                  Var2 = factor(Var2, levels = order), 
                  interesting = (Var2 %in% c("superior", "control_capacity")) | (Var1 %in% c("superior", "control_capacity")), 
                  interesting = dplyr::if_else(interesting, "bold", "plain"))
  p1 <- plot_df %>%
    ggplot(aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(width = 0.9, height = 0.9, size = 0.25) +
    geom_text(aes(label = round(value, 2), fontface = interesting), size = 2) +
    coord_equal() +
    scale_fill_gradient2(low = my_pallete()$light_orange, 
                         high = my_pallete()$light_purple, 
                         mid = "white", 
                         midpoint = 0, na.value = NA) +
    scale_color_manual(values = c(NA, "black")) + 
    base_ggplot_theme() +
    scale_x_discrete(labels = c("A" = expression(bold(alpha)))) +
    labs(title= 'Correlation between control and commonly used "keystoness" metrics', x = "", y = "")
  
  list(p1)
  
}

make_fig_control_capacity <- function(sigma_phi_df, species_empirical_coov, metadata){
  require(ggplot2)
  p2df <- sigma_phi_df %>%
    dplyr::inner_join(species_empirical_coov) %>%
    filter_networks_df(metadata) %>%
    tidyr::gather(key = "metric", value = "value", control_capacity, superior) 
  
  invasive <- p2df %>%
    dplyr::filter(invasive)
  
  p2 <- p2df %>%
    ggplot(aes(x = value, fill = interaction(invasive, guild))) +
    geom_histogram(aes(y = ..count..), geom = "line", position = position_stack(), binwidth =  1/25) +
    facet_wrap(~metric, scales = "free") +
    # scale_y_sqrt() +
    scale_fill_manual(values = c(my_pallete()$light_orange, my_pallete()$dark_orange, my_pallete()$light_purple),
                      name = "", labels = c("native plants", "invasive plants", "pollinators")) +
    base_ggplot_theme() +
    theme(legend.position = c(1, 1),
          legend.justification = c(1,1),
          legend.background = element_rect(fill = "NA"), legend.key.size = unit(0.15, "in"))
  p2
  list(p2)
}

# drake::loadd(sl_char_corr, sigma_phi_df, species_empirical_coov)

colorado <- function(src, boulder) {
  if (!is.factor(src)) src <- factor(src)                   # make sure it's a factor
  src_levels <- levels(src)                                 # retrieve the levels in their order
  brave <- boulder %in% src_levels                          # make sure everything we want to make bold is actually in the factor levels
  if (all(brave)) {                                         # if so
    b_pos <- purrr::map_int(boulder, ~which(.==src_levels)) # then find out where they are
    b_vec <- rep("plain", length(src_levels))               # make'm all plain first
    b_vec[b_pos] <- "bold"                                  # make our targets bold
    b_vec                                                   # return the new vector
  } else {
    stop("All elements of 'boulder' must be in src")
  }
}

## PLOT SECONDARY EXTINCTIONS
# secondary_ext_std %>%
#   dplyr::mutate(interesting = metric %in% c("superior", "control_capacity")) %>%
#   dplyr::mutate(add_seco = seco_prop - prim_prop) %>%
#   dplyr::filter(!metric %in% c("eigen.directed", "page_rank.directed")) %>%
#   dplyr::filter(guild == "both") %>%
#   ggplot(aes(x = prim_prop, y = n_comp, colour = metric, size = interesting)) +
#   geom_line(family = "binomial", se = F) +
#   # scale_x_log10() +
#   # scale_y_log10() +
#   facet_wrap(~net_name, scales = "free") +
#   scale_size_manual(values = c(0.25, 1))
# 
# secondary_ext_std %>%
#   dplyr::mutate(interesting = metric %in% c("superior", "control_capacity")) %>%
#   dplyr::mutate(add_seco = seco_prop - prim_prop) %>%
#   dplyr::filter(!metric %in% c("eigen.directed", "page_rank.directed")) %>%
#   # dplyr::filter(guild == "pol") %>%
#   ggplot(aes(x = prim_prop, y = add_seco, colour = metric, size = interesting)) +
#   geom_point(family = "binomial", se = F, alpha = 0.5) +
#   geom_smooth(family = "binomial", se = F) +
#   # scale_x_log10() + 
#   # scale_y_log10() + 
#   facet_wrap(~guild) +
#   scale_size_manual(values = c(0.25, 1))
