make_fig_species_level <- function(sl_char_corr){
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
  plot_df %>%
    ggplot(aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(width = 0.9, height = 0.9, size = 0.25) +
    geom_text(aes(label = round(value, 2), fontface = interesting), size = 3) +
    coord_equal() +
    scale_fill_gradient2(low = my_pallete()$light_orange, 
                         high = my_pallete()$light_purple, 
                         mid = "white", 
                         midpoint = 0, na.value = NA) +
    scale_color_manual(values = c(NA, "black")) + 
    base_ggplot_theme() +
    scale_x_discrete(labels = c("A" = expression(bold(alpha)))) +
    labs(title= 'Correlation between control and commonly used "keystoness" metrics', x = "", y = "")
}
drake::loadd(sl_char_corr)

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

