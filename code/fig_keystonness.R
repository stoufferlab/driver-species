make_fig_correlation <- function(sl_char_corr, sl_characteristics, metadata){
  require(ggplot2)
  
  special <- c("control_capacity", "superior")
  order <- sl_char_corr[[1]] %>% 
    as.dist() %>%
    hclust() %>% {
      l <- . 
    l$labels[l$order]
    }
  order <- c(order[!order %in% special], special)
  
  plot_df <- sl_char_corr[[1]][order, order] %>% {
    l <- .
    # l[upper.tri(l, diag = T)] <- NA
    l
  } %>%
    reshape2::melt() %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(Var1 = factor(Var1, levels = rev(order)), 
                  Var2 = factor(Var2, levels = order), 
                  interesting = (Var1 %in% c("superior", "control_capacity")), 
                  interesting = dplyr::if_else(interesting, "bold", "plain"))
  
  labeller_long <- function(y){
    x <- order[y]
    dplyr::case_when(
      x == "closeness" ~ expression("closeness  (l)"),
      x == "page_rank.nondirected" ~ expression("page rank (g)"),
      x == "eigen.nondirected" ~ expression("eigen centrality (e)"),
      x == "superior" ~ expression(bold(paste("superior node (", sigma, ")"))),
      x == "betweenness" ~ expression("betweenness (w)"),
      x == "control_capacity" ~ expression(bold(paste("control capacity (", phi, ")"))),
      x == "degree"~ expression("degree (k)")
    )
  }
  labeller_short <- function(y){
    x <- order[y]
    dplyr::case_when(
      x == "closeness" ~ expression("l"),
      x == "page_rank.nondirected" ~ expression("g"),
      x == "eigen.nondirected" ~ expression("e"),
      x == "superior" ~ expression(sigma),
      x == "betweenness" ~ expression("w"),
      x == "control_capacity" ~ expression(phi),
      x == "degree" ~ expression("k")
    )
  }
  labeller_short_rev <- function(y){
    x <- rev(order)[y]
    dplyr::case_when(
      x == "closeness" ~ expression("l"),
      x == "page_rank.nondirected" ~ expression("g"),
      x == "eigen.nondirected" ~ expression("e"),
      x == "superior" ~ expression(sigma),
      x == "betweenness" ~ expression("w"),
      x == "control_capacity" ~ expression(phi),
      x == "degree" ~ expression("k")
    )
  }
  
p1 <- plot_df %>%
    ggplot(aes(x = as.numeric(Var1), y = as.numeric(Var2), fill = value)) +
    geom_tile(width = 0.9, height = 0.9, size = 0.25) +
    geom_text(aes(label = round(value, 2), fontface = "plain", size = interesting)) +
    coord_equal() +
    scale_fill_gradient2(low = my_pallete()$light_orange, 
                         high = my_pallete()$light_purple, 
                         mid = "white", 
                         midpoint = 0, na.value = NA, 
                         name = "") +
    scale_color_manual(values = c(NA, "black")) + 
    scale_y_continuous(breaks = length(order):1,
                       labels = labeller_long,
                       sec.axis = sec_axis(trans = ~.,
                                           breaks = length(order):1,
                                           labels = labeller_short),
                       expand = c(0,0)) +
    scale_x_continuous(breaks = length(order):1, 
                       labels = labeller_short_rev,
                       sec.axis = sec_axis(trans = ~.,
                                           breaks = length(order):1,
                                           labels = labeller_short_rev),
                       expand = c(0,0)) +
    scale_size_manual(values = c(2, 2)) + 
    labs(title = '(a) mean correlation between centrality metrics', x = "", y = "") + 
    base_ggplot_theme() +
    theme(#legend.position = c(1,1.15), 
      legend.position = "none", 
          legend.justification = c(0.5,0.5),
          legend.background = element_rect(fill = "NA"), legend.key.size = unit(0.25, "in"), 
      panel.border = element_rect(fill = NA, colour = NA), 
      axis.ticks = element_line(colour = NA))


plot_df_sd <- sl_char_corr[[2]][order, order] %>% {
  l <- .
  # l[upper.tri(l, diag = T)] <- NA
  l
} %>%
  reshape2::melt() %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(Var1 = factor(Var1, levels = rev(order)), 
                Var2 = factor(Var2, levels = order), 
                interesting = (Var1 %in% c("superior", "control_capacity")), 
                interesting = dplyr::if_else(interesting, "bold", "plain"))

p2 <- plot_df_sd %>%
  ggplot(aes(x = as.numeric(Var1), y = as.numeric(Var2), fill = value)) +
  geom_tile(width = 0.9, height = 0.9, size = 0.25) +
  geom_text(aes(label = round(value, 2), fontface = "plain", size = interesting)) +
  coord_equal() +
  scale_fill_gradient2(low = my_pallete()$light_orange, 
                       high = my_pallete()$light_purple, 
                       mid = "white", 
                       midpoint = 0, na.value = NA, 
                       name = "") +
  scale_color_manual(values = c(NA, "black")) + 
  scale_y_continuous(breaks = length(order):1,
                     labels = labeller_short,
                     sec.axis = sec_axis(trans = ~.,
                                         breaks = length(order):1,
                                         labels = labeller_long),
                     expand = c(0,0)) +
  scale_x_continuous(breaks = length(order):1, 
                     labels = labeller_short_rev,
                     sec.axis = sec_axis(trans = ~.,
                                         breaks = length(order):1,
                                         labels = labeller_short_rev),
                     expand = c(0,0)) +
  scale_size_manual(values = c(2, 2)) + 
  labs(title = '(b) sd correlation between centrality metrics', x = "", y = "") + 
  base_ggplot_theme() +
  theme(#legend.position = c(1,1.15), 
    legend.position = "none", 
    legend.justification = c(0.5,0.5),
    legend.background = element_rect(fill = "NA"), legend.key.size = unit(0.25, "in"), 
    panel.border = element_rect(fill = NA, colour = NA), 
    axis.ticks = element_line(colour = NA))

  list(p1, p2)
  
}
# drake::loadd(sl_char_corr, sl_characteristics, metadata)
# drake::loadd(species_model_cc)
make_fig_control_capacity <- function(sl_characteristics){
  require(ggplot2)
 
  
  p5 <- sl_characteristics %>%
    filter_networks_df(metadata) %>%
    dplyr::filter(guild == "pla") %>%
    ggplot(aes(x = control_capacity, colour = guild)) +
    stat_density(geom = "line", 
                 bw = "SJ", 
                 show.legend = FALSE) +
    geom_point(aes(x = 1, y = 0), 
               fill = my_pallete()$dark_orange,
               colour = "black", 
               shape = 21,
               size = 1) +
    scale_color_manual(values = my_pallete()$dark_orange) +
    base_ggplot_theme() +
    labs(title = "(a) Control capacity of plants", 
         x = latex2exp::TeX("control capacity ($\\phi$)"), 
         y = "density") +
    scale_y_continuous(limits = c(0,9)) +
    theme(legend.position = c(1, 0),
          legend.justification = c(1,0),
          legend.background = element_rect(fill = "NA"), 
          legend.key.size = unit(0.15, "in"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
  
    p6 <- sl_characteristics %>%
    filter_networks_df(metadata) %>%
    dplyr::filter(guild == "pol") %>%
    ggplot(aes(x = control_capacity, colour = guild)) +
    stat_density(geom = "line", 
                 bw = "SJ", 
                 show.legend = FALSE) +
    scale_color_manual(values = my_pallete()$dark_purple) +
    base_ggplot_theme() +
    labs(title = "(b) Control capacity of pollinators", 
         x = latex2exp::TeX("control capacity ($\\phi$)"), 
         y = "density") +
    scale_y_continuous(limits = c(0,9)) +
    theme(legend.position = c(1, 0),
          legend.justification = c(1,0),
          legend.background = element_rect(fill = "NA"), 
          legend.key.size = unit(0.15, "in"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
  
  list(p5, p6) 
}
# drake::loadd(sl_characteristics, metadata)

make_fig_species_predicted <- function(species_model_cc){

  df_cc <- get_predictions_df_species_level(species_model_cc)
  invasive_cc <- df_cc %>%
    dplyr::filter(invasive)
  
  ylims <- c(0, 1)
  
  p1 <- partial_plot(df_cc, "pushpull_o", "plogis(pred)", "glm") + 
    labs(title = "(a)", 
         x = "dependence asymmetry", 
         y = latex2exp::TeX("partial residuals")) +
    coord_cartesian(ylim = ylims)
  
  p2 <- partial_plot(df_cc, "strength_o", "plogis(pred)", "glm") +
    labs(title = "(b)", 
         x = "visitation strength", 
         y = latex2exp::TeX("partiall residuals")) +
    scale_x_continuous(breaks = log(c(1,10,100)), labels = exp) +
    coord_cartesian(ylim = ylims)
  
  
  p3 <- partial_plot(df_cc, "nested_o", "plogis(pred)", "glm") +
    labs(title = "(c)", 
         x = "contribution to nestedness", 
         y = latex2exp::TeX("partial residuals")) +
    coord_cartesian(ylim = ylims)
  
  
list(p1, p2, p3)  
}


make_fig_species_partial <- function(species_model_cc){
  require(ggplot2)
  df_cc <- get_predictions_df_species_level(species_model_cc)
  invasive_cc <- df_cc %>%
    dplyr::filter(invasive)
  
  add_res <- function(x, res){
    x + res
  }
  
  ylims <- c(-11, 20)
  p1 <- partial_plot(df_cc, "pushpull_o", "I(pushpull_comp + res)", "lm") + 
    labs(title = "(c)", 
         x = "dependence asymmetry", 
         y = latex2exp::TeX("partial residuals")) +
    coord_cartesian(ylim = ylims)
  
  p2 <- partial_plot(df_cc, "strength_o", "I(strength_comp + res)", "lm") +
    labs(title = "(b)", 
         x = "visitation strength", 
         y = latex2exp::TeX("partial residuals")) +
    scale_x_continuous(breaks = log(c(1,10,100)), labels = exp) +
    coord_cartesian(ylim = ylims)
  
  
  p3 <- partial_plot(df_cc, "nested_o", "I(nested_comp + res)", "lm") +
    labs(title = "(a)", 
         x = "contribution to nestedness", 
         y = latex2exp::TeX("partial residuals")) +
    coord_cartesian(ylim = ylims)
  
  p4 <- partial_plot(df_cc, "degree_o", "I(degree_comp + res)", "lm") +
    labs(title = "(d)", 
         x = "degree", 
         y = latex2exp::TeX("partiall residuals")) +
    scale_x_continuous(breaks = log(c(1,10,100)), labels = exp) +
    coord_cartesian(ylim = ylims)
  
  # cowplot::plot_grid(p1,p2,p3,p4)
  list(p3, p2, p1, p4)
}


get_predictions_df_species_level <- function(species_model_cc){
  cc_mod_average <- MuMIn::model.avg(species_model_cc$fixed)
  
  cc_model <- species_model_cc$fixed[[which.min(species_model_cc$fixed %>% purrr::map(AICcmodavg::AICc))]]
  
  species_model_cc %>%
    extract2("df") %>%
    modelr::add_predictions(cc_mod_average) %>%
    dplyr::mutate(control_capacity = dplyr::case_when(
      control_capacity == 1 ~ 0.99, 
      control_capacity == 0 ~ 0.01, 
      TRUE ~ control_capacity
    ), response = qlogis(control_capacity), 
    res = response - pred) %>% 
    dplyr::mutate(guildpol = dplyr::if_else(guild == "pol", 1, 0), 
                  nested_comp = beta_component(cc_mod_average, 
                                               list(nestedcontribution, guildpol), 
                                               c("nestedcontribution", "guildpol")),
                  strength_comp = beta_component(cc_mod_average, 
                                                 list(species.strength, guildpol), 
                                                 c("species.strength", "guildpol")),
                  pushpull_comp = beta_component(cc_mod_average, 
                                                 list(interaction.push.pull, guildpol), 
                                                 c("interaction.push.pull", "guildpol")), 
                  degree_comp = beta_component(cc_mod_average, 
                                                 list(degree, guildpol), 
                                                 c("degree", "guildpol"))) %>%
    dplyr::mutate(strength_o = unscale(species.strength), 
                  nested_o = unscale(nestedcontribution), 
                  pushpull_o = unscale(interaction.push.pull),
                  degree_o = unscale(degree))

  
}

partial_plot <- function(df, x, y, smooth_method = "lm"){
  p <-  df %>%
    ggplot(aes_string(x = x, y = y)) +
    geom_hline(yintercept = 0, size = 0.25, linetype = 2) + 
    geom_point(aes(colour = guild), shape = 21, size = 1, alpha = 0.15) +
    geom_smooth(aes(color = guild, fill = guild), 
                method = smooth_method,
                method.args = list(family = "binomial"),
                se = T, 
                size = 0.5, 
                alpha = 0.2) +
    scale_color_manual(values = c(my_pallete()$dark_orange, 
                                  my_pallete()$dark_purple), 
                       name = "", 
                       labels = c("plants", "pollinators")) +
    scale_fill_manual(values = c(my_pallete()$light_orange, 
                                 my_pallete()$light_purple), 
                      name = "", 
                      labels = c("plants", "pollinators")) +
    base_ggplot_theme() +
    theme(legend.position = c(0, 1.15),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = "NA"), 
          legend.key.size = unit(0.15, "in"), 
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm")) 
 p
}

beta_component <- function(avg, vectors, names){
  names <- sort(names)
  coef <- avg$coefficients["full", c("(Intercept)", names, paste(names[1], names[2], sep = ":"))]
  coef[2] * vectors[[1]] + coef[3] * vectors[[2]] + coef[4] * vectors[[1]] * vectors[[2]]
}

make_fig_superior <- function(species_model_superior){
  ## superior
  require(ggplot2)
  
  su_model <- species_model_superior$fixed[[which.min(species_model_superior$fixed %>% purrr::map(AICcmodavg::AICc))]]
  
  df_su <- species_model_superior %>%
    extract2("df") %>%
    modelr::add_predictions(su_model) %>%
    dplyr::mutate(species.strength_o = unscale(species.strength), 
                  interaction.push.pull_o = unscale(interaction.push.pull))
  
  invasive_su <- df_su %>%
    dplyr::filter(invasive) 
  
  p3 <- df_su %>%
    ggplot(aes(x = interaction.push.pull_o, y = plogis(pred))) +
    geom_point(shape = 21, size = 1, colour = "grey50", alpha = 0.25) +
    # scale_x_log10() + 
    geom_smooth(method = "glm", 
                method.args = list(family = "binomial"), 
                se = T, 
                colour = "black", 
                size = 0.5) +
    geom_point(data = invasive_su, aes(y = plogis(pred)), 
               fill = my_pallete()$dark_orange,
               colour = "black", 
               shape = 21,
               size = 1) +
    base_ggplot_theme() +
    labs(title = "(a)", 
         x = "direction of asymmetry", 
         y = latex2exp::TeX("superior prob. ($\\sigma$)")) +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
  
  p4 <- df_su %>%
    ggplot(aes(x = species.strength_o, y = plogis(pred))) +
    geom_point(shape = 21, size = 1, colour = "grey50", alpha = 0.25) +
    scale_x_log10() + 
    geom_smooth(method = "glm", 
                method.args = list(family = "binomial"), 
                se = T, 
                colour = "black",
                size = 0.5) +
    geom_point(data = invasive_su, aes(y = plogis(pred)), 
               fill = my_pallete()$dark_orange,
               colour = "black", 
               shape = 21,
               size = 1)  +
    base_ggplot_theme() +
    labs(title = "(b)", 
         x = "visitation strength", 
         y = latex2exp::TeX("superior prob. ($\\sigma$)")) +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
  
  list(p3, p4)
}

# drake::loadd(sl_char_corr, sigma_phi_df, species_empirical_coov)

unscale <- function(x){
  x * attr(x, "scaled:scale") +  attr(x, "scaled:center")
}

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

# drake::loadd(species_model_cc, species_model_superior)
make_fig_models_degree <- function(species_model_cc, species_model_superior){
  require(ggplot2)
  
  cc_model <- species_model_cc$fixed[[which.min(species_model_cc$fixed %>% purrr::map(AICcmodavg::AICc))]]
  
  df_cc <- species_model_cc %>%
    extract2("df") %>%
    modelr::add_predictions(cc_model) %>%
    dplyr::mutate(degree_o = unscale(degree))
  
  invasive_cc <- df_cc %>%
    dplyr::filter(invasive)
  
  p1 <- df_cc %>%
    ggplot(aes(x = degree_o, y = control_capacity)) +
    geom_point(aes(colour = guild), shape = 21, size = 1, alpha = 0.25, 
               position = position_jitter(0)) +
    scale_color_manual(values = c(my_pallete()$dark_orange, 
                                  my_pallete()$dark_purple), 
                       name = "", 
                       labels = c("plants", "pollinators")) +
    scale_fill_manual(values = c(my_pallete()$light_orange, 
                                 my_pallete()$light_purple), 
                      name = "", 
                      labels = c("plants", "pollinators")) +
    scale_x_continuous(breaks = log(c(1,4,10, 40)), labels = exp) +
    # scale_x_log10(breaks = c(1,4,10,40)) +
    base_ggplot_theme() +
    labs(title = "(a)", 
         x = "degree", 
         y = latex2exp::TeX("control capacity ($\\phi$)")) +
    theme(legend.position = c(1, 0),
          legend.justification = c(1,0),
          legend.background = element_rect(fill = "NA"), 
          legend.key.size = unit(0.15, "in"), 
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm")) 
  
  
  
  # superior
  
  su_model <- species_model_superior$fixed[[which.min(species_model_superior$fixed %>% purrr::map(AICcmodavg::AICc))]]
  
  df_su <- species_model_superior %>%
    extract2("df") %>%
    modelr::add_predictions(su_model) %>%
    dplyr::mutate(degree_o = unscale(degree))
  
  invasive_su <- df_su %>%
    dplyr::filter(invasive) 
  
  p2 <- df_su %>%
    ggplot(aes(x = degree_o, y = plogis(pred))) +
    geom_point(shape = 21, size = 1, colour = "grey50", alpha = 0.25, 
               position = position_jitter(0)) +
  scale_x_continuous(breaks = log(c(1,4,10, 40)), labels = exp) +
    base_ggplot_theme() +
    labs(title = "(b)", 
         x = "degree", 
         y = latex2exp::TeX("superior prob. ($\\sigma$)")) +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"))
  
  list(p1, p2)
}

