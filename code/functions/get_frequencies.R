get_frequencies <- function(z){
  z %>%
    plyr::ldply(function(x){
      plyr::ldply(x, function(y){
        matched <- y$matched %>%
          dplyr::rename(species = ve,
                        m_freq = ma, 
                        m_weight = we)
        driver <- y$driver %>%
          dplyr::rename(species = vs,
                        d_freq = ma, 
                        d_weight = we)
        dplyr::inner_join(matched, driver, by = "species") %>%
          dplyr::mutate(n_matchings = attr(y, "n_matchings"))
      }, .id = "threshold") 
    }, .id = "net_name") %>% 
    dplyr::mutate(threshold = as.numeric(as.character(threshold))) %>%
    dplyr::filter(threshold %in% c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) %>%
    dplyr::mutate(m_reim = m_freq/n_matchings,
                  d_reim = d_freq/n_matchings,
                  d_weight = d_weight/n_matchings)
} 
