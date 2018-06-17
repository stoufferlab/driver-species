pretty_term_names <- function(x){
  x %>%
    stringr::str_replace(stringr::fixed("species.strength"), 
                         "visitation strength")%>%
    stringr::str_replace(stringr::fixed("nestedcontribution"), 
                         "contribution to nestedness")%>%
    stringr::str_replace(stringr::fixed("sd_(Intercept).guild"), 
                         "std. dev. (guild)")%>%
    stringr::str_replace(stringr::fixed("sd_interaction.push.pull.guild"), 
                         "std. dev. (asymmetry | guild)")%>%
    stringr::str_replace(stringr::fixed("cor_(Intercept).interaction.push.pull.guild"), 
                         "cor. (asymmetry | guild)") %>%
    stringr::str_replace(stringr::fixed("interaction.push.pull"), 
                         "asymmetry")
}

pretty_net_names <- function(x){
  x %>% 
    stringr::str_replace(stringr::fixed("bal_eff"), 
                         "efficiency") %>%
    stringr::str_replace(stringr::fixed("bal_imp"), 
                         "importance") %>%
    stringr::str_replace(stringr::fixed("bal_vis"), 
                         "visitation")
}
