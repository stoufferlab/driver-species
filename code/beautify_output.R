pretty_term_names <- function(x, version = c("long", "short")){
  if(version[1] == "long"){
    x %<>%
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
                           "asymmetry") %>%
      stringr::str_replace(stringr::fixed("guildpol"), 
                           "level (pol.)")
  } else if (version[1] == "short"){
    x %<>%
      stringr::str_replace(stringr::fixed("species.strength"), 
                           "$s$") %>%
      stringr::str_replace(stringr::fixed("nestedcontribution"), 
                           "$n$") %>%
           stringr::str_replace(stringr::fixed("interaction.push.pull"), 
                           "$a$") %>%
      stringr::str_replace(stringr::fixed("(Intercept)"), 
                           "int.") %>%
      stringr::str_replace(stringr::fixed("degree"), 
                           "$k$") %>%
      stringr::str_replace(stringr::fixed("guild"), 
                           "$l$") %>%
      stringr::str_replace(stringr::fixed("df"), 
                           "d.f.") %>%
      stringr::str_replace(stringr::fixed("delta"), 
                           "$\\Delta$AICc")
      
  }
  x
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
