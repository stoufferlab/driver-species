all_three_matchings <- function(x, timeout = Inf, ...){
  x <- x %>%
    split_bilinks() %>%
    extract2(1) 
  
  try({
    all_m <- R.utils::withTimeout({
      all_m <- all_matchings(x)
      all_m
    }, onTimeout = "silent", timeout = timeout)
  })
  
  if(is.null(all_m)) return(NA)
  
  matching_sizes <- all_m %>% 
    purrr::map(igraph::graph_attr,"matching_size") %>% 
    unlist() 
  
  matching_size_ok <- matching_sizes %>%
    subtract(mean(.)) %>% {
      all(. == 0)
    }
  
  matching_size <- unique(matching_sizes)
  
  if(!matching_size_ok) stop("matching sizes are not rge same")
  
  flatten <- . %>%
    purrr::map(as.list) %>%
    purrr::pmap(sum) %>%
    unlist() 
  
  all_m %>%
    purrr::map(igraph::vertex_attr, "matched") %>%
    purrr::pmap(rbind) %>%
    unlist() %>%
    matrix(nrow = length(all_m)) %>%
    colSums()
  
  c_capacity <- all_m %>%
    purrr::map(igraph::vertex_attr, "matched") %>%
    flatten() %>%
    subtract(length(all_m), .) %>%
    divide_by(length(all_m))
  
  superiorness <- all_m %>%
    purrr::map(igraph::vertex_attr, "superior") %>%
    flatten() %>%
    divide_by(length(all_m))
  
  list(matching_size = matching_size, 
       control_capacity = c_capacity, 
       superiorness = superiorness)
}

all_three_input_graph <- function(x, method = "published", ...){
  
  cc <- x %>%
    split_bilinks() %>%
    extract2(1) %>%
    control_capacity(method = method)
  
  matching_size <- igraph::graph_attr(cc, "matching_size")
  c_capacity <- igraph::vertex_attr(cc, "control_capacity")
  superiorness <- igraph::vertex_attr(cc,"superior")
  
  list(matching_size = matching_size, 
       control_capacity = c_capacity, 
       superiorness = superiorness)
}

# compare_control_configuration_methods <- function(x){
#   comparison <- list(all_three_matchings(x), 
#                      all_three_input_graph(x))
#   
#   general_agreement <- purrr::pmap(comparison, cor, method = "spearman")
#   general_agreement$matching_size <- comparison[[1]][[1]] == comparison[[2]][[1]]
#   
#   general_agreement
# }

# determine spearman correlations between alternatives and 
cc_correlations <- function(x,y,z){
  get_cc <- . %>% purrr::map_if(!is.na(.), ~.$control_capacity)
  
  cc <- list(x, y, z) %>%
    `names<-`(c("published", "mine", "base")) %>%
    purrr::map(get_cc)
  
  correlation_with_cliques <- . %>% purrr::map2(cc[[3]], function(x,y) {
    if(length(x) == length(y)) {
      cor(x,y, method = "spearman")
    } else{
      NA
    }
  })
  
  ones_and_zeroes <- function(x, level){
    x %>% purrr::map2(cc[[3]], function(x, y){
      if(length(x) == length(y)){
        sum(x[y==level] != level)/length(x)
      } else {
        NA
      }
    }) 
  }
  
  tidy_res <- function(x, name_col){
    x %>%
      purrr::map(unlist) %>%
      dplyr::as_data_frame() %>%
      dplyr::mutate(name = names(cc[[1]])) %>%
      tidyr::gather("method", !!name_col, published , mine)
  }
  
  corr <- cc[1:2] %>%
    purrr::map(correlation_with_cliques) %>%
    tidy_res("corr")
  
  redundant <- cc[1:2] %>% 
    purrr::map(ones_and_zeroes, 0) %>%
    tidy_res("redundant")
  
  critical <- cc[1:2] %>% 
    purrr::map(ones_and_zeroes, 1) %>%
    tidy_res("critical")
  
  dplyr::inner_join(corr, redundant, by = c("name", "method")) %>% 
    dplyr::inner_join(critical, by = c("name", "method")) %>%
    dplyr::rename(net_name = name)
}

compare_cc_options <- function(x, metadata){
  x  %>%
    dplyr::inner_join(metadata, by = "net_name") %>%
    dplyr::filter(study != "ballantyne") %>%
    dplyr::group_by(method) %>%
    dplyr::summarise(corr = mean(corr, na.rm = T)) %>%
    dplyr::mutate(corr = 1/corr) %>%
    dplyr::mutate_if(is.numeric, dplyr::min_rank) %>%
    tidyr::gather(key = "key", value = "value", -method) %>%
    dplyr::group_by(method) %>%
    dplyr::summarise(value = mean(value)) %>%
    dplyr::arrange(value) %>%
    dplyr::slice(1) %$%
    method
}

# drake::loadd(control_input_pub_method, control_input_pub_method, control_lineg_method)
# x <- control_input_pub_method
# y <- control_input_min_method
# z <- control_lineg_method

test_control_metrics <- function(n, algorithm = c("input_graph", "complementer"), exclude = NULL, method = "published", timeout = Inf) {
  # chose function for algorith
  if(algorithm[1] == "input_graph"){
    alg_fun <- all_three_input_graph
  } else if (algorithm[1] == "complementer"){
    alg_fun <- all_three_matchings
  }
  # order networks by ascending size
  n <- n %>%
    purrr::map(~ length(igraph::V(.))) %>%
    unlist() %>%
    sort() %>% names %>% {
      `[`(n, .)
    }
  
  out <- foreach (i =1:length(n)) %dopar% {
    message("Processing ", names(n[i]), " with ", algorithm, " algorithm")
    cat("Processing ", names(n[i]), " with ", algorithm, " algorithm\n")
    # if network should be excluded
    if(names(n)[i] %in% exclude) {
      return(NA)
    } else {
      alg_fun(n[[i]], method = method, timeout = timeout) %>%
        return()
    }
  }
  out %>%
    `names<-`(names(n))
}


# drake::loadd(directed_networks)
# x <- directed_networks$SA_exp
# y <- directed_networks$SEL1OP
# y <- directed_networks$FRA1OP
# y <- directed_networks$MIQ2OP
# x <- split_bilinks(y)[[1]]
# # x <- directed_networks$bal_vis
# x <- drake::readd(en_star)
# 
# ig <- control_capacity(x)
# 
# par(mfrow = c(1,2))
# 
# ig %>%
#   igraph::graph_attr("input_graph") %>%
#   ntw_format_theme() %>%
#   ntw_input_graph_theme() %>%
#   igraph::set_vertex_attr("size", value = 15) %>%
#   igraph::set_vertex_attr("label.color", value = "grey") %>%
#   plot()
# 
# ig %>%
#   ntw_format_theme() %>%
#   ntw_matched_theme() %>%
#   igraph::set_vertex_attr("size", value = 15) %>%
#   igraph::set_edge_attr("arrow.size", value = 0.1) %>%
#   plot()
# 
# n1 <- all_three_matchings(x, 0.1)
# n2 <- all_three_input_graph(x, method = "published")
# comparison <- list(n1, n2)
# 
# dplyr::data_frame(sp = igraph::V(x)$name,
#                   cc_matchings = comparison[[1]]$control_capacity,
#                   cc_ig = comparison[[2]]$control_capacity) %T>%
#   View() %$% {
#     plot(cc_matchings, cc_ig)
#     print(cor(cc_matchings, cc_ig))
#     abline(0, 1)
#   }
