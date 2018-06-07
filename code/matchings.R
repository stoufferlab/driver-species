#' takes a directed graph and convert it into a bipartite representation with
#' duplicated nodes
#' 
#' @param x an igraph directed network
#' @param weighted wether the output should include weights or not
#' @return an igraph object with the bipartite representation. Vertex names correspond to the original with and added ..f for origin nodes and ..t for receiving nodes. Attribute legacy types includes the original types from the original bipartite representation
#' 
#' @examples
alternative_bipartite <- function(x, weighted = FALSE){
  
  if(weighted) {
    attribute <- "weight"
  } else {
    attribute <- NULL
  }
  
  # error checking to be added
  # 1. check that x is bipartite (has type attribute)
  # 2. check that if weighted=TRUE the weight attibute actually exists
  
  igraph::as_adjacency_matrix(x, attr = attribute, edges = F, sparse = F) %>%
    `rownames<-`(paste0(rownames(.), "..f")) %>%
    `colnames<-`(paste0(colnames(.), "..t")) %>%
    igraph::graph_from_incidence_matrix(weighted = attribute) %>%
    igraph::set_vertex_attr("legacy.type", value = igraph::V(x)$type)
}

maximum_matching <- function(x, weighted = FALSE){
  
  x_bip <- alternative_bipartite(x, weighted = weighted)
  
  m <- igraph::max_bipartite_match(x_bip)
  
   matched_edges_bip <- m %$%
    matching %>%
    extract(igraph::V(x_bip)$type) %>% {
      cbind(from = ., 
            to = names(.))
    } %>% 
     dplyr::as_data_frame() %>%
     dplyr::filter(!is.na(from)) %>%
     as.matrix() %>%
     t() %>%
     igraph::get.edge.ids(x_bip, ., error = F)
   
   x %>%
     igraph::set_graph_attr("matching_size", m$matching_size) %>%
     igraph::set_graph_attr("bipartite_representation", x_bip) %>%
     generate_matched_graph(matched_edges_bip)
}

input_graph <- function(x){
  
  if(!"matching_size" %in% igraph::graph_attr_names(x) | 
     !"matched" %in% igraph::vertex_attr_names(x) |
     !"matched" %in% igraph::edge_attr_names(x)){
    x %<>% maximum_matching()
  }
  
  driver <- igraph::V(x)[!igraph::V(x)$matched]
  non_driver <- igraph::V(x)[igraph::V(x)$matched]
  
  get_input_graph <- function(z, y){
    z %>%
      purrr::map(~ get_control_adjacent(y, .)) %>%
      purrr::map(make_graph_from_vertex) %>%
      `$<-`(delete_graph_attr, FALSE) %>%
      do.call(union_input_graphs, .)
  } 
  
  
  # sets attribute 'input_node' that determines wether a node is redudndant
  # (FALSE) or a possible input node (TRUE)
  ig_d <- get_input_graph(driver, x) %>%
    igraph::set_vertex_attr("input_node", value = TRUE)
  
  ig_nd <- get_input_graph(non_driver, x) %>% 
    igraph::set_vertex_attr("input_node", value = F)
  potential_driver_nodes <- igraph::V(ig_d)
  to_delete <- potential_driver_nodes$name[potential_driver_nodes$name %in% igraph::V(ig_nd)$name]
  ig_nd %<>% igraph::delete_vertices(to_delete)
  ig <- union_input_graphs(ig_d, ig_nd, delete_graph_attr = F) 
  
  igraph::set_graph_attr(x, "input_graph", ig)
}

get_control_adjacent <- function(x, d){
  
  d_bip <- paste0(d$name, "..t")
  
  adjacent_bip <- igraph::V(x$bipartite_representation)[igraph::distances(x$bipartite_representation, d_bip) == 2] %>%
    purrr::map_lgl(~is_adjacent(x$bipartite_representation, d_bip, .)) %>%
    `names<-`(stringi::stri_sub(names(.), 1, -4))

  igraph::V(x)[c(d$name, names(adjacent_bip[adjacent_bip]))]
}

# Chech of d_bip and pab are control adjacent
is_adjacent <- function(xb, d_bip, pab){
  xb %>%
    igraph::all_shortest_paths(d_bip, pab) %$%
    res %>%
    purrr::map(~ get_edges_between_nodes(xb, .)) %>%
    purrr::map(~ my_xor(.$matched[1], .$matched[2])) %>%
    purrr::pmap(any) %>%
    unlist()
}

# for a vertex list a, b, c finds the edges a-b and b-c in graph xb
get_edges_between_nodes <- function(xb, node_list){
  node_list %>% {
    c(.[1], .[2], .[2], .[3])
  } %>%
    igraph::get.edge.ids(xb, .) %>% {
      igraph::E(xb)[.]
    }
}

my_xor <- function(x, y, first_true = FALSE){
  if(first_true) {
    z <- (x == TRUE) & (y == FALSE)
  } else{
    z <- xor(x, y)
  }
  return(z)
}

make_graph_from_vertex <- function(v){
  n_nodes <- length(v)
  if(n_nodes == 1) {
    mini_network <- igraph::make_full_graph(n_nodes)
  } else{
    mini_network <- igraph::make_star(length(v), mode = "undirected")
  }
  mini_network %>%
    igraph::as_adjacency_matrix(sparse = F) %>%
    igraph::graph_from_adjacency_matrix(mode = "undirected") %>%
    igraph::set_vertex_attr("name", value = v$name) %>%
    igraph::set_vertex_attr("type", value = v$type) 
}

union_input_graphs <- function(..., delete_graph_attr = TRUE){
  nets <- list(...)
  
  at <- igraph::vertex_attr_names(nets[[1]])
  at <- at[at != "name"]
  n_nets <- length(nets)
  
  joint_network <- nets %>%
    do.call(igraph::union, .)
  if(length(at) > 0){
    for (i in 1:length(at)){
      values <- 1:n_nets %>%
        purrr::map(~ coalece_attribute(joint_network, at[i], .)) %>%
        do.call(dplyr::coalesce, .)
      for(j in 1:n_nets){
        if(delete_graph_attr){
          joint_network %<>%
            igraph::delete_graph_attr(paste("name", j, sep = "_")) %>%
            igraph::delete_graph_attr(paste("center", j, sep = "_")) %>%
            igraph::delete_graph_attr(paste("mode", j, sep = "_"))
        }
        joint_network %<>%
          igraph::delete_vertex_attr(paste(at[i], j, sep = "_"))
      }
      joint_network %<>%
        igraph::set_vertex_attr(at[i], value = values)
    } 
  }
  joint_network
}

coalece_attribute <- function(x, y, n){
  igraph::vertex_attr(x, name = paste(y, n, sep = "_"))
}

control_capacity <- function(x, method = "published"){
  
  if(!"input_graph" %in% igraph::graph_attr_names(x)){
    x %<>% input_graph(method = method)
  }
  
  # find components without a driver node
  comp <- igraph::induced_subgraph(x$input_graph, 
                                   igraph::V(x$input_graph)[!igraph::V(x$input_graph)$input_node]) %>%
    igraph::components() 
  
  # get a list of the replacable nodes (excluding the single ones)
  combi <- attr2df(x, "vertex", "matched") %>%
    dplyr::inner_join(attr2df(x$input_graph, "vertex", "input_node"), by = "name") %>%
    dplyr::filter(matched & input_node) %$%
    name %>%
    as.list() %>%
    purrr::map(~igraph::adjacent_vertices(x$input_graph, ., mode = "all")) %>%
    purrr::map(~ c(names(.), as.character(.[[1]]$name))) 
  
  n_control_configurations <- get_n_comb(combi)
  
  # always matched nodes
  always_matched <- attr2df(x, "vertex", "matched") %>%
    dplyr::inner_join(attr2df(x$input_graph, "vertex", "input_node"), by = "name") %>%
    dplyr::filter(matched & ! input_node) %$% 
    name %>% {
      n <- .
      rep(n_control_configurations, length(.)) %>%
        `names<-`(., n)
    }
  
  # a data frame of n_matched & control capacity
  cc <- combi %>% 
    purrr::imap(combinations_per_component, combi) %>% 
    unlist %>% 
    c(always_matched) %>%
    dplyr::data_frame(name = names(.), n_matched = .) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(n_matched = sum(n_matched)) %>% 
    dplyr::right_join(attr2df(x, "vertex", "name"), by = "name") %>% 
    dplyr::mutate(n_matched = dplyr::if_else(is.na(n_matched), 0, n_matched),
                  cc = 1 - n_matched/n_control_configurations)
  
  ## generate all combinations instead
  # m_o <- do.call(expand.grid, combi) %>%
  #   as.matrix()
  # not_duplicated <- m_o %>%
  #   apply(1, anyDuplicated) %>%
  #   magrittr::is_less_than(1)
  # m <- m_o[not_duplicated, ]
  # m %>% table
  
  x %<>%
    # store control capacity in the input graph
    igraph::graph_attr("input_graph") %>%
    igraph::set_vertex_attr("control_capacity", cc$name, cc$cc) %>%
    igraph::set_graph_attr(x, "input_graph", .) %>%
    # store control capacity in the main graph
    igraph::set_vertex_attr("control_capacity", cc$name, cc$cc) %>%
    igraph::set_graph_attr("n_control_configurations", n_control_configurations)
}

combinations_per_component <- function(component, list_index, full_list){
  component %>%
    as.list() %>%
    `names<-`(., .) %>%
    purrr::map(n_other_comb, list_index, full_list)
}

n_other_comb <- function(node_name, list_index, full_list){
  # remove other instances of node_name in the list
  full_list[-list_index] %>%
    purrr::map(~ .[. != node_name]) %>%
    get_n_comb()
}


# get total number of combinations in a list of elements excluding repeated 
get_n_comb <- function(this_list){
  
  lengths <- this_list %>%
    purrr::map(length)
  
  repeated_elements <- this_list %>%
    unlist() %>%
    table() %>% {
      .[is_greater_than(., 1)]
    } %>% 
    names() %>%
    as.list()
  
  repeated_combi <- repeated_elements %>%
    purrr::map(function(w){
      this_list %>%
        purrr::map(~ w %in% .) %>%
        unlist() %>%
        not() %>%
        extract(lengths, .) %>%
        do.call(prod, .)
    }) %>% unlist() %>% sum()
  
  do.call(prod, lengths) %>%
    subtract(repeated_combi)
}

attr2df <- function(x, type = c("vertex"), attr_name){
  if(type == "vertex"){
    get_fun <- igraph::vertex_attr
  }
  dplyr::data_frame(name = get_fun(x, "name")) %>%
    dplyr::mutate(!! attr_name := get_fun(x, attr_name))
}

all_matchings <- function(x){
  
  if(!"matching_size" %in% igraph::graph_attr_names(x) | 
     !"matched" %in% igraph::vertex_attr_names(x) |
     !"matched" %in% igraph::edge_attr_names(x)){
    x %<>% maximum_matching()
  }
  
  matched_edges_bip_list <- x$bipartite_representation %>%
    igraph::make_line_graph() %>%
    igraph::complementer() %>% 
    igraph::max_cliques(min = x$matching_size, 
                        max = x$matching_size)
  
  matched_edges_bip_list %>%
    purrr::map(~ generate_matched_graph(x, .)) %>%
    return()
}

generate_matched_graph <- function(x, matched_edges_bip) {
  
  igraph::E(x$bipartite_representation)$matched <- FALSE
  igraph::E(x$bipartite_representation)[matched_edges_bip]$matched <- TRUE
  
  # a matrix with the ends and tails of the matched edges
  matched_edges <- matched_edges_bip %>% 
    igraph::ends(x$bipartite_representation, .) %>% 
    t() %>%
    stringi::stri_sub(., 1, -4)
  
  igraph::E(x)$matched <- FALSE
  igraph::E(x)[igraph::get.edge.ids(x, matched_edges, error = F)]$matched <- TRUE
  
  igraph::V(x)$matched <- FALSE
  igraph::V(x)$superior <- FALSE
  # need the ends of the matched edges, so select only the even ones
  igraph::V(x)[matched_edges[c(F, T)]]$matched <- TRUE
  igraph::V(x)[matched_edges[c(T, F)]]$superior <- TRUE
  
  x
}

add_control_nodes <- function(x, base_name = "u"){
  igraph::V(x)$control_type <- "a"
  igraph::E(x)$control_type <- "a"
  
  driver_nodes <-  igraph::V(x)[na.omit(!igraph::V(x)$matched)]
  n_new_vertices <- length(igraph::V(x)) - x$matching_size
  new_vertices_names <- paste(base_name, 1:n_new_vertices, sep = "_")
  
  x %>% 
    igraph::add_vertices(n_new_vertices, name = new_vertices_names, control_type = "b") %>%
    igraph::add_edges(edges = rbind(new_vertices_names, driver_nodes$name), control_type = "b") %>%
    return()
}
