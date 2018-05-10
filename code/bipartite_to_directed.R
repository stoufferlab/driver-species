
#' Title
#'
#' @param x an incidence matrix or an igraph network. If an igraph network the higher level argument must be specified
#' @param type type of directed links desired. Asymmetry produces a single
#' @param higher_level 
#'
#' @return
#' @export
#'
#' @examples
as_directed_network <- function(x, 
                                direction = c("asymmetry", "dependence", "top-down", "bottom-up"), 
                                ties = c("both", "none", "top-down", "bottom-up"), higher_level){
  # if is an igraph object construct an incidence matrix with the weights
  if(class(x) == "igraph"){
    types <- dplyr::data_frame(name = igraph::V(x)$name, type = igraph::V(x)$type)
    x <- x %>% igraph::as_incidence_matrix(types = igraph::V(.)$type == higher_level, attr = "weight")
  } else {
    types <- dplyr::data_frame(name = c(rownames(x), colnames(x)), 
                               type = c(rep(F, nrow(x)), rep(T, ncol(x))))
  }
  if(direction[1] == "top-down"){
    y <- igraph::graph_from_incidence_matrix(x, directed = T, "in", weighted = T)
  } else if(direction[1] == "bottom-up"){
    y <- igraph::graph_from_incidence_matrix(x, directed = T, "out", weighted = T)
  } else if(direction[1] %in% c("asymmetry", "dependence")){
    y <- x %>%
      bipartite::linklevel("dependence")
    if(direction[1] == "asymmetry"){
      y <- y %>% get_assymetry(ties = ties)
    } 
    y <- two_incidence_to_adjacency(y)
    y <- igraph::graph_from_adjacency_matrix(y, mode = "directed", weighted = T)
  }
  y %>% 
    assign_vertex_types(types)
}

assign_vertex_types <- function(x, types){
  net_names <- dplyr::data_frame(name = igraph::V(x)$name)
  net_name_type <- net_names %>% dplyr::inner_join(types, by = "name")
  igraph::set_vertex_attr(x, "type", value = net_name_type$type)
}

two_incidence_to_adjacency <- function(a){
  x <- a[[1]]
  y <- a[[2]]
  # fill "inverse" diagonal
  adj_matrix <- rbind(
    cbind(matrix(0,nrow(x), nrow(x)), x),
    cbind(t(y), matrix(0, ncol(x), ncol(x))))
  colnames(adj_matrix) <- rownames(adj_matrix)
  adj_matrix
}

get_assymetry <- function(a, ties){
  if(ties[1]=="both"){
    r <- c(1,1)
  } else if(ties[1]=="none"){
    r <- c(0,0)
  } else if(ties[1]=="bottom-up"){
    r <- c(1,0)
  } else if(ties[1]=="top-down"){
    r <- c(0,1)
  }
  list(
    (a[[1]]-a[[2]])/pmax(a[[1]], a[[2]]),
    (a[[2]]-a[[1]])/pmax(a[[1]], a[[2]])
  ) %>%
    purrr::map2(r, replace_zeros) %>%
    purrr::map(replace_negatives) %>%
    purrr::map(replace_nas)
    
}

replace_negatives <- function(x, y = 0){
  x[x<0] <- y
  x
}

replace_nas <- function(x, y = 0){
  x[is.na(x)] <- y
  x
}

replace_zeros <- function(x,y = 0){
  x[x==0] <- y
  x
}

