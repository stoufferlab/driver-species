#' Title
#' 
#' Takes a bipartite network and makes it directed  whether  from set A to set
#' B, bidirectional or depending on the weight of the interaction
#' 
#' @param net
#'   
#' @return
#' @export
#' 
#' @examples
bipartite_digraph <- function(net,
											type = c("weight", "z-bi", "AB", "BA"),
											keep = c("all", "A", "B", "random"),
											weight.type = c("max_dep", "asymmetry"), 
											scale = F){
  
	# sanity checks
	stopifnot(
		# stop if it's not an igraph object
		any(class(net) == "igraph"),
		# stop if it's not bipartite
		dplyr::n_distinct(igraph::vertex_attr(net, "type")) == 2
	)
	
  # is it an unweighted network?
  if(dplyr::n_distinct(igraph::E(net)$weight) == 1) {
    uw <- TRUE
  } else {
    uw <- FALSE
  }
  
	# if its bidirectional just make an easy convertion
	if (type[1] == "z-bi") {
	  # get dependencies
	  dependencies <- net %>% 
	    igraph::as_adjacency_matrix(sparse = 0, attr = "weight")
	  
	  if(!uw){
	    dependencies %<>%
	      apply(1, function(x){x / sum(x)}) %>% t()
	  } 
	  
	  y <- igraph::graph_from_adjacency_matrix(dependencies, mode = "directed",
	                                      weighted = TRUE) %>%
	    igraph::set_vertex_attr(name = "type", 
	                            value = igraph::vertex_attr(net, "type")) %>%
	    graph_reverse()
	  
	}
  
	# if it's by weight
	else if (type[1] == "weight") {
		# get adjancecy matrix
		adj_o <- igraph::as_adjacency_matrix(net, attr = "weight", sparse = F)
		# calculate dependencies
		adj <- apply(adj_o, 1, function(x){x / sum(x)}) %>% t()
		
		# see which one depends more on each other to establish the direction of the
		# link
		adj_max <- adj
		for(i in 1:nrow(adj)){
			for(j in 1:ncol(adj)){
			  max_dep <- ton(adj[j,i], adj[i,j], `>=`)
			  if (weight.type[1] == "max_dep") {
			    adj_max[i, j] <- max_dep
			  } else if (weight.type[1] == "asymmetry"){
			    adj_max[i, j] <- as(adj[j,i], adj[i,j], max_dep)
			  }
			  
				if(uw & adj_max[i,j] != 0) adj_max[i,j] <- 1
			}
		}
		
		if (scale) adj_max <- adj_max * adj_o
		
		# create graph
		y <- igraph::graph_from_adjacency_matrix(adj_max, 
																						 mode = "directed", 
																						 weighted = TRUE) %>%
			igraph::set_vertex_attr(name = "type", 
															value = igraph::vertex_attr(net, "type"))
		
		if (keep[1] != "all") y <- check_double_links(y, keep)
	
	# if it's one way or the other
	} else {

		adj <- igraph::as.directed(net) %>% igraph::as_adj(sparse = F)
		part <- net %>% igraph::vertex_attr("type") %>% factor() %>% as.numeric()
		if (type[1] == "AB") {
			adj[part == 1,  ] <- 0
		}	else adj[part == 2,  ] <- 0
		y <- igraph::graph_from_adjacency_matrix(adj, mode = "directed") %>%
			igraph::set_edge_attr("weight", 
														value = igraph::edge_attr(net, "weight")) %>%
			igraph::set_vertex_attr("type", 
															value = igraph::vertex_attr(net, "type")) %>%
		  graph_reverse()
	}
	
	y
}

# return the first element if it's the largest, 0 otherwise
ton <- function (x, y, fun = `>`) {
	if (fun(x, y)){
		return(x)
	} else {
		return(0)
	}
}

# return the asymetry on the direction of the largest 0 otherwise,
as <- function(x, y, tonn){
  if (tonn == 0) {
    return(0)
  } else if (x ==y) {
  	return(0.5)
  } else {
    abs(x-y) / max(x, y)
  }
}

# change the double links to single ones
check_double_links <- function(x, keep){
	el <- igraph::as_edgelist(x)
	# identify duplicates
	df <- data.frame (Vp = NULL, Vm = NULL, rep = NULL)
	# cycle through each row
	for (i in 1:nrow(el)){
		w <- rev(el[i,])
		w <- which(apply(el, 1, identical, w))
		# if there is a duplicate link 
		if (length(w) > 0) {
			temp_frame <- data.frame(Vp = i, 
															 Vm = w, 
															 rep = dplyr::n_distinct(df$rep) + 1)
			# if is the second or larger duplicate
			if (nrow(df) >= 1){
				w <- as.matrix(c(w, i))
				# see if it has been found before
				prev <- as.matrix(df[, 1:2]) %>%
					apply(1, function(x, w) {
						x[1] == w[1] & x[2] == w[2]
					}, w)
				if (any(prev)) temp_frame$rep[1] <- df$rep[which(prev)]
			}
			# append to current results
			df <- rbind(df, temp_frame)
		}
	}
	
	if(nrow(df)==0) return(x)
	
	out <- plyr::dlply(df, "rep", function(d, keep){
		if (keep == "A") ind <- 1
		else if (keep == "B") ind <- 2
		else if (keep == "random") ind <- round(runif(1, min = 1, max = 2))
		x <- igraph::delete_edges(x, d$Vp[ind])
	}, keep)
	
	out$`1`	
}

p <- function(x, ...){
	igraph::plot.igraph(x = x , 
	                    edge.arrow.size = 0.25, 
											vertex.color = factor(igraph::vertex_attr(x, "type")), 
											edge.label = round(E(x)$weight, digits =2), 
											edge.curved = T,...)
}

# reverse direction of edges
graph_reverse <- function (graph) {
  if (!igraph::is.directed(graph))
    return(graph)
  e <- igraph::get.data.frame(graph, what="edges")
  ## swap "from" & "to"
  neworder <- 1:length(e)
  neworder[1:2] <- c(2,1)
  e <- e[neworder]
  names(e) <- names(e)[neworder]
  igraph::graph.data.frame(e, vertices = igraph::get.data.frame(graph, what="vertices"))
}