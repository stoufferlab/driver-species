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
											type = c("bi", "weight", "AB", "BA"),
											keep = c("all", "A", "B", "random")){
	
	# sanity checks
	stopifnot(
		# stop if it's not an igraph object
		any(class(net) == "igraph"),
		# stop if it's not bipartite
		dplyr::n_distinct(igraph::vertex_attr(net, "type")) == 2
	)
	
	# if its bidirectional just make an easy convertion
	if (type[1] == "bi") y <- igraph::as.directed(net)
	
	# if it's by weight
	else if (type[1] == "weight") {
		# get adjancecy matrix
		adj <- igraph::as_adjacency_matrix(net, attr = "weight", sparse = F)
		# calculate dependencies
		adj <- apply(adj, 1, function(x){x / sum(x)}) %>% t()
		
		# see which one depends more on each other to establish the direction of the
		# link
		adj_max <- adj
		for(i in 1:nrow(adj)){
			for(j in 1:ncol(adj)){
				adj_max[i,j] <- ton(adj[i,j], adj[j,i], `<=`)
			}
		}
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
															value = igraph::vertex_attr(net, "type"))
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
	
	out <- plyr::dlply(df, "rep", function(d, keep){
		if (keep == "A") ind <- 1
		else if (keep == "B") ind <- 2
		else if (keep == "random") ind <- round(runif(1, min = 1, max = 2))
		x <- igraph::delete_edges(x, d$Vp[ind])
	}, keep)
	
	out$`1`	
}

p <- function(x, ...){
	igraph::plot.igraph(x = x , edge.arrow.size = 0.15, 
											vertex.color = factor(igraph::vertex_attr(x, "type")), ...)
}