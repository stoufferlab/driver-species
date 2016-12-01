# find the times a species is present in the maximum matching set

matched_frequency <- function(n, matching_size, type = "weight", keep = "all", prop = 1, batch = 1000000, simplify = T, weight.type = "max_dep", scale = F, tmpdir = tempdir()){
	
	# transform network
	m <- n %>%
		keep_largest_component() %>%
		bipartite_digraph(type, keep, weight.type, scale) %>% 
		digraph_bipartite()
	
	w <- igraph::E(m)$weight
	
	# if there matching size and weight are not provided calculate them
	if(methods::hasArg(matching_size) & methods::hasArg(matching_weight)) {
	  matching <- list(matching_size = matching_size,
	                   matching_weight = matching_weight)
	} else {
	  matching <- m %>%
	    igraph::max_bipartite_match() 
	}
	
	# precision for computations
	m_prec <- .Machine$double.eps * matching$matching_size * 2
	
	temporary_file <- tempfile(tmpdir = tmpdir)
	message("file is ", temporary_file)
	
	message("finding all possible maximum matchings")
	# find all possible maximum matchings
	matched_links <- m %>%
		igraph::make_line_graph() %>%
		igraph::complementer() %>% 
		igraph::max_cliques(min = matching$matching_size, 
												max = matching$matching_size,
												file = temporary_file)
	
	# given the list of matched links return a vector of driver species (TRUE/FALSE)
	is_matched <- function(x, m){
		m_n <- igraph::ends(m, igraph::E(m)[x + 1])[,2]
		nam <- igraph::V(m)[igraph::V(m)$type]$name
		nam %in% m_n
	}

	# estimate number of lines in file
	onelinefile <- tempfile()
	readLines(temporary_file, 1) %>% write(onelinefile)
	nlines <- file.info(temporary_file)$size %>% 
		magrittr::divide_by(file.info(onelinefile)$size) %>%
		ceiling()
	
	blocks <- seq(1, nlines, batch)
	
	message("reading maximum matching file")
	freq_matching <- 
	  plyr::llply(blocks, function(i){
	    matchings <- scan(temporary_file, 
	                      nlines = batch, 
	                      skip = i-1, 
	                      quiet = T) %>%
	      add(1) %>% 
	      matrix(ncol = matching$matching_size, byrow = TRUE)
	    
	    if(nrow(matchings) == 0) return(NULL)
	    
	    weights <- w[matchings] %>% 
	      matrix(ncol = matching$matching_size)
	    
	    # m_weights_exp <- apply(weights, 1, sum) %>% {exp(.)/sum(exp(.))}

	    l <- lapply(prop, function(pr){
	      mat <- matrix(NA, ncol = matching$matching_size, nrow = nrow(matchings))
	      for(j in 1:nrow(weights)){
	        if(pr * matching$matching_weight - sum(weights[j, ]) <= m_prec){
	          mat[j, ] <- matchings[j, ]
	        } 
	      }
	      mat <- mat[complete.cases(mat), , drop = F]
	      
	      if(nrow(mat) == 0) return(NULL)
	      
	      ends <- igraph::ends(m, igraph::E(m))
	      freq <- mat %>% tabulate(nbins = length(w))
	      o <- list(n_matchings = nrow(mat),
	           matched_vertex = dplyr::data_frame(
	             ve = ends[, 2],
	             vs = ends[, 1],
	             ma = freq,
	             we = freq * w))
	      
	      o$pointing_vertex <- o$matched_vertex  %>%
	        dplyr::group_by(vs) %>%
	        dplyr::summarise(ma = sum(ma),
	                         we = sum(we))
	      
	      o$matched_vertex %<>% 
	        dplyr::group_by(ve) %>%
	        dplyr::summarise(ma = sum(ma),
	                         we = sum(we))
	      
	      return(o)
	      
	    })
		names(l) <- prop
		return(l)
		 
	}, .parallel = T)

	freq_matching %<>% revert_list()
	
	# for each proportion consolidate results
	freq_matching <- lapply(freq_matching, function(f){
	  
	  tot_matched <- f %>%
	    plyr::ldply(function(x) x$matched_vertex) %>%
	    dplyr::group_by(ve) %>%
	    dplyr::summarise(ma = sum(ma),
	                     we = sum(we)) %>%
	    dplyr::full_join(dplyr::data_frame(ve = igraph::V(m)[igraph::V(m)$type]$name),
	                     by = "ve") %>% 
	    dplyr::mutate(ma = replace(ma, is.na(ma), 0),
	                  we = replace(we, is.na(we), 0),
	                  ve = substr(ve, 1, nchar(ve)-3))
	  
	  tot_driver <- f %>%
	    plyr::ldply(function(x) x$pointing_vertex) %>%
	    dplyr::group_by(vs) %>%
	    dplyr::summarise(ma = sum(ma),
	                     we = sum(we)) %>%
	    dplyr::full_join(dplyr::data_frame(vs = igraph::V(m)[!igraph::V(m)$type]$name),
	                     by = "vs") %>% 
	    dplyr::mutate(ma = replace(ma, is.na(ma), 0),
	                  we = replace(we, is.na(we), 0),
	                  vs = substr(vs, 1, nchar(vs)-3))
	  
	  tot_n_matchings <- f %>%
	    plyr::llply(function(x) x$n_matchings) %>% unlist() %>% sum()
	  
	  o <- list(matched = tot_matched,
	       driver = tot_driver)
	  
	  `attr<-`(o, "n_matchings", tot_n_matchings)
	})
	
	if(length(prop) == 1 & simplify) {
	  return(freq_matching[[1]])
	} else {
	  return(freq_matching)
	}
	
}


revert_list <- function(ls) { # @Josh O'Brien
  # get sub-elements in same order
  x <- lapply(ls, `[`, names(ls[[1]]))
  # stack and reslice
  apply(do.call(rbind, x), 2, as.list) 
}

