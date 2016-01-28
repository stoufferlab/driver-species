# find the times a species is present in the maximum matching set

matched_frequency <- function(n, matching_size, type = "weight", keep = "all", batch = 10000){
	
	# transform network
	m <- n %>%
		keep_largest_component() %>%
		bipartite_digraph(type, keep) %>%
		digraph_bipartite()
	
	temporary_file <- tempfile()
	
	# find all possible maximum matchings
	matched_links <- m %>%
		igraph::make_line_graph() %>%
		igraph::complementer() %>% 
		igraph::max_cliques(min = matching_size, 
												max = matching_size,
												file = temporary_file)
	
	# given the list of matched links return a vector of driver species (TRUE/FALSE)
	is_matched <- function(x, net){
		m_n <- igraph::ends(m, igraph::E(net)[x + 1])[,2]
		nam <- igraph::V(net)[igraph::V(net)$type]$name
		!(nam %in% m_n)
	}

	i <- 0
	tot_n_matchings <- 0
	freq_matching <- NULL
	
	repeat{
		matchings <- scan(temporary_file, 
											nlines = batch, 
											skip = i * batch, 
											quiet = F)
		i <- i + 1
		n_matchings <- length(matchings) / matching_size
		if (n_matchings == 0) break

		this_freq_matchings <- matchings %>% 
			matrix(ncol = matching_size, byrow = T) %>%
			plyr::aaply(1, is_matched, net = m) %>%
			plyr::aaply(2, sum)
		
		freq_matching %<>%
			rbind(this_freq_matchings)
		
		tot_n_matchings %<>% add(n_matchings)
	}
	
	freq_matching %>%
		plyr::aaply(2, sum) %>% 
		magrittr:set_names(igraph::V(m)[igraph::V(m)$type]$name %>% substr(1, nchar(.)-3))
}


