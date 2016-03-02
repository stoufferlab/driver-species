# find the times a species is present in the maximum matching set

matched_frequency <- function(n, matching_size, type = "weight", keep = "all", batch = 1000000){
	
	# transform network
	m <- n %>%
		keep_largest_component() %>%
		bipartite_digraph(type, keep) %>%
		digraph_bipartite()
	
	temporary_file <- tempfile()
	message("file is ", temporary_file)
	
	message("finding all possible maximum matchings")
	# find all possible maximum matchings
	matched_links <- m %>%
		igraph::make_line_graph() %>%
		igraph::complementer() %>% 
		igraph::max_cliques(min = matching_size, 
												max = matching_size,
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
	nlines <- file.size(temporary_file) %>% 
		magrittr::divide_by(file.size(onelinefile)) %>%
		ceiling()
	
	blocks <- seq(1, nlines, batch)
	
	message("reading maximum matching file")
	freq_matching <- 
		plyr::llply(blocks, function(i){
		
		matchings <- scan(temporary_file, 
											nlines = batch, 
											skip = i-1, 
											quiet = T)

		list(n_matchings = length(matchings) / matching_size,
				 matched_vertex = dplyr::data_frame(
				 	ve = igraph::ends(m, igraph::E(m))[,2],
				 	ma = (matchings + 1) %>% tabulate(nbins = length(igraph::E(m)))) %>%
				 	dplyr::group_by(ve) %>%
				 	dplyr::summarise(ma = sum(ma)))
		
	}, .parallel = T)

	#new
	tot_matched <- freq_matching %>%
		plyr::ldply(function(x) x$matched_vertex) %>%
		dplyr::group_by(ve) %>%
		dplyr::summarise(ma = sum(ma)) %>%
		dplyr::full_join(dplyr::data_frame(ve = igraph::V(m)[igraph::V(m)$type]$name),
										 by = "ve") %>% 
		dplyr::mutate(ma = replace(ma, is.na(ma), 0),
									ve = substr(ve, 1, nchar(ve)-3)) %$% {
										as.vector(ma) %>%
											magrittr::set_names(ve)
									}
	
	tot_n_matchings <- freq_matching %>%
		plyr::laply(function(x) x$n_matchings) %>% sum()
	
	`attr<-`(tot_matched, "n_matchings", tot_n_matchings)
	
}


