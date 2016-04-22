(matchings + 1) %>% matrix(ncol = matching_size, byrow = T) %>%
  apply(1, function(x) sum(igraph::E(m)$weight[x])) %>% stem()

  
  ml <- m %>%
    keep_largest_component() %>%
    bipartite_digraph() %>%
    digraph_bipartite()
  
  ml %>%
    igraph::max_bipartite_match() 
  
  c <- ml %>%
    igraph::make_line_graph() %>%
    igraph::complementer() %>% 
    igraph::max_cliques(min = 6, 
                        max = 6)
  
  lapply(c, function(x) sum(igraph::E(ml)$weight[x])) %>% 
    unlist() %>% hist
  
  profvis(matched_frequency(onet[[15]], prop = 0))
  for(k in 1:length(onet)){
    print(names(onet)[k])
    print(k)
    matched_frequency(onet[[k]], prop = c(1, 0.8, 0.5, 0.2, 0))
  }
  
  
  ### Modified 2
  
  profvis({
    matchings <- scan(temporary_file, 
                      nlines = batch, 
                      skip = i-1, 
                      quiet = T) %>%
      add(1) %>% 
      matrix(ncol = matching$matching_size, byrow = TRUE)
    
    weights <- w[matchings] %>% 
      matrix(ncol = matching$matching_size)
    
    l <- lapply(prop, function(pr){
    mat <- matrix(NA, ncol = matching$matching_size, nrow = nrow(matchings))
    wo <- rep(NA, nrow(matchings))
    for(j in 1:nrow(weights)){
      wo[j] <- sum(weights[j, ])
      if(pr * matching$matching_weight - sum(weights[j, ]) <= .Machine$double.eps * 2){
        mat[j, ] <- matchings[j, ]
      } 
    }
    mat <- mat[complete.cases(mat), ]
      if(is.null(matchings)) return(NULL)
      
      list(n_matchings = nrow(mat),
           matched_vertex = dplyr::data_frame(
             ve = igraph::ends(m, igraph::E(m))[,2],
             ma = (mat) %>% tabulate(nbins = length(w))) %>%
             dplyr::group_by(ve) %>%
             dplyr::summarise(ma = sum(ma)))
      
    })
  })