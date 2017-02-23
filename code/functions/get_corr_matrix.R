
get_corr_matrix <- function(x, column, thresholds = seq(0, 1, 0.1)) {
	correlation_matrix <- matrix(0, 
															 ncol = length(thresholds),
															 nrow = length(thresholds),
															 dimnames = list(thresholds, thresholds))
	# print(head(x))
	for (i in 1:nrow(correlation_matrix)){
		for(j in 1:ncol(correlation_matrix)){
			t1 <- rownames(correlation_matrix)[i]
			t2 <- colnames(correlation_matrix)[j]
			# message("i is", i, " / j is ", j)
			# print(as.numeric(c(t1, t2)))

			y <- x %>%
				dplyr::filter(threshold %in% as.numeric(c(t1, t2)))
			
			if(dplyr::n_distinct(y$threshold) <= 1) {
				correlation_matrix[i,j] <- NA
			} else {
				correlation_matrix[i,j] <- y %>% 
					reshape2::dcast(species ~ threshold, value.var = column) %>%
					{cor(.[, 2], .[,3], method = "spearman")} 
			}
		}
	}
	correlation_matrix
}
