categ_node <- function(this, baseline){
	x <- this - baseline
	x[x < 0] <- -1
	x[x > 0] <- 1
	plyr::mapvalues(x, 
									from = c(-1, 0, 1), 
									to = c("redundant", "ordinary", "critical"), 
									warn_missing = FALSE)
}

categ_link <- function(this, baseline){
	x <- this - baseline
	x[x < 0] <- -1
	x[x > 0] <- 1
	plyr::mapvalues(x, 
									from = c(-1, 0, 1), 
									to = c("ordinary", "redundant", "critical"), 
									warn_missing = FALSE)
}

