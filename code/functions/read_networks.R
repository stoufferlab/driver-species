read_networks <- function(x) {
	# x folder with the networks
	
	net <- x %>%
		list.files(full.names = T) %>%
		lapply(readRDS)
	
	names(net) <- x %>%
		list.files(full.names = F) %>%
		substr(1, nchar(.)-4)
	
	return(net)
}