library(doMC)
registerDoMC(cores = 8L)


plyr::l_ply(1:8, function(x) {
	Sys.sleep(40)
	name <- paste("core", x, "; task", as.numeric(commandArgs(trailingOnly = T)[1]))
	writeLines(name, paste0("~/core", x, "_task", as.numeric(commandArgs(trailingOnly = T)[1]), ".txt"))
}, .parallel = TRUE)
