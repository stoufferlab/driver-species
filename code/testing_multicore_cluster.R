registerDoMC(cores = 4L)


plyr::l_ply(1:4, function(x) {
	Sys.sleep(40)
	name <- paste("core", x, "; task", as.numeric(commandArgs(trailingOnly = T)[1]))
	writeLines(name, paste0("~/core", x, "_task", as.numeric(commandArgs(trailingOnly = T)[1]), ".txt"))
})
