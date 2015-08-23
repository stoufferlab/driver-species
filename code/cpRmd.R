library(knitr)
args <- commandArgs(trailingOnly = TRUE)
setwd("../results/")
knit(paste0("../code/", as.character(args[1])))