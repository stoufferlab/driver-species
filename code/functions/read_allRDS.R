# function to read all RDS files in a folde3r and put them into a named list.
# The names on the list correspond to the filenames without the extension

read_allRDS <- function(folder){
  o <- folder %>%
    list.files(full.names = T) %>%
    lapply(readRDS)
  n <- list.files(folder) %>%
    stringr::str_split("\\.") %>%
    lapply(`[`, 1) %>%
    unlist()
  names(o) <- n
  return(o)
}