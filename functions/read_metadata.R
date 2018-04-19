#' Read metadata file and rename method column as count
#'
#' @param x metadata file location
#'
#' @return a data frame
#'
read_metadata <- function(x){
	x %>% 
		readr::read_csv() %>% 
		dplyr::tbl_df() %>%
		dplyr::rename(count = method)
}
