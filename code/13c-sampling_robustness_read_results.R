library(magrittr)

# parameters
opt <- list(
	input_folder = "./data/processed/sampling_robustness/asymmetry/scaled_FALSE",
	baseline_folder = "./data/processed/matching_frequency/asymmetry/scaled_FALSE"
)


# n_d comparison
n_d_sampling <- list.files(opt$input_folder, full.names = T) %>% 
	plyr::ldply(function(x){
		# read sampling characteristics from the filename
		exploded_name <- x %>% tools::file_path_sans_ext() %>% basename() %>% 
			stringr::str_split(pattern = "_") %>% unlist()
		if(length(exploded_name) > 6){
			net_name <- paste(exploded_name[1], exploded_name[2], sep = "_")
		} else{
			net_name <- exploded_name[1]
		}
		all_matchings <- readRDS(x) %>% extract2(1) 
		driver_data <- all_matchings %$% driver
		n_matchings <- attr(all_matchings, 'n_matchings')
		n_d <- driver_data %>% dplyr::mutate(map = ma/n_matchings) %$% sum(map)
		s <- nrow(driver_data)
		dplyr::data_frame(net_name = net_name,
							 version = dplyr::nth(exploded_name, -1),
							 replicate = dplyr::nth(exploded_name, -2),
							 sampling_proportion = as.numeric(dplyr::nth(exploded_name, -4)),
							 N_d = n_d,
							 s = nrow(driver_data)) %>%
			dplyr::mutate(n_d = N_d/s)
	}, .progress = "text", .inform = TRUE)


n_d_baseline <- list.files(opt$baseline_folder, full.names = T) %>%
	plyr::ldply(function(x){
		# read sampling characteristics from the filename
		exploded_name <- x %>% tools::file_path_sans_ext() %>% basename() %>% 
			stringr::str_split(pattern = "_") %>% unlist()
		if(length(exploded_name) > 2){
			net_name <- paste(exploded_name[1], exploded_name[2], sep = "_")
		} else{
			net_name <- exploded_name[1]
		}
		all_matchings <- readRDS(x) %>% extract2(1) 
		driver_data <- all_matchings %$% driver
		n_matchings <- attr(all_matchings, 'n_matchings')
		n_d <- driver_data %>% dplyr::mutate(map = ma/n_matchings) %$% sum(map)
		s <- nrow(driver_data)
		dplyr::data_frame(net_name = net_name,
							 version = dplyr::nth(exploded_name, -1),
							 replicate = as.character(0),
							 sampling_proportion = 1,
							 N_d = n_d,
							 s = nrow(driver_data)) %>%
			dplyr::mutate(n_d = N_d/s)
	}, .progress = "text")

n_d_sampling %<>% dplyr::bind_rows(n_d_baseline)
n_d_baseline %<>%
	dplyr::group_by(net_name, version) %>%
	dplyr::summarise(n_d_base = mean(n_d))
n_d_sampling %<>%
	dplyr::inner_join(n_d_baseline)
	
# f_d and f_s comparison

# f_d = nmatchings - matched$ma
# f_s = driver$ma
f_baseline <- list.files(opt$baseline_folder, full.names = T) %>%
	plyr::ldply(function(x){
		# read sampling characteristics from the filename
		exploded_name <- x %>% tools::file_path_sans_ext() %>% basename() %>% 
			stringr::str_split(pattern = "_") %>% unlist()
		if(length(exploded_name) > 2){
			net_name <- paste(exploded_name[1], exploded_name[2], sep = "_")
		} else{
			net_name <- exploded_name[1]
		}
		matchings_summary <- readRDS(x)
		matchings_summary <- matchings_summary$`0.5`
		n_matchings <- attr(matchings_summary, "n_matchings")
		f_d <- matchings_summary$matched %>% 
			dplyr::select(-we) %>%
			dplyr::mutate(ma = n_matchings - ma,
										frequency = "driver") %>%
			dplyr::rename(species = ve)
		
		f_s <- matchings_summary$driver %>%
			dplyr::select(-we) %>%
			dplyr::mutate(frequency = "superior") %>%
			dplyr::rename(species = vs)
		
		dplyr::bind_rows(f_d, f_s) %>%
			dplyr::mutate(net_name = net_name,
										version = dplyr::nth(exploded_name, -1))
		
	}, .inform = T)

f_sampling <- list.files(opt$input_folder, full.names = T) %>% 
	plyr::ldply(function(x){
		# read sampling characteristics from the filename
		exploded_name <- x %>% tools::file_path_sans_ext() %>% basename() %>% 
			stringr::str_split(pattern = "_") %>% unlist()
		if(length(exploded_name) > 6){
			net_name <- paste(exploded_name[1], exploded_name[2], sep = "_")
		} else{
			net_name <- exploded_name[1]
		}
		matchings_summary <- readRDS(x)
		matchings_summary <- matchings_summary$`0.5`
		n_matchings <- attr(matchings_summary, "n_matchings")
		f_d <- matchings_summary$matched %>% 
			dplyr::select(-we) %>%
			dplyr::mutate(ma = n_matchings - ma,
										frequency = "driver") %>%
			dplyr::rename(species = ve)
		
		f_s <- matchings_summary$driver %>%
			dplyr::select(-we) %>%
			dplyr::mutate(frequency = "superior") %>%
			dplyr::rename(species = vs)
		
		tryCatch({
			dplyr::bind_rows(f_d, f_s) %>%
				dplyr::mutate(net_name = net_name,
											version = dplyr::nth(exploded_name, -1)) %>% 
				dplyr::inner_join(f_baseline, 
													by = c("net_name", "version", "species", "frequency")) %>%
				plyr::ddply("frequency", function(y){
					cor(y$ma.x, y$ma.y, method = "spearman")
				}) %>%
				dplyr::rename(correlation = V1) %>%
				dplyr::mutate(net_name = net_name, 
											version = dplyr::nth(exploded_name, -1), 
											replicate = dplyr::nth(exploded_name, -2),
											sampling_proportion = as.numeric(dplyr::nth(exploded_name, -4)))
		}, error = function(e) NULL)	
	}, .progress = "text", .inform = T)
	

n_d_sampling %>%
	dplyr::inner_join(n_d_baseline) %>% 
	saveRDS("./data/processed/sampling_robustness/n_d_sampling.rds")

f_sampling %>%
	saveRDS("./data/processed/sampling_robustness/f_sampling.rds")
