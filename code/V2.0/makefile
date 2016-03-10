all: prepare_data matching randomisations alt_matching properties models

n_cores = 4
d_f = ../../data/V2.0/
n_perm = 999

prepare_data: $(d_f)networks/*

$(d_f)networks/*: 01-prepare_bartomeus_data.R 02-prepare_lopezaraiza_data.R 03-prepare_ballantyne_data.R ../../data/networks_lopezaraiza/* ../../data/Ballantyne-2015-raw_data.csv ../../data/Bartomeus_Ntw_nceas.txt
	Rscript 01-prepare_bartomeus_data.R; \
	Rscript 02-prepare_lopezaraiza_data.R; \
	Rscript 03-prepare_ballantyne_data.R

matching: dependency_matching ../../data/ntw_info.csv $(d_f)networks/*

dependency_matching: $(d_f)n_matched.rds $(d_f)matching_frequency.rds $(d_f)networks/*

bidirectional_matching: $(d_f)n_matched_bi.rds $(d_f)matching_frequency_bi.rds $(d_f)networks/*

$(d_f)n_matched.rds $(d_f)matching_frequency.rds: 04-calculate_matchings.R $(d_f)networks/*
	Rscript 04-calculate_matchings.R $(n_cores)

$(d_f)n_matched_bi.rds $(d_f)matching_frequency_bi.rds: 09-calculate_matchings_weighted.R
	Rscript 09-calculate_matchings_weighted.R $(n_cores)

randomisations: $(d_f)random_weights.rds $(d_f)random_directions.rds $(d_f)random_weights_bi.rds $(d_f)networks/*

$(d_f)random_weights.rds: 05-random_weights.R
	Rscript $< $(n_cores) $(n_perm)

$(d_f)random_weights_bi.rds: 11-random_weights_bi.R
	Rscript $< $(n_cores) $(n_perm)

$(d_f)random_directions.rds: 06-random_directions.R
	Rscript $< $(n_cores) $(n_perm)

alt_matching: $(d_f)n_matched_all_types.rds $(d_f)networks/*

$(d_f)n_matched_all_types.rds: 07-alternative_matchings.R
	Rscript $<

properties: $(d_f)species_properties.rds

$(d_f)species_properties.rds: 08-species_properties.R
	Rscript $< $(n_cores) $(n_perm)

models: $(d_f)species_models.rds

$(d_f)species_models.rds: 10-species_models.R
	Rscript $<
