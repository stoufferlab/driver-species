all: prepare_data matching randomisations

n_cores = 4
d_f = ../../data/V2.0/

prepare_data: $(d_f)networks/*

$(d_f)networks/*: 01-prepare_bartomeus_data.R 02-prepare_lopezaraiza_data.R 03-prepare_ballantyne_data.R
	Rscript 01-prepare_bartomeus_data.R; \
	Rscript 02-prepare_lopezaraiza_data.R; \
	Rscript 03-prepare_ballantyne_data.R

matching: $(d_f)n_matched.rds $(d_f)matching_frequency.rds $(d_f)networks/*

$(d_f)n_matched.rds $(d_f)matching_frequency.rds: 04-calculate_matchings.R
	Rscript 04-calculate_matchings.R 4

randomisations: $(d_f)random_weights.rds $(d_f)networks/*

$(d_f)random_weights.rds: 05-random_weights.R
	Rscript $< 4 999