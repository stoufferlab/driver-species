CC=g++
CFLAGS=

all: initialise explore analysis

initialise: generate_networks

explore: example_matchings

analysis: redundancy matching

generate_networks: ../results/01_generate_networks.md

example_matchings: ../results/02_example_maximum_matching.md

redundancy: ../data/redundancy/node/* ../results/05_read_redundancy.md

matching: ../data/maximum_matchings/* ../data/node_max-matching.dat

../results/01_generate_networks.md: 01_generate_networks.Rmd ../data/Bartomeus_Ntw_nceas.txt 
	Rscript cpRmd.R $<
	
../results/02_example_maximum_matching.md: 02_example_maximum_matching.Rmd
	Rscript cpRmd.R $<
	
../data/maximum_matchings/*: 03_ntw_maximum_matchings.R _03_ntw_maximum_matchings.sh ../data/networks/*
	bash _03_ntw_maximum_matchings.sh 57

../data/redundancy/node/*: 04_critical_elements.R _04_critical_elements.sh functions
	bash _04_critical_elements.sh 60

../results/05_read_redundancy.md ../data/node_redundancy.dat: 05_read_redundancy.Rmd
	Rscript cpRmd.R $<

../data/node_max-matching.dat: 06_read_max_match.R ../data/maximum_matchings/* 
	Rscript $<