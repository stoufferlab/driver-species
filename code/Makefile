CC=g++
CFLAGS=

all: initialise explore analysis

initialise: generate_networks

explore: example_matchings

analysis: redundancy

generate_networks: ../results/01_generate_networks.md

example_matchings: ../results/02_example_maximum_matching.md

redundancy: ../data/redundancy/edge/* ../results/05_read_redundancy.md

../results/01_generate_networks.md: 01_generate_networks.Rmd ../data/Bartomeus_Ntw_nceas.txt 
	Rscript cpRmd.R $<
	
../results/02_example_maximum_matching.md: 02_example_maximum_matching.Rmd
	Rscript cpRmd.R $<

../data/redundancy/edge/*: 04_critical_elements.R _04_critical_elements.sh functions
	bash _04_critical_elements.sh 60

../results/05_read_redundancy.md ../data/node_redundancy.dat: 05_read_redundancy.Rmd
	Rscript cpRmd.R $<
