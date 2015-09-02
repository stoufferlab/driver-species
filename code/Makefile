CC=g++
CFLAGS=

all: initialise explore

initialise: generate_networks

explore: example_matchings

example_matchings: 02_example_maximum_matching.Rmd 
	Rscript cpRmd.R $<

generate_networks: 01_generate_networks.Rmd
	Rscript cpRmd.R $<

