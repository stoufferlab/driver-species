CC=g++
CFLAGS=

all: initialise

initialise: generate_networks

generate_networks: 01_generate_networks.Rmd
	Rscript cpRmd.R $<

weighted_bipartite_matching: weighted_bipartite_matching.cpp
	$(CC) -o $@ $< -lm -lcblas -lgsl -lgslcblas 

