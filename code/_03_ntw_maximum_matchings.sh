#! /bin/bash

for i in `seq $1`
do
	Rscript 03_ntw_maximum_matchings.R $i
done