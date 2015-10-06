#! /bin/bash

for i in `seq $1`
do
	Rscript 04_critical_elements.R $i
done
	