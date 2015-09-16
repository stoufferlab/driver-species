#! /bin/bash

for i in `seq $1`
do
	RScript 04_critical_elements.R $i
done
	