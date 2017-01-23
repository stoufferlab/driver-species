#!/bin/bash

#$ -V
#$ -cwd
#$ -j y
#$ -S /bin/bash
#$ -pe multi_thread 32
#$ -m a
#$ -M efc29@uclive.ac.nz
#$ -r yes
#$ -l mem_free=16G,h_vmem=16G

Rscript --no-save --no-restore 09-calculate_matchings_weighted.R 32 $1
exit 0
