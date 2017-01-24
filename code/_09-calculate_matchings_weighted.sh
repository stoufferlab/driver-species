#!/bin/bash

#$ -V
#$ -cwd
#$ -j y
#$ -S /bin/bash
##$ -pe multi_thread ${n_cores} # needs to be specified externally
#$ -m a
#$ -M efc29@uclive.ac.nz
#$ -r yes
#$ -l mem_free=16G,h_vmem=16G

Rscript --no-save --no-restore 09-calculate_matchings_weighted.R $2 $1
exit 0
