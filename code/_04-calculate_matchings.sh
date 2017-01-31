#!/bin/bash

#$ -V
#$ -cwd
#$ -j y
#$ -S /bin/bash
##$ -pe multi_thread 47 # needs to be specified externally
#$ -m a
#$ -t 1-24
#$ -M efc29@uclive.ac.nz
#$ -r yes
#$ -l mem_free=16G,h_vmem=16G

Rscript --no-save --no-restore 04-calculate_matchings.R $SGE_TASK_ID $1 $2
exit 0
