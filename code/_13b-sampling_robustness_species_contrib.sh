#!/bin/bash

#$ -V
#$ -cwd
#$ -j y
#$ -S /bin/bash
#$ -t 1-12024
##$ -tc 24
#$ -pe multi_thread 8
#$ -m a
#$ -M efc29@uclive.ac.nz
#$ -r yes
#$ -l mem_free=16G,h_vmem=16G

Rscript --no-save --no-restore 13b-sampling_robustness_species_contrib.R $SGE_TASK_ID $1 $2 $3
exit 0
