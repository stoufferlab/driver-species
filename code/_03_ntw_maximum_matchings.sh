#!/bin/bash

#$ -V
#$ -cwd
#$ -j y
#$ -S /bin/bash
#$ -t 1-60
#$ -m a
#$ -M efc29@uclive.ac.nz
#$ -r yes

Rscript --no-save --no-restore 03_ntw_maximum_matchings.R $SGE_TASK_ID 
exit 0