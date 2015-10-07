#!/bin/bash

#$ -V
#$ -cwd
#$ -j y
#$ -S /bin/bash
#$ -t 1-1199
#$ -m a
#$ -M efc29@uclive.ac.nz
#$ -r yes

Rscript --no-save --no-restore 06_read_max_match.R $SGE_TASK_ID 
exit 0