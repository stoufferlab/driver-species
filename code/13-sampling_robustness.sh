#!/bin/bash

#$ -V
#$ -cwd
#$ -j y
#$ -S /bin/bash
#$ -t 1-501
#$ -pe multi_thread 8
#$ -m a
#$ -M efc29@uclive.ac.nz
#$ -r yes
#$ -l mem_free=16G,h_vmem=16G

Rscript --no-save --no-restore 13-sampling_robustness.R $SGE_TASK_ID 
exit 0