#!/bin/bash

#$ -V
#$ -cwd
#$ -j y
#$ -S /bin/bash
#$ -t 1-4
#$ -pe multi_thread 8
#$ -m a
#$ -M efc29@uclive.ac.nz
#$ -r yes
#$ -l mem_free=8G,h_vmem=8G

Rscript --no-save --no-restore testing_multicore_cluster.R $SGE_TASK_ID 
exit 0