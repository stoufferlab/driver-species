#!/bin/bash

# Redirect the STDOUT and STDERR files to the ~/jobs directory
#$ -o /home/klw83/jobs/outputs/
#$ -e /home/klw83/jobs/errors/

# This script, ladies and gentlemen, is in bash
#$ -S /bin/bash

# Operate in the current directory
#$ -cwd

## End SGE Settings
################################

network=$1
n_links=$2
target_links=$3

cat data/$network | ./code/a.out $n_links $target_links > data/$file_name


