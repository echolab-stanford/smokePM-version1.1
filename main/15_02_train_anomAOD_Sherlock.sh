#!/bin/bash
#
#SBATCH --job-name=train_AOD
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=24
#SBATCH --mem-per-cpu=16GB
#SBATCH --time=72:00:00
#SBATCH --output=train_AOD.log
#SBATCH --mail-type=ALL

# load modules
ml R/4.0.2

# execute script
Rscript scripts/main/15_02_train_AOD_Sherlock.R
