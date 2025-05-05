#!/bin/bash
#
#SBATCH --job-name=predict_smokePM
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=16
#SBATCH --time=3:00:00
#SBATCH --mem-per-cpu=6G
#SBATCH --output=logs/predict_smokePM.log
#SBATCH --mail-type=ALL

# load modules
ml physics udunits
ml R/4.0.2

# execute script
Rscript scripts/main/17_04_predict_smokePM_Sherlock.R
