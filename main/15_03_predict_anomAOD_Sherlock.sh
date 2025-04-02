#!/bin/bash
#
#SBATCH --job-name=predict_AOD
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=32
#SBATCH --time=1-00:00:00
#SBATCH --mem-per-cpu=8G
#SBATCH --mail-type=ALL

# load modules
ml physics udunits 
ml R/4.0.2

# execute script
Rscript scripts/main/15_03_predict_anomAOD_Sherlock.R "20060101" "20061231" 4
