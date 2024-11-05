#!/bin/bash
#
#SBATCH --job-name=predict_smokePM
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=16
#SBATCH --time=2-00:00:00
#SBATCH --mem-per-cpu=16G
#SBATCH --output=logs/predict_smokePM.log
#SBATCH --mail-type=ALL

# load modules
ml physics gdal/2.2.1 udunits/2.2.26 proj/4.9.3 geos/3.6.2
ml R/4.0.2

# execute script
Rscript scripts/main/16_04_predict_smokePM_Sherlock.R
