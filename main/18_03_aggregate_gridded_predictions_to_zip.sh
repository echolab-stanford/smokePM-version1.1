#!/bin/bash
#
#SBATCH --job-name=aggregate_zip
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=32GB
#SBATCH --time=8:00:00
#SBATCH --output=aggregate_zip.log
#SBATCH --mail-type=ALL

# load modules
ml physics gdal udunits proj geos
ml R/4.2.0

# execute script
Rscript /scratch/users/mmarti04/smokePM-prediction/scripts/main/18_03_aggregate_gridded_predictions_to_zip.R