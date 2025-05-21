#!/bin/bash
#
#SBATCH --job-name=extract_pop
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=32GB
#SBATCH --time=4:00:00
#SBATCH --output=extract_pop_tract.log
#SBATCH --mail-type=ALL

# load modules
ml physics gdal udunits proj geos
ml R/4.2.0

# execute script
Rscript /scratch/users/mmarti04/smokePM-prediction/scripts/main/extract_tract_pop_from_census.R