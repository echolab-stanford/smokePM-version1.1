#!/bin/bash

#SBATCH  -p dev
#SBATCH --job-name=interp_smokePM
#SBATCH --nodes=1              
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4GB
#SBATCH --time=2:00:00
#SBATCH --mail-type=ALL
#SBATCH --output interp_smokePM.log


ml physics udunits gdal geos
ml R/4.0.2

Rscript ./scripts/15_11_interpolate_smokePM_to_grid.R 
