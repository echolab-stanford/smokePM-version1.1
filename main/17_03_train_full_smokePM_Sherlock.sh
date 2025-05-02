#!/bin/bash

#SBATCH  -p serc
#SBATCH --job-name=train_smokePM_full
#SBATCH --nodes=1              
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=16GB
#SBATCH --time=7-00:00:00
#SBATCH --mail-type=ALL
#SBATCH --output=logs/train_smokePM_full.log

ml R/4.0.2

Rscript scripts/main/17_03_train_smokePM_Sherlock.R 99 "aod_anom_pred" "2024-12-31" "smokePM_full_model_training.rds"
