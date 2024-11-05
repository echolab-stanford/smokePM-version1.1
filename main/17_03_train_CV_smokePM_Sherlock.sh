#!/bin/bash
export IFS=","

cat $GROUP_HOME/smokePM_prediction/output/version1.1/smokePM/smokePM_training_jobs.csv | while read a b c; do 

job_file="train_smokePM_fold${a}_drop${b}_end${c}.job"

echo "#!/bin/bash

#SBATCH  -p serc
#SBATCH --job-name=train_smokePM_fold${a}_drop${b}_end${c}
#SBATCH --nodes=1              
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=16GB
#SBATCH --time=7-00:00:00
#SBATCH --mail-type=ALL
#SBATCH --output=logs/train_smokePM_fold${a}_drop${b}_end${c}.log

ml R/4.0.2

Rscript scripts/main/16_03_train_smokePM_Sherlock.R "$a" "$b" "$c" " > $job_file

    sbatch $job_file

done



