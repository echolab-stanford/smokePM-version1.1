#!/bin/bash
export IFS=","

cat ./output/smokePM_interpolation/interpolation_tuning_jobs.csv | while read a b; do 

job_file="tune_interp_fold${a}${b}.job"

echo "#!/bin/bash

#SBATCH  -p serc
#SBATCH --job-name=tune_interp_fold${a}${b}
#SBATCH --nodes=1              
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4GB
#SBATCH --time=4:00:00
#SBATCH --mail-type=ALL
#SBATCH --output tune_interp_fold${a}${b}.log


ml physics udunits gdal geos
ml R/4.0.2


Rscript ./scripts/15_10_tune_smokePM_interpolation.R "$a" "$b" " > $job_file

    sbatch $job_file

done



