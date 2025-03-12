#!/bin/bash
export IFS=","

cat ./output/smokePM_interpolation/interpolation_tuning_jobs.csv | while read a b c d; do 

job_file="tune_interp_fold${a}${b}_${c}_${d}.job"

echo "#!/bin/bash

#SBATCH  -p serc
#SBATCH --job-name=tune_interp_fold${a}${b}_${c}_${d}
#SBATCH --nodes=1              
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4GB
#SBATCH --time=4:00:00
#SBATCH --mail-type=ALL
#SBATCH --output tune_interp_fold${a}${b}_${c}_${d}.log


ml physics udunits gdal geos
ml R/4.0.2


Rscript ./scripts/16_01_tune_smokePM_interpolation.R "$a" "$b" "$c" "$d" " > $job_file

    sbatch $job_file

done



