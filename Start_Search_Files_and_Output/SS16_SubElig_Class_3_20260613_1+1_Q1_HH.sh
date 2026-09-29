#!/bin/bash

 
#SBATCH --job-name=SS16_SubElig_Class_3_20260613_1+1_Q1_HH

#SBATCH --output=out_SS16_SubElig_Class_3_20260613_1+1_Q1_HH

#SBATCH --error=err_SS16_SubElig_Class_3_20260613_1+1_Q1_HH

#SBATCH --mem=120g

#SBATCH -n 1

#SBATCH --cpus-per-task=1

#SBATCH -t 3:00:00

#SBATCH --mail-type=all

#SBATCH --mail-user=saumitra@ad.unc.edu

 
module add r/4.4.0

Rscript SS16_SubElig_Class_3_20260613_1+1_Q1_HH.r


