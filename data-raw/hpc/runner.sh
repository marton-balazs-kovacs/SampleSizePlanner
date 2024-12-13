#!/bin/bash
#SBATCH --job-name=rope_anova16
#SBATCH --output=rope_anova16.out
#SBATCH --error=rope_anova16.err
#SBATCH --cpus-per-task=1   # Adjust to the number of CPUs you need
#SBATCH --mem=5G           # Adjust memory as necessary
#SBATCH --partition=hpc2019  # Choose the appropriate partition

source /users/usumusu/.bashrc

Rscript  /users/usumusu/SampleSizePlanner/data-raw/rope_anova_precalculations.r # Run your R script

