#!/bin/bash

#SBATCH --job-name=simulations
#SBATCH --partition=short
#SBATCH --output=logs/sim_output_%A_%a.out  # Redirect standard output to logs directory
#SBATCH --error=logs/sim_error_%A_%a.err    # Redirect standard error to logs directory
#SBATCH --array=1-5                       # Job array of size 100
#SBATCH --cpus-per-task=1                   # Number of CPU cores per job

# Load the R module
module load R/4.3.2-gfbf-2023a  # Adjust according to your cluster's configuration

# List of data generator names
data_generators=("SmallFactors_3x3_Strong" "SmallFactors_3x3_Moderate" "SmallFactors_3x3_Weak" "LargeFactors_5x5_Strong" "LargeFactors_5x5_Moderate" "LargeFactors_5x5_Weak" "Overlap2_Small_Strong" "Overlap2_Small_Moderate" "Overlap2_Small_Weak" "Overlap2_Large_Strong" "Overlap2_Large_Moderate" "Overlap2_Large_Weak" "Overlap3_Small_Strong" "Overlap3_Small_Moderate" "Overlap3_Small_Weak" "Overlap3_Large_Strong" "Overlap3_Large_Moderate" "Overlap3_Large_Weak")

# Number of simulations per data generator
num_simulations=5

# Calculate the data generator and simulation index based on the SLURM array index
total_simulations=$((${#data_generators[@]} * num_simulations))
index=$((SLURM_ARRAY_TASK_ID - 1))
gen_index=$((index / num_simulations))
sim_index=$((index % num_simulations + 1))

# Run the R script with the appropriate arguments
Rscript Scripts/run_single_simulation.R ${data_generators[$gen_index]} $sim_index
