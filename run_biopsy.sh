#!/bin/bash
#SBATCH --job-name="embryo_biopsy_sim"
#SBATCH --partition=parallel
#SBATCH --time=00-12:00:00
#SBATCH --mem=64G
#SBATCH --mail-type=end
#SBATCH --mail-user=qyang40@jhu.edu
#SBATCH --array=1-60%20  
#SBATCH --account=rmccoy22

# shell script specific to user's directory paths and environments
source ~/.bashrc
conda activate ~/miniconda3/envs/aneuploidy_rates

# Set working directory and filenames based on date
basedir=$(pwd)
now=$(date +"%Y-%m-%d")
outdir="${now}_results"
mkdir -p "${outdir}"

# Determine group ID and size
TASK_ID=${SLURM_ARRAY_TASK_ID}
GROUP_SIZE=50000
START_ROW=$(( (TASK_ID - 1) * GROUP_SIZE + 1 ))
END_ROW=$(( TASK_ID * GROUP_SIZE ))

# Run R script with bounds and ID
Rscript ${basedir}/biopsy_simulation.R "$START_ROW" "$END_ROW" "$TASK_ID" "$outdir"