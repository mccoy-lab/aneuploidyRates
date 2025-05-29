#! /bin/bash
#SBATCH --job-name="aneuploidy_rates_run"
#SBATCH --partition=parallel
#SBATCH --time 00-60:00:00
#SBATCH --mem=100G
#SBATCH --mail-type=end
#SBATCH --mail-user=qyang40@jhu.edu
#SBATCH --array=1-11%11
#SBATCH --account=rmccoy22

# shell script specific to user's directory paths
source ~/.bashrc
conda init --all
conda activate ~/miniconda3/envs/aneuploidy_rates
filepath=$(pwd)

# create folders for storing results, named based on dates
now=$(date +"%Y-%m-%d")
outdir="${now}"
mkdir -p "${outdir}_${SLURM_ARRAY_TASK_ID}"
export basedir=${filepath}

echo ${basedir}
echo ${SLURM_ARRAY_TASK_ID}
# arguments: full data CSV, filtered data CSV, task ID 
Rscript ${basedir}/misdiagnosed_rates.R "${basedir}/${outdir}_${SLURM_ARRAY_TASK_ID}/full_data.csv" "${basedir}/${outdir}_${SLURM_ARRAY_TASK_ID}/data.csv" ${SLURM_ARRAY_TASK_ID}
