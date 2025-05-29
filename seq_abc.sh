#! /bin/bash
#SBATCH --job-name="aneuploidy_rates_run_sequential_abc"
#SBATCH --partition=parallel
#SBATCH --time 00-60:00:00
#SBATCH --mem=100G
#SBATCH --mail-type=end
#SBATCH --mail-user=qyang40@jhu.edu
#SBATCH --array=1
#SBATCH --account=rmccoy22


# shell script specific to user's directory paths
source ~/.bashrc
conda init --all
conda activate ~/miniconda3/envs/aneuploidy_rates
filepath=$(pwd)

# create folders for storing results, named based on dates
now=$(date +"%Y-%m-%d")
outdir="${now}c"
mkdir -p $outdir
export basedir=${filepath}

echo ${basedir}
echo ${SLURM_ARRAY_TASK_ID}
# arguments: output CSV for full data (including prop.aneu), filtered data CSV
Rscript ${basedir}/sequential_abc.R  "${basedir}/${outdir}/full_data.csv" "${basedir}/${outdir}/data.csv"
