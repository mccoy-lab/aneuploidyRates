#! /bin/bash
#SBATCH --job-name="aneuploidy_rates_run"
#SBATCH --partition=parallel
#SBATCH --time 00-08:30:00
#SBATCH --mem=100G
#SBATCH --mail-type=end
#SBATCH --mail-user=qyang40@jhu.edu
#SBATCH --array=1-100%10
#SBATCH --account=rmccoy22


# module load r
source ~/.bashrc
conda init --all
conda activate ~/miniconda3/envs/aneuploidy_rates
filepath=$(pwd)

now=$(date +"%Y-%m-%d")
outdir=$now
mkdir -p $outdir

export basedir=${filepath}

echo ${basedir}
echo ${SLURM_ARRAY_TASK_ID}
Rscript ${basedir}/generate_data.R > $basedir/$outdir/${SLURM_ARRAY_TASK_ID}.txt
