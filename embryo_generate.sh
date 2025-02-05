#! /bin/bash
#SBATCH --job-name="aneuploidy_rates_generate_misclassification"
#SBATCH --partition=parallel
#SBATCH --time 00-60:00:00
#SBATCH --mem=100G
#SBATCH --mail-type=end
#SBATCH --mail-user=qyang40@jhu.edu
#SBATCH --array=1-11%11
#SBATCH --account=rmccoy22


# module load r
# conda activate aneuploidy_rates
source ~/.bashrc
conda init --all
conda activate ~/miniconda3/envs/aneuploidy_rates
# cd /home/qyang40/scratch16-rmccoy22/qyang40/aneuploidyRates/
# cd ..
filepath=$(pwd)

now=$(date +"%Y-%m-%d")
outdir="${now}"
mkdir -p $outdir

export basedir=${filepath}
# export workdir=${filepath}

echo ${basedir}
echo ${SLURM_ARRAY_TASK_ID}
Rscript ${basedir}/embryo_generate.R  ${SLURM_ARRAY_TASK_ID} ${basedir} "${basedir}/${outdir}/data_${SLURM_ARRAY_TASK_ID}.csv"
