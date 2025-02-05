#! /bin/bash
#SBATCH --job-name="aneuploidy_rates_run"
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
mkdir -p "${outdir}_${SLURM_ARRAY_TASK_ID}"
export basedir=${filepath}
# export workdir=${filepath}

echo ${basedir}
echo ${SLURM_ARRAY_TASK_ID}
# Rscript ${basedir}/misdiagnosed_rates.R ${SLURM_ARRAY_TASK_ID}> $basedir/$outdir/${SLURM_ARRAY_TASK_ID}.csv
# output_file="${basedir}/${outdir}/${SLURM_ARRAY_TASK_ID}.csv"
Rscript ${basedir}/misdiagnosed_rates.R "${basedir}/${outdir}_${SLURM_ARRAY_TASK_ID}/full_data.csv" "${basedir}/${outdir}_${SLURM_ARRAY_TASK_ID}/data.csv" ${SLURM_ARRAY_TASK_ID}
