#! /bin/bash
#SBATCH --job-name="aneuploidy_rates_run"
#SBATCH --partition=defq
#SBATCH --time 00-08:30:00
#SBATCH --mem=100G
#SBATCH --mail-type=end
#SBATCH --mail-user=qyang40@jhu.edu
#SBATCH --array=1-20%10
#SBATCH --account=rmccoy22


# module load r
# conda activate aneuploidy_rates
source ~/.bashrc
conda init --all
conda activate ~/miniconda3/envs/aneuploidy_rates
# cd /home/qyang40/scratch16-rmccoy22/qyang40/aneuploidyRates/
cd ..
filepath=$(pwd)

now=$(date +"%Y-%m-%d")
outdir=$now
mkdir -p data/$outdir

export basedir=${filepath}
export workdir=${filepath}

echo ${basedir}
echo ${SLURM_ARRAY_TASK_ID}
Rscript ${basedir}/data/generate_data.R > $workdir/data/$outdir/${SLURM_ARRAY_TASK_ID}.txt
