#! /bin/bash
#SBATCH --job-name="aneuploidy_rates_run"
#SBATCH --partition=defq
#SBATCH --time 00-01:30:00
#SBATCH --mem=100G
#SBATCH --mail-type=end
#SBATCH --mail-user=qyang40@jhu.edu
#SBATCH --array=1-10%5
#SBATCH --account=rmccoy22


module load r
# cd /home/qyang40/scratch16-rmccoy22/qyang40/aneuploidyRates/
cd ..
filepath=$(pwd)

export basedir=${filepath}
export workdir=${filepath}

echo ${basedir}
echo ${SLURM_ARRAY_TASK_ID}
Rscript ${basedir}/R/find_rates.R > $workdir/data/${SLURM_ARRAY_TASK_ID}.txt