#!/usr/bin/env bash
#SBATCH --ntasks=35
#SBATCH --cpus-per-task=1
#SBATCH -t 24:00:00
#SBATCH -J endorse
#SBATCH -o log.%j
#SBATCH --mail-type=begin
#SBATCH --mail-type=end

echo '-------------------------------'
cd ${SLURM_SUBMIT_DIR}
echo ${SLURM_SUBMIT_DIR}
echo Running on host $(hostname)
echo Time is $(date)
echo SLURM_NODES are $(echo ${SLURM_NODELIST})
echo '-------------------------------'
echo -e '\n\n'

srun Rscript endorse-comp-iters.R 35

