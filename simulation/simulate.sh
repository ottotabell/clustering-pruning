#!/bin/bash
#SBATCH --job-name=causal_sim
#SBATCH --account=jkarvane  # Replace with your project number
#SBATCH --partition=small          # Or 'test' for short jobs
#SBATCH --time=70:00:00            # Max time: 4 hours (format: HH:MM:SS)
#SBATCH --array=1-30
#SBATCH --ntasks=1
#SBATCH --nodes=1          # Number of cores for parallel processing
#SBATCH --mem-per-cpu=4G           # Memory per CPU
#SBATCH --output=logs/sim_%A_%a.out # %A = job ID, %a = array index
#SBATCH --error=logs/sim_%A_%a.err

# Load R module
module load r-env

if test -f ~/.Renviron; then
sed -i '/TMPDIR/d' ~/.Renviron
fi


# Set number of threads for R
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

# Run your R script
srun Rscript 08_run_simulation.R $SLURM_ARRAY_TASK_ID