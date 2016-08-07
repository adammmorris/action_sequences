#!/bin/bash
#
#SBATCH -p serial_requeue
#SBATCH -J dez_1b_simsMB_test
#SBATCH -n 1
#SBATCH -N 1
#SBATCH -t 0-00:15
#SBATCH --mem 1000
#SBATCH -o /scratch/amorris/%A/%a/output
#SBATCH -e /scratch/amorris/%A/%a/error
#SBATCH --mail-type=ALL
#SBATCH --mail-user=adammorris@g.harvard.edu

nStarts = '1'
fixedParams = '[-1 -1 -1 -1 -1 0 0]'

dataname = "sims_MB.mat"
envname = "env_1b.mat"
savename = "params"

datapath = "fitting/1b/sims_MB"
envpath = "env"
savepath = "fitting/1b/sims_MB"

envscratch = "/scratch/amorris/$SLURM_JOBID/$SLURM_ARRAY_TASK_ID"
savescratch = "/scratch/amorris/$SLURM_JOBID/$SLURM_ARRAY_TASK_ID/params"

homedir = "/users/amorris/Documents/Dezfouli/git/Simulations";
scratchdir = "/scratch/amorris/$SLURM_JOBID/$SLURM_ARRAY_TASK_ID"

cp "$homedir/$data/$dataname" "$scratchdir/"
cp "$homedir/$env/$envname" "$scratchdir/"

matlab -nodisplay -nosplash -nodesktop -r "addpath $scratchdir; fitModel('$scratchdir/$dataname', '$scratchdir/$envname', '$scratchdir/$savename/', $nStarts, $SLURM_ARRAY_TASK_ID, $fixedParams); exit;"