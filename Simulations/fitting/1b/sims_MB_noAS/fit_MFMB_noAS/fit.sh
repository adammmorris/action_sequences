#!/bin/bash
#
#SBATCH -p ncf
#SBATCH -t 0-01:00
#SBATCH --mem 350

nStarts="20"
fixedParams="[-1 -1 -1 -1 -1 0 0]"

dataname="sims_MB.mat"
envname="env_1b.mat"
savename="params_MB_noAS" # this should reflect what you're assuming in fixedParams
outputname="Params_Subj${SLURM_ARRAY_TASK_ID}.txt"

datapath="fitting/1b/sims_MB"
envpath="env"
savepath="fitting/1b/sims_MB"

homedir="/users/amorris/Documents/Dezfouli/git/Simulations";
scratchdir="/scratch/amorris/$SLURM_JOBID/$SLURM_ARRAY_TASK_ID"

if [ ! -f "$homedir/$datapath/$savename/$outputname" ]; then

	if [ ! -d "$scratchdir/$savename/" ]; then
		mkdir -p "$scratchdir/$savename/"
	fi
	if [ ! -d "$homedir/$datapath/$savename/" ]; then
		mkdir "$homedir/$datapath/$savename/"
	fi

	cp "$homedir/$datapath/$dataname" "$scratchdir/"
	cp "$homedir/$envpath/$envname" "$scratchdir/"

	matlab -nojvm -nodisplay -nosplash -nodesktop -r "addpath $homedir; fitModel('$scratchdir/$dataname', '$scratchdir/$envname', '$scratchdir/$savename/', $nStarts, $SLURM_ARRAY_TASK_ID, $fixedParams); exit;"

	mv "$scratchdir/$savename/$outputname" "$homedir/$datapath/$savename/$outputname"
fi
