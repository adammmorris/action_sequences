#!/bin/bash
#
#SBATCH -p ncf
#SBATCH -t 0-01:00
#SBATCH --mem 275
#SBATCH -o /dev/null
#SBATCH -e /dev/null

nStarts="10"
fixedParams="[-1 -1 -1 -1 -1 -1 1]"

dataname="sims.mat"
envname="env_1b_fix.mat"
savename="params"
outputname="Params_Subj${SLURM_ARRAY_TASK_ID}.txt"

datapath="fitting/1b_fix/sims_MFMB_MFMB"
envpath="env"
savepath="fitting/1b_fix/sims_MFMB_MFMB/fit_MFMB_MFMB"

homedir="/users/amorris/Documents/Dezfouli/git/Simulations";
scratchdir="/scratch/amorris/$SLURM_JOBID/$SLURM_ARRAY_TASK_ID"

if [ ! -f "$homedir/$datapath/$savename/$outputname" ]; then

	if [ ! -d "$scratchdir/$savename/" ]; then
		mkdir -p "$scratchdir/$savename/"
	fi
	if [ ! -d "$homedir/$savepath/$savename/" ]; then
		mkdir "$homedir/$savepath/$savename/"
	fi

	cp "$homedir/$datapath/$dataname" "$scratchdir/"
	cp "$homedir/$envpath/$envname" "$scratchdir/"

	matlab -nojvm -nodisplay -nosplash -nodesktop -r "addpath $homedir; fitModel('$scratchdir/$dataname', '$scratchdir/$envname', '$scratchdir/$savename/', $nStarts, $SLURM_ARRAY_TASK_ID, $fixedParams); exit;"

	mv "$scratchdir/$savename/$outputname" "$homedir/$savepath/$savename/$outputname"
fi
