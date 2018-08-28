#!/bin/bash
#
#SBATCH -p serial_requeue
#SBATCH -t 0-02:00
#SBATCH --mem 700
#SBATCH -N 1
#SBATCH -c 8
#SBATCH -o /dev/null
#SBATCH -e /dev/null
source new-modules.sh; module load matlab/R2015b-fasrc01

simsName=${1}
fitName=${2}
fixedParams=${3}
numStarts=${4}

priorPDFs="{@(x) log(betapdf(x, 1.2, 1.2)), @(x) log(gampdf(x, 4.82, .88)), @(x) log(gampdf(x, 4.82, .88)), @(x) log(normpdf(x, .15, 1.42)), @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1)), @(x) log(betapdf(x, 1.2, 1.2))}"

dataname="data.mat"
envname="2step_extreme"
outputname="${SLURM_ARRAY_TASK_ID}.txt"

datapath="fitting/$envname/$simsName"
envpath="env"
savepath="fitting/$envname/$simsName/$fitName"

homedir="/users/amorris/Documents/dezfouli/simulations";
scratchdir="/n/regal/cushman_lab/amorris/$SLURM_JOBID/$SLURM_ARRAY_TASK_ID"

if [ ! -f "$homedir/$savepath/$outputname" ]; then

	if [ ! -d "$scratchdir/" ]; then
		mkdir -p "$scratchdir/"
	fi
	if [ ! -d "$homedir/$savepath/" ]; then
		mkdir "$homedir/$savepath/"
	fi

	cp "$homedir/$datapath/$dataname" "$scratchdir/"
	cp "$homedir/$envpath/${envname}.mat" "$scratchdir/"

	srun -n 1 -c 8 matlab-default -nodisplay -nosplash -nodesktop -r "addpath $homedir; addpath '$homedir/utilities'; fitModel('$scratchdir/$dataname', '$scratchdir/${envname}.mat', '$scratchdir/', $fixedParams, $priorPDFs, $SLURM_ARRAY_TASK_ID, $numStarts, true); exit;"

	mv "$scratchdir/$outputname" "$homedir/$savepath/$outputname"
fi