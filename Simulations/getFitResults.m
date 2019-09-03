%% fitResults
% For a given dataset, get all the model fitting results, and compare
% models.

%% setup
envName = 'expt1'; % either 'expt1' or 'expt2', depending on which experiment you want
whichEnv = ['env/' envName '.mat'];
simsName = 'data'; % if you want the fits to human data, put 'data'; if you want the fits to a model simulation, put 'sims_<model-name>' (see the folders in 'fitting')

main = ['fitting/' envName '/' simsName];
datapath = [main '/data.mat'];
fitpath = [main '/fit.mat'];

load(datapath);

subjlist = 1:length(subjMarkers);
%subjlist = 1;

numSubjects = length(subjlist);

modelNames_all = {'MFMB_noAS', 'MB_MB', 'MFMB_MB', 'MB_MFMB', 'MFMB_MFMB'};
modelParams = {
    [-10 -10 -10 -10 -10, -10, 0, 0], ...
    [-10 -10 -10 -10 -10, 1, 1, 1], ...
    [-10 -10 -10 -10 -10, -10, 1, 1], ...
    [-10 -10 -10 -10 -10, 1, -10, 1], ...
    [-10 -10 -10 -10 -10, -10, -10, 1]};
whichParams_all = {1:6, 1:5, 1:6, [1:5 7], 1:7};

whichModels = 1:5;

%% extract results
load(fitpath);

numChoices = zeros(numSubjects, 1);
LLs_chance = zeros(numSubjects, 1);
for subj_ind = 1:length(subjlist)
    subj = subjlist(subj_ind);
    if subj < length(subjMarkers)
        index = subjMarkers(subj):(subjMarkers(subj + 1) - 1);
    else
        index = subjMarkers(subj):size(results, 1);
    end
    
    numChoices(subj) = length(index) * 2;
    LLs_chance(subj) = log(1 / 2) * length(index) * 2;
end

modelNames = modelNames_all(whichModels);
whichParams = whichParams_all(whichModels);
numModels = length(modelNames);

details = zeros(numSubjects, numModels, 4);
for i = 1:numModels
    model = whichModels(i);
    details(:, i, 1) = results{model}(subjlist, 1);
    details(:, i, 2) = results{model}(subjlist, 2);
    details(:, i, 3) = results{model}(subjlist, 3);
    details(:, i, 4) = results{model}(subjlist, 4);
    
    optParams{model} = optParams{model}(subjlist,:);
end

%% run comparison
compareModels_bayes(optParams(whichModels), details, 2, LLs_chance);

%% save
csvwrite([main '/params.csv'], optParams{5});