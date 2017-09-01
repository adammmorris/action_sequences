%% fitResults
% For a given dataset, get all the model fitting results, and compare
% models.

envName = '1b_fix';
whichEnv = ['env/' envName '.mat'];
simsName = 'real3';

main = ['fitting/' envName '/' simsName];
datapath = [main '/data.mat'];
realData = true;
numSubjects = 189;

load(whichEnv);
load(datapath);

numChoices = zeros(numSubjects, 1);
LLs_chance = zeros(numSubjects, 1);
for subj = 1:numSubjects
    if subj < length(subjMarkers)
        index = subjMarkers(subj):(subjMarkers(subj + 1) - 1);
    else
        index = subjMarkers(subj):size(results, 1);
    end
    
    numChoices(subj) = length(index) * 2;
    LLs_chance(subj) = log(1 / 2) * length(index) * 2;
end

modelNames_all = {'MB_MB', 'MB_MFMB', 'MFMB_MB', 'MFMB_MFMB', 'MFMB_noAS', 'MF_MB'};
whichParams_all = {1:4, 1:5, [1:4 6], 1:6, 1:5, 1:4};

whichModels = 1:6;

modelNames = modelNames_all(whichModels);
whichParams = whichParams_all(whichModels);

numModels = length(modelNames);
paramEstimates = cell(numModels, 1);
negLLs = zeros(numSubjects, numModels);
goodSubjects = true(numSubjects, 1);
corrs = cell(numModels);

numExtraVars = 3; % [post LL LME]

for i = 1:numModels
    savePath = [main '/fit_' modelNames{i} '/'];
    [paramEstimates{i}, goodSubjects_cur, corrs{i}] = ...
        parseFitOutput(savePath, numSubjects, whichParams{i}, numExtraVars, datapath, realData);
    goodSubjects = goodSubjects & goodSubjects_cur; % drop any subjects we didn't get
end

for i = 1:numModels
    paramEstimates{i}(~goodSubjects, :) = [];
    paramEstimates{i}(paramEstimates{i} == -Inf) = -realmax;
end

[params, details] = generateParamsCell(paramEstimates{:});

%% Model comparison
compareModels_bayes(params, details, 4, numChoices, LLs_chance(goodSubjects));