clearvars
addpath 'utilities'
datapath = 'fitting/1b_fix/real3/';
envpath = 'env/1b_fix.mat';
numStarts = 10;
numSubj = 201;

priors = {@(x) log(betapdf(x, 1.1, 1.1)), @(x) log(gampdf(x, 1.2, 5)), @(x) log(gampdf(x, 1.2, 5)), @(x) log(normpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1))};

modelNames = {'MFMB_noAS', 'MB_MB', 'MFMB_MB', 'MB_MFMB', 'MFMB_MFMB'};
modelParams = {
    [-10 -10 -10 -10, -10, 0, 0], ...
    [-10 -10 -10 -10, 1, 1, 1], ...
    [-10 -10 -10 -10, -10, 1, 1], ...
    [-10 -10 -10 -10, 1, -10, 1], ...
    [-10 -10 -10 -10, -10, -10, 1]};

optParams = cell(length(modelNames), 1);
results = cell(length(modelNames), 1);
parfor m = 1:length(modelNames)
    modelName = modelNames{m};
    params = modelParams{m};
    optParams_subj = zeros(numSubj, sum(params == -10));
    results_subj = zeros(numSubj, 4);
    for s = 1:numSubj
        [optParams_subj(s,:), results_subj(s,:)] = fitModel([datapath 'data.mat'], envpath, [datapath 'fit_' modelName '/'], params, priors, s, numStarts, false);
    end
    
    optParams{m} = optParams_subj;
    results{m} = results_subj;
end

save([datapath 'fit.mat'], 'optParams', 'results');