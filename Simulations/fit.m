clearvars
addpath 'utilities'
datapath = 'fitting/2step/real1/';
envpath = 'env/2step.mat';
numStarts = 10;
numSubj = 1;

priors = {
    @(x) log(unifpdf(x, 0, 1)), ...
    @(x) log(unifpdf(x, 0, 10)), ...
    @(x) log(unifpdf(x, 0, 10)), ...
    @(x) log(unifpdf(x, -5, 5)), ...
    @(x) log(unifpdf(x, 0, 1)), ...
    @(x) log(unifpdf(x, 0, 1)), ...
    @(x) log(unifpdf(x, 0, 1))};


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
    for s = 1:numSubj
        [optParams{m}, results{m}] = fitModel([datapath 'data.mat'], envpath, [datapath 'fit_' modelName '/'], params, priors, s, numStarts, false);
    end
end

save([datapath 'fit.mat'], 'optParams', 'results');