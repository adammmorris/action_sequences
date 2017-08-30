clearvars
addpath 'utilities'
datapath = 'fitting/1b_fix/real2/';
priors = {@(x) log(betapdf(x, 1.2, 1.2)), @(x) log(gampdf(x, 4.82, .88)), @(x) log(gampdf(x, 4.82, .88)), @(x) log(normpdf(x, .15, 1.42)), @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1)), @(x) log(betapdf(x, 1.2, 1.2))};
numStarts = 10;
numSubj = 201;

modelNames = {'MB_MB', 'MB_MFMB', 'MFMB_MB', 'MFMB_MFMB', 'MFMB_noAS'};
modelParams = {[1 1 1 0], [1 -10 1 0], [-10 1 1 0], [-10 -10 1 0], [-10 0 0 0]};

for m = 1:length(modelNames)
    modelName = modelNames{m};
    params = modelParams{m};
    parfor s = 1:numSubj
        fitModel([datapath 'data.mat'], 'env/1b_fix.mat', [datapath 'fit_' modelName '/'], [-10 -10 -10 -10 params], priors, s, numStarts, false);
    end
end
