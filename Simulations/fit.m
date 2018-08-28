clearvars
addpath 'utilities'
datapath = 'fitting/1b_fix/real3/';
priors = {@(x) log(unifpdf(x, 0, 1)), @(x) log(betapdf(x, 1.2, 1.2)), @(x) log(gampdf(x, 4.82, .88)), @(x) log(gampdf(x, 4.82, .88)), @(x) log(normpdf(x, .15, 1.42)), @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1))};
numStarts = 2;
numSubj = 1;

modelNames = {'MB_MB2', 'MFMB_MFMB_rAS'};
modelParams = {[1 -10 -10 -10 -10 1 1], [-10 -10 -10 -10 -10 -10 -10]};

for m = 2:length(modelNames)
    modelName = modelNames{m};
    params = modelParams{m};
    for s = 1:numSubj
        fitModel([datapath 'data.mat'], 'env/1b_fix.mat', [datapath 'fit_' modelName '/'], params, priors, s, numStarts, false);
    end
end
