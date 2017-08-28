datapath = 'fitting/1b_fix/real2/';
modelName = 'MB_MB';
priors = {@(x) log(betapdf(x, 1.2, 1.2)), @(x) log(gampdf(x, 4.82, .88)), @(x) log(gampdf(x, 4.82, .88)), @(x) log(normpdf(x, .15, 1.42)), @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1)), @(x) log(betapdf(x, 1.2, 1.2)), @(x) log(betapdf(x, 1.2, 1.2))};
numStarts = 10;
subject = 1;

params = [-10 -10 -10 -10 1 1 1 0 0];
fitModel([datapath 'data.mat'], 'env/1b_fix.mat', [datapath 'fit_' modelName], params, priors, subject, numStarts, false);