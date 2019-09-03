%% posterior.m
% Computes the posterior for a model by adding in the prior likelihoods.

function [logp, logl] = posterior(envInfo, results, freeParams, fixedParams, priorPDFs)
logl = likelihood(envInfo, results, freeParams, fixedParams);
logp = logl;

whichParams = find(fixedParams == -10);
for k = 1:length(whichParams)
    logp = logp + priorPDFs{whichParams(k)}(freeParams(k));
end

if logp == -Inf, logp = -realmax; end