function [logp, logl] = posterior(envInfo, results, freeParams, fixedParams, priorPDFs)
logl = likelihood(envInfo, results, freeParams, fixedParams);
logp = logl;

whichParams = find(fixedParams == -1);
for k = 1:length(whichParams)
    logp = logp + priorPDFs{whichParams(k)}(freeParams(k));
end

if logp == -Inf, logp = -realmax; end