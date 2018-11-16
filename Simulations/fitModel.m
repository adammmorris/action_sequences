%% fitModel
% Calculates the maximum-likelihood parameters (and overall negLL) for a
% model.

%% Inputs
% dataPath: path to a file containing (a) 'results', a (numRounds * numSubjects) x 4,
% which gives (for each subject & round) [A1 S2 A2 Re], and (b)
% 'subjMarkers', a numSubjects-length vector which gives the starting row #
% in results for each subject. Should already have practice rounds filtered out.
% whichEnv: path to environment file with 'envInfo' struct.
% savePath: folder to save results in (should end with /!)
% numStarts: how many starts to do
% fixedParams: which params to fix, and to what values. There are 7
% possible parameters: [lr temp1 temp2 stay w_MB w_MB_AS use_AS].
%   If you want to let a parameter be free, set to -1.
%   Otherwise, set to the value you want to fix it at.
%   Note that use_AS must be fixed. If it is fixed to 0, w_MB_AS should
%   also be fixed (doesn't matter to what value).

function [optParams, results] = fitModel(dataPath, whichEnv, savePath, fixedParams, priorPDFs, whichSubj, numStarts, on_cluster)
%% Load data
load(dataPath);

%% Set up
numSubjects = length(subjMarkers);

% whichSubj is from 1 to numSubjects
if (whichSubj < 1 || whichSubj > numSubjects)
    error('whichSubj must be between 1 and numSubjects');
end

% Do params
% [lr temp1 temp2 stay w_MB w_MB_AS use_AS]
K_PARAM_IND = [false false false false false false true];

freeParams = fixedParams == -10;
freeParams_noK = freeParams;
freeParams_noK(K_PARAM_IND) = false;
nFreeParams = sum(freeParams);
nContFreeParams = sum(freeParams_noK);
bounds = [0 0 0 -5 0 0 0; 1 10 10 5 1 1 1];

% Calculate starts
starts = zeros(numStarts, nContFreeParams);
bounds_fp = bounds(:, freeParams_noK);
for i = 1:nContFreeParams
    ub = bounds_fp(2,i);
    lb = bounds_fp(1,i);
    starts(:,i) = rand(numStarts, 1) * (ub-lb) + lb;
end

%% Start!
load(whichEnv);

if whichSubj < length(subjMarkers)
    index = subjMarkers(whichSubj):(subjMarkers(whichSubj + 1) - 1);
else
    index = subjMarkers(whichSubj):size(results, 1);
end
 
options = optimoptions('fmincon', 'Display', 'off');
options_unc = optimoptions(@fminunc, 'Display', 'Off', 'Algorithm', 'quasi-newton', 'MaxFunEvals', 0);

if fixedParams(K_PARAM_IND) == -10
    krange = bounds(1,K_PARAM_IND):bounds(2,K_PARAM_IND);
    nDiscrete = length(krange);
    logposts = zeros(nDiscrete, 1); % p(data | cont_params) * p(cont_params) * p(discrete_param)
    hessians = cell(nDiscrete, 1);
    optParams_all = zeros(nDiscrete, nContFreeParams);
    
    for nToEval_ind = 1:nDiscrete
        f = @(params) -posterior(envInfo, results(index, :), [krange(nToEval_ind) params], fixedParams, priorPDFs);
        logposts_starts = zeros(numStarts, 1);
        params_starts = zeros(numStarts, nContFreeParams);
        
        for thisStart = 1:numStarts
            [params_starts(thisStart, :), logposts_starts(thisStart), ~, ~, ~, ~] = ...
                fmincon(f, starts(thisStart, :), [], [], [], [], ...
                bounds(1, freeParams_noK), bounds(2, freeParams_noK), [], options);
        end
        
        [~, bestStart] = min(logposts_starts);
        logposts(nToEval_ind) = -logposts_starts(bestStart);
        optParams_all(nToEval_ind, :) = params_starts(bestStart, :);
        
        [~, ~, ~, ~, ~, hessians{nToEval_ind}] = NumHessian(f, optParams_all(nToEval_ind, :));
    end
    
    lme = log((2*pi)^(nContFreeParams / 2) * sum(exp(logposts) .* (cellfun(@det, hessians) .^ (-1/2))));
    [post, optParams_ind] = max(logposts);
    optParams = [krange(optParams_ind) optParams_all(optParams_ind, :)];
else
    f = @(params) -posterior(envInfo, results(index, :), params, fixedParams, priorPDFs);
    
    logposts_starts = zeros(numStarts, 1);
    params_starts = zeros(numStarts, nFreeParams);
    
    for thisStart = 1:numStarts
        [params_starts(thisStart, :), logposts_starts(thisStart)] = ...
            fmincon(f, starts(thisStart, :), [], [], [], [], ...
            bounds(1, freeParams), bounds(2, freeParams), [], options);
    end
    
    [~, bestStart] = min(logposts_starts);
    post = -logposts_starts(bestStart);
    optParams = params_starts(bestStart, :);
  
    %hessian = NumHessian(f, optParams);
    [~, ~, ~, ~, ~, hessian] = fminunc(f, optParams, options_unc);
    lme = nFreeParams / 2 * log(2*pi) + post - .5 * log(det(hessian));
end

ll = likelihood(envInfo, results(index, :), optParams, fixedParams);
bic = nFreeParams * (log(length(index)) - log(2*pi)) - 2 * ll;
if isnan(lme) || ~isreal(lme) || isinf(lme) % resort to BIC
    lme = -0.5 * bic;
end

results = [post, ll, bic, lme];
%csvwrite([savePath num2str(whichSubj) '.txt'], [post, ll, bic, lme, optParams]);

% if on_cluster
%     delete(gcp);
% end