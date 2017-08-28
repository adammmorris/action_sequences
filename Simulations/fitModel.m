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

function fitModel(dataPath, whichEnv, savePath, fixedParams, priorPDFs, whichSubj, numStarts, on_cluster)
%% Load data
load(dataPath);

%% Set up
numSubjects = length(subjMarkers);

% whichSubj is from 1 to numSubjects
if (whichSubj < 1 || whichSubj > numSubjects)
    error('whichSubj must be between 1 and numSubjects');
end

if fixedParams(7) == -10
    error('use_AS (the 7th element of fixedParams) must be fixed.');
end

% Do params
% [lr temp1 temp2 stay w_MB w_MB_AS use_AS lr_trans lr_miller]
freeParams = fixedParams == -10;
numFreeParams = sum(freeParams);

bounds = [0 0 0 -5 0 0 0 0 0; 1 10 10 5 1 1 1 1 1];
    
% Calculate starts
starts = zeros(numStarts, numFreeParams);
bounds_fp = bounds(:, freeParams);
for i = 1:numFreeParams
    ub = bounds_fp(2,i);
    lb = bounds_fp(1,i);
    starts(:,i) = rand(numStarts, 1) * (ub-lb) + lb;
end

%% Start!
load(whichEnv);

%options = optimoptions(@fmincon, 'Display', 'off', 'UseParallel', false);
options_unc = optimoptions(@fminunc, 'Display', 'Off', 'Algorithm', 'quasi-newton', 'MaxFunEvals', 0);

if whichSubj < length(subjMarkers)
    index = subjMarkers(whichSubj):(subjMarkers(whichSubj + 1) - 1);
else
    index = subjMarkers(whichSubj):size(results, 1);
end

f = @(params) -posterior(envInfo, results(index, :), params, fixedParams, priorPDFs);

logposts_starts = zeros(numStarts, 1);
params_starts = zeros(numStarts, numFreeParams);

parfor thisStart = 1:numStarts
    [params_starts(thisStart, :), logposts_starts(thisStart)] = ...
        fmincon(f, starts(thisStart, :), [], [], [], [], ...
        bounds(1, freeParams), bounds(2, freeParams), []);
end

[~, bestStart] = min(logposts_starts);
post = -logposts_starts(bestStart);
optParams = params_starts(bestStart, :);

[~, ~, ~, ~, ~, hessian] = fminunc(f, optParams, options_unc);
lme = numFreeParams / 2 * log(2*pi) + post - .5 * log(det(hessian));

ll = likelihood(envInfo, results(index, :), optParams, fixedParams);
if isnan(lme) || ~isreal(lme) || isinf(lme) % resort to BIC
    lme = -0.5 * (numFreeParams * (log(length(index) * 2) - log(2*pi)) - 2 * ll);
end

csvwrite([savePath num2str(whichSubj) '.txt'], [post, ll, lme, optParams]);

if on_cluster
    delete(gcp);
end