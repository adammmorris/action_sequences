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

function fitModel(dataPath, whichEnv, savePath, numStarts, whichSubj, fixedParams)
%% Load data
load(dataPath);

%% Set up
numSubjects = length(subjMarkers);

% whichSubj is from 1 to numSubjects
if (whichSubj < 1 || whichSubj > numSubjects)
    error('whichSubj must be between 1 and numSubjects');
end

if fixedParams(7) == -1
    error('use_AS (the 7th element of fixedParams) must be fixed.');
end

% Do params
% [lr temp1 temp2 stay w_MB w_MB_AS use_AS]
freeParams = fixedParams == -1;
numFreeParams = sum(freeParams);

bounds = [0 0 0 0 0 0 0; 1 2 2 2 1 1 1];
    
% Calculate starts
starts = zeros(numStarts, numFreeParams);
for i = 1:numFreeParams
    starts(:,i) = linspace(bounds(1,i),bounds(2,i),numStarts);
end

optParams = zeros(numStarts, numFreeParams);
lik = zeros(numStarts, 1);

%% Start!
options = psoptimset('CompleteSearch', 'on', 'SearchMethod', {@searchlhs});

load(whichEnv);

for thisStart = 1:numStarts
    if whichSubj < length(subjMarkers)
        index = subjMarkers(whichSubj):(subjMarkers(whichSubj + 1) - 1);
    else
        index = subjMarkers(whichSubj):size(results, 1);
    end
    
    [optParams(thisStart, :), lik(thisStart), ~] = ...
        patternsearch(@(params) likelihood(envInfo, results(index, :), params, fixedParams), ...
        starts(thisStart, :), [], [], [], [], ...
        bounds(1, freeParams), bounds(2, freeParams), options);
end

[~, bestStart] = min(lik);
csvwrite([savePath 'Params_Subj' num2str(whichSubj) '.txt'], [lik(bestStart) optParams(bestStart, :)]);
end