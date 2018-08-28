% Procedure for model selection using individual MAP parameter estimates

% params is a cell of length numModels, where params{i} is a numSubjects x
%   numParams for model i
% details is a numSubjects x numModels x 4, giving [posterior likelihood bics LME]
% The models must be in the same order, because:
% preferredModel tells us which (in order) of those models is the preferred
%   one
% nChoices, a numSubjects x 1 matrix, gives the number of choices each
% subject made (that contributed to the likelihood)
% LLs_chance: log-likelihoods under chance

function [pseudoR2s, paramPercentiles, globalLRtests, indivLRtests, AICs, BICs, BMS] = compareModels_bayes(params, details, preferredModel, LLs_chance)

% Get # of subjects, models, & parameters per model
numModels = length(params);
if size(details, 2) ~= numModels
    error('details must have length(params) columns');
end

posts = details(:, :, 1);
LLs = details(:, :, 2);
BICs = details(:, :, 3);
LMEs = details(:, :, 4);

numSubjects = size(LLs, 1);
numParams = zeros(1,numModels);
for thisModel = 1:numModels
    numParams(thisModel) = size(params{thisModel},2);
    if size(params{thisModel},1) ~= numSubjects
        error('Each parameter matrix must have size(negLLs,1) rows');
    end
end

% Calculate the pseudo-R^s for each subject
pseudoR2s = 1 - (LLs ./ repmat(LLs_chance, 1, numModels)); % McFadden


% For each model, calculate the 25th, 50th, and 75th percentiles of each
%   parameter, of the LLs (not the neg LLs)
paramPercentiles = cell(numModels, 1);
for thisModel = 1:numModels
    paramPercentiles{thisModel} = zeros(3, numParams(thisModel)+2);
    for thisParam = 1:numParams(thisModel)
        paramPercentiles{thisModel}(:,thisParam) = prctile(params{thisModel}(:,thisParam),[25 50 75])';
    end
    paramPercentiles{thisModel}(:,end-1) = prctile(LLs(:,thisModel),[25 50 75])';
    paramPercentiles{thisModel}(:,end) = prctile(pseudoR2s(:,thisModel),[25 50 75])';
end

% Get population-level negLLs
LLs_global = sum(LLs, 1);

% Compare our preferred model to other models using global likelihood ratio
%   tests
% All 1-tailed
globalLRtests = zeros(3,numModels); % top row is the test statistic, second is the df, bottom is the p-value

globalLRtests(1,:) = 2*(LLs_global - LLs_global(preferredModel));
globalLRtests(2,:) = numParams(preferredModel)*numSubjects - numParams.*numSubjects;
globalLRtests(3,:) = 1-chi2cdf(globalLRtests(1,:), globalLRtests(2,:));

% Do likelihood ratio tests for each subject comparing each model to our
%   preferred model
% Outputs p values
indivLRtests = zeros(numSubjects,numModels); % p values

for thisSubj = 1:numSubjects
    x = 2*(LLs(thisSubj,:) - LLs(thisSubj,preferredModel));
    v = numParams(preferredModel) - numParams;
    indivLRtests(thisSubj,:) = 1-chi2cdf(x,v);
end

% Get global AICs
AICs = 2*(numParams)*numSubjects - 2*(LLs_global);
BICs = sum(BICs, 1);

% Do Bayesian model selection
BMS = cell(3,1); % first is model probabilities, second is expected posterior, third is protected exceedance probabilities
[BMS{1}, BMS{2}, ~, BMS{3}] = bms(LMEs);

% Display tables

% Table #1: parameter percentiles for preferred model
format;
str = '    Param1';
for i = 2:numParams(preferredModel)
    str = [str '    Param' num2str(i)];
end
str = [str '    LLs'];
str = [str '    pseudo-R^2s'];
disp(str)
disp(paramPercentiles{preferredModel});

disp(''); % newline

% Table #2: for each model..
format short g;
disp('       post       LL       globalLR       indivLR       AICs       BICs      ModelProbs     ExceedanceProbs');
disp([sum(posts, 1)' LLs_global' sum(indivLRtests < .05, 1)' AICs' BICs' BMS{2}' BMS{3}']);
end