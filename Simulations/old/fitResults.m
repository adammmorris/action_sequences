%% fitResults
% For a given dataset, get all the model fitting results, and compare
% models.

numSubjects = 101;
numChoices = 125 * 2; % numRounds x numChoices per round
negLLs_chance = repmat(-1 * log(.5 ^ numChoices), numSubjects, 1);

% savePaths = {'fitting/1b_fix/sims_MFMB_MB/fit_MFMB_MFMB/params/', ...
%     'fitting/1b_fix/sims_MFMB_MB/fit_MFMB_MB/params/', ...
%     'fitting/1b_fix/sims_MFMB_MB/fit_MFMB_MF/params/', ...
%     'fitting/1b_fix/sims_MFMB_MB/fit_MFMB_noAS/params/'};
% numFreeParams = {6, 5, 5, 5};
savePaths = {'fitting/2step/real_v1/fit_MFMB_MFMB/params/', ...
    'fitting/2step/real_v1/fit_MFMB_MB/params/', ...
    'fitting/2step/real_v1/fit_MB_MFMB/params/', ...
    'fitting/2step/real_v1/fit_MB_MB/params/'};
numFreeParams = {6, 5, 5, 4};

numModels = size(savePaths, 2);
paramEstimates = cell(numModels, 1);
negLLs = zeros(numSubjects, numModels);
goodSubjects = true(numSubjects, 1);

for i = 1:numModels
    [paramEstimates{i}, goodSubjects_cur] = parseFitOutput(savePaths{i}, numSubjects, numFreeParams{i});
    negLLs(:, i) = paramEstimates{i}(:, 1);
    paramEstimates{i} = paramEstimates{i}(:, 2:end);
    goodSubjects = goodSubjects & goodSubjects_cur; % drop any subjects we didn't get
end

for i = 1:numModels
    paramEstimates{i} = paramEstimates{i}(goodSubjects, :);
end

compareModels(paramEstimates, negLLs(goodSubjects, :), 1, negLLs_chance(goodSubjects, :), numChoices);