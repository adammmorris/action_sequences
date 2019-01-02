main1 = ['fitting/1b_fix/real3'];
main2 = ['fitting/1b_fix_extreme/real3'];
datapath = [main1 '/data.mat'];
fitpath1 = [main1 '/fit.mat'];
fitpath2 = [main2 '/fit.mat'];

load(datapath);
subjlist = 1:length(subjMarkers);
numSubjects = length(subjlist);

modelNames_all = {'MFMB_MFMB'};
modelParams = {[-10 -10 -10 -10, -10, -10, 1]};
whichParams_all = {1:6};

whichModels = 1;

% first
load(fitpath1);

numChoices = zeros(numSubjects, 1);
LLs_chance = zeros(numSubjects, 1);
for subj_ind = 1:length(subjlist)
    subj = subjlist(subj_ind);
    if subj < length(subjMarkers)
        index = subjMarkers(subj):(subjMarkers(subj + 1) - 1);
    else
        index = subjMarkers(subj):size(results, 1);
    end
    
    numChoices(subj) = length(index) * 2;
    LLs_chance(subj) = log(1 / 2) * length(index) * 2;
end

modelNames = modelNames_all(whichModels);
whichParams = whichParams_all(whichModels);
numModels = length(modelNames);

details = zeros(numSubjects, numModels * 2, 4);
optParams_comb = cell(numModels * 2, 1);
for i = 1:numModels
    model = 5;
    details(:, i, 1) = results{model}(subjlist, 1);
    details(:, i, 2) = results{model}(subjlist, 2);
    details(:, i, 3) = results{model}(subjlist, 3);
    details(:, i, 4) = results{model}(subjlist, 4);
    optParams_comb{1} = optParams{1};
end

% second
load(fitpath2)
for i = 1:numModels
    model = whichModels(i);
    details(:, 1+i, 1) = results(subjlist, 1);
    details(:, 1+i, 2) = results(subjlist, 2);
    details(:, 1+i, 3) = results(subjlist, 3);
    details(:, 1+i, 4) = results(subjlist, 4);
    optParams_comb{2} = optParams;
end

compareModels_bayes(optParams_comb, details, 1, LLs_chance);