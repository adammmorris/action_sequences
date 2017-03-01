%% Parameters
numAgents = 200;
numRounds = 175;
MF_generalizes = 0;
whichEnv = 'env/env_1b_fix.mat';
%whichModel = 'fitting/1b_fix/sims_MFMB_MFMB/sims.mat';
whichModel = 'data/1b_fix/sims_MFMB_MF.csv';
debug = 0;

% Set up their parameters
actualParams = zeros(numAgents, 8); % [lr1, lr2, elig, temp1, temp2, stay, weight_MBAS, weight_MB, MF_S2_smart]
for thisSubj = 1:numAgents
    lr = rand();
    temp1 = 1 + randn() * .35;
    temp2 = 1 + randn() * .35;
    stay = 1 + randn() * .35;
    
    w_MB = rand();
    w_MB_AS = rand();
    use_AS = 1;
    
    actualParams(thisSubj,:) = [lr temp1 temp2 stay w_MB w_MB_AS use_AS MF_generalizes];
end

load(whichEnv);
results_all = model(envInfo, actualParams, numRounds, debug);

%% Get earnings
% earnings = zeros(numAgents, 1);
% for i = 1:numAgents, earnings(i) = sum(results((numRounds * (i - 1) + 1) : (numRounds * i), 4));
% end
% mean(earnings)
% std(earnings) / sqrt(numAgents)

%% Write data
if strcmp(whichModel((end-2):end), 'csv')
    % For R
    csvwrite_with_headers(whichModel, results_all, {'Action1', 'S2', 'Action2', 'rt2', 'Re', 'subject', 'round'});
elseif strcmp(whichModel((end-2):end), 'mat')
    % For fitting in MATLAB
    results = results_all(:, 1:4);
    subjMarkers = getSubjMarkers(results_all(:, 5));
    save(whichModel, 'results', 'subjMarkers', 'actualParams');
end