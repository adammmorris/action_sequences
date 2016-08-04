%% Parameters
numAgents = 1;
numRounds = 125;
MF_generalizes = 0;
whichEnv = 'env/env_1b';
whichModel = 'fitting/1b/test';
debug = 0;

% Set up their parameters
actualParams = zeros(numAgents, 8); % [lr1, lr2, elig, temp1, temp2, stay, weight_MBAS, weight_MB, MF_S2_smart]
for thisSubj = 1:numAgents
    % This is all very random
    %lr = rand();
    %temp1 = rand();
    %temp2 = temp1;
    %stay = 0;
    lr = .2;
    temp1 = 1;
    temp2 = 1;
    stay = 1;
    
    w_MB = 1;
    w_MB_AS = 0;
    use_AS = 0;
    
    actualParams(thisSubj,:) = [lr temp1 temp2 stay w_MB w_MB_AS use_AS MF_generalizes];
end

load(strcat(whichEnv, '.mat'));
results_all = model(envInfo, actualParams, numRounds, debug);

%% Get earnings
% earnings = zeros(numAgents, 1);
% for i = 1:numAgents, earnings(i) = sum(results((numRounds * (i - 1) + 1) : (numRounds * i), 4));
% end
% mean(earnings)
% std(earnings) / sqrt(numAgents)

%% Write data
% For R
csvwrite_with_headers(strcat(whichModel, '.csv'), results_all, {'Action1', 'S2', 'Action2', 'Re', 'subject', 'round'});

% For fitting in MATLAB
results = results_all(:, 1:4);
subjMarkers = getSubjMarkers(results_all(:, 5));
save(strcat(whichModel, '.mat'), 'results', 'subjMarkers', 'actualParams');

%% Fit?
fitModel(strcat(whichModel, '.mat'), whichEnv, '', 5, 1, [-1 -1 -1 -1 -1 0 0])