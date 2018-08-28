%% Parameters
numAgents = 500;
numRounds = 125;
env = '1b_fix';
modelName = 'MB_MB';
estimateTrans = false;
doMiller = false;
debug = 0;

whichEnv = ['env/' env '.mat'];
whichModel = ['data/' env '/sims_' modelName '.csv'];

% Set up their parameters
actualParams = zeros(numAgents, 9); % [lr1, lr2, elig, temp1, temp2, stay, weight_MBAS, weight_MB, lr_trans, lr_miller]
for thisSubj = 1:numAgents
    lr = betarnd(1.2, 1.2);
    if estimateTrans, lr_trans = betarnd(1.2, 1.2);
    else lr_trans = 0;
    end
    if doMiller, lr_miller = betarnd(1.2, 1.2);
    else lr_miller = 0;
    end
    temp1 = gamrnd(4.82, .88);
    temp2 = gamrnd(4.82, .88);
    stay = normrnd(.15, 1.42);
    
    if strcmp(modelName, 'MB_noAS')
        w_MB = 1;
        w_MB_AS = 0;
        use_AS = 0;
    elseif strcmp(modelName, 'MFMB_noAS')
        w_MB = rand();
        w_MB_AS = 0;
        use_AS = 0;
    elseif strcmp(modelName, 'MB_MB')
        w_MB = 1;
        w_MB_AS = 1;
        use_AS = 1;
    elseif strcmp(modelName, 'MFMB_MB')
        w_MB = rand();
        w_MB_AS = 1;
        use_AS = 1;
    elseif strcmp(modelName, 'MB_MFMB')
        w_MB = 1;
        w_MB_AS = rand();
        use_AS = 1;
    elseif strcmp(modelName, 'MF_MF')
        w_MB = rand();
        w_MB_AS = rand();
        use_AS = 1;
    end
    
    actualParams(thisSubj,:) = [lr temp1 temp2 stay w_MB w_MB_AS use_AS lr_trans lr_miller];
end

load(whichEnv);
results_all = model(envInfo, actualParams, numRounds, debug);

%% Get earnings
earnings = zeros(numAgents, 1);
for i = 1:numAgents
    earnings(i) = sum(results_all((numRounds * (i - 1) + 1) : (numRounds * i), 4));
end
mean(earnings)
% std(earnings) / sqrt(numAgents)

%% Write data
if strcmp(whichModel((end-2):end), 'csv')
    % For R
    csvwrite_with_headers(whichModel, results_all, {'Action1', 'S2', 'Action2', 'Re', 'subject', 'round', 'rt2'});
elseif strcmp(whichModel((end-2):end), 'mat')
    % For fitting in MATLAB
    results = results_all(:, 1:4);
    subjMarkers = getSubjMarkers(results_all(:, 5));
    save(whichModel, 'results', 'subjMarkers', 'actualParams');
end