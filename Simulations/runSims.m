%% runSims.m
% Runs simulations.

%% Parameters
numAgents = 300;
numRounds = 125;
env = 'expt1';

% The model-naming convention is as follows:
% <[MF, MB, MFMB]>_<[MF, MB, MFMB, noAS]>
% The first half indicates type of control of single-step actions.
% The second half indicates control of action sequences (or noAS if
% there are no action sequences).
modelName = 'MFMB_noAS';

whichEnv = ['env/' env '.mat'];
whichModel = ['sims/' env '/sims_' modelName '.mat'];

% Set up their parameters
actualParams = zeros(numAgents, 7); % [lr, temp1, temp2, stay, w_MB, w_MBAS, use_AS]
for thisSubj = 1:numAgents
    lr = betarnd(1.1,1.1);
    temp1 = gamrnd(1.2,5);
    temp2 = gamrnd(1.2,5);
    stay = normrnd(0,1);
    
    if strcmp(modelName, 'MFMB_noAS')
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
    elseif strcmp(modelName, 'MFMB_MF')
        w_MB = rand();
        w_MB_AS = 0;
        use_AS = 1;
    elseif strcmp(modelName, 'MB_MFMB')
        w_MB = 1;
        w_MB_AS = rand();
        use_AS = 1;
    elseif strcmp(modelName, 'MFMB_MFMB')
        w_MB = rand();
        w_MB_AS = rand();
        use_AS = 1;
    elseif strcmp(modelName, 'MB_noAS')
        w_MB = 1;
        w_MB_AS = 0;
        use_AS = 0;
    end
    
    actualParams(thisSubj,:) = [lr temp1 temp2 stay w_MB w_MB_AS use_AS];
end

load(whichEnv);
results_all = runModel(envInfo, actualParams);

%% Save data
if strcmp(whichModel((end-2):end), 'csv')
    % For R
    csvwrite_with_headers(whichModel, results_all, {'Action1', 'S2', 'Action2', 'Re', 'subject', 'round', 'rt2'});
elseif strcmp(whichModel((end-2):end), 'mat')
    % For fitting in MATLAB
    results = results_all(:, 1:4);
    subjMarkers = getSubjMarkers(results_all(:, 5));
    save(whichModel, 'results', 'subjMarkers', 'actualParams');
end