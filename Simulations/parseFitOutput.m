%% parseFitOutput
% Parses the output of the fitting process.

%% Inputs
% savePath: path to folder Params_SubjX estimates
% dataPath: path to .mat file w/ actualParams (optional)

%% Outputs
% paramEstimates: estimated params for each subject (with negLL in first
% column)
% goodSubjects: tells which subjects we got parameter estimates for
% corrs: correlations between actual & estimated params (if dataPath was
% inputted)

function [paramEstimates, goodSubjects, corrs] = parseFitOutput(savePath, numSubjects, numFreeParams, dataPath)

paramEstimates = zeros(numSubjects, numFreeParams + 1); % first column is negLL
goodSubjects = false(numSubjects,1);

for i = 1:numSubjects
    name = [savePath 'Params_Subj' num2str(i) '.txt'];
    if exist(name,'file')
        paramEstimates(i,:) = csvread(name);
        goodSubjects(i) = true;
    end
end

%% Get correlations
if nargin == 4
    load(dataPath);
    corrs = zeros(numFreeParams, 1);
    for i = 1:numFreeParams
        temporary = corrcoef(actualParams(goodSubjects, i), paramEstimates(goodSubjects, i+1));
        corrs(i) = temporary(2,1);
    end
end