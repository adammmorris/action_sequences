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

function [paramEstimates, goodSubjects, corrs] = parseFitOutput(savePath, numSubjects, whichParams, numExtraVars, dataPath, realData)

numFreeParams = length(whichParams);
paramEstimates = zeros(numSubjects, numFreeParams + numExtraVars);
goodSubjects = false(numSubjects,1);

for whichSubj = 1:numSubjects    
    name = [savePath num2str(whichSubj) '.txt'];
    if exist(name, 'file')
        paramEstimates(whichSubj, :) = csvread(name);
        goodSubjects(whichSubj) = true;
    end
end

%% Get correlations
corrs = zeros(numFreeParams, 1);
if ~realData
    load(dataPath);
    for i = 1:numFreeParams
        temporary = corrcoef(actualParams(goodSubjects, whichParams(i)), paramEstimates(goodSubjects, i + numExtraVars));
        corrs(i) = temporary(2,1);
    end
end