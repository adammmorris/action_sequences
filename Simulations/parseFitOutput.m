%% parseFitOutput
% Parses the output of the fitting process.

numSubjects = 1;
numFreeParams = 5;
savePath = '';

paramEstimates = zeros(numSubjects, numFreeParams + 1); % first column is negLL
good = false(numSubjects,1);

for i = 1:numSubjects
    name = [savePath 'Params_Subj' num2str(i) '.txt'];
    if exist(name,'file')
        paramEstimates(i,:) = csvread(name);
        good(i) = true;
    end
end