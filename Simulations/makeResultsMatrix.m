%% makeResultsMatrix
% Converts a csv data file into a .mat file for model fitting.

dataFile = 'data/1b/sims_MB.csv';
savePath = 'data/1b/sims_MB.mat';

results_tbl = readtable(dataFile);
results = [results_tbl.Action1 results_tbl.S2 results_tbl.Action2 results_tbl.Re];
results = results(results_tbl.practice == 0);

subjMarkers = getSubjMarkers(results_tbl.subject);

save(savePath, 'results', 'subjMarkers');