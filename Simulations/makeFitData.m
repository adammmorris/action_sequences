%% makeFitData
% Translates csv file from R into matlab file for fitting.

results_csv = csvread('../expt1/data_fitting.csv');

results = results_csv(:, 1:4);
subjMarkers = getSubjMarkers(results_csv(:, 5));

save('fitting/expt1/data/data.mat', 'results', 'subjMarkers');