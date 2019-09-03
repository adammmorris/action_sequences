%% makeFitData
% Translates csv file from R into matlab file for fitting.

%results_csv = csvread('../expt1/data_fitting.csv');
results_csv = csvread('fitting/leor/data/data_fitting.csv');

results = results_csv(:, 1:4);
subjMarkers = getSubjMarkers(results_csv(:, 5));

save('fitting/leor/data/data.mat', 'results', 'subjMarkers');