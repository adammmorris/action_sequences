%% makeFitData

results_csv = csvread('../Behavioral/2step/v1/data_fitting.csv');

results = results_csv(:, 1:4);
results(:,4) = (results(:,4) + 5) / 5; % normalize rewards
subjMarkers = getSubjMarkers(results_csv(:, 5));

save('fitting/2step/v1/data.mat', 'results', 'subjMarkers');