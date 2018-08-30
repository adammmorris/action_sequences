%% makeFitData

results_csv = csvread('../Behavioral/1b_fix/v3/real/data_fitting.csv');

results = results_csv(:, 1:4);
%results(:,4) = (results(:,4) + 5) / 10; % normalize rewards
subjMarkers = getSubjMarkers(results_csv(:, 5));

save('fitting/1b_fix/real3/data.mat', 'results', 'subjMarkers');
%save('fitting/1b_fix_extreme/real1/data.mat', 'results', 'subjMarkers');