load(strcat(whichEnv, '.mat'));
%likelihood(envInfo, results, [.2 1 1 1 1], [-1 -1 -1 -1 -1 0 0])

model(envInfo, repmat([.2 1 1 1 .5 0 0 0], 10, 1), 250, 0);