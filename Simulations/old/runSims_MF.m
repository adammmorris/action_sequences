%% Parameters
numAgents = 1000;
numRounds = 250;
debug = 0;

results = model_MF(numAgents, numRounds, debug);

csvwrite_with_headers('sims.csv', results, {'Action1', 'S2', 'Action2', 'Re', 'subject', 'round', 'bestA1', 'bestA2', 'Q', 'PE'});