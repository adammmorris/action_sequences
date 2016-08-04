%% createEnv
% Creates the environment for "model.m".
% Keeps actions & sequences separate, instead of combining into a single Q
% value.

% Generally, the model assumes that (a) sequences are only in stage
% 1, and (b) transitions from S2 to S3 are deterministic.

% These can be changed when actually running the simulation,
% but we set them to a max value here.
numAgents_max = 1000;
numRounds_max = 250;

whichEnv = 'env_1b';

%% Set up states/actions

if strcmp(whichEnv, 'env_det')
    % For each stage, what states are available?
    states = {1, 2:3, 4:6};
    % For each state, what actions are available?
    actions = {1:2, 1:2, 2:3, 0, 0, 0};
    % What about action sequences?
    sequences = {1:4, 0, 0, 0, 0, 0};
    % How are these sequences defined?
    % For every sequence (rows), gives the appropriate action for every stage
    % (columns). Zero indicates that the sequence has ended.
    sequences_def = [1 1 0; 1 2 0; 2 2 0; 2 3 0];
elseif strcmp(whichEnv, 'env_db') || strcmp(whichEnv, 'env_db_probs') || strcmp(whichEnv, 'env_db_continuous') % The task from Dezfouli & Balleine's 2013 PLoS CB paper
    states = {1, 2:3, 4:7};
    % For each state, what actions are available?
    actions = {1:6, 1:2, 1:2, 0, 0, 0, 0};
    % What about action sequences?
    sequences = {3:6, 0, 0, 0, 0, 0};
    % How are these sequences defined?
    % For every sequence index in S1 (rows), gives the appropriate action for every stage
    % (columns). Zero indicates that the sequence has ended.
    sequences_def = [0 0 0; 0 0 0; 1 1 0; 1 2 0; 2 1 0; 2 2 0];
elseif strcmp(whichEnv, 'env_1b')
    states = {1, 2:4, 5:10};
    actions = {1:6, 1:2, 1:2, 1:2, 0, 0, 0, 0, 0, 0};
    sequences = {3:6, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    sequences_def = [0 0 0; 0 0 0; 1 1 0; 1 2 0; 2 1 0; 2 2 0];
end

numStates = max([states{:}]);
numActions = max([actions{:}]);
numSequences = max([sequences{:}]);

% For each stage, converts sequences (rows) to actions (cols)
% (Currently only done for stage 1)
seqs_to_actions = {zeros(numSequences, numActions)};
for thisStage = 1:2
    for thisSequence = 1:numSequences
        seqs_to_actions{thisStage}(thisSequence, nonzeros(sequences_def(thisSequence, thisStage))) = 1;
    end
end

%% Transitions
likelyTransition = zeros(numStates, numActions);
unlikelyTransition = zeros(1, numActions); % we currently only allow stochastic transitions from stage 1's single state

if strcmp(whichEnv, 'env_det')
    transitionProb = 1;

    likelyTransition(1, 1) = 2;
    likelyTransition(1, 2) = 3;
    likelyTransition(2, actions{2}) = [4 5];
    likelyTransition(3, actions{3}) = [5 6];

    unlikelyTransition(1, 1) = 3;
    unlikelyTransition(1, 2) = 2;
elseif strcmp(whichEnv, 'env_db') || strcmp(whichEnv, 'env_db_probs') || strcmp(whichEnv, 'env_db_continuous')
    transitionProb = .75;

    likelyTransition(1, 1) = 2; % actions
    likelyTransition(1, 2) = 3;
    likelyTransition(1, 3) = 4; % sequences
    likelyTransition(1, 4) = 5;
    likelyTransition(1, 5) = 6;
    likelyTransition(1, 6) = 7;
    likelyTransition(2, actions{2}) = [4 5];
    likelyTransition(3, actions{3}) = [6 7];

    unlikelyTransition(1, 1) = 3;
    unlikelyTransition(1, 2) = 2;
    unlikelyTransition(1, 3) = 6;
    unlikelyTransition(1, 4) = 7;
    unlikelyTransition(1, 5) = 4;
    unlikelyTransition(1, 6) = 5;
elseif strcmp(whichEnv, 'env_1b')
    transitionProb = .75;
    
    likelyTransition(1, 1) = 2; % actions
    likelyTransition(1, 2) = 3;
    likelyTransition(1, 3) = 5; % sequences
    likelyTransition(1, 4) = 6;
    likelyTransition(1, 5) = 7;
    likelyTransition(1, 6) = 8;
    likelyTransition(2, actions{2}) = [5 6];
    likelyTransition(3, actions{3}) = [7 8];
    likelyTransition(4, actions{4}) = [9 10];

    unlikelyTransition(1, 1) = 4;
    unlikelyTransition(1, 2) = 4;
    unlikelyTransition(1, 3) = 9;
    unlikelyTransition(1, 4) = 10;
    unlikelyTransition(1, 5) = 9;
    unlikelyTransition(1, 6) = 10;
end

% Transition prob matrix
transition_probs = zeros(numStates, numActions, numStates);

for j = actions{1} % State 1 first..
    transition_probs(1, j, likelyTransition(1, j)) = transitionProb;
    transition_probs(1, j, unlikelyTransition(1, j)) = 1 - transitionProb;
end

for i = 2:numStates % Then the rest of the states
    for j = nonzeros(actions{i})'
        transition_probs(i, j, likelyTransition(i, j)) = 1;
    end
end

%% Rewards
rewardStates = states{3};
rewards = zeros(numRounds_max, numStates, numAgents_max);
if strcmp(whichEnv, 'env_db') || strcmp(whichEnv, 'env_det') || strcmp(whichEnv, 'env_1b')
    stdShift = 2;
    rewardRange_hi = 5;
    rewardRange_lo = -5;
    rewardsAreProbs = 0;
    
    for thisAgent = 1:numAgents_max
        rewards(1, rewardStates, thisAgent) = randsample(rewardRange_lo : rewardRange_hi, length(rewardStates), true);

        for thisRound = 1:(numRounds_max - 1)
            re = rewards(thisRound, rewardStates, thisAgent) + round(randn(length(rewardStates), 1)' * stdShift);
            re(re > rewardRange_hi) = 2 * rewardRange_hi - re(re > rewardRange_hi);
            re(re < rewardRange_lo) = 2 * rewardRange_lo - re(re < rewardRange_lo);
            rewards(thisRound + 1, rewardStates, thisAgent) = re;
        end
    end
elseif strcmp(whichEnv, 'env_db_probs')
    stdShift = .025;
    rewardRange_hi = .75;
    rewardRange_lo = .25;
    rewardsAreProbs = 1;
    
    for thisAgent = 1:numAgents_max
        rewards(1, rewardStates, thisAgent) = rand(length(rewardStates), 1) * (rewardRange_hi - rewardRange_lo) + rewardRange_lo;

        for thisRound = 1:(numRounds_max - 1)
            re = rewards(thisRound, rewardStates, thisAgent) + randn(length(rewardStates), 1)' * stdShift;
            re(re > rewardRange_hi) = 2 * rewardRange_hi - re(re > rewardRange_hi);
            re(re < rewardRange_lo) = 2 * rewardRange_lo - re(re < rewardRange_lo);
            rewards(thisRound + 1, rewardStates, thisAgent) = re;
        end
    end
elseif strcmp(whichEnv, 'env_db_continuous')
    stdShift = 1.5;
    rewardRange_hi = 5;
    rewardRange_lo = -5;
    rewardsAreProbs = 0;
    
    for thisAgent = 1:numAgents_max
        rewards(1, rewardStates, thisAgent) = rand(length(rewardStates), 1) * (rewardRange_hi - rewardRange_lo) + rewardRange_lo;

        for thisRound = 1:(numRounds_max - 1)
            re = rewards(thisRound, rewardStates, thisAgent) + randn(length(rewardStates), 1)' * stdShift;
            re(re > rewardRange_hi) = 2 * rewardRange_hi - re(re > rewardRange_hi);
            re(re < rewardRange_lo) = 2 * rewardRange_lo - re(re < rewardRange_lo);
            rewards(thisRound + 1, rewardStates, thisAgent) = re;
        end
    end
end

%% Save
envInfo = {states, actions, sequences, sequences_def, transition_probs, rewards, rewardsAreProbs, numAgents_max};
% save(strcat(whichEnv, '.mat'), 'numAgents_max', 'states', 'actions', 'sequences', 'sequences_def', ...
%     'transition_probs', 'rewards', 'rewardsAreProbs');
save(strcat(whichEnv, '.mat'), 'envInfo');