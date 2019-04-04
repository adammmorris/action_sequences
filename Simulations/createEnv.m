%% createEnv
% Creates the task environments.

clearvars

% These can be changed when actually running the simulation,
% but we set them to a max value here.
numAgents_max = 1000;
numRounds_max = 250;

% possible environments are: expt1 (normal setup for experiment 1),
% expt1_binary (experiment 1 w/ binary rewards), expt1_rbterm (experiment 1
% w/ a reward-based terminal state representation), expt2, and expt2_rbterm
whichEnv = 'expt1';

%% Set parameters
rewardsAreProbs = false;
if strcmp(whichEnv, 'expt1_binary')
    rewardsAreProbs = true;
end

rbTerm = false; % rbTerm stands for "reward-based terminal state representation"
if strcmp(whichEnv, 'expt1_binary') || strcmp(whichEnv, 'expt1_rbterm') || strcmp(whichEnv, 'expt1_rbterm') 
    rbTerm = true;
end

%% Set up states/actions

if strcmp(whichEnv, 'expt1') % The task from Dezfouli & Balleine's 2013 PLoS CB paper
    states = {1, 2:3, 4:7};
    % For each state, what actions are available?
    actions = {1:6, 1:2, 1:2, 0, 0, 0, 0};
    % What about action sequences?
    sequences = {3:6, 0, 0, 0, 0, 0, 0};
    % How are these sequences defined?
    % For every sequence index in S1 (rows), gives the appropriate action for every stage
    % (columns). Zero indicates that the sequence has ended.
    sequences_def = [0 0 0; 0 0 0; 1 1 0; 1 2 0; 2 1 0; 2 2 0];
elseif strcmp(whichEnv, 'expt1_binary')
    states = {1, 2:3, 4:5};
    actions = {1:6, 1:2, 1:2, 0, 0};
    sequences = {3:6, 0, 0, 0, 0};
    sequences_def = [0 0 0; 0 0 0; 1 1 0; 1 2 0; 2 1 0; 2 2 0];
elseif strcmp(whichEnv, 'expt1_rbterm')
    states = {1, 2:3, 4:14};
    actions = {1:6, 1:2, 1:2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    sequences = {3:6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    sequences_def = [0 0 0; 0 0 0; 1 1 0; 1 2 0; 2 1 0; 2 2 0];
elseif strcmp(whichEnv, 'expt2')
    states = {1, 2:4, 5:9};
    actions = {1:6, 1:2, 1:2, 1:2, 0, 0, 0, 0, 0, 0};
    sequences = {3:6, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    sequences_def = [0 0 0; 0 0 0; 1 1 0; 1 2 0; 2 1 0; 2 2 0]; 
elseif strcmp(whichEnv, 'expt2_rbterm')
    states = {1, 2:4, 5:15};
    actions = {1:6, 1:2, 1:2, 1:2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    sequences = {3:6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
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

if strcmp(whichEnv, 'expt1')
    transitionProb = .8;

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
elseif strcmp(whichEnv, 'expt1_binary') || strcmp(whichEnv, 'expt1_rbterm')
    transitionProb = .8;

    likelyTransition(1, 1) = 2;
    likelyTransition(1, 2) = 3;
    unlikelyTransition(1, 1) = 3;
    unlikelyTransition(1, 2) = 2;
elseif strcmp(whichEnv, 'expt2')
    transitionProb = .8;
    
    likelyTransition(1, 1) = 2; % actions
    likelyTransition(1, 2) = 3;
    likelyTransition(1, 3) = 5; % sequences
    likelyTransition(1, 4) = 6;
    likelyTransition(1, 5) = 7;
    likelyTransition(1, 6) = 8;
    likelyTransition(2, actions{2}) = [5 6];
    likelyTransition(3, actions{3}) = [7 8];
    likelyTransition(4, actions{4}) = [9 9];

    unlikelyTransition(1, 1) = 4;
    unlikelyTransition(1, 2) = 4;
    unlikelyTransition(1, 3) = 9;
    unlikelyTransition(1, 4) = 9;
    unlikelyTransition(1, 5) = 9;
    unlikelyTransition(1, 6) = 9;
elseif strcmp(whichEnv, 'expt2_rbterm')
    transitionProb = .8;

    likelyTransition(1, 1) = 2;
    likelyTransition(1, 2) = 3;
    unlikelyTransition(1, 1) = 4;
    unlikelyTransition(1, 2) = 4;
end

% Transition prob matrix
transition_probs = zeros(numStates, numActions, numStates);

if rbTerm
    % stage 1 actions
    for j = setdiff(actions{1}, sequences{1})
        transition_probs(1, j, likelyTransition(1, j)) = transitionProb;
        transition_probs(1, j, unlikelyTransition(1, j)) = 1 - transitionProb;
    end
    
    % stage 1 sequences
    for j = sequences{1}
        for k = states{3}
            transition_probs(1, j, k) = 1 / length(states{3});
        end
    end
    
    % stage 2 actions
    for i = 2:numStates % Then the rest of the states
        for j = nonzeros(actions{i})'
            for k = states{3}
                transition_probs(2, j, k) = 1 / length(states{3});
            end
        end
    end
else
    for j = actions{1} % State 1 first..
        transition_probs(1, j, likelyTransition(1, j)) = transitionProb;
        transition_probs(1, j, unlikelyTransition(1, j)) = 1 - transitionProb;
    end

    for i = 2:numStates % Then the rest of the states
        for j = nonzeros(actions{i})'
            transition_probs(i, j, likelyTransition(i, j)) = 1;
        end
    end
end

%% Rewards

if rbTerm
    numDist = length(states{2}) * 2;
    rewardStates = 1:numDist; % assumes 2 actions per stage 2 state
    rewards = zeros(numRounds_max, numDist, numAgents_max);
else
    rewardStates = states{3};
    rewards = zeros(numRounds_max, numStates, numAgents_max);
end

if rewardsAreProbs
    stdShift = .05;
    rewardRange_hi = .75;
    rewardRange_lo = .25;
    
    for thisAgent = 1:numAgents_max
        rewards(1, rewardStates, thisAgent) = rand(length(rewardStates), 1) * (rewardRange_hi - rewardRange_lo) + rewardRange_lo;

        for thisRound = 1:(numRounds_max - 1)
            re = rewards(thisRound, rewardStates, thisAgent) + randn(length(rewardStates), 1)' * stdShift;
            re(re > rewardRange_hi) = 2 * rewardRange_hi - re(re > rewardRange_hi);
            re(re < rewardRange_lo) = 2 * rewardRange_lo - re(re < rewardRange_lo);
            rewards(thisRound + 1, rewardStates, thisAgent) = re;
        end
    end
else
    stdShift = 1.75;
    rewardRange_hi = 5;
    rewardRange_lo = -5;
    
    for thisAgent = 1:numAgents_max
        rewards(1, rewardStates, thisAgent) = randsample(rewardRange_lo : rewardRange_hi, length(rewardStates), true);

        for thisRound = 1:(numRounds_max - 1)
            re = rewards(thisRound, rewardStates, thisAgent) + round(randn(length(rewardStates), 1)' * stdShift);
            re(re > rewardRange_hi) = 2 * rewardRange_hi - re(re > rewardRange_hi);
            re(re < rewardRange_lo) = 2 * rewardRange_lo - re(re < rewardRange_lo);
            rewards(thisRound + 1, rewardStates, thisAgent) = re;
        end
    end
end

%% Save
envInfo = {states, actions, sequences, sequences_def, transition_probs, rewards, rewardsAreProbs, rbTerm, numAgents_max};
save(['env/' whichEnv '.mat'], 'envInfo');