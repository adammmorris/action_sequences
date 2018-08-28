%% likelihood
% Calculates the negative log-likelihood of choices in multi-stage DM task, given a set of
% parameters.

%% Inputs
% envInfo: a struct with (at least) 5 elements: states, actions, sequences, sequences_def, transition_probs.
% results: [A1 S2 A2 Re] for a particular subject. Note that A1 and A2 are
% a bit different than in the generative model; here, they are only
% low-level actions (and not action sequences). For most of these games,
% they should be either 1 or 2.
% params: [lr temp1 temp2 stay w_MB w_MB_AS use_AS]
% fixedParams: a 7-length vector, telling you which elements of 'params' to
% use (if fixedParams(i) == -1) or to ignore (and use the value of
% fixedParams(i)). This is a quirk which helps with the fitting.

%% Outputs
% results: [A1 S2 A2 Re subjectId]

%% Notes
% In the terminology here, 'choice' can refer to either an action or a
% sequence. Stage 1 has both a choice and an action (which can be different
% if the choice was a sequence). For Stage 2, they're the same.
% 'transition_probs' has the transition probs for both actions and
% sequences, but only the actions ones actually get implemented - the
% sequence ones are purely for agent planning.

function [likelihood] = likelihood(envInfo, results, freeParams, fixedParams)

%% Load environment info
states = envInfo{1};
actions = envInfo{2};
sequences = envInfo{3};
sequences_def = envInfo{4};
transition_probs = envInfo{5};
extremeRep = envInfo{8};

numStates = max([states{:}]);
numActions = max([actions{:}]);

%% Loop through agents
numRounds = size(results, 1);

% Agent parameters
params = zeros(7, 1);
params(fixedParams == -10) = freeParams;
params(fixedParams ~= -10) = fixedParams(fixedParams ~= -10);
lr = params(1);
temp1 = params(2);
temp2 = params(3);
stay = params(4);
elig = 1;

% Weights
w_MB = params(5); % % of non-AS valuation done in MB way
w_MF = 1 - w_MB; % % done in MF way
w_MB_AS = params(6); % % of AS valuation done in MB way
w_MF_AS = 1 - w_MB_AS; % % done in MF way
use_AS = params(7); % relative weighting on action sequences

Q_MB = zeros(numStates, numActions);
Q_MF = zeros(numStates, numActions);

if extremeRep
    for k = 1:length(states{3})
        Q_MB(states{3}(k), 1) = (k - 1) / 10; % normed versions..
    end
end

lastAction1 = 0;
lastAction2 = 0;

likelihood = 0;

% Normalize rewards
rewards_normed = (results(:,4) + 5) / 10;

%% Loop through rounds
for thisRound=1:numRounds
    %% Stage 1
    S1 = states{1};
    S2s = states{2};
    S3s = states{3};
    S1_choices = actions{S1}; % both actions & sequences
    S1_seqs = sequences{S1}; % just sequences
    S1_actions = S1_choices; % just actions
    S1_actions(S1_seqs) = [];
    
    % MB (for both actions & sequences)
    % This is less general/elegant, but optimized for our current tasks (to make
    % fitting faster).
    if w_MB > 0 % don't bother doing this unless we have to
        Q_MB(S1, S1_choices) = 0;
        
        for a1 = S1_actions
            % Loop through subsequent states
            for nextState = S2s
                Q_MB(S1, a1) = Q_MB(S1, a1) + transition_probs(S1, a1, nextState) * ...
                    max(permute(transition_probs(nextState, actions{nextState}, S3s), [2 3 1]) *...
                    Q_MB(S3s, 1));
            end
        end
        
        if use_AS > 0 && w_MB_AS > 0
            for a1 = S1_seqs
                for nextState = S3s % if it's a stage 3 state (i.e. from a sequence), skip S2
                    Q_MB(S1, a1) = Q_MB(S1, a1) + transition_probs(S1, a1, nextState) * Q_MB(nextState, 1);
                end
            end
        end
    end
    
    Q_weighted = zeros(1, length(S1_choices));
    
    if use_AS, Q_weighted(S1_seqs) = w_MB_AS * Q_MB(S1, S1_seqs) + w_MF_AS * Q_MF(S1, S1_seqs);
    else Q_weighted(S1_seqs) = -Inf;
    end
    Q_weighted(S1_actions) = w_MB * Q_MB(S1, S1_actions) + w_MF * Q_MF(S1, S1_actions);
    weighted_vals = temp1 * Q_weighted + stay * (S1_choices == lastAction1);
    
    % Get action
    action1 = results(thisRound, 1);
    
    % Update likelihood
    probs1 = exp(weighted_vals) / sum(exp(weighted_vals));
    likelihood = likelihood + log(probs1(S1_choices == action1) + sum(probs1(sequences_def(:, 1) == action1)));
    
    % Transition
    S2 = results(thisRound, 2);
    S2_choices = actions{S2}; % in these environments, there are no sequences in stage 2
    
    % Update after first choice
    Q_MF(S1, action1) = Q_MF(S1, action1) + lr * (max(Q_MF(S2, S2_choices)) - Q_MF(S1, action1));
    
    %% Start stage 2
    
    % Update MB
    if w_MB > 0
        Q_MB(S2, S2_choices) = permute(transition_probs(S2, S2_choices, S3s), [2 3 1]) * Q_MB(S3s, 1);
    end
    
    % Get action
    action2 = results(thisRound, 3);
    
    % Combine Q vals
    Q_weighted = w_MB * Q_MB(S2, S2_choices) + w_MF * Q_MF(S2, S2_choices);
    
    probs2 = exp(temp2 * Q_weighted + stay * (S2_choices == lastAction2)) / ...
        sum(exp(temp2 * Q_weighted + stay * (S2_choices == lastAction2)));
    
    % This is tricky. We need to calculate the probability of each action2,
    % including sequences.
    actualProb = zeros(length(S2_choices), 1);
    
    % To do that, we decompose p(action2 | action1) into
    % sum(p(action2 | choice1, action1) * p(choice1 | action1)), or
    % sum(p(action2 | choice1) * p(action1 | choice1) * p(choice1) /
    %   sum(p(action1 | choice1) * p(choice1))).
    
    % Let's start by calculating that denominator.
    % p(action1 | choice1) is 1 for (a) the choice1 == action1 and
    % (b) any sequences involving action1.
    % Therefore...
    denom = sum(probs1(S1_choices == action1 | (sequences_def(:,1) == action1)'));
    
    % Now, we loop through possible A2s
    for possibleA2_ind = 1:length(S2_choices)
        possibleA2 = S2_choices(possibleA2_ind);
        
        % There's two choice1's to consider, action1 itself and the
        % sequence of [action1 action2].
        viableS1choices = [find(S1_choices == action1) ...
            find(sequences_def(:,1) == action1 & sequences_def(:, 2) == possibleA2)];
        
        % We need three values for each viable S1 choice,
        % p(action2 | choice1), p(action1 | choice1), and p(choice1).
        % p(action1 | choice1) is 1 for both of these.
        % p(choice1) is just given by probs1.
        % That leaves...
        probA2_givenChoice1 = [probs2(possibleA2) 1];
        
        % Therefore...
        actualProb(possibleA2_ind) = sum(probA2_givenChoice1 .* probs1(viableS1choices) / denom);
    end
    
    likelihood = likelihood + log(actualProb(action2));
    
    %% Finish Stage 2
    reward_normed = rewards_normed(thisRound);
    
    % Infer transition
    if extremeRep
        S3 = states{3}(results(thisRound, 4) + 6); % need unnormed reward
    else
        S3 = fastrandsample(permute(transition_probs(S2, action2, :), [3 1 2])', 1);
    end
    
    %% Update algorithms
    executedSeq = find(sequences_def(:, 1) == action1 & sequences_def(:,2) == action2);
    
    % Update MF
    % First choice
    Q_MF(S1, action1) = Q_MF(S1, action1) + lr * (max(Q_MF(S2, S2_choices)) - Q_MF(S1, action1));
    
    % Second choice
    delta = reward_normed - Q_MF(S2, action2);
    Q_MF(S2, action2) = Q_MF(S2, action2) + lr * delta;
    Q_MF(S1, action1) = Q_MF(S1, action1) + lr * elig * delta;
    
    % Sequence
    Q_MF(S1, executedSeq) = Q_MF(S1, executedSeq) + lr * (reward_normed - Q_MF(S1, executedSeq));
    
    % Update MB
    if extremeRep
        transition_probs(S2, action2, S3) = transition_probs(S2, action2, S3) + ...
            lr * (1 - transition_probs(S2, action2, S3));
        transition_probs(S2, action2, :) = transition_probs(S2, action2, :) / ...
            sum(transition_probs(S2, action2, :));
        
        transition_probs(S1, executedSeq, S3) = transition_probs(S1, executedSeq, S3) + ...
            lr * (1 - transition_probs(S1, executedSeq, S3));
        transition_probs(S1, executedSeq, :) = transition_probs(S1, executedSeq, :) / ...
            sum(transition_probs(S1, executedSeq, :));
    else
        Q_MB(S3,1) = Q_MB(S3,1) + lr * (reward_normed - Q_MB(S3, 1));
    end
    
    lastAction1 = action1;
    lastAction2 = action2;
end