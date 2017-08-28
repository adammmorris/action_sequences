%% model
% Simulates RL agents on a two-stage environment.

%% Inputs
% envInfo: struct with 9 elements, {states actions sequences
% sequences_def transition_probs rewards rewardsAreProbs extremeRep numAgents_max}
% params (numAgents x 10): [lr temp1 temp2 stay w_MB_AS w_MB use_AS lr_trans
% lr_miller]
% numRounds: how many rounds to run
% debug: output debugging info?

%% Outputs
% results: [A1 S2 A2 RT2 Re subjectId]

%% Notes
% In the terminology here, 'choice' can refer to either an action or a
% sequence. Stage 1 has both a choice and an action (which can be different
% if the choice was a sequence). For Stage 2, they're the same.
% 'transition_probs' has the transition probs for both actions and
% sequences, but only the actions ones actually get implemented - the
% sequence ones are purely for agent planning.

function [results] = model(envInfo, params, numRounds, debug)
if nargin < 4, debug = 0; end
if nargin < 3, numRounds = 250; end
if nargin < 2, params = [.1 1 1 1 .5 .5 0 0]; end

%% Load environment info
states = envInfo{1};
actions = envInfo{2};
sequences = envInfo{3};
sequences_def = envInfo{4};
transition_probs = envInfo{5};
rewards = envInfo{6};
rewardsAreProbs = envInfo{7};
extremeRep = envInfo{8};
numAgents_max = envInfo{9};

numStates = max([states{:}]);
numActions = max([actions{:}]);

% Normalize rewards
rewards_normed = (rewards + 5) / 10;

%% Loop through agents
numAgents = size(params, 1);

results = zeros(numAgents * numRounds, 7);
result_counter = 1;

agentNumbers = randsample(numAgents_max, numAgents)';
for thisAgent = 1:numAgents
    % This is just used to get rewards
    agentNum = agentNumbers(thisAgent);
    
    % Agent parameters
    lr = params(thisAgent, 1);
    temp1 = params(thisAgent, 2);
    temp2 = params(thisAgent, 3);
    stay = params(thisAgent, 4);
    elig = 1;
    rt_cost_nonseq = 1;
    
    % Weights
    w_MB = params(thisAgent, 5); % % of non-AS valuation done in MB way
    w_MF = 1 - w_MB; % % done in MF way
    w_MB_AS = params(thisAgent, 6); % % of AS valuation done in MB way
    w_MF_AS = 1 - w_MB_AS; % % done in MF way
    use_AS = params(thisAgent, 7); % relative weighting on action sequences
    
    lr_trans = params(thisAgent, 8);
    % habit LR (use original LR for reward, lr_trans for transition estimation, w_MB for goal weight, w_MB_AS for habit weight)
    lr_miller = params(thisAgent, 9); % if this is > 0, use_AS must be 0!
    
    rg = 0;
    rzero = 0;
    
    Q_MB = zeros(numStates,numActions);
    Q_MF = zeros(numStates,numActions);
    Q_H = zeros(numStates, numActions);
    
    if extremeRep
        if rewardsAreProbs
            Q_MB(states{3}(1), 1) = 1;
            Q_MB(states{3}(2), 1) = 0;
        else
            for k = 1:length(states{3})
                Q_MB(states{3}(k), 1) = (k - 1) / 10; % normed versions..
            end
        end
    end
    
    lastChoice1 = 0;
    lastChoice2 = 0;
    
    likelihood = zeros(numAgents,1);
    
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
        % This is a bit gimmicky. Proper way would be to loop through every
        % possible next state, and check whether it's an S2 or S3. But,
        % since all of our tasks have actions that go to S2 and sequences
        % that go to S3, we can do it this way for speed.
        Q_MB(S1, S1_choices) = 0;
        if w_MB > 0
            for a1 = S1_actions
                % Loop through subsequent states
                for nextState = S2s
                        Q_MB(S1, a1) = Q_MB(S1, a1) + transition_probs(S1, a1, nextState) * ...
                            max(permute(transition_probs(nextState, actions{nextState}, S3s), [2 3 1]) *...
                            Q_MB(S3s, 1));
                end
            end
            for a1 = S1_seqs
                for nextState = S3s
                    Q_MB(S1, a1) = Q_MB(S1, a1) + transition_probs(S1, a1, nextState) * Q_MB(nextState, 1);
                end
            end
        end
        
        Q_weighted = zeros(1, length(S1_choices));
        if lr_miller > 0
            probs_goalcontroller = 1 / (1 + exp(w_MB_AS * mean(Q_H(S1,:) .^ 2) - w_MB * (rg - rzero) ^ 2));
           	if rand() < probs_goalcontroller
                curGoalController = 1;
                Q_weighted(S1_actions) = Q_MB(S1, S1_actions);
            else
                curGoalController = 0;
                Q_weighted(S1_actions) = Q_H(S1, S1_actions);
            end
            
            weighted_vals = temp1 * Q_weighted;
        else
            % Combine Q vals
            if use_AS, Q_weighted(S1_seqs) = w_MB_AS * Q_MB(S1, S1_seqs) + w_MF_AS * Q_MF(S1, S1_seqs);
            else Q_weighted(S1_seqs) = -Inf;
            end
            Q_weighted(S1_actions) = w_MB * Q_MB(S1, S1_actions) + w_MF * Q_MF(S1, S1_actions);

            % Choose action
            weighted_vals = temp1 * Q_weighted + stay * (S1_choices == lastChoice1);
        end

        probs = exp(weighted_vals) / sum(exp(weighted_vals));
        choice1 = S1_choices(fastrandsample(probs, 1));
        
        % Update likelihood
        likelihood(thisAgent) = likelihood(thisAgent) + log(probs(S1_choices == choice1));
        
        %% If we chose an action..
        if any(choice1 == S1_actions)
            action1 = choice1; % This is what we observe externally
            
            % Transition
            S2 = fastrandsample(permute(transition_probs(S1, choice1, :), [3 1 2])', 1);
            S2_choices = actions{S2};
        
            %% Start stage 2
            
            % Update MB
            Q_MB(S2, S2_choices) = permute(transition_probs(S2, S2_choices, states{3}), [2 3 1]) * Q_MB(states{3}, 1);
        
            % Combine Q vals
            Q_weighted = zeros(1, length(S2_choices));
            if lr_miller > 0
                if curGoalController
                    Q_weighted(S2_choices) = Q_MB(S2, S2_choices);
                else
                    Q_weighted(S2_choices) = Q_H(S2, S2_choices);
                end
                
                weighted_vals = temp2 * Q_weighted;
            else
                Q_weighted = w_MB * Q_MB(S2, S2_choices) + w_MF * Q_MF(S2, S2_choices);
                
                % Choose action
                weighted_vals = temp2 * Q_weighted + stay * (S2_choices == lastChoice2);
            end
        
            % Choose action
            probs = exp(weighted_vals) / sum(exp(weighted_vals));
            choice2 = S2_choices(fastrandsample(probs, 1));
            action2 = choice2;
            
            % Transition & get reward
            if ~extremeRep
                % Transition
                S3 = fastrandsample(permute(transition_probs(S2, choice2, :), [3 1 2])', 1);
                
                if rewardsAreProbs
                    if rand() < rewards(thisRound, S3, agentNum)
                        reward_normed = 1;
                    else
                        reward_normed = 0;
                    end

                    reward = reward_normed;
                else
                    reward_normed = rewards_normed(thisRound, S3, agentNum);
                    reward = rewards(thisRound, S3, agentNum);
                end
            else
                % here, rewards are indexed by the stage 2 state-option
                % pair, not the stage 3 state
                % this assumes there are 2 actions in each stage 2 state
                re_ind = sub2ind([2, length(states{2})], choice2, S2 - 1);
                if rewardsAreProbs
                    if rand() < rewards(thisRound, re_ind, agentNum)
                        reward_normed = 1;
                        S3 = states{3}(1);
                    else
                        reward_normed = 0;
                        S3 = states{3}(2);
                    end
                    
                    reward = reward_normed;
                else
                    reward_normed = rewards_normed(thisRound, re_ind, agentNum);
                    reward = rewards(thisRound, re_ind, agentNum);
                    S3 = states{3}(reward + 6); % to go from -5:5 to 1:11
                end
                
                transition_probs(S2, choice2, S3) = transition_probs(S2, choice2, S3) + ...
                    lr * (1 - transition_probs(S2, choice2, S3));
                % renormalize
                transition_probs(S2, choice2, :) = transition_probs(S2, choice2, :) / sum(transition_probs(S2, choice2, :));
            end
            
            RT2 = rt_cost_nonseq;
            
        %% If we chose a sequence..
        elseif any(choice1 == S1_seqs)
            % Get actions for this sequence
            action1 = sequences_def(choice1, 1); % This is what we observe externally
            choice2 = sequences_def(choice1, 2);
            action2 = choice2;
            
            % Which states did we transition to?
            S2 = fastrandsample(permute(transition_probs(S1, action1, :), [3 1 2])', 1);
            S2_choices = actions{S2};

            % Get reward
            if ~extremeRep
                % Transition
                S3 = fastrandsample(permute(transition_probs(S2, choice2, :), [3 1 2])', 1);
                
                if rewardsAreProbs
                    if rand() < rewards(thisRound, S3, agentNum)
                        reward_normed = 1;
                    else
                        reward_normed = 0;
                    end

                    reward = reward_normed;
                else
                    reward_normed = rewards_normed(thisRound, S3, agentNum);
                    reward = rewards(thisRound, S3, agentNum);
                end
            else
                % here, rewards are indexed by the stage 2 state-option
                % pair, not the stage 3 state
                % this assumes there are 2 actions in each stage 2 state
                re_ind = sub2ind([2, length(states{2})], choice2, S2 - 1);
                if rewardsAreProbs
                    if rand() < rewards(thisRound, re_ind, agentNum)
                        reward_normed = 1;
                        S3 = states{3}(1);
                    else
                        reward_normed = 0;
                        S3 = states{3}(2);
                    end
                    
                    reward = reward_normed;
                else
                    reward_normed = rewards_normed(thisRound, re_ind, agentNum);
                    reward = rewards(thisRound, re_ind, agentNum);
                    S3 = states{3}(reward + 6); % to go from -5:5 to 1:11
                end
            end
            
            RT2 = 0;
        end
        
        % Update likelihood
        likelihood(thisAgent) = likelihood(thisAgent) + log(probs(S2_choices == choice2));
        
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

            transition_probs(S1, action1, S2) = transition_probs(S1, action1, S2) + ...
                lr_trans * (1 - transition_probs(S1, action1, S2));
            transition_probs(S1, action1, :) = transition_probs(S1, action1, :) / ...
                sum(transition_probs(S1, action1, :));
        end
        
        % Update Miller's habits
        Q_H(S1, action1) = Q_H(S1, action1) + lr_miller * (1 - Q_H(S1, action1));
        others = setdiff(actions{S1}, action1);
        Q_H(S1, others) = Q_H(S1, others) + lr_miller * (0 - Q_H(S1, others));

        % Second choice
        Q_H(S2, action2) = Q_H(S2, action2) + lr_miller * (1 - Q_H(S2, action2));
        others = setdiff(actions{S2}, action2);
        Q_H(S2, others) = Q_H(S2, others) + lr_miller * (0 - Q_H(S2, others));
        
        if curGoalController
            rg = rg + lr * (reward - rg);
        end
        
        rzero = rzero + lr * (reward - rzero);
        
        lastChoice1 = choice1;
        lastChoice2 = choice2;
        
        %% Record results
        results(result_counter, :) = [action1 S2 choice2 reward agentNum thisRound RT2];
        if debug
            disp({'A1', 'S2', 'A2', 'Re', 'Subj', 'Round', 'RT2'});
            disp(results(result_counter, :));
        end
        result_counter = result_counter + 1;
    end
end