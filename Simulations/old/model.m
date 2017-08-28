%% model
% Simulates RL agents on a two-stage environment (somewhat arbitrary, but modeled after Expt 2a in
% the Cushman & Morris (2015) paper on habitual goal selection).

%% Inputs
% params (numAgents x 9): [lr1 lr2 elig temp1 temp2 stay w_MBAS w_MFAS w_MB
% MF_generalizes]
% environment: the name of the environment, with a corresponding .mat file
% in the current folder. The file must have these variables: numAgents_max,
% numRounds_max, states, actions, numStates, numActions, transition_probs, rewards.

%% Outputs
% results: [A1 S2 A2 Re subjectId]

function [results] = model(params, environment, numRounds, debug)
if nargin < 4, debug = 0; end
if nargin < 3, numRounds = 250; end
if nargin < 2, environment = 'env_db'; end
if nargin < 1, params = [.1 .1 .95 1 1 1 .25 .25 .25 1]; end

load(strcat(environment, '.mat'));

%% Loop through agents
numAgents = size(params, 1);

results = zeros(numAgents * numRounds, 8);
result_counter = 1;

agentNumbers = randsample(numAgents_max, numAgents)';
for thisAgent = 1:numAgents
    % This is just used to get rewards
    agentNum = agentNumbers(thisAgent);
    
    lr1 = params(thisAgent,1);
    lr2 = params(thisAgent,2);
    elig = params(thisAgent,3);
    temp1 = params(thisAgent,4);
    temp2 = params(thisAgent,5);
    stay = params(thisAgent,6);
    w_MB_AS = params(thisAgent,7);
    w_MF_AS = params(thisAgent, 8);
    w_MB = params(thisAgent, 9);
    w_MF = 1 - w_MB_AS - w_MF_AS - w_MB;
    MF_generalizes = params(thisAgent, 10);
    
    Q_MB_AS = zeros(numStates, numSequences);
    Q_MF_AS = zeros(numStates, numSequences);
    Q_MB = zeros(numStates,numActions);
    Q_MF = zeros(numStates,numActions);
    
    lastChoice1 = 0;
    lastChoice2 = 0;
    
    likelihood = zeros(numAgents,1);
    
    %% Loop through rounds
    for thisRound=1:numRounds
        %% Stage 1
        S1 = states{1};
        S1_availActions = actions{S1};
        S1_availSeqs = sequences{S1};
        
        % MB AS
        Q_MB_AS(S1, :) = 0;
        for seq = S1_availSeqs
            % Right now, this just knows the true transition probs of each
            % sequence to each stage-3 state, and so it's going to act the
            % same as flat MB.
            % Add in estimation of transition probs later..
            for s2 = states{2}
                Q_MB_AS(S1, seq) = Q_MB_AS(S1, seq) + transition_probs(S1, sequences_def(seq, 1), s2) * (permute(transition_probs(s2, sequences_def(seq, 2), states{3}), [2 3 1]) * Q_MB(states{3}, 1));
            end
        end
        
        % MB
        for a1 = S1_availActions
            Q_MB(S1, a1) = 0;
            for s2 = states{2}
                Q_MB(S1, a1) = Q_MB(S1, a1) + transition_probs(S1, a1, s2) * max(permute(transition_probs(s2, actions{s2}, states{3}), [2 3 1]) * Q_MB(states{3}, 1));
            end
        end
        
        % Combine
        conversionMatrix = seqs_to_actions{1}(S1_availSeqs, S1_availActions);
        Q_weighted = w_MB_AS * Q_MB_AS(S1, S1_availSeqs) * conversionMatrix + ...
            w_MF_AS * Q_MF_AS(S1, S1_availSeqs) * conversionMatrix + ...
            w_MB * Q_MB(S1, S1_availActions) + w_MF * Q_MF(S1, S1_availActions);
        
        % Choose
        probs = exp(temp1 * Q_weighted + stay * (S1_availActions == lastChoice1)) / sum(exp(temp1 * Q_weighted + stay * (S1_availActions == lastChoice1)));
        choice1 = S1_availActions(fastrandsample(probs, 1));
        
        % Transition
        S2 = fastrandsample(permute(transition_probs(S1, choice1, :), [3 1 2])', 1);
        S2_availActions = actions{S2};
        
        if MF_generalizes, S2_MF = 2;
        else S2_MF = S2;
        end
        
        likelihood(thisAgent) = likelihood(thisAgent) + log(probs(S1_availActions == choice1));
        
        % Update after first choice
        Q_MF(S1, choice1) = Q_MF(S1, choice1) + lr1*(max(Q_MF(S2_MF, S2_availActions)) - Q_MF(S1, choice1));
        
        %% Stage 2
        Q_MB(S2, S2_availActions) = permute(transition_probs(S2, S2_availActions, states{3}), [2 3 1]) * Q_MB(states{3}, 1);
        
        % Find sequences that could have produced this
        possibleSeqs = find(sequences_def(:, 1) == choice1);
        
        % Put the AS's probability mass on those
        conversionMatrix = seqs_to_actions{2}(possibleSeqs, S2_availActions);
        Q_weighted = w_MB * Q_MB(S2, S2_availActions) + w_MF * Q_MF(S2_MF, S2_availActions) + ...
            w_MB_AS * Q_MB_AS(S1, possibleSeqs) * conversionMatrix + ...
            w_MF_AS * Q_MF_AS(S1, possibleSeqs) * conversionMatrix;
        
        probs = exp(temp2 * Q_weighted + stay * (S2_availActions == lastChoice2)) / sum(exp(temp2 * Q_weighted + stay * (S2_availActions == lastChoice2)));
        choice2 = S2_availActions(fastrandsample(probs, 1));
        
        S3 = fastrandsample(permute(transition_probs(S2, choice2, :), [3 1 2])', 1);
        
        if rewardsAreProbs
            if rand() < rewards(thisRound, S3, agentNum), reward = 1;
            else reward = 0;
            end
        else
            reward = rewards(thisRound, S3, agentNum);
        end
        
        likelihood(thisAgent) = likelihood(thisAgent) + log(probs(S2_availActions == choice2));

        % Update after second choice        
        delta = reward - Q_MF(S2_MF, choice2);
        Q_MF(S2_MF, choice2) = Q_MF(S2_MF, choice2)  + lr2 * delta;
        Q_MF(S1, choice1) = Q_MF(S1, choice1) + lr1 * elig * delta;
        
        executedSeq = find(sequences_def(:, 1) == choice1 & sequences_def(:,2) == choice2);
        Q_MF_AS(S1, executedSeq) = Q_MF_AS(S1, executedSeq) + lr1 * (reward - Q_MF_AS(S1, executedSeq));
        
        Q_MB(S3,1) = Q_MB(S3,1) + lr2 * (reward - Q_MB(S3, 1));
        
        lastChoice1 = choice1;
        lastChoice2 = choice2;
        
        %% Record results
        % Get best choices
        [~, bestS3] = max(rewards(thisRound, states{3}, agentNum));
        bestS3 = bestS3 + min(states{3}) - 1;
        [~, bestA1] = max(permute(transition_probs(S1, actions{1}, states{2}), [2 3 1]) * ...
            max(transition_probs(states{2}, actions{2}, bestS3), [], 2));
        [~, bestS3_cur] = max(rewards(thisRound, states{3}, agentNum) .* ...
            any(squeeze(transition_probs(S2, S2_availActions, states{3}) > 0)));
        bestS3_cur = bestS3_cur + min(states{3}) - 1;
        [~, bestA2] = max(transition_probs(S2, S2_availActions, bestS3_cur));
        
        results(result_counter, :) = [choice1 S2 choice2 reward agentNum thisRound bestA1 bestA2];
        if debug
            disp({'A1', 'S2', 'A2', 'Re', 'Subj', 'Round'});
            disp(results(result_counter, :));w
        end
        result_counter = result_counter + 1;
    end
end