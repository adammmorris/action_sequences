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

function [results] = model_MF(numAgents, numRounds, debug)

%% Loop through agents

% Record results
results = zeros(numAgents * numRounds, 10);
result_counter = 1;

% Reward parameters
% stdShift = .025;
% rewardRange_hi = .75;
% rewardRange_lo = .25;
stdShift = 2;
rewardRange_hi = 5;
rewardRange_lo = -5;

for thisAgent = 1:numAgents
    % Sample initial rewards
    rewards = round(rand(4, 1) * (rewardRange_hi - rewardRange_lo) + rewardRange_lo);
    
    % Parameters
    lr = .2;
    elig = 1;
    temp = 1;
    
    Q_MF = zeros(3, 2);
    
    %% Loop through rounds
    for thisRound=1:numRounds
        
        %% Stage 1
        S1 = 1;
        
        % Choose
        probs = exp(temp * Q_MF(S1, :)) / sum(exp(temp * Q_MF(S1, :)));
        choice1 = fastrandsample(probs, 1);
        
        % Transition
        if rand() < .7
            S2 = choice1 + 1;
        else
            if choice1 == 1
                S2 = 3;
            elseif choice1 == 2
                S2 = 2;
            end
        end
        
        % Update after first choice
        % This is for Q-learning
        Q_MF(S1, choice1) = Q_MF(S1, choice1) + lr * (max(Q_MF(S2, :)) - Q_MF(S1, choice1));

        %% Stage 2
        % Choose
        probs = exp(temp * Q_MF(S2, :)) / sum(exp(temp * Q_MF(S2, :)));
        choice2 = fastrandsample(probs, 1);
        
        % This is for SARSA
        %Q_MF(S1, choice1) = Q_MF(S1, choice1) + lr * (Q_MF(S2, choice2) - Q_MF(S1, choice1));

        if S2 == 2
            if choice2 == 1
                S3 = 4;
            elseif choice2 == 2
                S3 = 5;
            end
        elseif S2 == 3
            if choice2 == 1
                S3 = 6;
            elseif choice2 == 2
                S3 = 7;
            end
        end
        
        % Get reward
        reward = rewards(S3 - 3);
%         if rand() < rewards(S3 -3)
%             reward = 1;
%         else
%             reward = 0;
%         end
        
        % Update after second choice    
        curQ = Q_MF(S2, choice2);
        delta = reward - Q_MF(S2, choice2);
        Q_MF(S2, choice2) = Q_MF(S2, choice2)  + lr * delta;
        Q_MF(S1, choice1) = Q_MF(S1, choice1) + lr * elig * delta;
        
        % Record results
        [~, bestChoice2] = max(rewards);
        if bestChoice2 == 1 || bestChoice2 == 2
            bestChoice1 = 1;
        else
            bestChoice1 = 2;
        end
        
        if S2 == 2
            [~, bestChoice2_cur] = max(rewards(1:2));
        elseif S2 == 3
            [~, bestChoice2_cur] = max(rewards(3:4));
        end
            
        results(result_counter, :) = [choice1 S2 choice2 reward thisAgent thisRound bestChoice1 bestChoice2_cur curQ delta];
        if debug
            disp({'A1', 'S2', 'A2', 'Re', 'Subj', 'Round'});
            disp(results(result_counter, :));
            disp(Q_MF(S1, :));
        end
        result_counter = result_counter + 1;

        % Update rewards
        for i=1:4
            re = rewards(i) + round(randn() * stdShift);
            re(re > rewardRange_hi) = 2 * rewardRange_hi - re(re > rewardRange_hi);
            re(re < rewardRange_lo) = 2 * rewardRange_lo - re(re < rewardRange_lo);
            rewards(i) = re;
        end
    end
end