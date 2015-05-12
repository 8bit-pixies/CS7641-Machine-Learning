function [Y_Vector, X_Vector, Probability_Vector, Cost_Vector]=transition_matrix(ax,ay,p)
global Pos_Vector_Indexes Speed_Vector_Indexes
% transition_matrix: given a state represented by the index vectors and an 
%                    action ax,ay, this function computes the transition and
%                    cost matrixes associated to the MDP, for this action.
% Arguments --------------------------------------------------------------
%     [ax ay] : acceleration with respect to X and Y axis, i.e. the action taken;
%     p : the probability of command transmission failure (ie the probability to have
%         action (0,0) instead of (ax,ay). This argument is optional; if omitted,
%         then ax is considered an action in the 1:9 range and ay serves as p.
% Evaluation -------------------------------------------------------------
%     Y_Vector = the vector containing the row indexes of non-zero elements of 
%                P(:,:,action).
%     X_Vector = the vector containing the column indexes of non-zero elements of 
%                P(:,:,action).
%     Probability_Vector = the vector containing the values of non-zero elements of 
%                P(:,:,action).
%     Cost_Vector = the vector containing the costs associated to the transitions in
%                P(:,:,action).
%--------------------------------------------------------------------------
% In verbose mode, there is no difference to silent mode.
%--------------------------------------------------------------------------
% MDP Toolbox, INRA, BIA Toulouse, France
%--------------------------------------------------------------------------

% If the argument was an action, we convert to (ax,ay) format.
if nargin == 2
    p=ay;
    [ax,ay]=action_to_acceleration(ax);
end

% determining the constants we will need later on
state_length=(size(Pos_Vector_Indexes,1)*size(Speed_Vector_Indexes,1));
n_speed=size(Speed_Vector_Indexes,1);

% initialisations
current=1;
% As there are exactly 2 non-zero terms per row, except on the last two, we know Y_Vector will be
% [1;1;2;2;3;3;...;2*state_length+1;2*state_length+2]. As the last 2 terms represent the 
% 'accident' and 'endrace' states, both only have one successor.
Y_Vector=[reshape(([1:state_length]'*[1,1])',2*state_length,1);state_length+1;state_length+2];
% We know the size of X_vector, but not the values it will take, except the last, as the 'endrace' 
% state is absorbing.
X_Vector=[zeros(2*state_length,1);state_length+2;state_length+2];
% We know the size of X_vector, but not the values it will take, except the last two, as the 'endrace' 
% state is absorbing and the 'accident' state teleports the car back to start with probability 1.
Probability_Vector=[zeros(2*state_length,1);1;1];
Cost_Vector=[ones(2*state_length,1);100;0];


% Action: (0,0)
[X1,C1]=compute_transitions(1:state_length,0,0);

% Action: (ax,ay)
[X2,C2]=compute_transitions(1:state_length,ax,ay);

% Reshaping into the correct format
Probability_Vector(1:2*state_length)=repmat([p;1-p],state_length,1);
X_Vector(1:2*state_length)=sparse((1:2:2*state_length),(1:state_length),ones(state_length,1),2*state_length,state_length)*X1'+sparse((2:2:2*state_length),(1:state_length),ones(state_length,1),2*state_length,state_length)*X2';
