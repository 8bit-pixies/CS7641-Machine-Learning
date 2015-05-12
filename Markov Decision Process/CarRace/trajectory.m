function [T,C]=trajectory(s0,Policy,st_len,p)

% trajectory : Computes a trajectory from a given start state to the 'end' state, 
%              with respect to a certain policy. 
% Arguments ---------------------------------------------------------------
% Let s0 = starting state
%     Policy = the policy being followed.
%     st_len = number of allowed states on the track, (st_len+1) being 'accident' and
%              (st_len+2) being 'end of race'.
%     p = probability of command failure.
% Evaluation --------------------------------------------------------------
%     T = the computed trajectory.
%     C = the costs associated to the transitions. The first element is always 0 as
%         there has been no transsition at that time.   
%--------------------------------------------------------------------------
% In verbose mode, no additionnal display
%--------------------------------------------------------------------------
% MDP Toolbox, INRA, BIA Toulouse, France
%--------------------------------------------------------------------------

global Map_Data

% initialisations
current=1;
test=0;
T=[s0];
C=[0];

while test~=1,
    % we check whether we are still on the track
    if (T(current)<=st_len)
        [s,cost]=simulate_transitions(p,T(current),Policy(T(current)));
    elseif (T(current)==st_len+1)
        % if not then we are either on the 'accident' state...
        s=st_len+2;
        cost=100;
    elseif (T(current)==st_len+2)
        % ...or on the 'end of race' state.     
        test=1;
    end
    
    % updates
    T=[T;s];
    C=[C;cost];
    current=current+1;
end
