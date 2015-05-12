function [ax,ay]=action_to_acceleration(a)

%     action_to_acceleration: given an action a, this function computes the
%                             corresponding (ax,ay) vector
% Arguments --------------------------------------------------------------
%     a = the action taken (in 1:9) 
% Evaluation --------------------------------------------------------------
%     (ax,ay) = the corresponding acceleration vector's components.
%--------------------------------------------------------------------------
% In verbose mode, there is no difference to silent mode.
%--------------------------------------------------------------------------
% MDP Toolbox, INRA, BIA Toulouse, France
%--------------------------------------------------------------------------

if a==1
    ax=-1;ay=-1;
elseif a==2
    ax=-1;ay=0;
elseif a==3
    ax=-1;ay=1;
elseif a==4
    ax=0;ay=-1;
elseif a==5
    ax=0;ay=0;
elseif a==6
    ax=0;ay=1;
elseif a==7
    ax=1;ay=-1;
elseif a==8
    ax=1;ay=0;
elseif a==9
    ax=1;ay=1;
end             
