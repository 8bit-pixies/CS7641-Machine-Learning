function [s]=generate_starting_state()
global Map_Data Speed_Vector_Indexes Pos_Vector_Indexes Finish_Data
% generate_starting_state : generates a random state within the list of [x y 0 0] states
%                      with x,y on the start/finish line.
% Evaluation -------------------------------------------------------------
%     s = the starting state.
%--------------------------------------------------------------------------
% In verbose mode, there is no difference to silent mode.
%--------------------------------------------------------------------------
% MDP Toolbox, INRA, BIA Toulouse, France
%--------------------------------------------------------------------------

% We generate random points in the rectangle defined by the boundaries od the finish line
% and test whether they are actuelly finish line points. This approach was deemed faster
% than exaustive search of said rectangle for finish line points and then random selection 
% of one of them.

test=0;
while ~test
    y_rand=floor((abs(Finish_Data(1,1)-Finish_Data(2,1))+1)*rand(1))+min(Finish_Data(1,1),Finish_Data(2,1));
    x_rand=floor((abs(Finish_Data(1,2)-Finish_Data(2,2))+1)*rand(1))+min(Finish_Data(1,2),Finish_Data(2,2));
    test=Map_Data(y_rand,x_rand)==2;
end

s=convert_values_to_state(x_rand,y_rand,0,0);
