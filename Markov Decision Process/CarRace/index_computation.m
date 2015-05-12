function [Pos_Vector_Indexes,Speed_Vector_Indexes]=index_computation()
global VMAX Map_Data
%     index_computation : Deduces from the racetrack matrix the vectors containing 
%                         the indexes of valid positions ans speeds.
% Evaluation -------------------------------------------------------------
%     Pos_Vector_Indexes = vector containing the indexes of valid positions 
%                          within the racetrack matrix.
%     Speed_Vector_Indexes = vector containing the indexes of valid [vx vy] combinations;
%--------------------------------------------------------------------------
% In verbose mode, there is no difference to silent mode.
%--------------------------------------------------------------------------
% MDP Toolbox, INRA, BIA Toulouse, France
%--------------------------------------------------------------------------

[m n]=size(Map_Data);

% Reshaping the Map_Data matrix into a mn position vector;

Pos_Vector=reshape(Map_Data,m*n,1);

% Determining the indexes of non-zero elements:

Pos_Vector_Indexes=find(Pos_Vector);

% Determining the allowed speed combinations:
% First we compute the distance from the center:

Speed_Mat=repmat(([-VMAX:VMAX].*[-VMAX:VMAX])',1,(2*VMAX+1));
Speed_Mat=Speed_Mat+Speed_Mat';

% Then we test which values satisfy vx**2+vY**2<=VMAX**2 

Speed_Mat=(Speed_Mat<=VMAX*VMAX);

% Reshaping the Speed_Mat matrix into a vector;

Speed_Vector=reshape(Speed_Mat,((2*VMAX+1)*(2*VMAX+1)),1);

% Determining the indexes of non-zero elements:

Speed_Vector_Indexes=find(Speed_Vector);
