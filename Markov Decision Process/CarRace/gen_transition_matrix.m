function [P,C,cpu_time]=gen_transition_matrix(file_name,VMAX,m,n,p,penalty,sp)

global VMAX Map_Data Finish_Data Pos_Vector_Indexes Speed_Vector_Indexes

% gn_sp_transition_matrix: this script generates a transition matrix 
%                          from the file_name file, stores it
%                          in sparse format and writes it to the disk
%                          in the form of a sparse transition matrix for each
%                          action.
% Arguments --------------------------------------------------------------
%                          file_name = the location of the data file.
%                          VMAX = the maximum allowed speed, in norm.
%                          m,n = the dimensions of the data matrix
%                          p = an action's probability of non-transmission
%                          penalty = the penalty associated to an accident.
%                          sp = boolean indicating whether the process will be run using
%                               sparse matrices.
% Evaluation -------------------------------------------------------------
%                          cpu_time = the time needed to complete the program.
%--------------------------------------------------------------------------
% In verbose mode, there is no difference to silent mode.
%--------------------------------------------------------------------------
% MDP Toolbox, INRA, BIA Toulouse, France
%--------------------------------------------------------------------------

cpu_time = cputime;

% Reading the data from the file
Map_Data=read_data(file_name,m,n);

% Computing the indexes for position and speed
[Pos_Vector_Indexes,Speed_Vector_Indexes]=index_computation;

[Start_Indexes_Y Start_Indexes_X]=find(Map_Data==2);

% Computing the start-line data (boundaries, center, coefficients; see
% generate_starting_state.m for details
y1=Start_Indexes_Y(1);
x1=Start_Indexes_X(1);
y2=Start_Indexes_Y(size(Start_Indexes_Y,1));
x2=Start_Indexes_X(size(Start_Indexes_Y,1));
yc=Start_Indexes_Y(floor(size(Start_Indexes_Y,1)/2));
xc=Start_Indexes_X(floor(size(Start_Indexes_Y,1)/2));
a=(y1-y2)/(x1-x2);
b=y1-a*x1;

Finish_Data=[y1 x1;y2 x2;yc xc;VMAX*VMAX 0;a b];

% initialising the iteration counter
current=0;

% for each (s,t) action, the transition and cost matrices re computed in sparse format 
% (X,Y and Value vectors), and incorporated into the 3D matrix. 

if ~sp
    P=[];
    C=[];
else
    P=cell(9,1);
    C=cell(9,1);
end

st_len=size(Pos_Vector_Indexes,1)*size(Speed_Vector_Indexes,1);

for s=-1:1,
    for t=-1:1,       
        [Y X V Cost]=transition_matrix(s,t,p);
        Prob=sparse(Y,X,V,st_len+2,st_len+2);
        CC=sparse(Y,X,-Cost,st_len+2,st_len+2);
        courant=courant+1
        if ~sp
            P{courant}=Prob;
            C{courant}=CC;   
        else
            P(:,:,courant)=full(Prob);
            C(:,:,courant)=full(CC);
        end    
    end
end

cpu_time = cputime - cpu_time;
