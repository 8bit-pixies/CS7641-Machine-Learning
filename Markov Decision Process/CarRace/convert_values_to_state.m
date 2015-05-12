function [s]=convert_values_to_state(x,y,vx,vy)
global VMAX Map_Data Pos_Vector_Indexes Speed_Vector_Indexes
% convert_values_to_state: Converts a [x y vx vy] representation to the
%                              singleton [s] state representation.
% Arguments --------------------------------------------------------------
%     [x,y] = valid coordinates of the car with respect to X and Y axis 
%     [vx,vy] = valid speed of the car with respect to X and Y axis 
% Evaluation -------------------------------------------------------------
%     s = the converted state element; as there is a bijection between both
%         representations, it is unique for a given [x,y,vx,vy]
%--------------------------------------------------------------------------
% In verbose mode, there is no difference to silent mode.
%--------------------------------------------------------------------------
% MDP Toolbox, INRA, BIA Toulouse, France
%--------------------------------------------------------------------------

% Checking dimensions of the vectors

if size(x,2)==1
    x=x';
end
if size(y,2)==1
    y=y';
end

% We will first determine the speed_index and position_index, that is the 
% place occupied by the position and speed of the car within the ordered
% set of valid possibilities. Due to the structure of the state vector,
% we have position_index and speed_index equal to the indexes i and j where 
% Pos_Vector_Indexes(i)=(x-1)*m+y
% Speed_Vector_Indexes(j)=(vx-1)*(2*VMAX+1)+vy
% Caution: vx, vy are in the -VMAX:VMAX range, so we will need to recenter
% them to 1:2*VMAX+1

m=size(Map_Data,1);
position_index=zeros(size(x));
speed_index=zeros(size(x));
for t=1:size(x,2),
    position_index(t)=find(Pos_Vector_Indexes==((x(t)-1)*m+y(t)));
    speed_index(t)=find(Speed_Vector_Indexes==((vx(t)+VMAX)*(2*VMAX+1)+vy(t)+VMAX+1));
end

% We can now compute s, as s=n_speed*(position_index-1)+speed_index 

s=zeros(size(x));

exist=(x>0 & x<=size(Map_Data,2) & y>0 & y<=size(Map_Data,1));
s(exist)=size(Speed_Vector_Indexes,1)*(position_index(exist)-1)+speed_index(exist);
s(~exist)=0;
