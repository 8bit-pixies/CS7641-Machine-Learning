function [x,y,vx,vy]=get_values_from_state(s)
global VMAX Map_Data Pos_Vector_Indexes Speed_Vector_Indexes
% get_values_from_state: Gives the [x y vx vy] representation corresponding
%                            to an element of the state vector.     
% Arguments --------------------------------------------------------------
%     s = the element of the state vector to be converted;
% Evaluation -------------------------------------------------------------
%     [x,y] = coordinates of the car with respect to X and Y axis 
%     [vx,vy] = speed of the car with respect to X and Y axis 
%--------------------------------------------------------------------------
% In verbose mode, there is no difference to silent mode.
%--------------------------------------------------------------------------
% MDP Toolbox, INRA, BIA Toulouse, France
%--------------------------------------------------------------------------

% We will first determine the speed_index and position_index, that is the 
% place occupied by the position and speed of the car within the ordered
% set of valid possibilities. Due to the structure of the state vector,
% we have s=n_speed*(position_index-1)+speed_index, therefore
% speed_index=(s modulo n_speed) except if s=k*n_speed, in
% which case speed_index=n_speed. 
m=size(Map_Data,1);
a=mod(s,size(Speed_Vector_Indexes,1));

speed_index=zeros(size(s));
speed_index(find(~a))=size(Speed_Vector_Indexes,1);
speed_index(find(a))=a(find(a));

position_index=1+floor((s-speed_index)/size(Speed_Vector_Indexes,1));

% We will now determine the w_pos and w_speed, that is the 
% place occupied by the position and speed of the car within the ordered
% set of all possibilities.

w_pos=Pos_Vector_Indexes(position_index);
w_pos=reshape(w_pos,size(s));
w_speed=Speed_Vector_Indexes(speed_index);
w_speed=reshape(w_speed,size(s));

% We are now ready to compute x, y, vx and vy as:
% (x-1)m+y=w_pos and (vx-1)*(2*VMAX+1)+vy=w_speed
% We proceed in a similar fashion as above:
% Caution: vx, vy are in the 1:2*VMAX+1 range, so we will need to recenter
% them to -VMAX:VMAX

% Determining y:

a=mod(w_pos,m);

y=zeros(size(s));
y(find(~a))=m;
y(find(a))=a(find(a));

% Determining x:

x=1+floor((w_pos-y)/m);
    
% Determining vy:

a=mod(w_speed,2*VMAX+1);

vy=zeros(size(s));
vy(find(~a))=2*VMAX+1;
vy(find(a))=a(find(a));

% Determining vx:

vx=1+floor((w_speed-vy)/(2*VMAX+1));

% Recentering:

vx=vx-VMAX-1;
vy=vy-VMAX-1;
