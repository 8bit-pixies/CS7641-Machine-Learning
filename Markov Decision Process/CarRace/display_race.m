function display_race(S,Policy, st_len)
global Map_Data
%     display_race: given a series of states S, this function plots the corresponding
%                   (x,y,vx,vy) states.
% Arguments --------------------------------------------------------------
%     S = the vector containing the series of states representing a race.
%     st_len = the number of physical states.  
%--------------------------------------------------------------------------
% In verbose mode, there is no difference to silent mode.
%--------------------------------------------------------------------------
% MDP Toolbox, INRA, BIA Toulouse, France
%--------------------------------------------------------------------------

% Initialisation
X=[];
Y=[];
Ax=[];
Ay=[];

% For each state we compute the corresponding position and
% acceleration vectors

for t=1:size(S,1),
    if (S(t)<=st_len)
        [x y vx vy]=get_values_from_state(S(t));
        X=[X;x];
        Y=[Y;y];
        a=Policy(S(t));
        [ax,ay]=action_to_acceleration(a);
        Ax=[Ax;ax];
        Ay=[Ay;ay];
    end
end

% Finally, we display the track...
imagesc(Map_Data,[0 2]); colormap(gray);
hold on;
% ...the acceleration vectors... 
quiver(X,Y,Ax,Ay,'r');
% ...and the position vectors.
XX=(X(2:size(X))-X(1:(size(X)-1)));
YY=(Y(2:size(Y))-Y(1:(size(Y)-1)));
XX=[XX;XX(size(XX,1))+vx+ax];
YY=[YY;YY(size(YY,1))+vy+ay];
quiver(X,Y,XX,YY,'g')
hold off;
