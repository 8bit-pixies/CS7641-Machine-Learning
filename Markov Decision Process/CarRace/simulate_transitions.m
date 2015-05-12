function [X,Cost]=simulate_transitions(p,s,ax,ay);

global Map_Data Pos_Vector_Indexes Speed_Vector_Indexes Finish_Data VMAX
% simulate_transitions: given a state s and an action ax,ay, this function simulates
%                       the successor of s with respect to ax,ay, and the probability of 
%                       command failure. It also computes the cost associated to the 
%                       transition. However, non-physical states should NOT be entered.
% Arguments --------------------------------------------------------------
%     p : probability of command failure.
%     s : the starting state whose successor we want to simulate;
%     [ax ay] : acceleration with respect to X and Y axis, i.e. the action taken.
%               ay is optional, if not present, then ax is the action taken in 1..9 
% Evaluation -------------------------------------------------------------
%     X : the successor of s;
%     Cost : the cost associated to the transition.
%--------------------------------------------------------------------------
% In verbose mode, there is no difference to silent mode.
%--------------------------------------------------------------------------
% MDP Toolbox, INRA, BIA Toulouse, France
%--------------------------------------------------------------------------

% If the argument was an action, we convert to (ax,ay) format.
if nargin == 3
    [ax,ay]=action_to_acceleration(ax);
end

% we simulate the possibility of command failure
if rand(1)<p
    ax=0;
    ay=0;
end

% initialisations

m=size(Map_Data,1);
n=size(Map_Data,2);
state_length=(size(Pos_Vector_Indexes,1)*size(Speed_Vector_Indexes,1));
X=zeros(size(s));
Cost=ones(size(s));
done=zeros(size(s));

% computing the 'raw' successors

[x,y,vx,vy]=get_values_from_state(s);
xx=x;
yy=y;
vx=vx+ax;
vy=vy+ay;
x=x+vx;
y=y+vy;

% checking authorizes speeds
test_speed=((vx.*vx+vy.*vy)<=VMAX.*VMAX);
X=X+(~test_speed*(state_length+1));
done=done+~test_speed;

% checking outside successors

inside=(x>=1 & x<=n & y>=1 & y<=m);
X=X+(~inside & ~done)*(state_length+1);
done=done+(~inside & ~done);


% checking transit through out-of-bounds 

x_o=zeros(size(done));
y_o=zeros(size(done));

x_o=x.*(~done)+done;
y_o=y.*(~done)+done;

safe=safe_transition(xx,yy,x_o',y_o');

X=X+((~safe & ~done)*(state_length+1));
done=done+(~safe & ~done);

% cheking whether the endline is crossed

x_o=zeros(size(done));
y_o=zeros(size(done));

x_o=x.*(~done)+done;
y_o=y.*(~done)+done;

test_end=race_end(xx,yy,x,y);

Finish_Vector=zeros(1,2);

x1=Finish_Data(1,2);
y1=Finish_Data(1,1);
x2=Finish_Data(2,2);
y2=Finish_Data(3,1);

if (y1~=y2 & x1~=x2)
    Finish_Vector(1)=1/sqrt(1-(x1-x2)*(x1-x2)/((y1-y2)*(y1-y2)));
    Finish_Vector(2)=Finish_Vector(1)*(x1-x2)/(y2-y1);
elseif (x1==x2)
    Finish_Vector(1)=1;
    Finish_Vector(2)=0;
elseif (y1==y2)
    Finish_Vector(1)=0;
    Finish_Vector(2)=-1;
end

sense=Finish_Vector*[vx; vy];

% cheking whether the endline is crossed in the wrong sense

X=X+(safe & test_end & (sense<=0) & ~done)*(state_length+1);
done=done+(safe & test_end & (sense<=0) & ~done);

% winning moves
test_short=find(safe & test_end & (sense>0) & ~done);
for t=1:size(test_short,2),
    if Map_Data(yy(test_short(t)),xx(test_short(t)))~=2
        X(test_short(t))=state_length+2;
        done(test_short(t))=1;
    end
end

% checking out-of-tracks successors
x_o=zeros(size(done));
y_o=zeros(size(done));

x_o=x.*(~done)+done;
y_o=y.*(~done)+done;

out_of_track=(Map_Data((x_o-1)*m+y_o)~=1 & Map_Data((x_o-1)*m+y_o)~=2);
out_of_track=out_of_track.*(~done);

X=X+(out_of_track & (~done))*(state_length+1);
done=done+(out_of_track & ~done);

% computing 'normal' successors

x_o=zeros(size(done));
y_o=zeros(size(done));
vx_o=zeros(size(done));
vy_o=zeros(size(done));

x_o=x.*(~done)+x1*done;
y_o=y.*(~done)+y1*done;
vx_o=vx.*(~done);
vy_o=vy.*(~done);


X=X+(~done).*(convert_values_to_state(x_o,y_o,vx_o,vy_o));
