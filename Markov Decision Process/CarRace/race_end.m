function [result]=race_end(xt,yt,xt1,yt1)
global Map_Data Finish_Data Pos_Vector_Indexes Speed_Vector_Indexes
%     race_end : checks whether the transition from (xt,yt) to (xt1,yt1) crosses the finish line.
% Arguments --------------------------------------------------------------
%     (xt,yt) = The first position vector.
%     (xt1,yt1) = The successor vector. These arguments are optional: if not present,
%                 then xt and yt are states, not positions.
% Evaluation -------------------------------------------------------------
%     result = boolean vector; returns 1 if the car crosses the finish line.
%--------------------------------------------------------------------------
% In verbose mode, there is no difference to silent mode.
%--------------------------------------------------------------------------
% MDP Toolbox, INRA, BIA Toulouse, France
%--------------------------------------------------------------------------

% Initialisation
st_len=size(Pos_Vector_Indexes,1)*size(Speed_Vector_Indexes,1);
result=zeros(size(xt));

% important racetrack data: (x1,y1) and (x2,y2) are the boundaries of the finish line, 
% (xc,yc) are the coordinates of the center of the finish line, 
% v2=VMAX squared (so as not to compute it every time) and alpha,beta are
% the coefficients of the y=alpha*x+beta equation that the finish line represents

x1=Finish_Data(1,2);
y1=Finish_Data(1,1);
x2=Finish_Data(2,2);
y2=Finish_Data(3,1);
xc=Finish_Data(3,2);
yc=Finish_Data(3,1);
v2=Finish_Data(4,1);
alpha=Finish_Data(5,1);
beta=Finish_Data(5,2);

% % We check whether we have states or coordinates as arguments
if nargin==2
    t=(yt==st_len+2 | xt==st_len+2);
    result(t)=1;
    t=(yt<=st_len | xt<=st_len);
    xt1=zeros(size(xt));
    yt1=zeros(size(xt));
    [xt1(t),yt1(t),unused1,unused2]=get_values_from_state(yt(t)); 
    [xt(t),yt(t),unused1,unused2]=get_values_from_state(xt(t));     
    xt(~t)=Inf;
    yt(~t)=Inf;
end

% we only perform the check if the car is near enough
% the center of the finish line to make it in 1 step.

test_dist=((xc-xt).*(xc-xt)+(yc-yt).*(yc-yt)<3*v2);
x_diff=(xt~=xt1);

xx=zeros(size(xt));
yy=zeros(size(xt));

% We compute the coordinates of the intersection of the car's path
% with the finish line 

t=find(test_dist&x_diff);

a=zeros(size(xt));
b=zeros(size(xt));

a(t)=(yt(t)-yt1(t))./(xt(t)-xt1(t));
b(t)=yt(t)-a(t).*xt(t);

a_diff=(a~=alpha);

t=find(test_dist&x_diff&a_diff);

xx(t)=(-b(t)+beta)./(a(t)-alpha);        
yy(t)=a(t).*xx(t)+b(t);

t=find(test_dist&x_diff&(~a_diff)&(b==beta));
result(t)=1;

t=find(test_dist&(~x_diff));
xx(t)=xt(t);
yy(t)=alpha*xx(t)+beta;

% We check whether the intersection is within the rectangle formed by the two closest points;
% We add a small security margin to avois rounding errors.

e=0.001;

t=((xx>=min(x1,x2)-e)&(xx<=max(x1,x2)+e)&(yy>=min(y1,y2)-e)&(yy<=max(y1,y2)+e) & (xx>=min(xt,xt1)-e)&(xx<=max(xt,xt1)+e)&(yy>=min(yt,yt1)-e)&(yy<=max(yt,yt1)+e));
result(t)=1;
