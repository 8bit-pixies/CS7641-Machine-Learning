function res=safe_transition(xx,yy,x,y)
global Map_Data VMAX
% safe_transition: given a starting vector (xx,yy) and a target vector (x,y), this 
%                  function checks whether there are off-limits zones between
%                  both places.
% Arguments --------------------------------------------------------------
%     (xx,yy) = coordinates of the starting place.
%     (x,y) = coordinates of the target place.
% Evaluation -------------------------------------------------------------
%     test = boolean result of the check; 1 means 'safe transition'.
%--------------------------------------------------------------------------
% In verbose mode, there is no difference to silent mode.
%--------------------------------------------------------------------------
% MDP Toolbox, INRA, BIA Toulouse, France
%--------------------------------------------------------------------------

% Checking sizes

if size(x,1)~=size(xx,1)
    x=x';
    y=y';
end

% Initialisations

res=zeros(size(x));
res_temp=ones(size(x));

% Masking the relevant X/=XX items from the vector

test=(x~=xx);

x_t=test.*x+~test;
y_t=test.*y+~test;
xx_t=test.*xx+~test*2;
yy_t=test.*yy+~test*2;

% Computing the lines joining the points

a=(y_t-yy_t)./(x_t-xx_t);
b=y_t-a.*x_t;

x_min=min(x_t,xx_t);
x_m=abs(x_t-xx_t);

% Checking every point from XX to X for safe transition

for s=1:(3*VMAX),
    absc=(x_min+(s/(3*VMAX))*x_m);
    ord=round(a.*absc+b);
    res_temp=res_temp & round(absc)>=1 & round(absc)<=size(Map_Data,2);
    res_temp=res_temp & ord>=1 & ord<=size(Map_Data,2);
    res_temp=res_temp & (Map_Data((round(absc)-1)*size(Map_Data,1)+ord)~=0);
end

res=res+test.*res_temp;
res_temp=ones(size(x));

% Masking the relevant X=XX, Y/=YY items from the vector


test=(y~=yy)&(x==xx);

x_t=test.*x+~test;
y_t=test.*y+~test;
xx_t=test.*xx+~test*2;
yy_t=test.*yy+~test*2;

n_i=max(abs(y_t-yy_t));

% Checking every point from XX to X for safe transition

for s=1:n_i,
   ord=round(min(yy_t,y_t)+(s/n_i)*(abs(y_t-yy_t)));
   res_temp=res_temp & ord>=1 & ord<=size(Map_Data,2);
   res_temp=res_temp & (Map_Data((x_t-1)*size(Map_Data,1)+ord)~=0);    
end
    
res=res+test.*res_temp;
res=res+((x==xx)&(y==yy));
