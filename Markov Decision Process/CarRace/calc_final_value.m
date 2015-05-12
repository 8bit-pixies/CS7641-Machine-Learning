% calculate the value of the race....

function [val] = calc_final_value(S,Policy, st_len, V)

X=[];
Y=[];
Ax=[];
Ay=[];
val = 0 ;
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
        val = val + V(S(t));
    end
end

