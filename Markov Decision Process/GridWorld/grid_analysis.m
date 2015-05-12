addpath('../MDPtoolbox');

my_grid = grid_7;
my_grid = grid_13;
%my_grid = grid_7_easy;

%prepare
sz = size(my_grid,1)-2;
[P,R] = grid_to_MDP (my_grid,0.5); % when prob = 0.9

%solve
[V, policy, iter, cpu_time] = mdp_value_iteration ( P, R, 0.9 );
[V1, policy1, iter1, cpu_time1] = mdp_policy_iteration ( P, R, 0.9 ); % changing discount rate affects this maze
[V2, policy2, g, cpu_time2] = mdp_value_iterationGS(P, R, 0.9);
[Q, V3, policy3, md] = mdp_Q_learning(P, R, 0.9);

% check basic stats.
isequal(policy, policy1)
tic
display ( [ num2str(iter) '  &  ' num2str(cpu_time) ] );
toc
tic
display ( [ num2str(iter1) '  &  ' num2str(cpu_time1) ] );
toc

size(policy)