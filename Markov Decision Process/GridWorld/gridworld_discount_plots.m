addpath('../MDPtoolbox');

my_grid = grid_7;
%my_grid = grid_13;

%prepare
sz = size(my_grid,1)-2;
[P,R] = grid_to_MDP (my_grid,0.9); % when prob = 0.9

%discount = 0.9;
discount = 0.6;

%solve
vi_iter  = [];
pi_iter  = [];
rvi_iter = [];

vi_times = [];
pi_times = [];
rvi_times = [];
for discount = [0.65, 0.8, 0.9, 0.99]
    tic;
    [V, Policy, iter, cpu_time_] = mdp_value_iteration(P, R, discount);
    cpu_time = toc;
    
    tic;
    [V1, Policy1, iter1, cpu_time1_] = mdp_policy_iteration(P, R, discount);
    cpu_time1 = toc;
    tic;
    [V2, policy2, gg, iter2, cpu_time2_] = mdp_relative_value_iteration(P, R, discount);
    cpu_time2 = toc;
    
    vi_iter = [vi_iter, iter];
    pi_iter = [pi_iter, iter1];
    rvi_iter = [rvi_iter, iter2];
    
    vi_times = [vi_times, cpu_time];
    pi_times = [pi_times, cpu_time1];
    rvi_times = [rvi_times, cpu_time2];
end


vi_iter
pi_iter
rvi_iter

vi_times
pi_times
rvi_times
