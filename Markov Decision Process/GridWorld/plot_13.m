% plot grid for grid size 13, since not everything fits...we will not have
% the text labels. but have a colour bar instead.



% Andrew W Moore Carnegie Mellon on MDP

% Which one to choose?
% 
% The choice would be as follows:
% 
% Policy Iteration & Value Iteration:
% Which is best ???
% It depends.
% Lots of actions? Choose Policy Iteration
% Already got a fair policy? Policy Iteration
% Few actions, acyclic? Value Iteration



addpath('../MDPtoolbox');

my_grid = grid_13;
%my_grid = grid_n_13;

%prepare
sz = size(my_grid,1)-2;
[P,R] = grid_to_MDP (my_grid,0.6); % when prob = 0.9

discount = 0.9;

%solve
[V, policy, iter, cpu_time] = mdp_value_iteration ( P, R, discount );
[V1, policy1, iter1, cpu_time1] = mdp_policy_iteration ( P, R, discount ); % changing discount rate affects this maze
[V2, policy2, g, iter2, cpu_time2] = mdp_relative_value_iteration(P, R, discount );
% figure;
% colormap(gray); imagesc(my_grid);
% set(gca,'XTick',[0.5:1:31.5],...
%         'YTick',[0.5:1:31.5])
% grid on;

isequal(policy, policy1)

display ( [ num2str(iter) '  &  ' num2str(cpu_time) ] );
display ( [ num2str(iter1) '  &  ' num2str(cpu_time1) ] );

%plot policy

% get walls...
wall_grid = abs(my_grid(2:sz+1,2:sz+1))*-1;
wall_grid(1,1) = 2;
wall_grid(sz,sz) = 2;

cols = hot(15);
cols(1,:) = [0,0,0];
cols(15,:) = [1,1,1];

figure;
imagesc( reshape(V,sz,sz)' + wall_grid );
hold on

dir_x = zeros(sz,sz);
dir_y = zeros(sz,sz);
len = 0.5;

for i=1:sz
    for j=1:sz
        
        dir = policy( (i-1)*sz+j );
        
        if ( my_grid(i+1,j+1) == -1 || my_grid(i+1,j+1) == 1 )
            continue;
        end
        
        if ( dir == 1 )
            dir_x(i,j) = -len;
        elseif (dir ==2 )
            dir_x(i,j) = len;
        elseif ( dir == 3)
            dir_y(i,j) = -len;
        else
            dir_y (i,j) = len;
        end
    end
end

set(gca,'XTick',[0.5:1:31.5],...
        'YTick',[0.5:1:31.5])
grid on;
title (['Grid ', num2str(sz), ' - solution']);

colormap(cols);
%plot V-values
%figure;  imagesc( reshape(V,sz,sz)' + wall_grid );
h = colorbar;
set(h, 'ylim', [0.01 1.7])
[ x y ] = meshgrid (1:sz,1:sz);
quiver (x,y,dir_x,dir_y );

hold off;

% another plot

% for the 4 one...

figure;
imagesc( reshape(V2,sz,sz)' + wall_grid );
hold on

dir_x = zeros(sz,sz);
dir_y = zeros(sz,sz);
len = 0.5;

for i=1:sz
    for j=1:sz
        
        dir = policy2( (i-1)*sz+j );
        
        if ( my_grid(i+1,j+1) == -1 || my_grid(i+1,j+1) == 1 )
            continue;
        end
        
        if ( dir == 1 )
            dir_x(i,j) = -len;
        elseif (dir ==2 )
            dir_x(i,j) = len;
        elseif ( dir == 3)
            dir_y(i,j) = -len;
        else
            dir_y (i,j) = len;
        end
    end
end

set(gca,'XTick',[0.5:1:31.5],...
        'YTick',[0.5:1:31.5])
grid on;
title (['Grid ', num2str(sz), ' - solution']);

colormap(cols);
%plot V-values
%figure;  imagesc( reshape(V,sz,sz)' + wall_grid );
h = colorbar;
set(h, 'ylim', [0.01 1.7])
[ x y ] = meshgrid (1:sz,1:sz);
quiver (x,y,dir_x,dir_y );

hold off;




