
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

my_grid = grid_7;
my_grid = grid_13;

%prepare
sz = size(my_grid,1)-2;
[P,R] = grid_to_MDP (my_grid,0.9); % when prob = 0.9

%discount = 0.9;
discount = 0.6;

%solve
tic;
[V, policy, iter, cpu_time] = mdp_value_iteration ( P, R, discount );
toc;tic;
[V1, policy1, iter1, cpu_time1] = mdp_policy_iteration ( P, R, discount ); % changing discount rate affects this maze
toc;tic;
[V2, policy2, g, iter2, cpu_time2] = mdp_relative_value_iteration(P, R, discount );
toc;tic;
[V3, policy3, cpu_time3] = mdp_LP(P, R, discount );
toc;

isequal(policy, policy1)
isequal(policy, policy2)

display ( [ num2str(iter) '  &  ' num2str(cpu_time) ] );
display ( [ num2str(iter1) '  &  ' num2str(cpu_time1) ] );
display ( [ num2str(iter2) '  &  ' num2str(cpu_time2) ] );

%plot policy

% get walls...
wall_grid = abs(my_grid(2:sz+1,2:sz+1))*-1;
wall_grid(1,1) = 2;
wall_grid(sz,sz) = 2;

cols = hot(10);
cols(1,:) = [0,0,0];
cols(10,:) = [1,1,1];

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

[ x y ] = meshgrid (1:sz,1:sz);
quiver (x,y,dir_x,dir_y );
% for i=1:sz
%     for j=1:sz
%         if (V( (i-1)*sz+j ) ~= 0)
%             text( j-0.4, i-0.3, num2str(V( (i-1)*sz+j ), '%10.2f' ), 'BackgroundColor',[1 1 1] );
%         end
%     end
% end

hold off;

% V1
figure;
imagesc( reshape(V1,sz,sz)' + wall_grid );
hold on

dir_x = zeros(sz,sz);
dir_y = zeros(sz,sz);
len = 0.5;

for i=1:sz
    for j=1:sz
        
        dir = policy1( (i-1)*sz+j );
        
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

[ x y ] = meshgrid (1:sz,1:sz);
quiver (x,y,dir_x,dir_y );
% for i=1:sz
%     for j=1:sz
%         if (V( (i-1)*sz+j ) ~= 0)
%             text( j-0.4, i-0.3, num2str(V( (i-1)*sz+j ), '%10.2f' ), 'BackgroundColor',[1 1 1] );
%         end
%     end
% end

hold off;


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
cols = hot(4);
cols(1,:) = [0,0,0];
cols(2,:) = [0.5,0.5,0.5];
cols(3,:) = [0.5,0.5,0.5];
cols(4,:) = [1,1,1];

colormap(cols);

[ x y ] = meshgrid (1:sz,1:sz);
quiver (x,y,dir_x,dir_y );


hold off;

%LP solution sould be unchanged...
figure;
imagesc( reshape(V3,sz,sz)' + wall_grid );
hold on

dir_x = zeros(sz,sz);
dir_y = zeros(sz,sz);
len = 0.5;

for i=1:sz
    for j=1:sz
        
        dir = policy3( (i-1)*sz+j );
        
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
cols = hot(4);
cols(1,:) = [0,0,0];
cols(2,:) = [0.5,0.5,0.5];
cols(3,:) = [0.5,0.5,0.5];
cols(4,:) = [1,1,1];

colormap(cols);

[ x y ] = meshgrid (1:sz,1:sz);
quiver (x,y,dir_x,dir_y );


hold off;





