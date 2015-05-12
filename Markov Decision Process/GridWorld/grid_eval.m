function [ B, s ] = grid_eval( grid, i ,j )

B = ones(1,4);

if ( grid(i,j-1) == -1 )
    B(1) = 0;
end
if ( grid(i,j+1) == -1 )
    B(2) = 0;
end
if ( grid(i-1,j) == -1 )
    B(3) = 0;
end
if ( grid(i+1,j) == -1 )
    B(4) = 0;
end

s = sum(B);
