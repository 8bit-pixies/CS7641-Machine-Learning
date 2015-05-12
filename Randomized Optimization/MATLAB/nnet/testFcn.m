banana = @(x)100*(x(2)-x(1)^2)^2+(1-x(1))^2;

[x,fval] = fminsearch(banana,[-1.2, 1], 'PlotFcns',plotfun)