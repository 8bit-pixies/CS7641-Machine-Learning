% sample sa

function y = simple_objective(x)
   y = (4 - 2.1*x(1)^2 + x(1)^4/3)*x(1)^2 + x(1)*x(2) + ...
       (-4 + 4*x(2)^2)*x(2)^2;