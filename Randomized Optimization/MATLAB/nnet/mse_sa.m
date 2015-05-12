function [mse_calc pred] = mse_sa(x, net, inputs, targets)
% 'x' contains the weights and biases vector
% in row vector form as passed to it by the
% genetic algorithm. This must be transposed
% when being set as the weights and biases
% vector for the network.

% To set the weights and biases vector to the
% one given as input
net = setwb(net, x');

% To evaluate the ouputs based on the given
% weights and biases vector
y = net(inputs);

% Calculating the mean squared error (targets could have multiple
% classification (i.e. targets could be 2 dim) so take sum twice)
% and divide it by number of entries
[row col] = size(y);
mse_calc = sum(sum((y - targets).^2))/(row * col);
pred = round(y);
end