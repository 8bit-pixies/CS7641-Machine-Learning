function error = error_hill(x, net, inputs, targets)
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

% if output greater than 1 convert it to 1

pred = round(y);
pred = arrayfun(@(x) cust_ceil(x), pred);
error = confusion(pred, targets);

end