function [row col y] = confusion_hill(x, net, inputs, targets)

net = setwb(net, x');

% To evaluate the ouputs based on the given
% weights and biases vector
y = net(inputs);

% Calculating the mean squared error (targets could have multiple
% classification (i.e. targets could be 2 dim) so take sum twice)
% and divide it by number of entries
[row col] = size(y);



end