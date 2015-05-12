
tic;
% cd('C:/Users/Chapman/Documents/GATECH/ML-7641/Assignment2/MATLAB')
% read PIMA indian data into matlab
PIMA = csvread('../../data/pimatrain.csv');
inputs = PIMA(:, 1:end-1);
targets = PIMA(:, end);

% now try to figure out the prediction error for net.
PIMATEST = csvread('../../data/pimatest.csv');
inputs_test = PIMATEST(:, 1:end-1);
targets_test = PIMATEST(:, end);

% INITIALIZE THE NEURAL NETWORK PROBLEM %
% inputs for the neural net
tic
% number of neurons
n = 10;
net = feedforwardnet(n); %initialize with default neurons which is 10
% configure the neural network for this dataset
net = configure(net, inputs', targets')
% create handle to the MSE_TEST function, that
% calculates MSE

h = @(x) mse_hill(x, net, inputs', targets');

%cust plot fun
plottrainfun = @(optimvalues,flag) psplotvaliderror(optimvalues,flag, net, inputs', targets', ...
    inputs_test', targets_test');

% Setting the Genetic Algorithms tolerance for
% Add 'Display' option to display result of iterations
ps_opts = psoptimset ('CompletePoll', 'off', 'Display', 'iter', 'TimeLimit', 1200, 'TolX', 1e-6,...
    'PlotFcns', {@psplotbestf, @psplotfuncount, @psplotmeshsize, @psplotbestx, plottrainfun});

% copy from github pushkar's ml stuff
[n_attr, ~]  = size(inputs');
[n_class, ~] = size(targets');

% are total of n_attr*n input weights (uniform weight)
initial_il_weights = ones(1, n_attr*n)/(n_attr*n);
% There are n bias values, one for each neuron (random)
initial_il_bias    = rand(1, n);
% There is n_class output, so there are total of n_class*n output weights 
% (uniform weight)
initial_ol_weights = ones(1, n_class*n)/(n_class*n);
% There are n_class bias values, one for each output neuron (random)
initial_ol_bias    = rand(1, n_class);
% starting values
starting_values = [initial_il_weights, initial_il_bias, ...
                   initial_ol_weights, initial_ol_bias];


               
[x_hill, fval, flag, hill_output] = patternsearch(h, starting_values, [], [],[],[], -1e5, 1e5, ps_opts);

toc

[err pred_train] = mse_hill(x_hill', net, inputs', targets')
confusion(pred_train, targets')
[err pred] = mse_hill(x_hill', net, inputs_test', targets_test')
confusionmat(pred, targets_test')
confusion(pred, targets_test')

t=toc