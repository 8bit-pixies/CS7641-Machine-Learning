% cd('C:/Users/Chapman/Documents/GATECH/ML-7641/Assignment2/MATLAB')
% read PIMA indian data into matlab
PIMA = csvread('../../data/pimatrain.csv');
inputs = PIMA(:, 1:end-1);
targets = PIMA(:, end);


% number of neurons
n = 10;

% copy from github pushkar's ml stuff
[n_attr, ~]  = size(inputs');
[n_class, ~] = size(targets');

net = feedforwardnet(n); %initialize with default neurons which is 10

net = configure(net, inputs', targets')

h = @(x) mse_sa(x, net, inputs', targets');

% Unbounded
% placed bounding...
lb = -6;
ub = 6;

% Add 'Display' option to display result of iterations
sa_opts = saoptimset('AnnealingFcn', @annealingboltz, 'TemperatureFcn',@temperatureboltz, ...
    'ReannealInterval',100^100, ...
    'PlotFcns', {@saplotbestf,@saplotbestx,@saplotf,@saplotx,  @saplotstopping, @saplottemperature});

% are total of n_attr*n input weights (uniform weight)
initial_il_weights = ones(1, n_attr*n)/(n_attr*n);
% There are n bias values, one for each neuron (random)
initial_il_bias    = randn(1, n);
% There is n_class output, so there are total of n_class*n output weights 
% (uniform weight)
initial_ol_weights = ones(1, n_class*n)/(n_class*n);
% There are n_class bias values, one for each output neuron (random)
initial_ol_bias    = randn(1, n_class);
% starting values
starting_values = [initial_il_weights, initial_il_bias, ...
                   initial_ol_weights, initial_ol_bias];

[x_sa, fval, flag, output] = simulannealbnd(h, starting_values, [],[], sa_opts);

% now try to figure out the prediction error for net.
PIMATEST = csvread('../../data/pimatest.csv');
inputs_test = PIMATEST(:, 1:end-1);
targets_test = PIMATEST(:, end);

[err_sa pred_sa] = mse_sa(x_sa', net, inputs_test', targets_test')
confusionmat(pred_sa, targets_test')
confusion(pred_sa, targets_test')

