function state = gaplotvaliderror(options,state,flag, net, inputs, targets, input_tests, target_tests)
%PSplotTrainErrorF PlotFcn to plot best function value.
%   STOP = PSplotTrainErrorF(OPTIMVALUES,FLAG) where OPTIMVALUES is a structure
%   with the following fields:
%              x: current point X
%      iteration: iteration count
%           fval: function value
%       meshsize: current mesh size
%      funccount: number of function evaluations
%         method: method used in last iteration
%         TolFun: tolerance on function value in last iteration
%           TolX: tolerance on X value in last iteration
%
%   FLAG: Current state in which PlotFcn is called. Possible values are:
%           init: initialization state
%           iter: iteration state
%           done: final state
%
%   STOP: A boolean to stop the algorithm.
%
%   See also PATTERNSEARCH, GA, PSOPTIMSET.


%   Copyright 2003-2005 The MathWorks, Inc.
%   $Revision: 1.1.6.3 $  $Date: 2012/08/21 00:22:28 $

switch flag
    case 'init'
        [~,i] = min(state.Score);
        xarray = double(state.Population(i,:));

        error = error_hill(xarray', net, inputs, targets);
        error_test = error_hill(xarray', net, input_tests, target_tests);
        
        hold on;
        set(gca,'xlim',[0,options.Generations]);
        plotTrainError = plot(state.Generation, error,'.b');
        set(plotTrainError, 'Tag', 'gaplottrainerror');
        plotTestError = plot(state.Generation, error_test,'.g');
        set(plotTestError, 'Tag', 'gaplottesterror');
        
        xlabel('Generation','interp','none'); 
        ylabel('Error','interp','none')
        title(['Training Error: ', ' Test Error: '],'interp','none');
    case 'iter'
        [~,i] = min(state.Score);
        xarray = double(state.Population(i,:));

        error = error_hill(xarray', net, inputs, targets);
        error_test = error_hill(xarray', net, input_tests, target_tests);
        
        plotTrainError = findobj(get(gca,'Children'),'Tag','gaplottrainerror');
        plotTestError = findobj(get(gca,'Children'),'Tag','gaplottesterror');
        
        newX = [get(plotTrainError,'Xdata') state.Generation];
        newY = [get(plotTrainError,'Ydata') error];
        set(plotTrainError,'Xdata',newX, 'Ydata',newY);
        
        newY = [get(plotTestError,'Ydata') error_test];
        set(plotTestError,'Xdata',newX, 'Ydata',newY);
        
        set(get(gca,'Title'),'String',sprintf('Training Error: %g Test Error: %g',error, error_test));
        
    case 'done'
        Legd = legend('Training Error','Test Error');
        set(Legd, 'FontSize', 8);
        hold off;
end
