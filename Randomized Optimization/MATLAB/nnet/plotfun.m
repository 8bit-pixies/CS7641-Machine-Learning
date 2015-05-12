function stop = plotfun(x,optimValues)
        plot(optimValues.iteration,optimValues.fval,'g')
        stop=false;
    end