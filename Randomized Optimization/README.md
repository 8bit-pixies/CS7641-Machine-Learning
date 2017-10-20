The code used in this assignment relies on MATLAB. The MATLAB R2013a was used for all code in this assignment for the neural network portion. For the 3 problems ABAGAIL was used.

Required Software
-----------------

*  MATLAB R2013a   
*  ABAGAIL   
*  Jython   
*  Python 3.X  
*  R 3.1.X  


Data
----

The data set used is from the UCI machine learning data set, which is the Pima Indian diabetes data set. The whole data set, and the test and train data set are available in the "data" folder:

*  pima.csv : contains the whole data set  
*  pimatest.csv : is the validation/test data set  
*  pimatrain.csv : is the training data set  
*  pimatrain-1.csv : a subset of the training set, for variation of 2 fold cv
*  pimatrain-2.csv : a subset of the training set, for variation of 2 fold cv

This folder also contains the results from running the 3 problems, and is located in "data/logs". This directory also contains all the results of the exhaustive search performed by GA, MIMIC and SA. 

The exhaustive grid search performed by MATLAB for GA is provided in folder "data/logs". It has not been provided for SA since there were less than 10 combinations of parameters that were tested (fast, boltzman, exp at various levels). 

MATLAB 
------

**Neural Networks Only**

To run the code for determining optimal weights for neural networks, set MATLAB\nnet directory as the working directory,
and run 

*  gannet.m : for genetic algorithm
*  hillnnet.m : for randomized hill search
*  sannet2.m : for simulated annealing
*  sannet.m : for simulated annealling with restarts
*  backpropnnet.m : nnet solution using nnet

This directory includes the loss functions for each randomized optimization algorithm and the annealing function:

*  mse_ga.m : loss function for genetic algorithm  
*  mse_hill.m : loss function for hill climing  
*  mse_sa.m : loss function for simulated anneallng  
*  anneal.m : annealing function that is slightly modified. See here:  http://www.mathworks.com/matlabcentral/fileexchange/loadFile.do?objectId=10548&objectType=file, this file has been modified with the license.txt in the same directory.  
*  annealcv.m : annealing function that is slightly modified - this one for cross validation. See here:  http://www.mathworks.com/matlabcentral/fileexchange/loadFile.do?objectId=10548&objectType=file, this file has been modified with the license.txt in the same directory.  


Other helper scripts are within the same directory for monitoring and optimizing performance.

*  gaplotchange.m : custom plot function for GA using gaoptimset
*  gaplotvaliderror.m : custom plot function for GA using gaoptimset
*  psplotvaliderror.m : custom plot function for RHC using optimset


ABAGAIL
-------

The other 3 problems were completed using the ABAGAIL library. The source files were slightly modified and are provided in the ABAGAIL directory. The resulting jar is located in the root directory. To run the assignment, simply run the relevant "ABAGAIL/jython/run*.py". 

Be sure to change to parameters to perform the appropriate exhaustive grid search or problem size, and also point to the correct location of the *.jar file(s). The "run*.py" files were run in Python (CPython) version 3.

The modified files with ABAGAIL perform the following features (and more):

*  Evaluation functions to keep a count of the number of function evaluations.  
*  Double point cross over for genetic algorithms.  
*  Implementing Boltzmann and Fast cooling functions for SA.  




Results
-------

The results from the 3 problems are stored in "ABAGAIL/jython/log/*.csv". 

### Plotting Using R ###

Plot code is located in "R/" and requires R to run. To run the plots ensure you have the following R packages installed:

```
magrittr
dplyr
ggplot2
gtable
scales
gsubfn
gridExtra
```

The various scripts are described as follows:

*  cooling-temp-compare.r : used to generate the graph showing comparison of the various cooling functions for SA
*  countones.r : used to generate the graphs for count ones
*  fourpeaks.r : used to generate the graphs for four peaks.
*  knapsack.r : used to generate the graphs for knapsack.
*  bit2num-countones.r : used to generate the graphic about countones. 
*  utils.r : helper functions for the various scripts. 

