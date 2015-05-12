To be able to run this code you will need R to be installed.

http://www.r-project.org/

All code assumes that the working directory is where this file, README.txt resides. The structure of the folder is assumed to be as follows.

All R code is in the folder called "R", and the external data is located in the folder called "data". 


  csiu8
  │   Assignment1.Rproj
  │   README.txt
  │
  ├───data
  │   └───winequality
  │           winequality-red.csv
  │           winequality-white.csv
  │           winequality.names
  │
  └───R
          pima.R
          winequality.R



The following R packages are also required:

*  caret
*  mlbench
*  lattice
*  ggplot2
*  rpart
*  nnet
*  gbm
*  survival
*  splines
*  parallel
*  plyr  
*  kernlab  
*  reshape  
*  party
*  reshape2
*  e1071

All requirements can be easily installed by running

    install.packages("caret")
    install.packages("mlbench")
    install.packages("lattice")
    install.packages("ggplot2")
    install.packages("rpart")
    install.packages("nnet")
    install.packages("gbm")
    install.packages("survival")
    install.packages("splines")
    install.packages("parallel")
    install.packages("plyr")
    install.packages("kernlab")
    install.packages("reshape")
	install.packages("e1071")

To run the code, run:

*  winequality.r
*  pima.r

For the analysis that was produced. To run the graphs for the assignment, after the two files have been run, you can run:

*  plots.r
*  plotperformance.r



