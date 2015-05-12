# plot performance
library(gridExtra)

plot1 <- plot(pima.model$ctree)
plot2 <- plot(wine.models$ctree)

grid.arrange(plot1, plot2, ncol=2)

# neural nets

#nnet.pima$results
#rearrange columsn in nnet.pima$results (the first two)
library(lattice)

plot1 <- plot(pima.model$pcannet)
plot2 <- plot(wine.models$nnetpca)

grid.arrange(plot1, plot2, ncol=2)


# boosting
library(magrittr)
pima.model[c("ada", "gbm")] %>% resamples %>% bwplot

plot1 <- plot(pima.model$ada)
plot2 <- plot(wine.models$gbm)

grid.arrange(plot1, plot2, ncol=2)

#svm

pima.model$svm$results$sigma <- round(pima.model$svm$results$sigma, 4)
wine.models$svm$results$sigma <- round(wine.models$svm$results$sigma, 4)

plot1 <- plot(pima.model$svm)
plot2 <- plot(wine.models$svm)

grid.arrange(plot1, plot2, ncol=2)

# knn 

plot1 <- plot(pima.model$knnpca)
plot2 <- plot(wine.models$knnpca)

grid.arrange(plot1, plot2, ncol=2)

####


