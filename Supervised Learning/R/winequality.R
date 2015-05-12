# load the library
library(mlbench)
library(caret)
library(reshape)
library(plyr)
library(kernlab)
# load the dataset

run_analysis <- function(train.data, formula, y, control) {
  # prepare training scheme

  
  # train the rpart model
  set.seed(1123)
  modelctreepca <- train(formula, data=train.data, method="ctree", trControl=control, tuneLength=10, preProcess="pca")
  modelctree <- train(formula, data=train.data, method="ctree", trControl=control, tuneLength=10)
  modelctree2pca <- train(formula, data=train.data, method="ctree", trControl=control, tuneLength=10, preProcess="pca")
  modelctree2 <- train(formula, data=train.data, method="ctree", trControl=control, tuneLength=10)
  
  # train the nnet model
  set.seed(1123)
  modelnnet <- train(formula, data=train.data, method="nnet", trControl=control, trace=FALSE, 
                     tuneGrid=expand.grid(decay=seq(from=0, to=0.95, length.out=20), size=seq(from=0, to=20, by=2)))
  modelnnetpca <- train(formula, data=train.data, method="nnet", trControl=control, trace=FALSE, 
                        tuneGrid=expand.grid(decay=seq(from=0, to=0.95, length.out=20), size=seq(from=0, to=20, by=2)), 
                        preProcess="pca")
  modelpcannet <- train(formula, data=train.data, method="pcaNNet", trControl=control, trace=FALSE, 
                        tuneGrid=expand.grid(decay=seq(from=0, to=0.95, length.out=20), size=seq(from=0, to=20, by=2)))
  
  # train the GBM model
  set.seed(1123)
  modelgbmpca <- train(formula, data=train.data, method="gbm", trControl=control, verbose=FALSE, tuneLength=10, preProcess="pca")
  modelgbm <- train(formula, data=train.data, method="gbm", trControl=control, verbose=FALSE, tuneLength=10)
  
  # train the SVM model
  set.seed(1123)
  sigDist <- sigest(quality ~., data=train.data, frac=1)  
  modelsvm <- train(formula, data=train.data, method="svmRadial", trControl=control, 
                    tuneGrid=expand.grid(sigma=seq(from=sigDist[3], to=sigDist[1], length.out=10), C=2^(-2:7)))
  modelsvmpca <- train(formula, data=train.data, method="svmRadial", trControl=control, 
                    tuneGrid=expand.grid(sigma=seq(from=sigDist[3], to=sigDist[1], length.out=10), C=2^(-2:7)), preProcess="pca")
  
  # train the knn model
  set.seed(1123)
  modelknnpca <- train(formula, data=train.data, method="knn", trControl=control, tuneLength=10, preProcess="pca")
  modelknn <- train(formula, data=train.data, method="knn", trControl=control, tuneLength=10)
  
  modellist <- list(ctreepca=modelctreepca, ctree=modelctree, ctree2pca=modelctree2pca, ctree2=modelctree2, 
                    nnet=modelnnet, nnetpca=modelnnetpca, pcannet=modelpcannet, 
                    gbm=modelgbm, gbmpca=modelgbmpca, 
                    svm=modelsvm, svmpca=modelsvmpca, 
                    knn=modelknn, knnpca=modelknnpca
                    )
  
  return(modellist)
}

run_resample <- function(modellist) {
  # collect resamples
  results <- resamples(modellist)
  
  # summarize the distributions  
  resample.acc <- results$values[,grep("Accuracy$", names(results$values))]
  resample.k<- results$values[,grep("Kappa$", names(results$values))]
  names(resample.acc) <- names(modellist)
  names(resample.k) <- names(modellist)
  resample.acc$measure <- "Accuracy"
  resample.k$measure <- "Kappa"
  resample.acc <- melt(resample.acc)
  resample.k <- melt(resample.k)
  
  resample.data <- rbind(resample.acc, resample.k)
  
  return(resample.data)
}

run_confusion <- function(modellist, test.data, y) {
  # check confusion matrix
  for (model in modellist) {
    fit.test <- predict(model, newdata=test.data[,!(names(test.data) %in% c(y))])    
    cm <- confusionMatrix(fit.test, test.data[,names(test.data) %in% c(y)])
    print(model$method)
    print(cm)
  }
  return(Map(function(model) {
    fit.test <- predict(model, newdata=test.data[,!(names(test.data) %in% c(y))])    
    cm <- confusionMatrix(fit.test, test.data[,names(test.data) %in% c(y)])    
    return(as.list(cm))
  }, modellist))
}

run_confusion_plotdata <- function(modellist.cm) {
  cm.data = Map(function(model) {
    table <- as.matrix(model$table)
    dtable <- dim(table)[1]
    table <- matrix(table, dtable, dtable)
    x <- sum(diag(table))
    n <- sum(table)  
    p <- as.list(model$overall)$AccuracyNull
    
    b90 <- binom.test(x, n, p, conf.level=.90)
    b95 <- binom.test(x, n, p, conf.level=.95)
    
    return(list(mean=as.list(model$overall)$Accuracy, lower90 = b90$conf.int[1], upper90 = b90$conf.int[2],
                lower95=b95$conf.int[1], upper95=b95$conf.int[2]))
  }, modellist.cm)
  
  plot.data <- data.frame()
  
  cm.name <- factor(names(modellist.cm))
  
  for(x in 1:length(names(modellist.cm))){
    row.name = cm.data[[x]]
    row.name$model = names(modellist.cm)[x]
    plot.data <- rbind(plot.data, row.name)
    plot.data$model <- factor(plot.data$model, levels=names(modellist.cm))
  }
  
  #plot the data
  return(plot.data)
}


# data(PimaIndiansDiabetes)
# run_analysis(PimaIndiansDiabetes, diabetes~., "diabetes")

wq.red <- read.table(file.path(getwd(), "data", "winequality", "winequality-red.csv"), 
                     sep=";",
                     header=TRUE)
# discard obs less than 5 and greater than 7, since there isn't enough information
wq.red <- na.omit(wq.red)
wq.red <- wq.red[wq.red$quality %in% c(5,6,7),]
wq.red$quality <- factor(wq.red$quality, ordered=TRUE)

# split the data set into train and test
set.seed(1123)

train.data <- stratified(wq.red, "quality", 0.7)
test.data <- wq.red[-as.numeric(row.names(train.data)),]

control <- trainControl(method="repeatedcv", number=10, repeats=3, 
                        index=createFolds(train.data[,names(train.data) %in% c("quality")]))

wine.models <- run_analysis(train.data, quality~., "quality", control)

wine.cm <- run_confusion(wine.models, test.data, "quality")
wine.pdata <- run_confusion_plotdata(wine.cm)
wine.pdata$model <- factor(wine.pdata$model, levels=wine.pdata[order(wine.pdata$mean), "model"])

wine.resample <- run_resample(wine.models)


# plot performance of the tuning
#run_performance(wine.models)

#
ddply(wine.resample[wine.resample$measure=="Accuracy",], .(measure, variable), summarize, mean=round(mean(value, na.rm=TRUE), digits=4))[, c("variable", "mean")]
ddply(wine.resample[wine.resample$measure=="Kappa",], .(measure, variable), summarize, mean=round(mean(value, na.rm=TRUE), digits=4))[, c("variable", "mean")]
wine.pdata$mean <- round(wine.pdata$mean, digits=4)
wine.pdata[, c("mean", "model")]

# print out the times...
Map(function(model) {
  model$times
}, wine.models)

