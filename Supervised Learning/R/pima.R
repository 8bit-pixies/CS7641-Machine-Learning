# load the library
library(mlbench)
library(caret)
library(reshape)
library(plyr)
library(reshape)

library(kernlab)
# load the dataset

run_analysis <- function(train.data, formula, y, control) {
  # prepare training scheme
  
  # train the rpart model
  set.seed(1123)
  modelctree <- train(diabetes ~., data=train.data, method="ctree", trControl=control, tuneLength=10) # compare with rpart implementation
  modelctree2 <- train(diabetes ~., data=train.data, method="ctree2", trControl=control, tuneLength=10) # compare with rpart implementation
  modelctreepca <- train(diabetes ~., data=train.data, method="ctree", trControl=control, tuneLength=10, preProcess="pca") # compare with rpart implementation
  modelctree2pca <- train(diabetes ~., data=train.data, method="ctree2", trControl=control, tuneLength=10, preProcess="pca") # compare with rpart implementation
    
  
  # train the nnet model
  set.seed(1123)
  my.grid <- expand.grid(.decay = (10:20)/100, .size = c(3))
  #modelnnet <- train(formula, data=train.data, method="multinom", tuneLength=20, trControl=control, trace=FALSE, maxit=1000)
  #modelnnet <- train(formula, data=train.data, method="nnet", tuneGrid=my.grid, trControl=control, trace=FALSE, maxit=1000)
  modelnnetpca <- train(formula, data=train.data, method="nnet", 
                     tuneGrid=expand.grid(decay=seq(from=0, to=0.6, length.out=20), size=seq(from=0, to=20, by=2)),
                     trControl=control, trace=FALSE, preProcess="pca")
  modelpcannet <- train(formula, data=train.data, method="pcaNNet", 
                     tuneGrid=expand.grid(decay=seq(from=0, to=0.6, length.out=20), size=seq(from=0, to=20, by=2)),
                     trControl=control, trace=FALSE)
  modelnnet <- train(formula, data=train.data, method="nnet", 
                     tuneGrid=expand.grid(decay=seq(from=0, to=0.6, length.out=20), size=seq(from=0, to=20, by=2)),
                     trControl=control, trace=FALSE)
  
  # train the GBM model
  set.seed(1123)
  modelada <- train(formula, data=train.data, method="gbm", trControl=control, verbose=FALSE, tuneLength=10, distribution="adaboost")
  modelgbm <- train(formula, data=train.data, method="gbm", trControl=control, verbose=FALSE, tuneLength=10)
  
  modeladapca <- train(formula, data=train.data, method="gbm", trControl=control, verbose=FALSE, tuneLength=10, preProcess="pca")
  modelgbmpca <- train(formula, data=train.data, method="gbm", trControl=control, verbose=FALSE, tuneLength=10, preProcess="pca")
  
  # train the SVM model
  set.seed(1123)
  # modelsvm <- train(formula, data=train.data, method="svmRadial", trControl=control,tuneLength=10, preProcess="pca")
  sigDist <- sigest(formula, data = train.data, frac = 1)
  modelsvmpca <- train(formula, data=train.data, method="svmRadial", trControl=control, 
                    tuneGrid=expand.grid(sigma=seq(from=sigDist[3], to=sigDist[1], length.out=10), C=2^(-2:7)), preProcess="pca")
  modelsvm <- train(formula, data=train.data, method="svmRadial", trControl=control, 
                    tuneGrid=expand.grid(sigma=seq(from=sigDist[3], to=sigDist[1], length.out=10), C=2^(-2:7)))
  # train the knn model
  set.seed(1123)
  
  modelknnpca <- train(formula, data=train.data, method="knn", trControl=control, tuneLength=10, preProcess="pca")
  modelknn <- train(formula, data=train.data, method="knn", trControl=control, tuneLength=10)
  
  modellist <- list(ctreepca=modelctreepca, ctree=modelctree, ctree2pca=modelctree2pca, ctree2=modelctree2, 
                    nnetpca=modelnnetpca, pcannet=modelpcannet, nnet=modelnnet, 
                    ada=modelada, gbm=modelgbm, adapca=modeladapca, gbmpca=modelgbmpca, 
                    svm=modelsvm, svmpca=modelsvmpca, 
                    knn=modelknn, knnpca=modelknnpca)
  
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
  return(Map(function(model) {
    fit.test <- predict(model, newdata=test.data[,!(names(test.data) %in% c(y))])    
    cm <- caret::confusionMatrix(fit.test, test.data[,names(test.data) %in% c(y)])    
    return(as.list(cm))
  }, modellist))
}

run_performance <- function(modellist) {
  for(model in modellist) {
    print(model)
    print(plot(model, main=model$method))
    print(plot(model, metric="Kappa", main=model$method))
  }
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
data(PimaIndiansDiabetes)
PimaIndiansDiabetes <- na.omit(PimaIndiansDiabetes)

# split the data set into train and test
set.seed(1123)

train.data <- stratified(PimaIndiansDiabetes, "diabetes", 0.7)
test.data <- PimaIndiansDiabetes[-as.numeric(row.names(train.data)),]

control <- trainControl(method="repeatedcv", number=10, repeats=3, 
                        index=createFolds(train.data[,names(train.data) %in% c("diabetes")]))

pima.model <- run_analysis(train.data, diabetes~., "diabetes", control)
pima.resample <- run_resample(pima.model)
pima.cm <- run_confusion(pima.model, test.data, "diabetes")
pima.pdata <- run_confusion_plotdata(pima.cm)

ddply(pima.resample[pima.resample$measure=="Accuracy",], .(measure, variable), summarize, mean=round(mean(value), digits=4))[, c("variable", "mean")]
ddply(pima.resample[pima.resample$measure=="Kappa",], .(measure, variable), summarize, mean=round(mean(value), digits=4))[, c("variable", "mean")]
pima.pdata$mean <- round(pima.pdata$mean, digits=4)
pima.pdata[, c("mean", "model")]

#print out the times
Map(function(model) {
  model$times
}, pima.model)