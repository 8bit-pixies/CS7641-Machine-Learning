#' 04 Pima-nnet
#' 
#' Generate optimal clusters for:
#' * no transformations
#' * PCA
#' * ICA
#' * RCA
#' * RF
#' 
#' Add them as features and reapply nnets

library(mlbench)
library(caret)
library(plyr)
library(dplyr)
library(reshape2)
library(GGally)
library(ggbiplot)
library(gtable)
library(mclust)
library(randomForest)

source("R/random_projection_gauss.R")

predict.kmeans <- function(km, data) {
  k <- nrow(km$centers)
  n <- nrow(data)
  d <- as.matrix(dist(rbind(km$centers, data)))[-(1:k),1:k]
  out <- apply(d, 1, which.min)
  return(out)
}

data(PimaIndiansDiabetes)
PID <- na.omit(PimaIndiansDiabetes)
PID_nl <- dplyr::select(PID, -diabetes)
PID_pp <- preProcess(PID_nl) %>% predict(PID_nl) # preprocessed dataframe, centered and scaled

# add features...
#' generate the 3 datasets with dim reduction
#' make sure we take the "best" one when using rca

PID_pca <- prcomp(PID_nl, retx=TRUE, scale. = TRUE, center=TRUE)
PIDpca <- as.data.frame(PID_pca$x[,1:6])
PIDpca$diabetes <- PIDpca$diabetes

PID_ica = fastICA::fastICA(PID_nl, n.comp=6, verbose=TRUE, row.norm=TRUE)
PIDica = cbind(as.data.frame(PID_ica$S), diabetes=PID$diabetes)

PID_rca <- Map(function(x) {
  gaussian_random_projection(PID_nl, 6)
}, 1:100)

# get the ones which immitate the result best.
pidrcadiff <- Map(function(x) {
  sum((PID_nl - (x$RP %*% MASS::ginv(x$R)))^2)
}, PID_rca) %>% melt

bestrca <- pidrcadiff %>% arrange(value) %>% head(1)
names(bestrca) <- c("value", "k")
PIDrca <- cbind(as.data.frame(PID_rca[[bestrca$k]]$RP), diabetes=PID$diabetes)

PID_rf <- randomForest(diabetes~., PID)
PID_rfdf <- as.data.frame(varImp(PID_rf))
PID_rfdf$names <- row.names(PID_rfdf)
PID_rfdf <- PID_rfdf %>% arrange(desc(Overall))
PID_rfdf <- PID_rfdf[,c("names", "Overall")] %>% head(6)

### generate features..
PIDnlkm <- predict.kmeans(kmeans(PID_nl,5), PID_nl)
PIDpcakm <- predict.kmeans(kmeans(PID_pca$x[,1:6],3), PID_pca$x[,1:6])
PIDicakm <- predict.kmeans(kmeans(PID_ica$S,2), PID_ica$S)
PIDrcakm <- predict.kmeans(kmeans(PID_rca[[bestrca$k]]$RP,3), PID_rca[[bestrca$k]]$RP)
PIDrfkm <- predict.kmeans(kmeans(PID_nl[,PID_rfdf$names],3), PID_nl[,PID_rfdf$names])

PIDnlem <- predict(Mclust(PID_nl,6), PID_nl)
PIDpcaem <- predict(Mclust(PID_pca$x[,1:6],4), PID_pca$x[,1:6])
PIDicaem <- predict(Mclust(PID_ica$S,7), PID_ica$S)
PIDrcaem <- predict(Mclust(PID_rca[[bestrca$k]]$RP,4), PID_rca[[bestrca$k]]$RP)
PIDrfem <- predict(Mclust(PID_nl[,PID_rfdf$names],8), PID_nl[,PID_rfdf$names])

PID_final <- PID
PID_final$NLKM <- PIDnlkm
PID_final$pcaKM <- PIDpcakm
PID_final$icaKM <- PIDicakm
PID_final$rcaKM <- PIDrcakm
PID_final$rfKM <- PIDrfkm

PID_final$NLEM <- PIDnlem$classification
PID_final$pcaEM <- PIDpcaem$classification
PID_final$icaEM <- PIDicaem$classification
PID_final$rcaEM <- PIDrcaem$classification
PID_final$rfEM <- PIDrfem$classification

# run caret::train on it with nnet
folds <- createFolds(PID_final$diabetes, k = 6, list = TRUE, returnTrain = FALSE)
train_ind <- c(folds$Fold1, folds$Fold2)
valid_ind <- c(folds$Fold3)
test_ind <- c(folds$Fold4)

#' generate tunegrid...grid was determined by a single run and has been
#' hardcoded for reproducibility

pnetGrid <- expand.grid(size=c(1,3,5,7,9,11,13,15,17,19), 
                        decay=c(0, 0.005, 0.01, 0.02,0.04,0.06,0.08,0.1))
tr <- trainControl(method="cv", savePredictions=TRUE, index=valid_ind, indexOut=test_ind)
#PID_model_train <- caret::train(diabetes~., PID_final, method="nnet", tuneGrid=pnetGrid, trControl=tr)
# train and plot performance (accuracy) for every single plot on the training, validation and test set

# change the reference parameter...
PID_final$diabetes <- relevel(PID_final$diabetes, ref="pos")
levels(PID_final$diabetes) <- c(1, 0)

PID$diabetes <- relevel(PID_final$diabetes, ref="pos")
levels(PID$diabetes) <- c(1,0)

train_models <- apply(pnetGrid, 1, function(x) {
  #return(nnet(diabetes~., PID_final[train_ind,], size=x[1], decay=x[2]))
  return(caret::train(diabetes~., PID_final[train_ind,], method="nnet", tuneGrid = data.frame(size=x[1], decay=x[2]),
                      trControl=caret::trainControl(method="cv")))  
})

# plot the performance of each model (shoudl be 90) sicne this is a grid search there is no real order.
train_perf <- Map(function(x) {
  pred <- predict(x, PID_final[train_ind,])
  return(caret::confusionMatrix(pred, PID_final[train_ind, c("diabetes")])$overall['Accuracy'])
}, train_models) %>% unlist

valid_perf <- Map(function(x) {
  pred <- predict(x, PID_final[valid_ind,])
  return(caret::confusionMatrix(pred, PID_final[valid_ind, c("diabetes")])$overall['Accuracy'])
}, train_models) %>% unlist

test_perf <- Map(function(x) {
  pred <- predict(x, PID_final[test_ind,])
  return(caret::confusionMatrix(pred, PID_final[test_ind, c("diabetes")])$overall['Accuracy'])
}, train_models) %>% unlist

# vanilla performance
train_models_v <- apply(pnetGrid, 1, function(x) {
  #return(nnet(diabetes~., PID[train_ind,], size=x[1], decay=x[2]))
  
  return(caret::train(diabetes~., PID[train_ind,], method="nnet", 
                      tuneGrid = data.frame(size=x[1], decay=x[2]),
               trControl=caret::trainControl(method="cv")))  
})

# plot the performance of each model (shoudl be 90) sicne this is a grid search there is no real order.
train_perf_v <- Map(function(x) {
  pred <- predict(x, PID[train_ind,])
  return(caret::confusionMatrix(pred, PID[train_ind, c("diabetes")])$overall['Accuracy'])
}, train_models_v) %>% unlist

valid_perf_v <- Map(function(x) {
  pred <- predict(x, PID[valid_ind,])
  return(caret::confusionMatrix(pred, PID[valid_ind, c("diabetes")])$overall['Accuracy'])
}, train_models_v) %>% unlist

test_perf_v <- Map(function(x) {
  pred <- predict(x, PID[test_ind,])
  return(caret::confusionMatrix(pred, PID[test_ind, c("diabetes")])$overall['Accuracy'])
}, train_models_v) %>% unlist

#performance...
# somehow plot that and you're done, compare with no additional clusters.
pnnet_perf <- cbind(as.data.frame(pnetGrid), 
                    train_perf=train_perf, 
                    valid_perf=valid_perf, 
                    test_perf=test_perf,
                    train_perf_v=train_perf_v, 
                    valid_perf_v=valid_perf_v, 
                    test_perf_v=test_perf_v)

#' select the best value based on validation for the one with additional features and plot
#' that slice keeping decay constant.

#decay = 0.080...
pnnet_cons <- select(pnnet_perf[pnnet_perf$decay ==0.005,], -decay)
pnnet_melt <- reshape2::melt(pnnet_cons, "size")

xlim_points <- c(floor(min(pnnet_melt$value)*100)/100, ceiling(max(pnnet_melt$value)*100)/100)

g1 <- ggplot(pnnet_melt[pnnet_melt$variable %in% c("train_perf", "valid_perf", "test_perf"),], 
       aes(x=size, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Neural Net Performance - With Clusters as Features") +
  ylab("Accuracy") + 
  xlab("Number of hidden layers - decay constant at 0.005") +
  scale_y_continuous(limits = xlim_points) +
  scale_colour_discrete(name="",
                        breaks=c("train_perf", "valid_perf", "test_perf"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()

g2 <- ggplot(pnnet_melt[pnnet_melt$variable %in% c("train_perf_v", "valid_perf_v", "test_perf_v"),], 
             aes(x=size, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Neural Net Performance - No Additional Features") +
  ylab("Accuracy") + 
  xlab("Number of hidden layers - decay constant at 0.005") +
  scale_y_continuous(limits = xlim_points) +
  scale_colour_discrete(name="",
                        breaks=c("train_perf_v", "valid_perf_v", "test_perf_v"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()

library(gridExtra)
grid.arrange(g1, g2, ncol=2)

# calculate the avverage training time for `train_models` and `train_models_v`
t_times <- Map(function(x) {x$times$everything[['elapsed']]}, train_models) %>% unlist %>% sum
t_times_v <- Map(function(x) {x$times$everything[['elapsed']]}, train_models_v) %>% unlist %>% sum






