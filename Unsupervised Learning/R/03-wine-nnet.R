# 03 wine-nnet

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

wq <- read.table(file.path(getwd(), "data", "winequality", "winequality-red.csv"), 
                 sep=";",
                 header=TRUE)
# discard obs less than 5 and greater than 7, since there isn't enough information
wq <- na.omit(wq)
wq <- wq[wq$quality %in% c(5,6,7),]
wq$quality <- factor(wq$quality, ordered=TRUE)
wq_nl <- dplyr::select(wq, -quality)
wq_pp <- preProcess(wq_nl) %>% predict(wq_nl)

#' PCA
wq_pca <- prcomp(wq_nl, retx=TRUE, scale.=TRUE, center=TRUE)
wq_data <- data.frame(PC=1:length(wq_pca$sdev), Variance=wq_pca$sdev^2)
wq_propvar <- (wq_pca$sdev^2/sum(wq_pca$sdev^2)) %>% cumsum
wq_propvar <- data.frame(x=1:length(wq_propvar), cumvar=wq_propvar)
wqpca <- cbind(as.data.frame(wq_pca$x), quality=wq$quality)

#' ICA
wq_ica = fastICA::fastICA(wq_nl, n.comp=11, verbose=TRUE, row.norm=TRUE)
wqica=cbind(as.data.frame(wq_ica$S), quality=wq$quality)
wqn <- sapply(wqica[,!(names(wqica) %in% c("quality"))], function(x) (e1071::kurtosis(x))^2)
wqn <- melt(wqn)
wqn$name <- row.names(wqn)
wqn <- wqn[wqn$value < 1,]
# remove all...in wqn
wqica <- wqica[,!(names(wqica) %in% wqn$name)]

#' random projections
wq_rca <- Map(function(x) {
  gaussian_random_projection(wq_nl, 8)
}, 1:100)

# get the ones which immitate the result best.
wqrcadiff <- Map(function(x) {
  sum((PID_nl - (x$RP %*% MASS::ginv(x$R)))^2)
}, PID_rca) %>% melt

bestrca <- wqrcadiff %>% arrange(value) %>% head(1)
names(bestrca) <- c("value", "k")
wqrca <- cbind(as.data.frame(wq_rca[[bestrca$k]]$RP), quality=wq$quality)

# apply randomforest to get the mean gini, variable importance.
wq_rf <- randomForest(quality ~., wq)
wqrf <- as.data.frame(varImp(wq_rf))
wqrf$names <- row.names(wqrf)
wqrf <- wqrf %>% arrange(desc(Overall))
wqrf <- wqrf[,c("names", "Overall")]
wqrf.name <- wqrf$names[1:(length(wqrf$names)-2)]
wqrfdf <- wq[,c(wqrf.name, "quality")]

# c(dim(wqnilem$z)[2], dim(wqpcaem$z)[2], dim(wqicaem$z)[2], 
#   dim(wqrcaem$z)[2], dim(wqrfem$z)[2])

wqnlkm <- predict.kmeans(kmeans(wq_nl,5), wq_nl)
wqpcakm <- predict.kmeans(kmeans(dplyr::select(wqpca, -quality),3), dplyr::select(wqpca, -quality))
wqicakm <- predict.kmeans(kmeans(dplyr::select(wqica,-quality),2), dplyr::select(wqica,-quality))
wqrcakm <- predict.kmeans(kmeans(dplyr::select(wqrca, -quality),6), dplyr::select(wqrca, -quality))
wqrfkm <- predict.kmeans(kmeans(dplyr::select(wqrfdf,-quality),4), dplyr::select(wqrfdf,-quality))

wqnlem <- predict(Mclust(wq_nl,7), wq_nl)
wqpcaem <- predict(Mclust(dplyr::select(wqica, -quality),8), dplyr::select(wqica, -quality))
wqicaem <- predict(Mclust(dplyr::select(wqica,-quality),10), dplyr::select(wqica,-quality))
wqrcaem <- predict(Mclust(dplyr::select(wqrca, -quality),4), dplyr::select(wqrca, -quality))
wqrfem <- predict(Mclust(dplyr::select(wqrfdf,-quality),13), dplyr::select(wqrfdf,-quality))


wq_final <- wq
wq_final$NLKM <- wqnlkm
wq_final$pcaKM <- wqpcakm
wq_final$icaKM <- wqicakm
wq_final$rcaKM <- wqrcakm
wq_final$rfKM <- wqrfkm

wq_final$NLEM <- wqnlem$classification
wq_final$pcaEM <- wqpcaem$classification
wq_final$icaEM <- wqicaem$classification
wq_final$rcaEM <- wqrcaem$classification
wq_final$rfEM <- wqrfem$classification

wq_final <- na.omit(wq_final)

# run caret::train on it with nnet
folds <- createFolds(wq_final$quality, k = 6, list = TRUE, returnTrain = FALSE)
train_ind <- c(folds$Fold1, folds$Fold2)
valid_ind <- c(folds$Fold3)
test_ind <- c(folds$Fold4)

pnetGrid <- expand.grid(size=c(1,3,5,7,9,11,13,15,17,19), 
                        decay=c(0, 0.005, 0.01, 0.02,0.04,0.06,0.08,0.1))
# wq_model_train <- caret::train(quality~., wq_final[train_ind,], method="nnet", 
#                                tuneGrid=pnetGrid,
#                                trControl=trainControl(savePredictions=TRUE))

train_models <- apply(pnetGrid, 1, function(x) {
  #return(nnet(quality~., wq_final[train_ind,], size=x[1], decay=x[2]))
  return(caret::train(quality~., data=wq_final[train_ind,], method="nnet",
                      tuneGrid = data.frame(size=x[1], decay=x[2]),
                      trControl=caret::trainControl(method="cv")))
})

train_perf <- Map(function(x) {
  pred <- predict(x, wq_final[train_ind,])
  return(caret::confusionMatrix(pred, wq_final[train_ind, c("quality")])$overall['Accuracy'])
}, train_models) %>% unlist

valid_perf <- Map(function(x) {
  pred <- predict(x, wq_final[valid_ind,])
  return(caret::confusionMatrix(pred, wq_final[valid_ind, c("quality")])$overall['Accuracy'])
}, train_models) %>% unlist

test_perf <- Map(function(x) {
  pred <- predict(x, wq_final[test_ind,])
  return(caret::confusionMatrix(pred, wq_final[test_ind, c("quality")])$overall['Accuracy'])
}, train_models) %>% unlist

# vanilla performance...
train_models_v <- apply(pnetGrid, 1, function(x) {
  #return(nnet(quality~., wq[train_ind,], size=x[1], decay=x[2]))
  return(caret::train(quality~., data=wq[train_ind,], method="nnet",
                      tuneGrid = data.frame(size=x[1], decay=x[2]),
                      trControl=caret::trainControl(method="cv")))
})

train_perf_v <- Map(function(x) {
  pred <- predict(x, wq_final[train_ind,])
  return(caret::confusionMatrix(pred, wq_final[train_ind, c("quality")])$overall['Accuracy'])
}, train_models_v) %>% unlist

valid_perf_v <- Map(function(x) {
  pred <- predict(x, wq_final[valid_ind,])
  return(caret::confusionMatrix(pred, wq_final[valid_ind, c("quality")])$overall['Accuracy'])
}, train_models_v) %>% unlist

test_perf_v <- Map(function(x) {
  pred <- predict(x, wq_final[test_ind,])
  return(caret::confusionMatrix(pred, wq_final[test_ind, c("quality")])$overall['Accuracy'])
}, train_models_v) %>% unlist


#pcaNNet
train_models_p <- apply(pnetGrid, 1, function(x) {
  #return(nnet(quality~., wq_final[train_ind,], size=x[1], decay=x[2]))
  return(caret::train(quality~., data=wq_final[train_ind,], method="pcaNNet",
                      tuneGrid = data.frame(size=x[1], decay=x[2]),
                      trControl=caret::trainControl(method="cv")))
})

train_perf_p <- Map(function(x) {
  pred <- predict(x, wq_final[train_ind,])
  return(caret::confusionMatrix(pred, wq_final[train_ind, c("quality")])$overall['Accuracy'])
}, train_models_p) %>% unlist

valid_perf_p <- Map(function(x) {
  pred <- predict(x, wq_final[valid_ind,])
  return(caret::confusionMatrix(pred, wq_final[valid_ind, c("quality")])$overall['Accuracy'])
}, train_models_p) %>% unlist

test_perf_p <- Map(function(x) {
  pred <- predict(x, wq_final[test_ind,])
  return(caret::confusionMatrix(pred, wq_final[test_ind, c("quality")])$overall['Accuracy'])
}, train_models_p) %>% unlist


#performance...
# somehow plot that and you're done, compare with no additional clusters.
pnnet_perf <- cbind(as.data.frame(pnetGrid), 
                    train_perf=train_perf, 
                    valid_perf=valid_perf, 
                    test_perf=test_perf,
                    train_perf_v=train_perf_v, 
                    valid_perf_v=valid_perf_v, 
                    test_perf_v=test_perf_v            
                    )

#' select the best value based on validation for the one with additional features and plot
#' that slice keeping decay constant.

#decay = 0.10...
pnnet_cons <- select(pnnet_perf[pnnet_perf$decay ==0.1,], -decay)
pnnet_melt <- reshape2::melt(pnnet_cons, "size")

xlim_points <- c(floor(min(pnnet_melt$value)*100)/100, ceiling(max(pnnet_melt$value)*100)/100)

pnnet_perf <- cbind(as.data.frame(pnetGrid), 
                    train_perf=train_perf, 
                    valid_perf=valid_perf, 
                    test_perf=test_perf,
                    train_perf_v=train_perf_v, 
                    valid_perf_v=valid_perf_v, 
                    test_perf_v=test_perf_v,
                    train_perf_p=train_perf_p, 
                    valid_perf_p=valid_perf_p, 
                    test_perf_p=test_perf_p    
)


g1 <- ggplot(pnnet_melt[pnnet_melt$variable %in% c("train_perf", "valid_perf", "test_perf"),], 
             aes(x=size, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Neural Net Performance - With Clusters as Features") +
  ylab("Accuracy") + 
  xlab("Number of hidden layers - decay constant at 0.1") +
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
  xlab("Number of hidden layers - decay constant at 0.1") +
  scale_y_continuous(limits = xlim_points) +
  scale_colour_discrete(name="",
                        breaks=c("train_perf_v", "valid_perf_v", "test_perf_v"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()

g3 <- ggplot(pnnet_melt[pnnet_melt$variable %in% c("train_perf_p", "valid_perf_p", "test_perf_p"),], 
             aes(x=size, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Neural Net Performance - No Additional Features") +
  ylab("Accuracy") + 
  xlab("Number of hidden layers - decay constant at 0.1") +
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















