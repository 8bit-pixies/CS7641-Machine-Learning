# 04 - pima indian cluster performance

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
PIDnlkm <- predict.kmeans(kmeans(PID_nl,3), PID_nl)
PIDpcakm <- predict.kmeans(kmeans(PID_pca$x[,1:6],3), PID_pca$x[,1:6])
PIDicakm <- predict.kmeans(kmeans(PID_ica$S,3), PID_ica$S)
PIDrcakm <- predict.kmeans(kmeans(PID_rca[[bestrca$k]]$RP,3), PID_rca[[bestrca$k]]$RP)
PIDrfkm <- predict.kmeans(kmeans(PID_nl[,PID_rfdf$names],3), PID_nl[,PID_rfdf$names])

PIDnlem <- predict(Mclust(PID_nl,3), PID_nl)
PIDpcaem <- predict(Mclust(PID_pca$x[,1:6],3), PID_pca$x[,1:6])
PIDicaem <- predict(Mclust(PID_ica$S,3), PID_ica$S)
PIDrcaem <- predict(Mclust(PID_rca[[bestrca$k]]$RP,3), PID_rca[[bestrca$k]]$RP)
PIDrfem <- predict(Mclust(PID_nl[,PID_rfdf$names],3), PID_nl[,PID_rfdf$names])

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

clnames <- names(PID_final)[!(names(PID_final) %in% names(PID))]
ppid <- PID_final[,c(clnames, "diabetes")]

for(colname in clnames){
  ppid[,c(colname)] <- as.factor(ppid[,c(colname)])
}

pwqKM <- dplyr::select(ppid, ends_with("KM"), diabetes)
names(pwqKM) <- c("None", "PCA", "ICA", "RP", "RF", "diabetes")

pwqEM <- dplyr::select(ppid, ends_with("EM"), diabetes)
names(pwqEM) <- c("None", "PCA", "ICA", "RP", "RF", "diabetes")

ggpairs(pwqKM, color="diabetes", upper="blank",
        axisLabels='none') + theme_bw()

ggpairs(pwqEM, color="diabetes", upper="blank",
        axisLabels='none') + theme_bw()


