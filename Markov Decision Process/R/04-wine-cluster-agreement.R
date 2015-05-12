# cluster agreement

library(GGally)

# cluster agreement


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

wqnlkm <- predict.kmeans(kmeans(wq_nl,3), wq_nl)
wqpcakm <- predict.kmeans(kmeans(dplyr::select(wqpca, -quality),3), dplyr::select(wqpca, -quality))
wqicakm <- predict.kmeans(kmeans(dplyr::select(wqica,-quality),3), dplyr::select(wqica,-quality))
wqrcakm <- predict.kmeans(kmeans(dplyr::select(wqrca, -quality),3), dplyr::select(wqrca, -quality))
wqrfkm <- predict.kmeans(kmeans(dplyr::select(wqrfdf,-quality),3), dplyr::select(wqrfdf,-quality))

wqnlem <- predict(Mclust(wq_nl,3), wq_nl)
wqpcaem <- predict(Mclust(dplyr::select(wqica, -quality),3), dplyr::select(wqica, -quality))
wqicaem <- predict(Mclust(dplyr::select(wqica,-quality),3), dplyr::select(wqica,-quality))
wqrcaem <- predict(Mclust(dplyr::select(wqrca, -quality),3), dplyr::select(wqrca, -quality))
wqrfem <- predict(Mclust(dplyr::select(wqrfdf,-quality),3), dplyr::select(wqrfdf,-quality))


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

clnames <- names(wq_final)[!(names(wq_final) %in% names(wq))]
pwq <- wq_final[,c(clnames, "quality")]

for(colname in clnames){
  pwq[,c(colname)] <- as.factor(pwq[,c(colname)])
}

pwqKM <- dplyr::select(pwq, ends_with("KM"), quality)
names(pwqKM) <- c("None", "PCA", "ICA", "RP", "RF", "quality")

pwqEM <- dplyr::select(pwq, ends_with("EM"), quality)
names(pwqEM) <- c("None", "PCA", "ICA", "RP", "RF", "quality")

ggpairs(pwqKM, color="quality", upper="blank",
        axisLabels='none') + theme_bw()

ggpairs(pwqEM, color="quality", upper="blank",
        axisLabels='none') + theme_bw()
