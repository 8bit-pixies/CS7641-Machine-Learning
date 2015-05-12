# 02 wine data

library(caret)
library(dplyr)
library(magrittr)
library(reshape2)
library(randomForest)
library(mlbench)
library(dendextend)
library(mclust)
library(gridExtra)

source("R/random_projection_gauss.R")

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
  sum((wq_nl - (x$RP %*% MASS::ginv(x$R)))^2)
}, wq_rca) %>% melt

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
wqrf <- wq[,(names(wq) %in% c(wqrf.name, "quality"))]

# em
gen_cluster_plots <- function(hdmdata) {
  hdm_cl <- Mclust(hdmdata, G=1:13)
  return(hdm_cl)
}

# find optimal through EM
wqnilem <- gen_cluster_plots(wq_nl)
wqpcaem <- gen_cluster_plots(wq_pca$x[,1:6])
wqicaem <- gen_cluster_plots(select(wqica, -quality))
wqrcaem <- gen_cluster_plots(select(wqrca, -quality))
wqrfem  <- gen_cluster_plots(wq[,c(wqrf.name, "quality")])

# generate plots using ggplot...
wqnilembic <- wqnilem$BIC %>% as.data.frame %>% add_rownames  %>% melt("rowname") %>% na.omit
wqpcaembic <- wqpcaem$BIC %>% as.data.frame %>% add_rownames  %>% melt("rowname") %>% na.omit
wqicaembic <- wqicaem$BIC %>% as.data.frame %>% add_rownames  %>% melt("rowname") %>% na.omit
wqrcaembic <- wqrcaem$BIC %>% as.data.frame %>% add_rownames  %>% melt("rowname") %>% na.omit
wqrfembic <- wqrfem$BIC %>% as.data.frame %>% add_rownames  %>% melt("rowname") %>% na.omit


c(dim(wqnilem$z)[2], dim(wqpcaem$z)[2], dim(wqicaem$z)[2], 
  dim(wqrcaem$z)[2], dim(wqrfem$z)[2])

# rm. to draw train validate, test for the cluster plots.
folds <- createFolds(wq$quality, k = 6, list = TRUE, returnTrain = FALSE)
train_ind <- c(folds$Fold1, folds$Fold2)
valid_ind <- c(folds$Fold3)
test_ind <- c(folds$Fold4)

predict.kmeans <- function(km, data) {
  k <- nrow(km$centers)
  n <- nrow(data)
  d <- as.matrix(dist(rbind(km$centers, data)))[-(1:k),1:k]
  out <- apply(d, 1, which.min)
  return(out)
}


wq_kmeans <- function(mclustobj, traindata, validdata) {
  # assign the clusters to the traindata...
  # determine cluster to prediction...
  clus <- predict.kmeans(mclustobj, dplyr::select(traindata, -quality))
  info <- data.frame(quality=traindata$quality, clust=clus)
  info$clust %<>% as.factor
  
  mappings <- info %>% group_by(clust, quality) %>% tally %>% group_by(clust) %>% top_n(1) %>% dplyr::select(clust, quality)
  names(mappings) <- c("clust", "pred_quality")
  
  valid_pred <- predict.kmeans(mclustobj, dplyr::select(validdata, -quality))
  valid_pred %<>% as.factor
  valid_pred <- data.frame(quality=validdata$quality, clust=valid_pred)
  valid_pred$clust %<>% as.factor
  fin_data <- valid_pred %>% left_join(mappings)  
  return(fin_data)
  #pred_data <- predict.kmeans(kclust, dplyr::(traindata, -quality))  
}


wq_mclust <- function(mclustobj, traindata, validdata) {
  # assign the clusters to the traindata...
  # determine cluster to prediction...
  clus <- predict(mclustobj, dplyr::select(traindata, -quality))
  info <- data.frame(quality=traindata$quality, clust=clus$classification)
  info$clust %<>% as.factor
  
  mappings <- info %>% group_by(clust, quality) %>% tally %>% group_by(clust) %>% top_n(1) %>% dplyr::select(clust, quality)
  names(mappings) <- c("clust", "pred_quality")
  
  valid_pred <- predict(mclustobj, dplyr::select(validdata, -quality))
  valid_pred <- valid_pred$classification %>% as.factor
  valid_pred <- data.frame(quality=validdata$quality, clust=valid_pred)
  valid_pred$clust %<>% as.factor
  fin_data <- valid_pred %>% left_join(mappings)  
  return(fin_data)
}

wq_nl1 <- cbind(wq_nl, quality=wq$quality)
wqpca1 <- wqpca
wqrf1 <- wq[,c(wqrf.name, "quality")]

optimk <- data.frame()
for (k in 2:13) {
  wq_nl_cl <- kmeans(dplyr::select(wq_nl1[train_ind,], -quality), k)
  wqpca_cl <- kmeans(dplyr::select(wqpca1[train_ind,], -quality), k)
  wqica_cl <- kmeans(dplyr::select(wqica[train_ind,], -quality), k)
  wqrca_cl <- kmeans(dplyr::select(wqrca[train_ind,], -quality), k)
  wqrf_cl <- kmeans(dplyr::select(wqrf1[train_ind,], -quality), k)
  
  wq_nl_em <- Mclust(dplyr::select(wq_nl1[train_ind,], -quality), G=k)
  wqpca_em <- Mclust(dplyr::select(wqpca1[train_ind,], -quality), G=k)
  wqica_em <- Mclust(dplyr::select(wqica[train_ind,], -quality), G=k)
  wqrca_em <- Mclust(dplyr::select(wqrca[train_ind,], -quality), G=k)
  wqrf_em <- Mclust(dplyr::select(wqrf1[train_ind,], -quality), G=k)
  
  train_nl_kmeanscore <- wq_kmeans(wq_nl_cl, wq_nl1[train_ind,], wq_nl1[train_ind,]) 
  valid_nl_kmeanscore <- wq_kmeans(wq_nl_cl, wq_nl1[train_ind,], wq_nl1[valid_ind,]) 
  test_nl_kmeanscore <- wq_kmeans(wq_nl_cl, wq_nl1[train_ind,], wq_nl1[test_ind,])
  
  train_pca_kmeanscore <- wq_kmeans(wqpca_cl, wqpca1[train_ind,], wqpca1[train_ind,]) 
  valid_pca_kmeanscore <- wq_kmeans(wqpca_cl, wqpca1[train_ind,], wqpca1[valid_ind,]) 
  test_pca_kmeanscore <- wq_kmeans(wqpca_cl, wqpca1[train_ind,], wqpca1[test_ind,]) 
  
  train_ica_kmeanscore <- wq_kmeans(wqica_cl, wqica[train_ind,], wqica[train_ind,]) 
  valid_ica_kmeanscore <- wq_kmeans(wqica_cl, wqica[train_ind,], wqica[valid_ind,]) 
  test_ica_kmeanscore <- wq_kmeans(wqica_cl, wqica[train_ind,], wqica[test_ind,]) 
  
  train_rca_kmeanscore <- wq_kmeans(wqrca_cl, wqrca[train_ind,], wqrca[train_ind,]) 
  valid_rca_kmeanscore <- wq_kmeans(wqrca_cl, wqrca[train_ind,], wqrca[valid_ind,]) 
  test_rca_kmeanscore <- wq_kmeans(wqrca_cl, wqrca[train_ind,], wqrca[test_ind,]) 
  
  train_rf_kmeanscore <- wq_kmeans(wqrf_cl, wqrf1[train_ind,], wqrf1[train_ind,]) 
  valid_rf_kmeanscore <- wq_kmeans(wqrf_cl, wqrf1[train_ind,], wqrf1[valid_ind,]) 
  test_rf_kmeanscore <- wq_kmeans(wqrf_cl, wqrf1[train_ind,], wqrf1[test_ind,]) 
  
  #mclust
  
  train_nl_mclust <- wq_mclust(wq_nl_em, wq_nl1[train_ind,], wq_nl1[train_ind,]) 
  valid_nl_mclust <- wq_mclust(wq_nl_em, wq_nl1[train_ind,], wq_nl1[valid_ind,]) 
  test_nl_mclust <- wq_mclust(wq_nl_em, wq_nl1[train_ind,], wq_nl1[test_ind,]) 
  
  train_pca_mclust <- wq_mclust(wqpca_em, wqpca1[train_ind,], wqpca1[train_ind,]) 
  valid_pca_mclust <- wq_mclust(wqpca_em, wqpca1[train_ind,], wqpca1[valid_ind,]) 
  test_pca_mclust <- wq_mclust(wqpca_em, wqpca1[train_ind,], wqpca1[test_ind,]) 
  
  train_ica_mclust <- wq_mclust(wqica_em, wqica[train_ind,], wqica[train_ind,]) 
  valid_ica_mclust <- wq_mclust(wqica_em, wqica[train_ind,], wqica[valid_ind,]) 
  test_ica_mclust <- wq_mclust(wqica_em, wqica[train_ind,], wqica[test_ind,]) 
  
  train_rca_mclust <- wq_mclust(wqrca_em, wqrca[train_ind,], wqrca[train_ind,]) 
  valid_rca_mclust <- wq_mclust(wqrca_em, wqrca[train_ind,], wqrca[valid_ind,]) 
  test_rca_mclust <- wq_mclust(wqrca_em, wqrca[train_ind,], wqrca[test_ind,]) 
  
  train_rf_mclust <- wq_mclust(wqrf_em, wqrf1[train_ind,], wqrf1[train_ind,]) 
  valid_rf_mclust <- wq_mclust(wqrf_em, wqrf1[train_ind,], wqrf1[valid_ind,]) 
  test_rf_mclust <- wq_mclust(wqrf_em, wqrf1[train_ind,], wqrf1[test_ind,]) 
  
  ###
  
  at_nl <- caret::confusionMatrix(train_nl_kmeanscore$quality, train_nl_kmeanscore$pred_quality)$overall['Accuracy']
  av_nl <- caret::confusionMatrix(valid_nl_kmeanscore$quality, valid_nl_kmeanscore$pred_quality)$overall['Accuracy']
  ate_nl <- caret::confusionMatrix(test_nl_kmeanscore$quality, test_nl_kmeanscore$pred_quality)$overall['Accuracy']
  
  at_pca <- caret::confusionMatrix(train_pca_kmeanscore$quality, train_pca_kmeanscore$pred_quality)$overall['Accuracy']
  av_pca <- caret::confusionMatrix(valid_pca_kmeanscore$quality, valid_pca_kmeanscore$pred_quality)$overall['Accuracy']
  ate_pca <- caret::confusionMatrix(test_pca_kmeanscore$quality, test_pca_kmeanscore$pred_quality)$overall['Accuracy']
  
  at_ica <- caret::confusionMatrix(train_ica_kmeanscore$quality, train_ica_kmeanscore$pred_quality)$overall['Accuracy']
  av_ica <- caret::confusionMatrix(valid_ica_kmeanscore$quality, valid_ica_kmeanscore$pred_quality)$overall['Accuracy']
  ate_ica <- caret::confusionMatrix(test_ica_kmeanscore$quality, test_ica_kmeanscore$pred_quality)$overall['Accuracy']
  
  at_rca <- caret::confusionMatrix(train_rca_kmeanscore$quality, train_rca_kmeanscore$pred_quality)$overall['Accuracy']
  av_rca <- caret::confusionMatrix(valid_rca_kmeanscore$quality, valid_rca_kmeanscore$pred_quality)$overall['Accuracy']
  ate_rca <- caret::confusionMatrix(test_rca_kmeanscore$quality, test_rca_kmeanscore$pred_quality)$overall['Accuracy']
  
  at_rf <- caret::confusionMatrix(train_rf_kmeanscore$quality, train_rf_kmeanscore$pred_quality)$overall['Accuracy']
  av_rf <- caret::confusionMatrix(valid_rf_kmeanscore$quality, valid_rf_kmeanscore$pred_quality)$overall['Accuracy']
  ate_rf <- caret::confusionMatrix(test_rf_kmeanscore$quality, test_rf_kmeanscore$pred_quality)$overall['Accuracy']
  
  ###
  
  at_nl_em <- caret::confusionMatrix(train_nl_mclust$quality, train_nl_mclust$pred_quality)$overall['Accuracy']
  av_nl_em <- caret::confusionMatrix(valid_nl_mclust$quality, valid_nl_mclust$pred_quality)$overall['Accuracy']
  ate_nl_em <- caret::confusionMatrix(test_nl_mclust$quality, test_nl_mclust$pred_quality)$overall['Accuracy']
  
  at_pca_em <- caret::confusionMatrix(train_pca_mclust$quality, train_pca_mclust$pred_quality)$overall['Accuracy']
  av_pca_em <- caret::confusionMatrix(valid_pca_mclust$quality, valid_pca_mclust$pred_quality)$overall['Accuracy']
  ate_pca_em <- caret::confusionMatrix(test_pca_mclust$quality, test_pca_mclust$pred_quality)$overall['Accuracy']
  
  at_ica_em <- caret::confusionMatrix(train_ica_mclust$quality, train_ica_mclust$pred_quality)$overall['Accuracy']
  av_ica_em <- caret::confusionMatrix(valid_ica_mclust$quality, valid_ica_mclust$pred_quality)$overall['Accuracy']
  ate_ica_em <- caret::confusionMatrix(test_ica_mclust$quality, test_ica_mclust$pred_quality)$overall['Accuracy']
  
  at_rca_em <- caret::confusionMatrix(train_rca_mclust$quality, train_rca_mclust$pred_quality)$overall['Accuracy']
  av_rca_em <- caret::confusionMatrix(valid_rca_mclust$quality, valid_rca_mclust$pred_quality)$overall['Accuracy']
  ate_rca_em <- caret::confusionMatrix(test_rca_mclust$quality, test_rca_mclust$pred_quality)$overall['Accuracy']
  
  at_rf_em <- caret::confusionMatrix(train_rf_mclust$quality, train_rf_mclust$pred_quality)$overall['Accuracy']
  av_rf_em <- caret::confusionMatrix(valid_rf_mclust$quality, valid_rf_mclust$pred_quality)$overall['Accuracy']
  ate_rf_em <- caret::confusionMatrix(test_rf_mclust$quality, test_rf_mclust$pred_quality)$overall['Accuracy']
  
  rowdata <- cbind(k=k, at_nl=at_nl, av_nl=av_nl, ate_nl=ate_nl,
                   at_pca=at_pca, av_pca=av_pca, ate_pca=ate_pca,
                   at_ica=at_ica, av_ica=av_ica, ate_ica=ate_ica,
                   at_rca=at_rca, av_rca=av_rca, ate_rca=ate_rca,
                   at_rf=at_rf, av_rf=av_rf, ate_rf=ate_rf,
                   at_nl_em = at_nl_em , av_nl_em = av_nl_em , ate_nl_em = ate_nl_em ,
                   at_pca_em= at_pca_em, av_pca_em= av_pca_em, ate_pca_em= ate_pca_em,
                   at_ica_em= at_ica_em, av_ica_em= av_ica_em, ate_ica_em= ate_ica_em,
                   at_rca_em= at_rca_em, av_rca_em= av_rca_em, ate_rca_em= ate_rca_em,
                   at_rf_em = at_rf_em , av_rf_em = av_rf_em, ate_rf_em = ate_rf_em)
  optimk <- rbind(optimk, rowdata)
}

plotoptimk <- melt(optimk, id=c("k"))



#kmeans...
wqnilkm_t <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_nl", "av_nl", "ate_nl")),], 
                     aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Wine Quality - No Transformation") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (Kmeans)") +
  scale_colour_discrete(name="",
                        breaks=c("at_nl", "av_nl", "ate_nl"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()



wqpcakm_t <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_pca", "av_pca", "ate_pca")),], 
                     aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Wine Quality - PCA") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (Kmeans)") +
  scale_colour_discrete(name="",
                        breaks=c("at_pca", "av_pca", "ate_pca"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()

wqicakm_t <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_ica", "av_ica", "ate_ica")),], 
                     aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Wine Quality - ICA") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (Kmeans)") +
  scale_colour_discrete(name="",
                        breaks=c("at_ica", "av_ica", "ate_ica"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()

wqrcakm_t <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_rca", "av_rca", "ate_rca")),], 
                     aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Wine Quality - RP") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (Kmeans)") +
  scale_colour_discrete(name="",
                        breaks=c("at_rca", "av_rca", "ate_rca"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()


wqrfkm_t <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_rf", "av_rf", "ate_rf")),], 
                    aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Wine Quality - With Feature Selection") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (Kmeans)") +
  scale_colour_discrete(name="",
                        breaks=c("at_rf", "av_rf", "ate_rf"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()

# clustergram plots
n_clustergram <- function(ndata) {
  k_def <- many_kmeans(ndata, 2:13)
  pr <- princomp(ndata)
  pr1 <- predict(pr)[,1]
  
  return((clustergram(k_def, pr1)))
}

create_clustergram <- function(x, title="") {
  i_pos <- !duplicated(x$i)
  
  means <- ddply(x, c("cluster", "i"), summarise, 
                 min = min(adj), max = max(adj), mean=mean(adj))
  
  return(ggplot(x, aes(i)) +
           geom_ribbon(aes(y = adj, group = obs, fill = y, ymin = adj - line.width/2, ymax = adj + line.width/2)) + 
           #geom_errorbar(aes(ymin = min, ymax = max), data = means, width = 0.1) + 
           geom_point(aes(y=mean, size=1), data=means)+
           scale_x_continuous("Cluster", breaks = x$i[i_pos], labels = x$k[i_pos]) +
           ggtitle(title) +
           labs(y = "Cluster average", colour = "Obs\nvalue", fill = "Obs\nvalue") + 
           theme_bw() +
           theme(legend.position="none") 
  )
}

wqnilkm <- n_clustergram(wq_nl) %>% create_clustergram("No Transformation")
wqpcakm <- n_clustergram(wq_pca$x[,1:6])  %>% create_clustergram("PCA")
wqicakm <- n_clustergram(select(wqica, -quality))  %>% create_clustergram("ICA")
wqrcakm <- n_clustergram(select(wqrca, -quality)) %>% create_clustergram("RP")
wqrfkm <- n_clustergram(wq[,(names(wq) %in% wqrf.name)]) %>% create_clustergram("RF")

grid.arrange(wqnilkm, wqnilkm_t, 
             wqpcakm, wqpcakm_t,
             wqicakm, wqicakm_t,
             wqrcakm, wqrcakm_t, 
             wqrfkm, wqrfkm_t, ncol=2)


# EM

wqnilem_p <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_nl_em", "av_nl_em", "ate_nl_em")),], 
       aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Wine Quality - No Transformation") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (EM)") +
  scale_colour_discrete(name="",
                        breaks=c("at_nl_em", "av_nl_em", "ate_nl_em"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()


wqpcaem_p <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_pca_em", "av_pca_em", "ate_pca_em")),], 
       aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Wine Quality - PCA") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (EM)") +
  scale_colour_discrete(name="",
                        breaks=c("at_pca_em", "av_pca_em", "ate_pca_em"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()


wqicaem_p <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_ica_em", "av_ica_em", "ate_ica_em")),], 
       aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Wine Quality - ICA") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (EM)") +
  scale_colour_discrete(name="",
                        breaks=c("at_ica_em", "av_ica_em", "ate_ica_em"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()



wqrcaem_p <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_rca_em", "av_rca_em", "ate_rca_em")),], 
       aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Wine Quality - RP") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (EM)") +
  scale_colour_discrete(name="",
                        breaks=c("at_rca_em", "av_rca_em", "ate_rca_em"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()

wqrfem_p <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_rf_em", "av_rf_em", "ate_rf_em", "ate_rf_em")),], 
       aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Wine Quality - With Feature Selection") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (EM)") +
  scale_colour_discrete(name="",
                        breaks=c("at_rf_em", "av_rf_em", "ate_rf_em"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()


wqnilem_t <- ggplot(wqnilembic, aes(x=rowname, y=value, colour=variable, group=variable)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle("No Transformation")
wqpcaem_t <- ggplot(wqpcaembic, aes(x=rowname, y=value, colour=variable, group=variable)) + geom_line() + theme_bw() + 
  ggtitle("PCA")
wqicaem_t <- ggplot(wqicaembic, aes(x=rowname, y=value, colour=variable, group=variable)) + geom_line() + theme_bw()+ 
  ggtitle("ICA")
wqrcaem_t <- ggplot(wqrcaembic, aes(x=rowname, y=value, colour=variable, group=variable)) + geom_line() + theme_bw()+ 
  ggtitle("RP")
wqrfem_t <- ggplot(wqrfembic, aes(x=rowname, y=value, colour=variable, group=variable)) + geom_line() + theme_bw() +
  ggtitle("RF")


grid.arrange(wqnilem_t,wqnilem_p,
             wqpcaem_t,wqpcaem_p,
             wqicaem_t,wqicaem_p,
             wqrcaem_t,wqrcaem_p,
             wqrfem_t, wqrfem_p,
             ncol=2)









