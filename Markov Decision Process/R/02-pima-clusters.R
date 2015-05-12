#' 02 PIMA - Kmeans, EM, and performance against the labels.

library(caret)
library(dplyr)
library(magrittr)
library(reshape2)
library(randomForest)
library(mlbench)
library(dendextend)
library(mclust)
library(gridExtra)
source("R/clustergram-had.R")
source("R/random_projection_gauss.R")

data(PimaIndiansDiabetes)
PID <- na.omit(PimaIndiansDiabetes)
PID_nl <- dplyr::select(PID, -diabetes)
PID_pp <- preProcess(PID_nl) %>% predict(PID_nl) # preprocessed dataframe, centered and scaled

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


gen_cluster_plots <- function(hdmdata) {
  #clustergram(as.matrix(hdmdata), k.range=(2:10), line.width = 0.05)
  
  #   hc <- hclust(dist(PID_nl))
  #   hcd <- as.dendrogram(hc)
  #   d1 = color_branches(hcd, k=4)
  #   plot(d1)
  
  #' optimal clusters for EM can be solved analytically
  hdm_cl <- Mclust(hdmdata, G=1:13)
  #hdm_mbest <- dim(hdm_cl$z)[2]
  #   plot(pid_cl, what=c("BIC"))
  return(hdm_cl)
}

PID_nl1 <- cbind(PID_nl , PID$diabetes)
PIDpca1 <- cbind(PIDpca , PID$diabetes)
PIDica1 <- cbind(PIDica , PID$diabetes)
PIDrca1 <- cbind(PIDrca , PID$diabetes)
PIDrf1 <- cbind(PID[,PID_rfdf$names] , PID$diabetes)

# find optimal through EM
pidnilem <- gen_cluster_plots(PID_nl)
pidpcaem <- gen_cluster_plots(PID_pca$x[,1:6])
pidicaem <- gen_cluster_plots(select(PIDica, -diabetes))
pidrcaem <- gen_cluster_plots(select(PIDrca, -diabetes))
pidrfem  <- gen_cluster_plots(PID[,PID_rfdf$names])

# generate plots using ggplot...
pidnilembic <- pidnilem$BIC %>% as.data.frame %>% add_rownames  %>% melt("rowname") %>% na.omit
pidpcaembic <- pidpcaem$BIC %>% as.data.frame %>% add_rownames  %>% melt("rowname") %>% na.omit
pidicaembic <- pidicaem$BIC %>% as.data.frame %>% add_rownames  %>% melt("rowname") %>% na.omit
pidrcaembic <- pidrcaem$BIC %>% as.data.frame %>% add_rownames  %>% melt("rowname") %>% na.omit
pidrfembic <- pidrfem$BIC %>% as.data.frame %>% add_rownames  %>% melt("rowname") %>% na.omit

# create soln...
c(dim(pidnilem$z)[2], dim(pidpcaem$z)[2], dim(pidicaem$z)[2], 
  dim(pidrcaem$z)[2], dim(pidrfem$z)[2])

dim(pidnilem$z)[2]
dim(pidpcaem$z)[2]
dim(pidicaem$z)[2]
dim(pidrcaem$z)[2]
dim(pidrfem$z)[2]

# rm. to draw train validate, test for the cluster plots.

folds <- createFolds(PID$diabetes, k = 6, list = TRUE, returnTrain = FALSE)
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

pid_kmeans <- function(mclustobj, traindata, validdata) {
  # assign the clusters to the traindata...
  # determine cluster to prediction...
  clus <- predict.kmeans(mclustobj, dplyr::select(traindata, -diabetes))
  info <- data.frame(diabetes=traindata$diabetes, clust=clus)
  info$clust %<>% as.factor
  
  mappings <- info %>% group_by(clust, diabetes) %>% tally %>% group_by(clust) %>% top_n(1) %>% dplyr::select(clust, diabetes)
  names(mappings) <- c("clust", "pred_diabetes")
  
  valid_pred <- predict.kmeans(mclustobj, dplyr::select(validdata, -diabetes))
  valid_pred %<>% as.factor
  valid_pred <- data.frame(diabetes=validdata$diabetes, clust=valid_pred)
  valid_pred$clust %<>% as.factor
  fin_data <- valid_pred %>% left_join(mappings)  
  return(fin_data)
  #pred_data <- predict.kmeans(kclust, dplyr::(traindata, -diabetes))  
}

pid_mclust <- function(mclustobj, traindata, validdata) {
  # assign the clusters to the traindata...
  # determine cluster to prediction...
  clus <- predict(mclustobj, dplyr::select(traindata, -diabetes))
  info <- data.frame(diabetes=traindata$diabetes, clust=clus$classification)
  info$clust %<>% as.factor
  
  mappings <- info %>% group_by(clust, diabetes) %>% tally %>% group_by(clust) %>% top_n(1) %>% dplyr::select(clust, diabetes)
  names(mappings) <- c("clust", "pred_diabetes")
  
  valid_pred <- predict(mclustobj, dplyr::select(validdata, -diabetes))
  valid_pred <- valid_pred$classification %>% as.factor
  valid_pred <- data.frame(diabetes=validdata$diabetes, clust=valid_pred)
  valid_pred$clust %<>% as.factor
  fin_data <- valid_pred %>% left_join(mappings)  
  return(fin_data)
}

PID_nl1 <- cbind(PID_nl, diabetes=PID$diabetes)
PIDpca1 <- cbind(PIDpca, diabetes=PID$diabetes)
PIDrf1 <- PID[,c(PID_rfdf$names, "diabetes")]

optimk <- data.frame()
optimall <- data.frame()
for (k in 2:13) {
  PID_nl_cl <- kmeans(dplyr::select(PID_nl1[train_ind,], -diabetes), k)
  PIDpca_cl <- kmeans(dplyr::select(PIDpca1[train_ind,], -diabetes), k)
  PIDica_cl <- kmeans(dplyr::select(PIDica[train_ind,], -diabetes), k)
  PIDrca_cl <- kmeans(dplyr::select(PIDrca[train_ind,], -diabetes), k)
  PIDrf_cl <- kmeans(dplyr::select(PIDrf1[train_ind,], -diabetes), k)
  
  PID_nl_em <- Mclust(dplyr::select(PID_nl1[train_ind,], -diabetes), G=k)
  PIDpca_em <- Mclust(dplyr::select(PIDpca1[train_ind,], -diabetes), G=k)
  PIDica_em <- Mclust(dplyr::select(PIDica[train_ind,], -diabetes), G=k)
  PIDrca_em <- Mclust(dplyr::select(PIDrca[train_ind,], -diabetes), G=k)
  PIDrf_em <- Mclust(dplyr::select(PIDrf1[train_ind,], -diabetes), G=k)
  
  all_nl_kmeanscore <- pid_kmeans(PID_nl_cl, PID_nl1, PID_nl1) 
  train_nl_kmeanscore <- pid_kmeans(PID_nl_cl, PID_nl1[train_ind,], PID_nl1[train_ind,]) 
  valid_nl_kmeanscore <- pid_kmeans(PID_nl_cl, PID_nl1[train_ind,], PID_nl1[valid_ind,]) 
  test_nl_kmeanscore <- pid_kmeans(PID_nl_cl, PID_nl1[train_ind,], PID_nl1[test_ind,])
  
  all_pca_kmeanscore <- pid_kmeans(PIDpca_cl, PIDpca1, PIDpca1) 
  train_pca_kmeanscore <- pid_kmeans(PIDpca_cl, PIDpca1[train_ind,], PIDpca1[train_ind,]) 
  valid_pca_kmeanscore <- pid_kmeans(PIDpca_cl, PIDpca1[train_ind,], PIDpca1[valid_ind,]) 
  test_pca_kmeanscore <- pid_kmeans(PIDpca_cl, PIDpca1[train_ind,], PIDpca1[test_ind,]) 
  
  all_ica_kmeanscore <- pid_kmeans(PIDica_cl, PIDica, PIDica) 
  train_ica_kmeanscore <- pid_kmeans(PIDica_cl, PIDica[train_ind,], PIDica[train_ind,]) 
  valid_ica_kmeanscore <- pid_kmeans(PIDica_cl, PIDica[train_ind,], PIDica[valid_ind,]) 
  test_ica_kmeanscore <- pid_kmeans(PIDica_cl, PIDica[train_ind,], PIDica[test_ind,]) 
  
  all_rca_kmeanscore <- pid_kmeans(PIDrca_cl, PIDrca, PIDrca) 
  train_rca_kmeanscore <- pid_kmeans(PIDrca_cl, PIDrca[train_ind,], PIDrca[train_ind,]) 
  valid_rca_kmeanscore <- pid_kmeans(PIDrca_cl, PIDrca[train_ind,], PIDrca[valid_ind,]) 
  test_rca_kmeanscore <- pid_kmeans(PIDrca_cl, PIDrca[train_ind,], PIDrca[test_ind,]) 
  
  all_rf_kmeanscore <- pid_kmeans(PIDrf_cl, PIDrf1, PIDrf1) 
  train_rf_kmeanscore <- pid_kmeans(PIDrf_cl, PIDrf1[train_ind,], PIDrf1[train_ind,]) 
  valid_rf_kmeanscore <- pid_kmeans(PIDrf_cl, PIDrf1[train_ind,], PIDrf1[valid_ind,]) 
  test_rf_kmeanscore <- pid_kmeans(PIDrf_cl, PIDrf1[train_ind,], PIDrf1[test_ind,]) 

  #mclust
  all_nl_mclust <- pid_mclust(PID_nl_em, PID_nl1, PID_nl1) 
  train_nl_mclust <- pid_mclust(PID_nl_em, PID_nl1[train_ind,], PID_nl1[train_ind,]) 
  valid_nl_mclust <- pid_mclust(PID_nl_em, PID_nl1[train_ind,], PID_nl1[valid_ind,]) 
  test_nl_mclust <- pid_mclust(PID_nl_em, PID_nl1[train_ind,], PID_nl1[test_ind,]) 
  
  all_pca_mclust <- pid_mclust(PIDpca_em, PIDpca1, PIDpca1) 
  train_pca_mclust <- pid_mclust(PIDpca_em, PIDpca1[train_ind,], PIDpca1[train_ind,]) 
  valid_pca_mclust <- pid_mclust(PIDpca_em, PIDpca1[train_ind,], PIDpca1[valid_ind,]) 
  test_pca_mclust <- pid_mclust(PIDpca_em, PIDpca1[train_ind,], PIDpca1[test_ind,]) 
  
  all_ica_mclust <- pid_mclust(PIDica_em, PIDica, PIDica) 
  train_ica_mclust <- pid_mclust(PIDica_em, PIDica[train_ind,], PIDica[train_ind,]) 
  valid_ica_mclust <- pid_mclust(PIDica_em, PIDica[train_ind,], PIDica[valid_ind,]) 
  test_ica_mclust <- pid_mclust(PIDica_em, PIDica[train_ind,], PIDica[test_ind,]) 
  
  all_rca_mclust <- pid_mclust(PIDrca_em, PIDrca, PIDrca) 
  train_rca_mclust <- pid_mclust(PIDrca_em, PIDrca[train_ind,], PIDrca[train_ind,]) 
  valid_rca_mclust <- pid_mclust(PIDrca_em, PIDrca[train_ind,], PIDrca[valid_ind,]) 
  test_rca_mclust <- pid_mclust(PIDrca_em, PIDrca[train_ind,], PIDrca[test_ind,]) 
  
  all_rf_mclust <- pid_mclust(PIDrf_em, PIDrf1, PIDrf1) 
  train_rf_mclust <- pid_mclust(PIDrf_em, PIDrf1[train_ind,], PIDrf1[train_ind,]) 
  valid_rf_mclust <- pid_mclust(PIDrf_em, PIDrf1[train_ind,], PIDrf1[valid_ind,]) 
  test_rf_mclust <- pid_mclust(PIDrf_em, PIDrf1[train_ind,], PIDrf1[test_ind,]) 
  
  ###
  
  # pvalues for binom test can be calculated using:
  # caret::confusionMatrix(all_nl_kmeanscore$diabetes, all_nl_kmeanscore$pred_diabetes) 
  all_nl <- caret::confusionMatrix(all_nl_kmeanscore$diabetes, all_nl_kmeanscore$pred_diabetes)$overall['Accuracy']
  at_nl <- caret::confusionMatrix(train_nl_kmeanscore$diabetes, train_nl_kmeanscore$pred_diabetes)$overall['Accuracy']
  av_nl <- caret::confusionMatrix(valid_nl_kmeanscore$diabetes, valid_nl_kmeanscore$pred_diabetes)$overall['Accuracy']
  ate_nl <- caret::confusionMatrix(test_nl_kmeanscore$diabetes, test_nl_kmeanscore$pred_diabetes)$overall['Accuracy']
  
  all_pca <- caret::confusionMatrix(all_pca_kmeanscore$diabetes, all_pca_kmeanscore$pred_diabetes)$overall['Accuracy']
  at_pca <- caret::confusionMatrix(train_pca_kmeanscore$diabetes, train_pca_kmeanscore$pred_diabetes)$overall['Accuracy']
  av_pca <- caret::confusionMatrix(valid_pca_kmeanscore$diabetes, valid_pca_kmeanscore$pred_diabetes)$overall['Accuracy']
  ate_pca <- caret::confusionMatrix(test_pca_kmeanscore$diabetes, test_pca_kmeanscore$pred_diabetes)$overall['Accuracy']
  
  all_ica <- caret::confusionMatrix(all_ica_kmeanscore$diabetes, all_ica_kmeanscore$pred_diabetes)$overall['Accuracy']
  at_ica <- caret::confusionMatrix(train_ica_kmeanscore$diabetes, train_ica_kmeanscore$pred_diabetes)$overall['Accuracy']
  av_ica <- caret::confusionMatrix(valid_ica_kmeanscore$diabetes, valid_ica_kmeanscore$pred_diabetes)$overall['Accuracy']
  ate_ica <- caret::confusionMatrix(test_ica_kmeanscore$diabetes, test_ica_kmeanscore$pred_diabetes)$overall['Accuracy']
  
  all_rca <- caret::confusionMatrix(all_rca_kmeanscore$diabetes, all_rca_kmeanscore$pred_diabetes)$overall['Accuracy']
  at_rca <- caret::confusionMatrix(train_rca_kmeanscore$diabetes, train_rca_kmeanscore$pred_diabetes)$overall['Accuracy']
  av_rca <- caret::confusionMatrix(valid_rca_kmeanscore$diabetes, valid_rca_kmeanscore$pred_diabetes)$overall['Accuracy']
  ate_rca <- caret::confusionMatrix(test_rca_kmeanscore$diabetes, test_rca_kmeanscore$pred_diabetes)$overall['Accuracy']
  
  all_rf <- caret::confusionMatrix(all_rf_kmeanscore$diabetes, all_rf_kmeanscore$pred_diabetes)$overall['Accuracy']
  at_rf <- caret::confusionMatrix(train_rf_kmeanscore$diabetes, train_rf_kmeanscore$pred_diabetes)$overall['Accuracy']
  av_rf <- caret::confusionMatrix(valid_rf_kmeanscore$diabetes, valid_rf_kmeanscore$pred_diabetes)$overall['Accuracy']
  ate_rf <- caret::confusionMatrix(test_rf_kmeanscore$diabetes, test_rf_kmeanscore$pred_diabetes)$overall['Accuracy']
  
  ###
  all_nl_em <- caret::confusionMatrix(all_nl_mclust$diabetes, all_nl_mclust$pred_diabetes)$overall['Accuracy']  
  at_nl_em <- caret::confusionMatrix(train_nl_mclust$diabetes, train_nl_mclust$pred_diabetes)$overall['Accuracy']
  av_nl_em <- caret::confusionMatrix(valid_nl_mclust$diabetes, valid_nl_mclust$pred_diabetes)$overall['Accuracy']
  ate_nl_em <- caret::confusionMatrix(test_nl_mclust$diabetes, test_nl_mclust$pred_diabetes)$overall['Accuracy']
  
  all_pca_em <- caret::confusionMatrix(all_pca_mclust$diabetes, all_pca_mclust$pred_diabetes)$overall['Accuracy']
  at_pca_em <- caret::confusionMatrix(train_pca_mclust$diabetes, train_pca_mclust$pred_diabetes)$overall['Accuracy']
  av_pca_em <- caret::confusionMatrix(valid_pca_mclust$diabetes, valid_pca_mclust$pred_diabetes)$overall['Accuracy']
  ate_pca_em <- caret::confusionMatrix(test_pca_mclust$diabetes, test_pca_mclust$pred_diabetes)$overall['Accuracy']
  
  all_ica_em <- caret::confusionMatrix(all_ica_mclust$diabetes, all_ica_mclust$pred_diabetes)$overall['Accuracy']
  at_ica_em <- caret::confusionMatrix(train_ica_mclust$diabetes, train_ica_mclust$pred_diabetes)$overall['Accuracy']
  av_ica_em <- caret::confusionMatrix(valid_ica_mclust$diabetes, valid_ica_mclust$pred_diabetes)$overall['Accuracy']
  ate_ica_em <- caret::confusionMatrix(test_ica_mclust$diabetes, test_ica_mclust$pred_diabetes)$overall['Accuracy']
  
  all_rca_em <- caret::confusionMatrix(all_rca_mclust$diabetes, all_rca_mclust$pred_diabetes)$overall['Accuracy']
  at_rca_em <- caret::confusionMatrix(train_rca_mclust$diabetes, train_rca_mclust$pred_diabetes)$overall['Accuracy']
  av_rca_em <- caret::confusionMatrix(valid_rca_mclust$diabetes, valid_rca_mclust$pred_diabetes)$overall['Accuracy']
  ate_rca_em <- caret::confusionMatrix(test_rca_mclust$diabetes, test_rca_mclust$pred_diabetes)$overall['Accuracy']
  
  all_rf_em <- caret::confusionMatrix(all_rf_mclust$diabetes, all_rf_mclust$pred_diabetes)$overall['Accuracy']
  at_rf_em <- caret::confusionMatrix(train_rf_mclust$diabetes, train_rf_mclust$pred_diabetes)$overall['Accuracy']
  av_rf_em <- caret::confusionMatrix(valid_rf_mclust$diabetes, valid_rf_mclust$pred_diabetes)$overall['Accuracy']
  ate_rf_em <- caret::confusionMatrix(test_rf_mclust$diabetes, test_rf_mclust$pred_diabetes)$overall['Accuracy']
  
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
  
  rowdataall <- cbind(k=k, 
                      all_nl=all_nl,
                      all_pca=all_pca,
                      all_ica=all_ica,
                      all_rca=all_rca,
                      all_rf=all_rf,
                      all_nl_em=all_nl_em,
                      all_pca_em=all_pca_em,
                      all_ica_em=all_ica_em,
                      all_rca_em=all_rca_em,
                      all_rf_em=all_rf_em)
  optimall <- rbind(optimall, rowdataall)
}

#optimal_clusters....
pid_kmeans(PID_nl_cl, PID_nl1[train_ind,], PID_nl1[train_ind,])


plotoptimk <- melt(optimk, id=c("k"))


#kmeans...
pidnilkm_t <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_nl", "av_nl", "ate_nl")),], 
       aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Pima Indians - No Transformation") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (Kmeans)") +
  scale_colour_discrete(name="",
                        breaks=c("at_nl", "av_nl", "ate_nl"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()



pidpcakm_t <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_pca", "av_pca", "ate_pca")),], 
       aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Pima Indians - PCA") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (Kmeans)") +
  scale_colour_discrete(name="",
                        breaks=c("at_pca", "av_pca", "ate_pca"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()

pidicakm_t <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_ica", "av_ica", "ate_ica")),], 
       aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Pima Indians - ICA") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (Kmeans)") +
  scale_colour_discrete(name="",
                        breaks=c("at_ica", "av_ica", "ate_ica"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()

pidrcakm_t <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_rca", "av_rca", "ate_rca")),], 
       aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Pima Indians - RP") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (Kmeans)") +
  scale_colour_discrete(name="",
                        breaks=c("at_rca", "av_rca", "ate_rca"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()


pidrfkm_t <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_rf", "av_rf", "ate_rf")),], 
       aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Pima Indians - With Feature Selection") +
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

pidnilkm <- n_clustergram(PID_nl) %>% create_clustergram("No Transformation")
pidpcakm <- n_clustergram(PID_pca$x[,1:6])  %>% create_clustergram("PCA")
pidicakm <- n_clustergram(select(PIDica, -diabetes))  %>% create_clustergram("ICA")
pidrcakm <- n_clustergram(select(PIDrca, -diabetes)) %>% create_clustergram("RP")
pidrfkm <- n_clustergram(PID[,PID_rfdf$names]) %>% create_clustergram("RF")

grid.arrange(pidnilkm, pidnilkm_t, 
             pidpcakm, pidpcakm_t,
             pidicakm, pidicakm_t,
             pidrcakm, pidrcakm_t, 
             pidrfkm, pidrfkm_t, ncol=2)


# EM

pidnilem_p <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_nl_em", "av_nl_em", "ate_nl_em")),], 
       aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Pima Indians - No Transformation") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (EM)") +
  scale_colour_discrete(name="",
                        breaks=c("at_nl_em", "av_nl_em", "ate_nl_em"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()

pidpcaem_p <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_pca_em", "av_pca_em", "ate_pca_em")),], 
       aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Pima Indians - PCA") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (EM)") +
  scale_colour_discrete(name="",
                        breaks=c("at_pca_em", "av_pca_em", "ate_pca_em"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()


pidicaem_p <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_ica_em", "av_ica_em", "ate_ica_em")),], 
       aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Pima Indians - ICA") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (EM)") +
  scale_colour_discrete(name="",
                        breaks=c("at_ica_em", "av_ica_em", "ate_ica_em"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()



pidrcaem_p<- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_rca_em", "av_rca_em", "ate_rca_em")),], 
       aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Pima Indians - RP") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (EM)") +
  scale_colour_discrete(name="",
                        breaks=c("at_rca_em", "av_rca_em", "ate_rca_em"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()

pidrfem_p <- ggplot(plotoptimk[(plotoptimk$variable %in% c("at_rf_em", "av_rf_em", "ate_rf_em", "ate_rf_em")),], 
       aes(x=k, y=value, colour=variable, group=variable)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Pima Indians - With Feature Selection") +
  ylab("Accuracy") + 
  xlab("Number of Clusters (EM)") +
  scale_colour_discrete(name="",
                        breaks=c("at_rf_em", "av_rf_em", "ate_rf_em"),
                        labels=c("Training", "Validation", "Testing"))+
  theme_bw()


pidnilem_t <- ggplot(pidnilembic, aes(x=rowname, y=value, colour=variable, group=variable)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle("No Transformation")
pidpcaem_t <- ggplot(pidpcaembic, aes(x=rowname, y=value, colour=variable, group=variable)) + geom_line() + theme_bw() + 
  ggtitle("PCA")
pidicaem_t <- ggplot(pidicaembic, aes(x=rowname, y=value, colour=variable, group=variable)) + geom_line() + theme_bw()+ 
  ggtitle("ICA")
pidrcaem_t <- ggplot(pidrcaembic, aes(x=rowname, y=value, colour=variable, group=variable)) + geom_line() + theme_bw()+ 
  ggtitle("RP")
pidrfem_t <- ggplot(pidrfembic, aes(x=rowname, y=value, colour=variable, group=variable)) + geom_line() + theme_bw() +
  ggtitle("RF")

grid.arrange(pidnilem_t,pidnilem_p,
             pidpcaem_t,pidpcaem_p,
             pidicaem_t,pidicaem_p,
             pidrcaem_t,pidrcaem_p,
             pidrfem_t, pidrfem_p,
             ncol=2)




















