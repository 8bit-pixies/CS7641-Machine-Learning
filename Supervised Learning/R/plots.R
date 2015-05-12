# for plotting
library(ggplot2)
library(plyr)
library(reshape)
library(gridExtra)

# revalue the labels

pima.r <- pima.resample
pima.p <- pima.pdata

wine.r <- wine.resample
wine.p <- wine.pdata

pima.r$variable <- revalue(pima.r$variable, c("ctree"="DT", "pcannet"="NNET", "gbm"="BOOST", "svm"="SVM", "knnpca"="KNN"))
pima.p$model <- revalue(pima.p$model, c("ctree"="DT", "pcannet"="NNET", "gbm"="BOOST", "svm"="SVM", "knnpca"="KNN"))

wine.r$variable <- revalue(wine.r$variable, c("ctree"="DT", "nnetpca"="NNET", "gbm"="BOOST", "svm"="SVM", "knnpca"="KNN"))
wine.p$model <- revalue(wine.p$model, c("ctree"="DT", "nnetpca"="NNET", "gbm"="BOOST", "svm"="SVM", "knnpca"="KNN"))


pima.r <- pima.r[pima.r$variable %in% c("DT", "NNET", "BOOST", "SVM", "KNN"),]
pima.p <- pima.p[pima.p$model %in% c("DT", "NNET", "BOOST", "SVM", "KNN"),]

wine.r <- wine.r[wine.r$variable %in% c("DT", "NNET", "BOOST", "SVM", "KNN"),]
wine.p <- wine.p[wine.p$model %in% c("DT", "NNET", "BOOST", "SVM", "KNN"),]

names(pima.p) <- c("predict.mean","lower90", "upper90", "lower95", "upper95", "variable" )
names(wine.p) <- c("predict.mean","lower90", "upper90", "lower95", "upper95", "variable" )

# print the data to be placed in the table..
library(dplyr)
ddply(pima.r[pima.r$measure=="Accuracy",], .(measure, variable), summarize, mean=round(mean(value, na.rm=TRUE), digits=4))[, c("variable", "mean")] %>% 
  rename(., Accuracy.mean=mean) %>%
  inner_join(ddply(pima.r[pima.r$measure=="Kappa",], .(measure, variable), 
          summarize, mean=round(mean(value, na.rm=TRUE), digits=4))[, c("variable", "mean")],"variable") %>%
  inner_join(pima.p[,c("predict.mean", "variable")],"variable")


ddply(wine.r[wine.r$measure=="Accuracy",], .(measure, variable), summarize, mean=round(mean(value, na.rm=TRUE), digits=4))[, c("variable", "mean")] %>% 
  rename(., Accuracy.mean=mean) %>%
  inner_join(ddply(wine.r[wine.r$measure=="Kappa",], .(measure, variable), 
                   summarize, mean=round(mean(value, na.rm=TRUE), digits=4))[, c("variable", "mean")],"variable") %>%
  inner_join(wine.p[,c("predict.mean", "variable")],"variable")


# plotting

pima.r <- pima.resample
pima.p <- pima.pdata

wine.r <- wine.resample
wine.p <- wine.pdata

pima.r$variable <- revalue(pima.r$variable, c("ctree"="DT", "pcannet"="NNET", "ada"="BOOST", "svm"="SVM", "knnpca"="KNN"))
pima.p$model <- revalue(pima.p$model, c("ctree"="DT", "pcannet"="NNET", "ada"="BOOST", "svm"="SVM", "knnpca"="KNN"))

wine.r$variable <- revalue(wine.r$variable, c("ctree"="DT", "nnetpca"="NNET", "gbm"="BOOST", "svm"="SVM", "knnpca"="KNN"))
wine.p$model <- revalue(wine.p$model, c("ctree"="DT", "nnetpca"="NNET", "gbm"="BOOST", "svm"="SVM", "knnpca"="KNN"))

pima.r <- pima.r[pima.r$variable %in% c("DT", "NNET", "BOOST", "SVM", "KNN"),]
pima.p <- pima.p[pima.p$model %in% c("DT", "NNET", "BOOST", "SVM", "KNN"),]

wine.r <- wine.r[wine.r$variable %in% c("DT", "NNET", "BOOST", "SVM", "KNN"),]
wine.p <- wine.p[wine.p$model %in% c("DT", "NNET", "BOOST", "SVM", "KNN"),]


col_pallette <- trellis.par.get("superpose.polygon")$col
col_line <- trellis.par.get("superpose.line")$col

plot1 <- ggplot(pima.r[pima.r$measure=="Accuracy",], aes(factor(variable, levels=as.character(pima.p[order(pima.p$mean), "model"])), y=value)) +
  geom_boxplot(fill=col_pallette[1]) +
  xlab("Model") +
  ylab("Accuracy") +
  ggtitle("Pima Indians Diabetes; Training set Accuracy") +
  theme(plot.title=element_text(size=10))  +
  theme_bw()

plot2 <- ggplot(pima.r[pima.r$measure!="Accuracy",], aes(factor(variable, levels=pima.p[order(pima.p$mean), "model"]), y=value)) +
  geom_boxplot(fill=col_pallette[1]) +
  xlab("Model") +
  ylab("Kappa") +
  ggtitle("Pima Indians Diabetes; Training set Kappa Coefficient") +
  theme(plot.title=element_text(size=10))  +
  theme_bw()  

plot3 <- ggplot() +
  geom_errorbar(data=pima.p, mapping=aes(factor(model, levels=as.character(pima.p[order(pima.p$mean),"model"])), ymin=upper95, ymax=lower95), width=0.1, size=.75, colour=col_line[4]) +
  geom_errorbar(data=pima.p, mapping=aes(factor(model, levels=as.character(pima.p[order(pima.p$mean),"model"])), ymin=upper90, ymax=lower90), width=0.0, size=1, colour=col_line[1]) +
  geom_point(data=pima.p, mapping=aes(factor(model, levels=as.character(pima.p[order(pima.p$mean),"model"])), y=mean), 
             size=2.5) +
  xlab("Model") + 
  ylab("Accuracy") +
  ggtitle("Pima Indians Diabetes; Hold-out Test set Accuracy") +
  theme(plot.title=element_text(size=10))    +
  theme_bw()

plot4 <- ggplot(wine.r[wine.r$measure=="Accuracy",], aes(factor(variable, levels=wine.p[order(wine.p$mean), "model"]), y=value)) +
  geom_boxplot(fill=col_pallette[1]) +
  xlab("Model") +
  ylab("Accuracy") + 
  ggtitle("Wine Quality; Training set Accuracy") +
  theme(plot.title=element_text(size=10))   +
  theme_bw()

plot5 <- ggplot(wine.r[wine.r$measure!="Accuracy",], aes(factor(variable, levels=wine.p[order(wine.p$mean), "model"]), y=value)) +
  geom_boxplot(fill=col_pallette[1]) +
  xlab("Model") +
  ylab("Kappa") + 
  ggtitle("Wine Quality; Training set Kappa Coefficient") +
  theme(plot.title=element_text(size=10))    +
  theme_bw()

plot6 <- ggplot(wine.p, aes(factor(model, levels=as.character(wine.p[order(wine.p$mean), "model"])), y=mean)) +  
  geom_errorbar(data=wine.p, mapping=aes(x=factor(model, levels=as.character(wine.p[order(wine.p$mean), "model"])), ymin=upper95, ymax=lower95), width=0.1, size=.75, colour=col_line[4]) +
  geom_errorbar(data=wine.p, mapping=aes(x=factor(model, levels=as.character(wine.p[order(wine.p$mean), "model"])), ymin=upper90, ymax=lower90), width=0.0, size=1, colour=col_line[1]) +
  geom_point(size=2.5) +  
  xlab("Model") +
  ylab("Accuracy") + 
  ggtitle("Wine Quality; Hold-out Test set Accuracy") +
  theme(plot.title=element_text(size=10))    +
  theme_bw()

grid.arrange(plot3, plot6, plot1, plot4, plot2, plot5, ncol=2)

