#' Pima Diabetes Data Set

library(mlbench)
library(caret)
library(plyr)
library(dplyr)
library(reshape2)
library(GGally)
library(ggbiplot)
library(gtable)
library(randomForest)
library(gridExtra)

source("R/random_projection_gauss.R")

data(PimaIndiansDiabetes)
PID <- na.omit(PimaIndiansDiabetes)
PID_nl <- dplyr::select(PID, -diabetes)
PID_pp <- preProcess(PID_nl) %>% predict(PID_nl) # preprocessed dataframe, centered and scaled

# svg("plots/PID.svg", height = 12, width = 20)
# g <- ggpairs(PID, colour="diabetes")
# print(g)
# dev.off()

#ggpairs(PID, colour="diabetes")

#' PCA
PID_pca <- prcomp(PID_nl, retx=TRUE, scale. = TRUE, center=TRUE)
PID_data <- data.frame(PC=1:length(PID_pca$sdev), Variance=PID_pca$sdev^2)
PID_propvar <- (PID_pca$sdev^2/sum(PID_pca$sdev^2)) %>% cumsum
PID_propvar <- data.frame(x=1:length(PID_propvar), cumvar=PID_propvar)

g1<-ggplot(PID_data, aes(x=PC, y=Variance))+
  geom_point() + 
  geom_line() + 
  xlab("Factor Number")+
  ylab("Eigenvalue")+
  theme(legend.position = "none") +
  ggtitle("Scree Plot - for PID") + 
  theme_bw()

ggplot(PID_propvar, aes(x=x, y=cumvar))+
  geom_bar(stat="identity") +
  xlab("Number of Components")+
  ylab("Proportion of Variance Explained")+
  theme_bw()

g2 <- ggbiplot(PID_pca, groups = PID$diabetes,
         obs.scale = 1, var.scale = 1)+
  theme_bw()

ggbiplot(PID_pca, groups = PID$diabetes, choices=3:4,
         obs.scale = 1, var.scale = 1)+
  theme_bw()

grid.arrange(g1, g2, ncol=2)


PIDpca=cbind(as.data.frame(PID_pca$x), diabetes=PID$diabetes)

#ggpairs(PIDpca, colour="diabetes")
#' ICA
PID_ica = fastICA::fastICA(PID_nl, n.comp=8, verbose=TRUE, row.norm=TRUE)
PIDica=cbind(as.data.frame(PID_ica$S), diabetes=PID$diabetes)
sapply(PIDica[,!(names(PIDica) %in% c("diabetes"))], function(x) (e1071::kurtosis(x))^2)
sapply(PIDica[,!(names(PIDica) %in% c("diabetes"))], function(x) abs(e1071::kurtosis(x))) %>% summary
pidn <- sapply(PIDica[,!(names(PIDica) %in% c("diabetes"))], function(x) (e1071::kurtosis(x))^2)
pidn <- melt(pidn)
pidn$name <- row.names(pidn)
pidn <- pidn[pidn$value < 1,]
# remove all...in wqn
PIDica <- PIDica[,!(names(PIDica) %in% pidn$name)]


# remember to toss out the ones that look normal.

# ggplot...
histtop <- ggplot(PIDica) + geom_density(aes(PIDica$V2))
histright <- ggplot(PIDica) + geom_density(aes(PIDica$V7))+coord_flip()
scatterp <- ggplot(PIDica) + geom_point(aes(PIDica$V2, PIDica$V7))

g1 <- ggplot_gtable(ggplot_build(scatterp))
g2 <- ggplot_gtable(ggplot_build(histtop))
g3 <- ggplot_gtable(ggplot_build(histright))

# Get maximum widths and heights
maxWidth <- unit.pmax(g1$widths[2:3], g2$widths[2:3])
maxHeight <- unit.pmax(g1$heights[4:5], g3$heights[4:5])

# Set the maximums in the gtables for gt1, gt2 and gt3
g1$widths[2:3] <- as.list(maxWidth)
g2$widths[2:3] <- as.list(maxWidth)
g1$heights[4:5] <- as.list(maxHeight)
g3$heights[4:5] <- as.list(maxHeight)

# Create a new gtable
gt <- gtable(widths = unit(c(5, 1), "null"), height = unit(c(1, 3.5), "null"))

# Instert gt1, gt2 and gt3 into the new gtable
gt <- gtable_add_grob(gt, g1, 2, 1)
gt <- gtable_add_grob(gt, g2, 1, 1)
gt <- gtable_add_grob(gt, g3, 2, 2)

# And render the plot
grid.newpage()
grid.draw(gt)

#ggpairs(PIDica, colour="diabetes")

#' random projections
PID_rca <- Map(function(x) {
  gaussian_random_projection(PID_nl, 8)
}, 1:100)

# get the ones which immitate the result best.
PID_rca_diff <- Map(function(x) {
  sum((PID_nl - (x$RP %*% MASS::ginv(x$R)))^2)
}, PID_rca)

PID_pca_diff <- sum((PID_pp - (PID_pca$x %*% MASS::ginv(PID_pca$rotation)))^2)
PID_ica_diff <- sum((PID_pp - (PID_ica$S %*% MASS::ginv(PID_ica$K %*% PID_ica$W)))^2)

# for ? components
PID_pcarca_diff <- function(n.comp) {
  pidpcadiff <- sum((PID_pp - (PID_pca$x[,1:n.comp] %*% MASS::ginv(PID_pca$rotation[,1:n.comp])))^2)
  pidica = fastICA::fastICA(PID_nl, n.comp=n.comp, verbose=TRUE, row.norm=TRUE)  
  pidicadiff = sum((PID_pp - (pidica$S %*% MASS::ginv(pidica$K %*% pidica$W)))^2)
  
  pidrca <- Map(function(x) {
    gaussian_random_projection(PID_nl, n.comp)
  }, 1:100)
  
  # get the ones which immitate the result best.
  pidrcadiff <- Map(function(x) {
    sum((PID_nl - (x$RP %*% MASS::ginv(x$R)))^2)
  }, pidrca) %>% unlist
  
  return(list(PCA=pidpcadiff, ICA=pidicadiff, pidrcadiff=pidrcadiff))
}

pid_diff_all <- Map(function(x) {
  diff_dat <- melt(PID_pcarca_diff(x))
  diff_dat$ncomp <- x  
  return(diff_dat)
}, 1:dim(PID_nl)[2]) %>% Reduce(f=rbind, x=.)

# get 95 and 5th percentile of anything with pidrcadiff...
RP.95percentile <- pid_diff_all %>% 
  filter(L1=="pidrcadiff") %>%
  ddply(., .(ncomp), summarize, value=quantile(value, probs=0.95)) %>%
  mutate(L1="pidrcadiff", percentile=0.05)

RP.05percentile <- pid_diff_all %>% 
  filter(L1=="pidrcadiff") %>%
  ddply(., .(ncomp), summarize, value=quantile(value, probs=0.05)) %>%
  mutate(L1="pidrcadiff", percentile=0.95)

rcapall <- rbind(rcap95, rcap05, 
                 mutate(filter(pid_diff_all, L1!="pidrcadiff"), percentile=1))
rcapall$name <- apply(rcapall, 1, function(x) {
  if(x[4]<1) {
    return(paste("RP", paste0(as.numeric(x[4])*100, "th", collapse=""), "Percentile"))
  } else{
    return(x[3])
  }
})
g_1 <- ggplot(rcapall, aes(y=value, x=ncomp, colour=name))+
  geom_line() + theme(legend.position="bottom") + 
  xlab("# Components") + 
  ggtitle("L2 norm of data reconstruction")+
  ylab("L2 Norm")

ggplot(rcapall[, ], aes(y=value, x=ncomp, colour=name))+
  geom_line() + theme(legend.position="bottom") + 
  xlab("# Components") + 
  ylab("L2 Norm")

# apply randomforest to get the mean gini, variable importance.
PID_rf <- randomForest(diabetes ~., PID)
PIDrf <- as.data.frame(varImp(PID_rf))
PIDrf$names <- row.names(PIDrf)
PIDrf <- PIDrf %>% arrange(desc(Overall))
PIDrf <- PIDrf[,c("names", "Overall")]

g_2 <- ggplot(PIDrf, aes(y=Overall, x=names)) + 
  geom_bar(stat = 'identity') + 
  scale_x_discrete(limits = as.character(PIDrf$names)) + 
  xlab("Variable Names") + 
  ylab("Mean Gini Decrease") +
  ggtitle("Variable Importance using Random Forest")




grid.arrange(g1, g2, g_1, g_2, ncol=2)
