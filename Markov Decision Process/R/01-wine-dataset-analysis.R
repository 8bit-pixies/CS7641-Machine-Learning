#' wine dataset analysis
library(caret)
library(plyr)
library(dplyr)
library(reshape2)
library(GGally)
library(ggbiplot)
library(gtable)
library(randomForest)

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

g11 <- ggplot(wq_data, aes(x=PC, y=Variance))+
  geom_point() + 
  geom_line() + 
  xlab("Factor Number")+
  ylab("Eigenvalue")+
  theme(legend.position = "none") +
  ggtitle("Scree Plot - for Wine Quality") + 
  theme_bw()

ggplot(wq_propvar, aes(x=x, y=cumvar))+
  geom_bar(stat="identity") +
  xlab("Number of Components")+
  ylab("Proportion of Variance Explained")+
  theme_bw()

g12 <- ggbiplot(wq_pca, groups = wq$quality,
         obs.scale = 1, var.scale = 1)+
  theme_bw()

wqpca=cbind(as.data.frame(wq_pca$x), quality=wq$quality)

#' ICA
wq_ica = fastICA::fastICA(wq_nl, n.comp=11, verbose=TRUE, row.norm=TRUE)
wqica=cbind(as.data.frame(wq_ica$S), quality=wq$quality)
sapply(wqica[,!(names(wqica) %in% c("quality"))], function(x) (e1071::kurtosis(x))^2)

sapply(wqica[,!(names(wqica) %in% c("quality"))], function(x) abs(e1071::kurtosis(x)))

# remember to remove the ones that don't look normal. 

# ggplot...
histtop <- ggplot(wqica) + geom_density(aes(wqica$V2))
histright <- ggplot(wqica) + geom_density(aes(wqica$V7))+coord_flip()
scatterp <- ggplot(wqica) + geom_point(aes(wqica$V2, wqica$V7))

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

#' random projections
wq_rca <- Map(function(x) {
  gaussian_random_projection(wq_nl, 8)
}, 1:100)

# get the ones which immitate the result best.
wq_rca_diff <- Map(function(x) {
  sum((wq_nl - (x$RP %*% MASS::ginv(x$R)))^2)
}, wq_rca)

wq_pca_diff <- sum((wq_pp - (wq_pca$x %*% MASS::ginv(wq_pca$rotation)))^2)
wq_ica_diff <- sum((wq_pp - (wq_ica$S %*% MASS::ginv(wq_ica$K %*% wq_ica$W)))^2)

# for ? components
wq_pcarca_diff <- function(n.comp) {
  wqpcadiff <- sum((wq_pp - (wq_pca$x[,1:n.comp] %*% MASS::ginv(wq_pca$rotation[,1:n.comp])))^2)
  wqica = fastICA::fastICA(wq_nl, n.comp=n.comp, verbose=TRUE, row.norm=TRUE)  
  wqicadiff = sum((wq_pp - (wqica$S %*% MASS::ginv(wqica$K %*% wqica$W)))^2)
  
  wqrca <- Map(function(x) {
    gaussian_random_projection(wq_nl, n.comp)
  }, 1:100)
  
  # get the ones which immitate the result best.
  wqrcadiff <- Map(function(x) {
    sum((wq_nl - (x$RP %*% MASS::ginv(x$R)))^2)
  }, wqrca) %>% unlist
  
  return(list(PCA=wqpcadiff, ICA=wqicadiff, wqrcadiff=wqrcadiff))
}

wq_diff_all <- Map(function(x) {
  diff_dat <- melt(wq_pcarca_diff(x))
  diff_dat$ncomp <- x  
  return(diff_dat)
}, 1:dim(wq_nl)[2]) %>% Reduce(f=rbind, x=.)

# get 95 and 5th percentile of anything with pidrcadiff...
rcap95 <- wq_diff_all %>% 
  filter(L1=="wqrcadiff") %>%
  ddply(., .(ncomp), summarize, value=quantile(value, probs=0.95)) %>%
  mutate(L1="wqrcadiff", percentile=0.95)

rcap05 <- wq_diff_all %>% 
  filter(L1=="wqrcadiff") %>%
  ddply(., .(ncomp), summarize, value=quantile(value, probs=0.05)) %>%
  mutate(L1="wqrcadiff", percentile=0.05)

rcapall <- rbind(rcap95, rcap05, 
                 mutate(filter(wq_diff_all, L1!="wqrcadiff"), percentile=1))
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

# apply randomforest to get the mean gini, variable importance.
wq_rf <- randomForest(quality ~., wq)
wqrf <- as.data.frame(varImp(wq_rf))
wqrf$names <- row.names(wqrf)
wqrf <- wqrf %>% arrange((Overall))
wqrf <- wqrf[,c("names", "Overall")]

g_2 <- ggplot(wqrf, aes(y=Overall, x=names)) + geom_bar(stat = 'identity') + 
  scale_x_discrete(limits = as.character(wqrf$names))+
xlab("Variable Names") + 
  ylab("Mean Gini Decrease") +
  ggtitle("Variable Importance using Random Forest")+ coord_flip()

grid.arrange(g11, g12, g_1, g_2, ncol=2)

