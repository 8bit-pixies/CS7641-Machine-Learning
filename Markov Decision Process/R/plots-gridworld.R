# plot for car race 50

library(ggplot2)
library(reshape2)
cr50_iter <- data.frame(
  Discount=c(0.65,0.8, 0.9,0.99),
  vi = c(12,18,24,37),
  pi = c(8,8,10,13),
  rvi = c(7,5,3,2)  
)

cr50_time <- data.frame(
  Discount=c(0.65,0.8, 0.9,0.99),
  vi = c(0.0049,0.0064,0.0077,0.0088),
  pi = c(0.0092,0.0093,0.0081,0.0105),
  rvi = c(0.0024,0.0015,0.0007,0.0006)  
)

cr25_iter <- data.frame(
  Discount=c(0.65,0.8, 0.9,0.99),
  vi = c(7,9,12,21),
  pi = c(4,4,4,6),
  rvi = c(5,5,4,2)  
)

cr25_time <- data.frame(
  Discount=c(0.65,0.8, 0.9,0.99),
  vi = c(0.0023,0.0009,0.0010,0.0016),
  pi = c(0.002,0.0012,0.0012,0.0019),
  rvi = c(0.0006647,0.0003776,0.0003336,0.0004183)  
)


# plot for 50...
cr50_iter_ <- melt(cr50_iter, "Discount")
names(cr50_iter_) <- c("Discount", "Algorithm", "Iterations")

cr50_time_ <- melt(cr50_time, "Discount")
names(cr50_time_) <- c("Discount", "Algorithm", "Iterations")

cr25_iter_ <- melt(cr25_iter, "Discount")
names(cr25_iter_ ) <- c("Discount", "Algorithm", "Iterations")

cr25_time_ <- melt(cr25_time, "Discount")
names(cr25_time_) <- c("Discount", "Algorithm", "Iterations")

cr50_iter_$Algorithm <- sapply(cr50_iter_$Algorithm, function(x) {
  if (x=="vi") {return("Value Iteration")}
  if (x=="rvi") {return("Relative Value Iteration")}
  if (x=="pi") {return("Policy Iteration")}
  return("")
})


cr50_time_$Algorithm <- sapply(cr50_time_$Algorithm, function(x) {
  if (x=="vi") {return("Value Iteration")}
  if (x=="rvi") {return("Relative Value Iteration")}
  if (x=="pi") {return("Policy Iteration")}
  return("")
})


cr25_iter_$Algorithm <- sapply(cr25_iter_$Algorithm, function(x) {
  if (x=="vi") {return("Value Iteration")}
  if (x=="rvi") {return("Relative Value Iteration")}
  if (x=="pi") {return("Policy Iteration")}
  return("")
})


cr25_time_$Algorithm <- sapply(cr25_time_$Algorithm, function(x) {
  if (x=="vi") {return("Value Iteration")}
  if (x=="rvi") {return("Relative Value Iteration")}
  if (x=="pi") {return("Policy Iteration")}
  return("")
})

g1 <- ggplot(cr50_iter_, aes(x=Discount, y=Iterations, colour=Algorithm)) +
  geom_line() + ggtitle("Grid World (Iterations) - 11x11") + theme_bw() +
  theme(legend.position="bottom")

g2 <- ggplot(cr50_time_, aes(x=Discount, y=Iterations, colour=Algorithm)) +
  geom_line() + ggtitle("Grid World (Time) - 11x11") + theme_bw() +
  ylab("Time (Seconds)")

g3 <- ggplot(cr25_iter_, aes(x=Discount, y=Iterations, colour=Algorithm)) +
  geom_line() + ggtitle("Grid World (Iterations) - 5x5") + theme_bw()

g4 <- ggplot(cr25_time_, aes(x=Discount, y=Iterations, colour=Algorithm)) +
  geom_line() + ggtitle("Grid World (Time) - 5x5") + theme_bw()+
  ylab("Time (Seconds)")


library(gridExtra)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend<-g_legend(g1)

gg1 <- arrangeGrob(arrangeGrob(g1+ theme(legend.position="none"),
                               g2+ theme(legend.position="none"),
                               g3+ theme(legend.position="none"),
                               g4+ theme(legend.position="none"), ncol=2),
                   mylegend, nrow=2, heights=c(10,1))

