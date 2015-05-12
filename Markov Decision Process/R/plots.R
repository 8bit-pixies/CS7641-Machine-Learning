# plot for car race 50

library(ggplot2)
library(reshape2)
cr50_iter <- data.frame(
  Discount=c(0.65,0.8, 0.9,0.99),
  vi = c(14, 28, 66, 83),
  pi = c(22, 12, 23, 16),
  rvi = c(76, 75, 75, 73)  
  )

cr50_time <- data.frame(
  Discount=c(0.65,0.8, 0.9,0.99),
  vi = c(1.9375,1.8281,2.0938,2.1875),
  pi = c(0.5156,0.4844,0.4375,0.4531),
  rvi = c(0.1406,0.1406,0.1406,0.1406)  
)

cr25_iter <- data.frame(
  Discount=c(0.65,0.8, 0.9,0.99),
  vi = c(14, 28, 33, 36),
  pi = c(11, 10, 13, 11),
  rvi = c(29, 28, 27, 26)  
)

cr25_time <- data.frame(
  Discount=c(0.65,0.8, 0.9,0.99),
  vi = c(0.6875, 0.6719,0.6563,0.7188),
  pi = c(0.0625,0.1094,0.0625,0.0625),
  rvi = c(0.1406,0.1406,0.1406,0.1406)  
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
  geom_line() + ggtitle("Car Race (Iterations) - 50x50") + theme_bw() +
  theme(legend.position="bottom")

g2 <- ggplot(cr50_time_, aes(x=Discount, y=Iterations, colour=Algorithm)) +
  geom_line() + ggtitle("Car Race (Time) - 50x50") + theme_bw() +
  ylab("Time (Seconds)")

g3 <- ggplot(cr25_iter_, aes(x=Discount, y=Iterations, colour=Algorithm)) +
  geom_line() + ggtitle("Car Race (Iterations) - 25x25") + theme_bw()

g4 <- ggplot(cr25_time_, aes(x=Discount, y=Iterations, colour=Algorithm)) +
  geom_line() + ggtitle("Car Race (Time) - 25x25") + theme_bw()+
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


