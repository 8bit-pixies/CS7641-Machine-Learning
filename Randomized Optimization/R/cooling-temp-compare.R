x = 2:100
fast <- 100/(x+0.001)
boltz <- 100/log((x+0.001))
exp95 <- 100*(0.95 ^ x)
exp70 <- 100*(0.70 ^ x)

yy <- data.frame(x, fast, boltz, exp95, exp70)

library(reshape2)
library(ggplot2)

d2 <- melt(yy, id="x")
names(d2) <- c("iter", "coolfcn", "temp")

ggplot(d2, aes(x=iter, temp, colour=coolfcn))+
  geom_line() +
  theme_bw() +
  theme(legend.title=element_blank()) +
  xlab("Iteration") + 
  ylab("Temperature")+
  theme(legend.position="bottom")
ggsave("Cooling Function Comparison.png", width=4, height=3)
