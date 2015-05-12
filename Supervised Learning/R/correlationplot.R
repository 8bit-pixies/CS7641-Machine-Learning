# correlation plot

library(corrgram)
library(corrplot)

library(gridExtra)

library(ggplot2)

# doesn't work
plot1 <- corrgram(PimaIndiansDiabetes, order="OLO",lower.panel=panel.shade,
                  upper.panel=panel.conf)

plot2 <- corrgram(wq.red, order="OLO",lower.panel=panel.shade,
                  upper.panel=NULL)


# doesn't work
par(mfrow=c(1,2))

plot1 <- corrplot.mixed(cor(PimaIndiansDiabetes[,!(names(PimaIndiansDiabetes) %in% c("diabetes"))]), 
                        lower="square", upper="number",
                        order="hclust", tl.cex=0.7, cl.cex=0.5, tl.col="black")
plot2 <- corrplot.mixed(cor(wq.red[,!(names(wq.red) %in% c("quality"))]), 
                  lower="square", upper="number",
                  order="hclust", tl.cex=0.7, cl.cex=0.5, tl.col="black")

# ggplot


plot1 <- corrplot(cor(PimaIndiansDiabetes[,!(names(PimaIndiansDiabetes) %in% c("diabetes"))]), 
                  method="square",
                        order="hclust", tl.cex=0.7, cl.cex=0.5, tl.col="black", addrect=2)
plot2 <- corrplot(cor(wq.red[,!(names(wq.red) %in% c("quality"))]), 
                  method="square",
                        order="hclust", tl.cex=0.7, cl.cex=0.5, tl.col="black", addrect=3)

par(mfrow=c(1,1))



