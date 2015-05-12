#' plots

library(magrittr)
library(dplyr)
library(ggplot2)
library(gtable)
library(scales)
library(gsubfn) # for strapplyc
library(gridExtra) # use ?arrangeGrob, over ?grid.arrange

source(file.path("R", "utils.R"))

rast <- read.csv(file.path("ABAGAIL", "jython", "logs", "rast.csv"), header=FALSE)
fourpeaks <- read.csv(file.path("ABAGAIL", "jython", "logs", "fourpeaks.csv"), header=FALSE)
onemax <- read.csv(file.path("ABAGAIL", "jython", "logs", "onemax.csv"), header=FALSE)
knapsack <- read.csv(file.path("ABAGAIL", "jython", "logs", "knapsack.csv"), header=FALSE)

names(rast) <- c("Algorithm", "Description", "Value", "File")
names(onemax) <- c("Algorithm", "Description", "Value", "File")
names(knapsack) <- c("Algorithm", "Description", "Value", "File")
names(fourpeaks) <- c("Algorithm", "Description", "Value", "File")

rast$Description %<>% as.character
onemax$Description %<>% as.character
knapsack$Description %<>% as.character
fourpeaks$Description %<>% as.character

rast$File %<>% as.character
onemax$File %<>% as.character
knapsack$File %<>% as.character
fourpeaks$File %<>% as.character


file2num <- function(x) {
  return(x %>% strapplyc(., "(\\d+)") %>% unlist %>% as.numeric)
}

rast$Run <- rast$File %>% strapplyc(., "(\\d+)") %>% unlist %>% as.numeric
onemax$Run <- onemax$File %>% strapplyc(., "(\\d+)") %>% unlist %>% as.numeric
knapsack$Run <- knapsack$File %>% strapplyc(., "(\\d+)") %>% unlist %>% as.numeric
fourpeaks$Run <- fourpeaks$File %>% strapplyc(., "(\\d+)") %>% unlist %>% as.numeric

# limit to 100 and more

rast <- rast[rast$Run >= 100,]
onemax <- onemax[onemax$Run >= 100,]
knapsack <- knapsack[knapsack$Run >= 100,]
fourpeaks <- fourpeaks[fourpeaks$Run >= 100,]

rast$Description %<>% takeLastWord %>% unlist
onemax$Description %<>% takeLastWord %>% unlist
knapsack$Description %<>% takeLastWord %>% unlist
fourpeaks$Description %<>% takeLastWord %>% unlist

#####

#' rescale all datasets with respect to RHC

rast_baseline <- rast[rast$Algorithm == "RHC",]
onemax_baseline <- onemax[onemax$Algorithm == "RHC",]
knapsack_baseline <- knapsack[knapsack$Algorithm == "RHC",]
knapsack_MIMIC <- knapsack[knapsack$Algorithm == "MIMIC",]
fourpeaks_baseline <- fourpeaks[fourpeaks$Algorithm == "RHC",]

rast_baseline$Base <- rast_baseline$Value
onemax_baseline$Base <- onemax_baseline$Value
knapsack_baseline$Base <- knapsack_baseline$Value
knapsack_MIMIC$Base <- knapsack_MIMIC$Value
fourpeaks_baseline$Base <- fourpeaks_baseline$Value

rast_baseline$Value <- NULL
rast_baseline$Algorithm <- NULL
onemax_baseline$Value <- NULL
onemax_baseline$Algorithm <- NULL
knapsack_baseline$Value <- NULL
knapsack_baseline$Algorithm <- NULL
knapsack_MIMIC$Value <- NULL
knapsack_MIMIC$Algorithm <- NULL
fourpeaks_baseline$Value <- NULL
fourpeaks_baseline$Algorithm <- NULL


rast_all <- left_join(rast, rast_baseline)
onemax_all <- left_join(onemax, onemax_baseline)
knapsack_all <- left_join(knapsack, knapsack_baseline)
knapsack_all_MIMIC <- left_join(knapsack, knapsack_MIMIC)
fourpeaks_all <- left_join(fourpeaks, fourpeaks_baseline)

rast_all$Ratio <- rast_all$Value / rast_all$Base
onemax_all$Ratio <- onemax_all$Value / onemax_all$Base
knapsack_all$Ratio <- knapsack_all$Value / knapsack_all$Base
knapsack_all_MIMIC$Ratio <- knapsack_all_MIMIC$Value / knapsack_all_MIMIC$Base
fourpeaks_all$Ratio <- fourpeaks_all$Value / fourpeaks_all$Base

rast_all$Diff <- rast_all$Value - rast_all$Base
onemax_all$Diff <- onemax_all$Value - onemax_all$Base
knapsack_all$Diff <- knapsack_all$Value - knapsack_all$Base
knapsack_all_MIMIC$Diff <- knapsack_all_MIMIC$Value - knapsack_all_MIMIC$Base
fourpeaks_all$Diff <- fourpeaks_all$Value - fourpeaks_all$Base

#rast_all$Ratio[is.nan(rast_all$Ratio)] <- 0
#rast_all$Ratio[rast_all$Ratio==-Inf] <- 0

#relevel everything
rast$Algorithm <- factor(rast$Algorithm, levels=c("RHC", "SA", "GA", "MIMIC"))
onemax$Algorithm <- factor(onemax$Algorithm, levels=c("RHC", "SA", "GA", "MIMIC"))
knapsack$Algorithm <- factor(knapsack$Algorithm, levels=c("RHC", "SA", "GA", "MIMIC"))
fourpeaks$Algorithm <- factor(fourpeaks$Algorithm, levels=c("RHC", "SA", "GA", "MIMIC"))

rast_all$Algorithm <- factor(rast_all$Algorithm, levels=c("RHC", "SA", "GA", "MIMIC"))
onemax_all$Algorithm <- factor(onemax_all$Algorithm, levels=c("RHC", "SA", "GA", "MIMIC"))
knapsack_all$Algorithm <- factor(knapsack_all$Algorithm, levels=c("RHC", "SA", "GA", "MIMIC"))
knapsack_all_MIMIC$Algorithm <- factor(knapsack_all_MIMIC$Algorithm, levels=c("RHC", "SA", "GA", "MIMIC"))
fourpeaks_all$Algorithm <- factor(fourpeaks_all$Algorithm, levels=c("RHC", "SA", "GA", "MIMIC"))

#####

p1 <- ggplot(rast[rast$Description=="results",], aes(x=Run, y=Value, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  scale_y_continuous(trans=symlog_trans()) + 
  ggtitle("Rastrigin Function - Max Value") +
  ylab("Value - Log 10 Scale") +
  theme_bw() +
  theme(legend.position="bottom")

#ggsave("rast_results.png", width=8, height=5)

ggplot(rast[rast$Description=="time",], aes(x=Run, y=Value, group=Algorithm)) +
  geom_line(aes(colour=Algorithm))+
  #scale_y_continuous(trans=log_trans(base=exp(1)))
  scale_y_continuous(trans=log2_trans()) + 
  ylab("Value - Log 2 Scale") +
  xlab("Size of the Vector") +
  ggtitle("Rastrigin Function - Time Taken") +
  theme_bw()

#ggsave("rast_time.png", width=8, height=5)



# ggplot(rast_all[rast_all$Description=="results",], aes(x=Run, y=Diff, group=Algorithm)) +
#   geom_line(aes(colour=Algorithm)) +
#   scale_y_continuous(trans=symlog_trans()) + 
#   ggtitle("Rastrigin Function - Max Value") +
#   theme_bw()


p2 <- ggplot(rast_all[rast_all$Description=="time",], aes(x=Run, y=Ratio, group=Algorithm)) +
  geom_line(aes(colour=Algorithm))+
  #scale_y_continuous(trans=log_trans(base=exp(1)))
  scale_y_continuous(trans=log2_trans()) + 
  ylab("Ratio to RHC - Log 2 Scale") +
  xlab("Size of the Vector") +
  ggtitle("Rastrigin Function - Ratio of Time Taken to RHC") +
  theme_bw()

mylegend<-g_legend(p1)
p3 <- arrangeGrob(arrangeGrob(p1 + theme(legend.position="none"),
                              p2 + theme(legend.position="none"),
                              nrow=1),
                  mylegend, nrow=2,heights=c(10, 1))

ggsave("rast.png", p3, width=10, height=5)


#####

ggplot(onemax[onemax$Description=="results",], aes(x=Run, y=Value, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  scale_y_continuous(trans=symlog_trans()) + 
  ggtitle("One Max Function - Max Value") +
  theme_bw()

ggplot(onemax[onemax$Description=="calls",], aes(x=Run, y=Value, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  scale_y_continuous(trans=log10_trans()) + 
  theme_bw()

ggsave("onemax_results.png", width=8, height=5)

ggplot(onemax[onemax$Description=="time",], aes(x=Run, y=Value, group=Algorithm)) +
  geom_line(aes(colour=Algorithm))+
  #scale_y_continuous(trans=log_trans(base=exp(1)))
  scale_y_continuous(trans=log2_trans()) + 
  ggtitle("Count Ones Function - Time Taken") +
  theme_bw()

ggsave("onemax_time.png", width=8, height=5)




p1 <- ggplot(onemax_all[onemax_all$Description=="results",], aes(x=Run, y=Diff, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  scale_y_continuous(trans=symlog_trans()) + 
  ylab("Ratio to RHC - Log 10 Scale") +
  xlab("Size of Vector") +
  ggtitle("Count Ones - Ratio of Max Values to RHC") +
  theme_bw() +
  theme(legend.position="bottom")


p2 <- ggplot(onemax_all[onemax_all$Description=="time",], aes(x=Run, y=Ratio, group=Algorithm)) +
  geom_line(aes(colour=Algorithm))+
  #scale_y_continuous(trans=log_trans(base=exp(1)))
  scale_y_continuous(trans=log2_trans()) + 
  ylab("Ratio to RHC - Log 2 Scale") + 
  xlab("Size of Vector") + 
  ggtitle("Count Ones - Ratio of Time Taken to RHC") +
  theme_bw()

p3 <- ggplot(onemax_all[onemax_all$Description=="calls",], aes(x=Run, y=Ratio, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  scale_y_continuous(trans=log10_trans()) +
  ylab("Ratio to RHC - Log 10 Scale") + 
  xlab("Size of Vector") + 
  ggtitle("Count Ones - Ratio of f-evals to RHC") +
  theme_bw()

mylegend<-g_legend(p1)
p4 <- arrangeGrob(arrangeGrob(p1 + theme(legend.position="none"),
                              p2 + theme(legend.position="none"),
                              p3 + theme(legend.position="none"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 1))

ggsave("onemax.png", p4, width=10, height=8)

#####

ggplot(knapsack[knapsack$Description=="results",], aes(x=Run, y=Value, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  scale_y_continuous(trans=symlog_trans()) + 
  ggtitle("Knapsack - Max Value Taken") +
  theme_bw()

ggplot(knapsack[knapsack$Description=="calls",], aes(x=Run, y=Value, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  scale_y_continuous(trans=symlog_trans()) + 
  ggtitle("Knapsack - Max Value Taken") +
  theme_bw()


ggsave("knapsack_results.png", width=8, height=5)

ggplot(knapsack[knapsack$Description=="time",], aes(x=Run, y=Value, group=Algorithm)) +
  geom_line(aes(colour=Algorithm))+
  #scale_y_continuous(trans=log_trans(base=exp(1)))
  scale_y_continuous(trans=log2_trans()) + 
  ggtitle("Knapsack - Time Taken") +
  theme_bw()

ggsave("knapsack_time.png", width=8, height=5)




ggplot(knapsack_all[knapsack_all$Description=="results",], aes(x=Run, y=Diff, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  ggtitle("Knapsack - Difference from RHC") +
  theme_bw()

ggplot(knapsack_all[knapsack_all$Description=="calls",], aes(x=Run, y=Diff, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  scale_y_continuous(trans=symlog_trans()) + 
  ggtitle("Knapsack - Difference from RHC") +
  theme_bw()

p1<- ggplot(knapsack_all_MIMIC[knapsack_all_MIMIC$Description=="results",], aes(x=Run, y=Ratio, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  ggtitle("Knapsack - Ratio from MIMIC") +
  ylab("Ratio to MIMIC") + 
  xlab("Number of Items") + 
  theme_bw() +
  theme(legend.position="bottom")


p2 <- ggplot(knapsack_all[knapsack_all$Description=="time",], aes(x=Run, y=Ratio, group=Algorithm)) +
  geom_line(aes(colour=Algorithm))+
  #scale_y_continuous(trans=log_trans(base=exp(1)))
  scale_y_continuous(trans=symlog_trans()) + 
  ggtitle("Knapsack - Ratio of Time Taken from RHC") +
  ylab("Ratio to RHC - Log 10 Scale") + 
  xlab("Number of Items") + 
  theme_bw()

p3 <- ggplot(knapsack_all[knapsack_all$Description=="calls",], aes(x=Run, y=Ratio, group=Algorithm)) +
  geom_line(aes(colour=Algorithm))+
  #scale_y_continuous(trans=log_trans(base=exp(1)))
  scale_y_continuous(trans=log2_trans()) + 
  ylab("Value - Log 2 Scale") +
  xlab("Number of Items") +
  ggtitle("Knapsack - Ratio of f-evals to RHC") +
  theme_bw()

mylegend<-g_legend(p1)
p4 <- arrangeGrob(arrangeGrob(p1 + theme(legend.position="none"),
                              p2 + theme(legend.position="none"),
                              p3 + theme(legend.position="none"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 1))

ggsave("knapsack.png", p4, width=10, height=8)

#####

ggplot(fourpeaks[fourpeaks$Description=="results",], aes(x=Run, y=Value, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  scale_y_continuous() + 
  ggtitle("Four Peaks - Max Value") +
  ylab("Value") +
  theme_bw()

#ggsave("rast_results.png", width=8, height=5)

ggplot(fourpeaks[fourpeaks$Description=="time",], aes(x=Run, y=Value, group=Algorithm)) +
  geom_line(aes(colour=Algorithm))+
  #scale_y_continuous(trans=log_trans(base=exp(1)))
  scale_y_continuous(trans=symlog_trans()) + 
  ylab("Value - Log 2 Scale") +
  xlab("Size of the Vector") +
  ggtitle("Rastrigin Function - Time Taken") +
  theme_bw()




ggplot(fourpeaks_all[fourpeaks_all$Description=="calls",], aes(x=Run, y=Ratio, group=Algorithm)) +
  geom_line(aes(colour=Algorithm))+
  #scale_y_continuous(trans=log_trans(base=exp(1)))
  scale_y_continuous(trans=log2_trans()) + 
  ylab("Value - Log 2 Scale") +
  xlab("Size of the Vector") +
  ggtitle("Rastrigin Function - Time Taken") +
  theme_bw()

ggsave("fourpeaks_calls.png", width=8, height=5)

p1 <- ggplot(fourpeaks_all[fourpeaks_all$Description=="results",], aes(x=Run, y=Ratio, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  scale_y_continuous() + 
  ggtitle("Four Peaks - Ratio of Max Value to RHC") +
  xlab("Size of the Vector") +
  theme_bw() +
  theme(legend.position="bottom")

p2 <- ggplot(fourpeaks_all[fourpeaks_all$Description=="time",], aes(x=Run, y=Ratio, group=Algorithm)) +
  geom_line(aes(colour=Algorithm))+
  #scale_y_continuous(trans=log_trans(base=exp(1)))
  scale_y_continuous(trans=log2_trans()) + 
  ylab("Ratio to RHC - Log 2 Scale") +
  xlab("Size of the Vector") +
  ggtitle("Four Peaks - Ratio of Time Taken to RHC") +  
  theme_bw() 

p3 <- ggplot(fourpeaks_all[fourpeaks_all$Description=="calls",], aes(x=Run, y=Ratio, group=Algorithm)) +
  geom_line(aes(colour=Algorithm))+
  #scale_y_continuous(trans=log_trans(base=exp(1)))
  scale_y_continuous(trans=log2_trans()) + 
  ylab("Value - Log 2 Scale") +
  xlab("Size of the Vector") +
  ggtitle("Four Peaks - Ratio of f-evals to RHC") +
  theme_bw()

mylegend<-g_legend(p1)
p4 <- arrangeGrob(arrangeGrob(p1 + theme(legend.position="none"),
                              p2 + theme(legend.position="none"),
                              p3 + theme(legend.position="none"),
                              nrow=2),
                  mylegend, nrow=2,heights=c(10, 1))

ggsave("fourpeaks.png", p4, width=10, height=8)







plot(1:100)


