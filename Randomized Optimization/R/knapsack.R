#fourpeaks

library(magrittr)
library(dplyr)
library(ggplot2)
library(gtable)
library(scales)
library(gsubfn) # for strapplyc
library(gridExtra) # use ?arrangeGrob, over ?grid.arrange

source(file.path("R", "utils.R"))

onemax <- read.csv(file.path("ABAGAIL", "jython", "logs", "knapsack.csv"), header=FALSE)
names(onemax) <- c("Algo_type", "Description", "Value", "File")
onemax$Algorithm <- onemax$Algo_type
# rename the levels for SA
levels(onemax$Algorithm) <- c("GA", "MIMIC", "RHC", "SA", "SA", "SA", "SA", "SA")

onemax$Description %<>% as.character
onemax$Description %<>% takeLastWord %>% unlist
onemax$File %<>% as.character

file2num <- function(x, num) {
  y <- x %>% strapplyc(., "(\\d+)") %>% unlist %>% as.numeric
  return(y[num])
}
# unique identifier
onemax$UID <- paste0(onemax$File, onemax$Algo_type, collpase="")
onemax$problemsize <- sapply(onemax$File, function(x) {file2num(x, 1)})

# get the best subset of stuff that we want for our final plots
final_dat <- onemax[onemax$Description == "results",] %>% group_by(Algorithm, problemsize) %>%
  filter(Value == max(Value)) %>%
  arrange(Algorithm, problemsize)

# filter by UID

# final data set prepare for plots...
final_set <- onemax[onemax$UID %in% final_dat$UID,]

baseline <- final_set[final_set$Algorithm == "RHC",]
baseline$Base <- baseline$Value
baseline$Value <- NULL
baseline$Algorithm <- NULL
baseline$Algo_type <- NULL
baseline$UID <- NULL
baseline$File <- NULL

baseline_MIMIC <- final_set[final_set$Algorithm == "MIMIC",]
baseline_MIMIC$Base <- baseline_MIMIC$Value
baseline_MIMIC$Value <- NULL
baseline_MIMIC$Algorithm <- NULL
baseline_MIMIC$Algo_type <- NULL
baseline_MIMIC$UID <- NULL
baseline_MIMIC$File <- NULL

final_all <- left_join(final_set, baseline)
final_all$Ratio <- final_all$Value / final_all$Base
final_all$Diff <- final_all$Value - final_all$Base

final_all$Algorithm <- factor(final_all$Algorithm, levels=c("RHC", "SA", "GA", "MIMIC"))
final_all$Run <- final_all$problemsize


final_all_MIMIC <- left_join(final_set, baseline_MIMIC)
final_all_MIMIC$Ratio <- final_all_MIMIC$Value / final_all_MIMIC$Base
final_all_MIMIC$Diff <- final_all_MIMIC$Value - final_all_MIMIC$Base
final_all_MIMIC$Algorithm <- factor(final_all_MIMIC$Algorithm, levels=c("RHC", "SA", "GA", "MIMIC"))
final_all_MIMIC$Run <- final_all_MIMIC$problemsize

final_set$Algorithm <- factor(final_set$Algorithm, levels=c("RHC", "SA", "GA", "MIMIC"))
final_set$Run <- final_set$problemsize


##################


ggplot(final_set[final_set$Description=="results",], aes(x=Run, y=Value, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  scale_y_continuous() + 
  ggtitle("One Max Function - Max Value") +
  theme_bw()+geom_abline(intercept=0, slope=1)

p2 <- ggplot(final_set[final_set$Description=="calls",], aes(x=Run, y=Value, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  scale_y_continuous() + 
  theme_bw() +
  ylab("Number of Function Calls") +
  xlab("Problem Size") +
  ggtitle("Count Ones - Number of Function Calls")

ggsave("onemax_results.png", width=8, height=5)

ggplot(final_set[final_set$Description=="time",], aes(x=Run, y=Value, group=Algorithm)) +
  geom_line(aes(colour=Algorithm))+
  #scale_y_continuous(trans=log_trans(base=exp(1)))
  scale_y_continuous(trans=log2_trans()) + 
  ggtitle("Count Ones Function - Time Taken") +
  theme_bw()

ggsave("onemax_time.png", width=8, height=5)




ggplot(final_all[final_all$Description=="results",], aes(x=Run, y=Ratio, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  scale_y_continuous() + 
  ylab("Ratio to RHC") +
  xlab("Size of Vector") +
  ggtitle("Count Ones - Ratio of Max Values to RHC") +
  theme_bw() +
  theme(legend.position="bottom")





###



p1<- ggplot(final_all_MIMIC[final_all_MIMIC$Description=="results",], aes(x=Run, y=Ratio, group=Algorithm)) +
  geom_line(aes(colour=Algorithm)) +
  ggtitle("Ratio from MIMIC") +
  ylab("Ratio to MIMIC") + 
  xlab("Number of Items") + 
  theme_bw() +
  theme(legend.position="bottom")


p3 <- ggplot(final_all[final_all$Description=="time",], aes(x=Run, y=Ratio, group=Algorithm)) +
  geom_line(aes(colour=Algorithm))+
  #scale_y_continuous(trans=log_trans(base=exp(1)))
  scale_y_continuous() + 
  ggtitle("Ratio of Time Taken from RHC") +
  ylab("Ratio to RHC") + 
  xlab("Number of Items") + 
  theme_bw()

p2 <- ggplot(final_all[final_all$Description=="calls",], aes(x=Run, y=Ratio, group=Algorithm)) +
  geom_line(aes(colour=Algorithm))+
  #scale_y_continuous(trans=log_trans(base=exp(1)))
  scale_y_continuous() + 
  ylab("Value") +
  xlab("Number of Items") +
  ggtitle("Ratio of f-evals to RHC") +
  theme_bw()

mylegend<-g_legend(p1)
p4 <- arrangeGrob(arrangeGrob(p1 + theme(legend.position="none"),
                              p2 + theme(legend.position="none"),
                              p3 + theme(legend.position="none"),
                              nrow=1),
                  mylegend, nrow=2,heights=c(6, 1))

ggsave("knapsack.png", p4, width=10, height=4)






