library(magrittr)

qplot(seq_along(1:11), 1:11, geom="step", direction = "vh")+theme_bw()

a = intToBits(100)

# count ones bit representation...
x <- Map(function(x) {
  itb <- intToBits(x)
  sum(sapply(itb,as.numeric))
}, 1:256) %>% unlist

qplot(1:256, x, geom="step", direction = "vh")+theme_bw()+xlab("Integer Representation of the Bitstring") +
  ylab("Number of Ones")+aes(color="red")+theme(legend.position="none")
ggsave("Count-ones.png", width=4, height=4)
