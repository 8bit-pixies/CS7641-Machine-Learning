source("R/random_projection_gauss.R")

johnson_loindenstrauss_min_dim <- function(n_samples, eps=0.1) {
  denominator = (eps ** 2 / 2) - (eps ** 3 / 3)
  # the min function is required in this case, due to the theorem 
  # which says that we require k0 to be less than k. 
  return (floor(4 * log(n_samples) / denominator))
}

jlm <- expand.grid(dim=seq(from=10, to=10^4, length.out=1000), eps=c(0.1,0.3,0.5,0.8,1))
jlm <- as.data.frame(jlm)
jlm$nmin <- apply(jlm, 1, function(x) {johnson_loindenstrauss_min_dim(x[1], x[2])})
jlm$eps <- as.factor(jlm$eps)
g1 <- ggplot(jlm, aes(x=dim, y=nmin, group=eps, colour=eps)) + 
  geom_line() + 
  ggtitle("Johnson-Lindenstrauss bounds") + 
  xlab("# observations to eps-embed")+
  ylab("Min # dim (log 10 scale)") +
  scale_y_log10()+
  theme_bw()

jlm1 <- expand.grid(dim=seq(from=1, to=10^2, length.out=100), eps=c(0.1,0.3,0.5,0.8,1))
jlm1 <- as.data.frame(jlm1)
jlm1$nmin <- apply(jlm1, 1, function(x) {johnson_loindenstrauss_min_dim(x[1], x[2])})
jlm1$eps <- as.factor(jlm1$eps)
g2 <- ggplot(jlm1, aes(x=dim, y=nmin, group=eps, colour=eps)) + 
  geom_line() + 
  ggtitle("Johnson-Lindenstrauss bounds") + 
  xlab("# observations to eps-embed")+
  ylab("Min # dim (log 10 scale)") +
  scale_y_log10()+
  theme_bw()

grid.arrange(g1, g2, ncol=2)