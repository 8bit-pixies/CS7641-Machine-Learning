ks.default <- function(rows) seq(2, max(3, rows %/% 4))

many_kmeans <- function(x, ks = ks.default(nrow(x)), ...) {
  ldply(seq_along(ks), function(i) {
    cl <- kmeans(x, centers = ks[i], ...)
    data.frame(obs = seq_len(nrow(x)), i = i, k = ks[i], cluster = cl$cluster)
  })
}

all_hclust <- function(x, ks = ks.default(nrow(x)), point.dist = "euclidean", cluster.dist = "ward") {
  d <- dist(x, method = point.dist)
  cl <- hclust(d, method = cluster.dist)
  
  ldply(seq_along(ks), function(i) {
    data.frame(
      obs = seq_len(nrow(x)), i = i, k = ks[i], 
      cluster = cutree(cl, ks[i])
    )
  })  
}

center <- function(x) x - mean(range(x))

#' @param clusters data frame giving cluster assignments as produced by 
#'   many_kmeans or all_hclust
#' @param y value to plot on the y-axis.  Should be length
#'   \code{max(clusters$obs)}
clustergram <- function(clusters, y, line.width = NULL) {
  clusters$y <- y[clusters$obs]
  clusters$center <- ave(clusters$y, clusters$i, clusters$cluster)  

  if (is.null(line.width)) {
    line.width <- 0.5 * diff(range(clusters$center, na.rm = TRUE)) / 
      length(unique(clusters$obs))
  }
  clusters$line.width <- line.width
  
  # Adjust center positions so that they don't overlap  
  clusters <- clusters[with(clusters, order(i, center, y, obs)), ]  
  clusters <- ddply(clusters, c("i", "cluster"), transform, 
    adj = center + (line.width * center(seq_along(y)))
  )
  
  structure(clusters, 
    class = c("clustergram", class(clusters)),
    line.width = line.width)
}

plot.clustergram <- function(x) {
  i_pos <- !duplicated(x$i)
  
  means <- ddply(x, c("cluster", "i"), summarise, 
    min = min(adj), max = max(adj), mean=mean(adj))
  
  ggplot(x, aes(i)) +
    geom_ribbon(aes(y = adj, group = obs, fill = y, ymin = adj - line.width/2, ymax = adj + line.width/2)) + 
    #geom_errorbar(aes(ymin = min, ymax = max), data = means, width = 0.1) + 
    geom_point(aes(y=mean, size=2), data=means)+
    scale_x_continuous("Cluster", breaks = x$i[i_pos], labels = x$k[i_pos]) +
    labs(y = "Cluster average", colour = "Obs\nvalue", fill = "Obs\nvalue")    
}


