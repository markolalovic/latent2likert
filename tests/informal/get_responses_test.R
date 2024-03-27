# testing get_responses function
compare_barplots <- function(n, mu, sd, gamma1, K, R) {
  d1 <- get_responses(n, mu, sd, gamma1, K, 0)
  d2 <- get_responses(n, mu, sd, gamma1, K, R)

  print(round(cor(d2), 3))

  par(mfrow=c(2, 3))
  for(i in 1:3) {
    barplot(prop.table(table(d1[, i])))
  }
  for(i in 1:3) {
    barplot(prop.table(table(d2[, i])))
  }
}

set.seed(12345)
R <- get_rand_corr_matrix(3)
R
compare_barplots(n=10^6,
                 mu=c(-1, 0, 1),
                 sd=c(1, 1, 1),
                 gamma1=c(0, 0, 0),
                 K=c(6, 6, 6),
                 R=R)

compare_barplots(n=10^6,
                 mu=c(0, 0, 0),
                 sd=c(0.5, 1, 1.5),
                 gamma1=c(0, 0, 0),
                 K=c(6, 6, 6),
                 R=0.9)

compare_barplots(n=10^6,
                 mu=c(0.5, 1, 0),
                 sd=c(1, 1, 0.5),
                 gamma1=c(0, 0, 0),
                 K=c(6, 6, 6),
                 R=0.9)

compare_barplots(n=10^6,
                 mu=c(-0.5, 0, 0.5),
                 sd=c(0.5, 0.5, 0.5),
                 gamma1=c(-0.3, -0.4, -0.5),
                 K=c(6, 6, 6),
                 R=0.9)

compare_barplots(n=10^6,
                 mu=c(-0.5, 0, 0.5),
                 sd=c(1, 1, 1),
                 gamma1=c(0.6, 0.4, 0.6),
                 K=c(6, 6, 6),
                 R=0.5)
