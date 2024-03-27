set.seed(123)

par(mfrow=c(2, 2))
for (i in 1:2) {
  cp <- get_random_cp()
  K <- sample(2:15, 1)

  cat("random cp", i, ":\n \t mu, sd, gamma1 \n \t", cp, "\n")
  sim <- simulate_responses(K, cp)
  pk <- round(sim$pk, 3)
  names(pk) <- 1:K

  barplot(pk)
  estimates <- estimate_mu_sd(pk, K, cp[["gamma1"]], TRUE)
  cat("estimates", i, ":\n \t", estimates, "\n")
}
