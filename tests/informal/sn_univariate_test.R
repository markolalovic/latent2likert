setwd("/home/marko/repos/responsesR/R/")
source("simulation_of_responses.R")
source("helper_functions.R")

set.seed(12345)

# test that sn::cp2dp and convert_params give the same results
ntests <- 1000
for (i in 1:ntests) {
  cp <- get_random_cp()
  dp1 <- sn::cp2dp(cp, family="SN")
  dp2 <- convert_params(cp)

  stopifnot(identical(dp1, dp2))
}

# test that sn::dsn and d_skew_normal give the same results
x <- seq(-5, 5, length = 100)
tol <- 1e-3
par(mfrow=c(2, 2))
for (i in 1:2) {
  cp <- get_random_cp()
  dp <- convert_params(cp)

  y1 <- d_skew_normal(x, dp[["xi"]], dp[["omega"]], dp[["alpha"]])
  y2 <- sn::dsn(x, dp = dp)

  plot(x, y1, type="l", lwd = 2, xlab = "", ylab = "")
  plot(x, y2, type="l", lwd = 2, xlab = "", ylab = "")
}

for (i in 1:ntests) {
  cp <- get_random_cp()
  dp <- convert_params(cp)

  y1 <- d_skew_normal(x, dp[["xi"]], dp[["omega"]], dp[["alpha"]])
  y2 <- sn::dsn(x, dp = dp)

  stopifnot(sum(y1 - y2) < tol)
}

# test that sn::rsn and get_responses give similar results
compare_rsn_to_responses <- function() {
  n <- 10^6
  K <- 15
  cp <- c(0, 0.5, runif(1, -0.5, 0.5))
  names(cp) <- c("mu", "sd", "gamma1")

  data <- get_responses(n, cp[["mu"]], cp[["sd"]], cp[["gamma1"]], K)

  dp <- convert_params(cp)
  y <- sn::rsn(n, dp = dp)
  cat("skewness, gamma1: \n")
  cat(e1071::skewness(y), cp["gamma1"], "\n")

  barplot(prop.table(table(data)))
  hist(y)
}
set.seed(12345)
par(mfrow=c(2, 2))
compare_rsn_to_responses()
compare_rsn_to_responses()

par(mfrow=c(2, 2))
compare_rsn_to_responses()
compare_rsn_to_responses()
