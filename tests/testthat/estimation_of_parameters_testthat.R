library("testthat")
library("responsesR")
context("Testing estimation of parameters")

set.seed(12345)
test_that("parameters `mu` and `sd` are well estimated given `pk` for normal case", {
  actual <- c(-1, 0.5) # actual mu and sd
  pk <- list("1"=0.313, "2"=0.579, "3"=0.105, "4"=0.003)
  estimates <- estimate_mu_sd(pk=pk, K=5)
  expect_equal(estimates, actual, tolerance=0.01)
})

test_that("parameters `mu` and `sd` are well estimated given `pk` for skew case", {
  actual <- c(0.146858, 1.084341) # actual mu and sd
  gamma1 <- -0.4565873 # skewness
  pk <- list("1"=0.036, "2"=0.089, "3"=0.142, "4"=0.185, "5"=0.21, "6"=0.201, "7"=0.137)
  estimates <- estimate_mu_sd(pk=pk, K=7, gamma1=gamma1)
  expect_equal(estimates, actual, tolerance=0.01)
})

test_that("parameters `mu` and `sd` are well estimated given data for normal case", {
  params <- c("mu"=0.5, "sd"=0.5) # actual parameters
  n <- 1000 # number of observations
  data <- get_responses(n=n, mu=params[["mu"]], sd=params["sd"], K=5)
  estimates <- estimate_parameters(data, 5)
  expect_equal(c(estimates), params, tolerance=0.05)
})

test_that("parameters `mu` and `sd` are well estimated given data for skew case", {
  params <- c("mu"=0.5, "sd"=0.5) # actual parameters
  n <- 1000 # number of observations
  gamma1 <- 0.5 # skewness
  data <- get_responses(n=n, mu=params[["mu"]], sd=params["sd"], gamma1=gamma1, K=5)
  estimates <- estimate_parameters(data, 5, gamma1)
  expect_equal(c(estimates), params, tolerance=0.05)
})

test_that("parameters `mu` and `sd` are well estimated given data for multivariate case", {
  # actual parameters
  mu <- c(0.6, -0.1, 0.3)
  sd <- c(1.3, 0.5, 1.5)

  K <- c(3, 4, 5)
  gamma1 <- c(-0.5, 0, 0.5)

  n <- 1000
  data <- get_responses(n, mu, sd, gamma1, K)

  estimates <- estimate_parameters(data, K, gamma1)
  actual <- as.table(rbind(mu, sd))
  dimnames(actual) <- dimnames(estimates)
  expect_equal(estimates, actual, tolerance=0.05)
})
