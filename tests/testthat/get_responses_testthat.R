library(testthat)
context("Testing get responses")

set.seed(12345)

test_that("props of random responses match actual props", {
  n <- 10^6
  mu <- c(0, -1, -1)
  sd <- c(1, 1, 0.5)
  gamma1 <- c(0.5, 0.5, 0.5)
  K <- c(5, 5, 5)
  R <- 0.5

  props <- rbind("Y1"=c("1"=0.162, "2"=0.296, "3"=0.284, "4"=0.188, "5"=0.070),
                 "Y2"=c("1"=0.551, "2"=0.249, "3"=0.131, "4"=0.056, "5"=0.012),
                 "Y3"=c("1"=0.564, "2"=0.370, "3"=0.063, "4"=0.003, "5"=0.000))

  d <- get_responses(n, mu, sd, gamma1, K, R)
  data_props <- t(apply(d, 2, function(q) prop.table(table(q))))
  expect_equal(data_props, props, tolerance=0.01)
})

test_that("correlation of random responses match actual correlation", {
  n <- 10^6
  mu <- c(0, -1, -1)
  sd <- c(1, 1, 0.5)
  gamma1 <- c(0.5, 0.5, 0.5)
  K <- c(5, 5, 5)
  R <- 0.5

  d <- get_responses(n, mu, sd, gamma1, K, R)
  R_data <- cor(d)

  R <- matrix(R, nrow = 3, ncol = 3)
  diag(R) <- 1
  dimnames(R) <- dimnames(R_data)

  expect_equal(R_data, R, tolerance=0.05)
})

test_that("correlation of random responses match actual correlation, harder case", {
  n <- 10^6
  mu <- c(-0.5, 0, 0.5)
  sd <- c(0.5, 0.5, 0.5)
  gamma1 <- c(-0.3, -0.4, -0.5) # harder
  K <- c(6, 6, 6)
  R <- 0.7

  d <- get_responses(n, mu, sd, gamma1, K, R)
  R_data <- cor(d)

  R <- matrix(R, nrow = 3, ncol = 3)
  diag(R) <- 1
  dimnames(R) <- dimnames(R_data)

  expect_equal(R_data, R, tolerance=0.109)
})
