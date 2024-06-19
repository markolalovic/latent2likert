context("Testing estimation of parameters")

testthat::test_that("`mean` and `sd` are well estimated given `pk` for normal case", {
  actual <- c(-1, 0.5) # actual mean and sd
  prob <- c("1" = 0.313, "2" = 0.579, "3" = 0.105, "4" = 0.003)
  estimates <- estimate_mean_and_sd(prob, 5)
  testthat::expect_equal(estimates, actual, tolerance = 0.1)
})

testthat::test_that("`mean` and `sd` are well estimated given `pk` for skew case", {
  actual <- c(0.146858, 1.084341) # actual mean and sd
  skew <- -0.4565873 # skewness
  prob <- c(
    "1" = 0.036, "2" = 0.089, "3" = 0.142,
    "4" = 0.185, "5" = 0.21, "6" = 0.201, "7" = 0.137
  )
  estimates <- estimate_mean_and_sd(prob, 7, skew)
  testthat::expect_equal(estimates, actual, tolerance = 0.05)
})
