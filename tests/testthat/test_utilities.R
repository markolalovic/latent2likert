context("Testing helper functions")

testthat::test_that("pad_levels gives the correct result", {
  pr <- c("2" = 0.25, "3" = 0.25, "4" = 0.50)
  n_levels <- 5
  actual_pr <- c("1" = 0, "2" = 0.25, "3" = 0.25, "4" = 0.50, "5" = 0)
  padded_pr <- pad_levels(pr, n_levels)
  testthat::expect_equal(padded_pr, actual_pr)
})

testthat::test_that("get_prop_table gives the correct result, univariate case", {
  data <- rep(c(1, 2, 3, 4), each = 2)
  tab <- get_prop_table(data, K = 4)
  correct_tab <- rep(0.25, 4)
  names(correct_tab) <- 1:4
  testthat::expect_true(identical(tab, correct_tab))
})

testthat::test_that("get_prop_table gives the correct result, multivariate case", {
  y <- rep(c(1, 2, 3, 4), each = 2)
  data <- cbind(y, y)
  tab <- get_prop_table(data, K = 4)

  correct_tab <- rbind(rep(0.25, 4), rep(0.25, 4))
  dimnames(correct_tab) <- dimnames(tab)

  testthat::expect_true(identical(tab, correct_tab))
})

testthat::test_that("pad_levels gives the correct result", {
  pk <- rep(0.25, 4)
  names(pk) <- 1:4
  padded_pk <- pad_levels(pk, 5)

  correct_pk <- c(rep(0.25, 4), 0)
  names(correct_pk) <- 1:5
  testthat::expect_true(identical(padded_pk, correct_pk))
})

testthat::test_that("percentify gives the correct result", {
  xbreaks <- seq(from = 0, to = 1, length.out = 6)
  xlabs <- vapply(xbreaks, percentify, character(1))
  correct_xlabs <- c("0%", "20%", "40%", "60%", "80%", "100%")
  testthat::expect_true(identical(xlabs, correct_xlabs))
})

# parameter conversion test
testthat::test_that("convert_params gives the same results as sn::cp2dp", {
  cp <- generate_random_cp()
  dp1 <- sn::cp2dp(cp, family = "SN")
  dp2 <- convert_params(cp)
  testthat::expect_true(all.equal(dp1, dp2))
})

# density expression test
testthat::test_that("density_sn gives the same results as sn::dsn", {
  cp <- generate_random_cp()
  dp <- convert_params(cp)

  x <- seq(-5, 5, length = 100)
  y1 <- density_sn(x, dp[["xi"]], dp[["omega"]], dp[["alpha"]])
  y2 <- sn::dsn(x, dp = dp)

  testthat::expect_equal(y1, y2)
})
