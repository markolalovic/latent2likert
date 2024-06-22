context("Testing helper functions")

testthat::test_that("pad_levels gives the correct result", {
  pr <- c("2" = 0.25, "3" = 0.25, "4" = 0.50)
  n_levels <- 5
  actual_pr <- c("1" = 0, "2" = 0.25, "3" = 0.25, "4" = 0.50, "5" = 0)
  padded_pr <- pad_levels(pr, n_levels)
  testthat::expect_equal(padded_pr, actual_pr)
})

testthat::test_that("response_prop gives the correct result,
  univariate case", {
  data <- rep(c(1, 2, 3, 4), each = 2)
  tab <- response_prop(data, n_levels = 4)
  correct_tab <- rep(0.25, 4)
  names(correct_tab) <- 1:4
  testthat::expect_true(identical(tab, correct_tab))
})

testthat::test_that("response_prop gives the correct result,
  multivariate case", {
  y <- rep(c(1, 2, 3, 4), each = 2)
  data <- cbind(y, y)
  tab <- response_prop(data, n_levels = 4)

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


testthat::test_that("plot_likert_transform runs without errors", {
  testthat::expect_error(
    plot_likert_transform(n_items = 3, n_levels = c(3, 4, 5)),
    NA
  )
  testthat::expect_error(
    plot_likert_transform(n_items = 3, n_levels = 5, mean = c(0, 1, 2)),
    NA
  )
  testthat::expect_error(
    plot_likert_transform(n_items = 3, n_levels = 5, sd = c(0.8, 1, 1.2)),
    NA
  )
})

testthat::test_that("delta_skew_normal returns correct value", {
  alpha <- 1
  result <- delta_skew_normal(alpha)
  expected_result <- 0.71
  testthat::expect_equal(result, expected_result, tolerance = 0.05)
})

testthat::test_that("mean_skew_normal returns correct value", {
  alpha <- 1
  result <- mean_skew_normal(alpha)
  expected_result <- 0.56
  testthat::expect_equal(result, expected_result, tolerance = 0.05)
})

testthat::test_that("var_skew_normal returns correct value", {
  alpha <- 1
  result <- var_skew_normal(alpha)
  expected_result <- 0.68
  testthat::expect_equal(result, expected_result, tolerance = 0.05)
})

testthat::test_that("scale_and_shift returns correct values", {
  dp <- c(xi = 1, omega = 2, alpha = 1)
  x <- c(-1, 0, 1)
  result <- scale_and_shift(x, dp)
  expected_result <- c(-0.7179052, -0.2179052, 0.2820948)
  testthat::expect_equal(result, expected_result, tolerance = 0.05)
})
