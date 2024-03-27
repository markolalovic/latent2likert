context("Testing univariate skew-normal functions")

# parameter conversion test
test_that("convert_params gives the same results as sn::cp2dp", {
  cp <- get_random_cp()
  dp1 <- sn::cp2dp(cp, family="SN")
  dp2 <- convert_params(cp)
  expect_that( identical(dp1, dp2), equals(TRUE) )
})

# density expression test
test_that("d_skew_normal gives the same results as sn::dsn", {
  cp <- get_random_cp()
  dp <- convert_params(cp)

  x <- seq(-5, 5, length = 100)
  y1 <- d_skew_normal(x, dp[["xi"]], dp[["omega"]], dp[["alpha"]])
  y2 <- sn::dsn(x, dp = dp)

  expect_that(y1, equals(y2))
})
