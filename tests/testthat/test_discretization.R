context("Testing discretization")

testthat::test_that("discretization of N(0,1) using 4 levels gives expected results", {
    res = discretize_density(density_func = dnorm, n_levels = 4)
    expected = list(
        prob = c(0.16, 0.34, 0.34, 0.16), 
        endp = c(-Inf, -0.98, 0, 0.98, Inf), 
        repr = c(-1.51, -0.45, 0.45, 1.51), 
        dist = 0.12)
    testthat::expect_equal(res, expected, tolerance = 0.05)
})

testthat::test_that("discretization of N(0,1) using 5 levels gives expected results", {
    res = discretize_density(density_func = dnorm, n_levels = 5)
    expected = list(
        prob = c(0.11, 0.24, 0.30, .24, 0.11), 
        endp = c(-Inf, -1.25, -0.38, 0.38, 1.25, Inf), 
        repr = c(-1.73, -0.77, 0, 0.77, 1.73), 
        dist = 0.08)
    testthat::expect_equal(res, expected, tolerance = 0.05)
})

testthat::test_that("discretization of skew-normal with mu=0, sd=1, gamma1=0.5
                    with 5 categories gives expected result", {              
    dp = convert_parameters(c("mu"=0, "sd"=1, "skew"=0.5))
    density_func = function(x) {
        density_sn(x, dp[["xi"]], dp[["omega"]], dp[["alpha"]])
    }
    res = discretize_density(density_func, 5)

    expected = list(
        prob = c(0.16, 0.3, 0.28, 0.19, 0.07), 
        endp = c(-Inf, -0.97, -0.2, 0.58, 1.57, Inf), 
        repr = c(-1.38, -0.57, 0.16, 0.99, 2.14), 
        dist = 0.08)
    testthat::expect_equal(res, expected, tolerance = 0.05)
})



# TODO: test a bunch of random skew normals

# TODO: move to simulation test
test_that("scaling and shifting skew normal parameters works", {
  expect_equal(delta_skew_normal(2), 2 / (sqrt(1 + 4)), tolerance=0.1)
  expect_equal(mean_skew_normal(2), 
               delta_skew_normal(2) * sqrt(2/pi), tolerance=0.1)
  expect_equal(var_skew_normal(2), 0.5, tolerance=0.1)
  
  x <- c(-0.98, 0, 0.98)
  dp <- c("xi"=0.5, "omega"=1, "alpha"=-1)
  x_scaled_and_shifted <- c(-1.48, -0.50, 0.48)
  expect_equal(scale_and_shift(x, dp), x_scaled_and_shifted)
})
