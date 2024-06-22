context("Testing discretization")

test_that("discretization of N(0,1) using 4 levels gives expected results", {
  res <- discretize_density(density_fn = dnorm, n_levels = 4)
  expected <- list(
    prob = c(0.16, 0.34, 0.34, 0.16),
    endp = c(-Inf, -0.98, 0, 0.98, Inf),
    repr = c(-1.51, -0.45, 0.45, 1.51),
    dist = 0.12
  )
  expect_equal(res, expected, tolerance = 0.05)
})

test_that("discretization of N(0,1) using 5 levels gives expected results", {
  res <- discretize_density(density_fn = dnorm, n_levels = 5)
  expected <- list(
    prob = c(0.11, 0.24, 0.30, .24, 0.11),
    endp = c(-Inf, -1.25, -0.38, 0.38, 1.25, Inf),
    repr = c(-1.73, -0.77, 0, 0.77, 1.73),
    dist = 0.08
  )
  expect_equal(res, expected, tolerance = 0.05)
})
