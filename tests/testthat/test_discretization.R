context("Testing optimal discretization")

test_that("discretization of N(0,1) with 5 categories gives expected result", {
  sim <- simulate_responses(K=5, params=c("mu"=0, "sd"=1, "gamma1"=0))
  pk <- c(0.107, 0.244, 0.298, 0.244, 0.107) # expected probabilities
  expect_equal(sim$pk, pk, tolerance=0.1)
})

test_that("discretization of N(0,1)  with 4 categories gives expected results", {
  sim <- simulate_responses(K=4, params=c("mu"=0, "sd"=1, "gamma1"=0))
  expected_sim <- list(
    "pk"=c(0.16, 0.34, 0.34, 0.16), # expected probabilities
    "xk"=c(-0.98, 0, 0.98) # expected decision thresholds
  )
  expect_equal(sim, expected_sim, tolerance=0.1)
})

test_that("discretization of skew-normal with mu=0, sd=1, gamma1=0.5
          with 5 categories gives expected result", {
  sim <- simulate_responses(K=5, params=c("mu"=0, "sd"=1, "gamma1"=0.5))
  expected_sim <- list(
    "pk"=c(0.162, 0.297, 0.284, 0.188, 0.07), # expected probabilities
    "xk"=c(-0.967, -0.193, 0.589, 1.579) # expected decision thresholds
  )
  expect_equal(sim, expected_sim, tolerance=0.1)
})

test_that("discretization of skew-normal with mu=-1, sd=0.5, gamma1=0.5
          with 5 categories gives expected result", {
  sim <- simulate_responses(K=5, params=c("mu"=-1, "sd"=0.5, "gamma1"=0.5))
  expected_sim <- list(
    "pk"=c(0.564, 0.370, 0.063, 0.004, 0), # expected probabilities
    "xk"=c(-0.967, -0.193, 0.589, 1.579) # expected decision thresholds
  )
  expect_equal(sim, expected_sim, tolerance=0.1)
})
