context("Testing estimation of parameters")

test_that("`mean` and `sd` are well estimated given `prob`
  for normal case", {
  actual <- c(-1, 0.5) # actual mean and sd
  prob <- c("1" = 0.313, "2" = 0.579, "3" = 0.105, "4" = 0.003)
  estimates <- estimate_mean_and_sd(prob, 5)
  expect_equal(estimates, actual, tolerance = 0.1)
})

test_that("`mean` and `sd` are well estimated given `prob`
  for skew case", {
  actual <- c(0.146858, 1.084341) # actual mean and sd
  skew <- -0.4565873 # skewness
  prob <- c(
    "1" = 0.036, "2" = 0.089, "3" = 0.142,
    "4" = 0.185, "5" = 0.21, "6" = 0.201, "7" = 0.137
  )
  estimates <- estimate_mean_and_sd(prob, 7, skew)
  expect_equal(estimates, actual, tolerance = 0.05)
})

test_that("estimate_params returns accurate estimates for
  latent parameters when data is a vector", {
  set.seed(12345) # for reproducibility

  # Generate test data for a single item (vector data)
  data_vector <- rlikert(
    size = 1000,
    n_items = 1,
    n_levels = 5,
    mean = 0.5,
    sd = 1.2,
    skew = 0
  )

  # Estimate parameters
  estimates <- estimate_params(data_vector, n_levels = 5, skew = 0)

  # Extract estimated means and standard deviations
  estimated_mean <- as.numeric(estimates["mean"])
  estimated_sd <- as.numeric(estimates["sd"])

  # Actual means and standard deviations
  actual_mean <- 0.5
  actual_sd <- 1.2

  # Check that estimates are close to actual mean and sd
  expect_equal(estimated_mean, actual_mean, tolerance = 0.1)
  expect_equal(estimated_sd, actual_sd, tolerance = 0.1)
})

test_that("estimate_params returns accurate estimates for
latent parameters", {
  set.seed(12345) # for reproducibility

  # Generate test data
  corr <- matrix(c(
    1.00, -0.63, -0.39,
    -0.63, 1.00, 0.41,
    -0.39, 0.41, 1.00
  ), nrow = 3)
  data <- rlikert(
    size = 1000,
    n_items = 3,
    n_levels = c(4, 5, 6),
    mean = c(0, -1, 0),
    sd = c(0.8, 1, 1),
    corr = corr
  )

  # Estimate parameters
  estimates <- estimate_params(data, n_levels = c(4, 5, 6), skew = 0)

  # Extract estimated means and standard deviations
  estimated_means <- as.numeric(estimates["mean", ])
  estimated_sds <- as.numeric(estimates["sd", ])

  # Actual means and standard deviations
  actual_means <- c(0, -1, 0)
  actual_sds <- c(0.8, 1, 1)

  # Check if estimated means are close to actual means and sds
  expect_equal(estimated_means, actual_means, tolerance = 0.1)
  expect_equal(estimated_sds, actual_sds, tolerance = 0.1)
})

test_that("plot_contour executes without errors", {
  # Define a simple objective function
  test_fn <- function(x, endp, prob, cdf_X) {
    u <- x[1]
    v <- x[2]
    y <- cdf_X(v * endp - u * v)
    return(matrix(utils::tail(y, -1) - utils::head(y, -1) - prob))
  }

  # Example parameters
  endp <- c(-Inf, -1, 0, 1, Inf)
  prob <- c(0.1, 0.2, 0.4, 0.3)
  cdf_X <- stats::pnorm
  trace <- matrix(c(rep(0, 100), rep(1, 100)), nrow = 2)

  # Check if plotting function executes without errors
  expect_silent(plot_contour(test_fn, endp, prob, cdf_X, trace))
})
