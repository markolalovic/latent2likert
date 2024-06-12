context("Testing estimation of parameters")

testthat::test_that("`mean` and `sd` are well estimated given `pk` for normal case", {
    actual <- c(-1, 0.5) # actual mean and sd
    prob <- list("1"=0.313, "2"=0.579, "3"=0.105, "4"=0.003)
    estimates <- estimate_mean_and_sd(prob, 5)
    testthat::expect_equal(estimates, actual, tolerance=0.1)
})

# TODO: fix the warnings
# “longer object length is not a multiple of shorter object length”
# Warning message in tail(y, -1) - head(y, -1) - prob:

# test_that("`mean` and `sd` are well estimated given `pk` for skew case", {
#   actual <- c(0.146858, 1.084341) # actual mean and sd
#   skew <- -0.4565873 # skewness
#   pk <- list("1"=0.036, "2"=0.089, "3"=0.142, 
#              "4"=0.185, "5"=0.21, "6"=0.201, "7"=0.137)
#   estimates <- estimate_mean_and_sd(pk=pk, n_levels=7, skew=skew)
#   expect_equal(estimates, actual, tolerance=0.1)
# })

# test_that("`mean` and `sd` are well estimated given data for normal case", {
#   params <- c("mu"=0.5, "sd"=0.5) # actual parameters
#   n <- 1000 # number of observations
#   data <- get_responses(n=n, mu=params[["mu"]], sd=params["sd"], n_levels=5)
#   estimates <- estimate_parameters(data, 5)
#   expect_equal(c(estimates), params, tolerance=0.1)
# })

# test_that("`mean` and `sd` are well estimated given data for skew case", {
#   params <- c("mu"=0.5, "sd"=0.5) # actual parameters
#   n <- 1000 # number of observations
#   skew <- 0.5 # skewness
#   data <- get_responses(n=n, mu=params[["mu"]], sd=params["sd"], 
#                         skew=skew, n_levels=5)
#   estimates <- estimate_parameters(data, 5, skew)
#   expect_equal(c(estimates), params, tolerance=0.1)
# })

# test_that("`mean` and `sd` are well estimated given data for multivariate case", {
#   # actual parameters
#   mu <- c(0.6, -0.1, 0.3)
#   sd <- c(1.3, 0.5, 1.5)

#   n_levels <- c(3, 4, 5)
#   skew <- c(-0.5, 0, 0.5)

#   n <- 1000
#   data <- get_responses(n, mu, sd, skew, n_levels)

#   estimates <- estimate_parameters(data, n_levels, skew)
#   actual <- as.table(rbind(mu, sd))
#   dimnames(actual) <- dimnames(estimates)
#   expect_equal(estimates, actual, tolerance=0.1)
# })

# test_that("parameters `mu` and `sd` are well estimated in hard random case", {
#   set.seed(12345)
#   cp <- get_random_cp()
#   n_levels <- sample(2:15, 1)

#   sim <- simulate_responses(n_levels, cp)
#   pk <- round(sim$pk, 3)
#   names(pk) <- 1:n_levels

#   # also checks that the contour plot works by setting trace = TRUE
#   estimates <- estimate_mu_sd(pk, n_levels, cp[["skew"]], trace = TRUE)
#   actual <- c(cp[["mu"]], cp[["sd"]])
#   expect_equal(estimates, actual, tolerance=0.1)
# })

# test_that("missing n_levels raises error", {
#   pk <- list("1"=0.313, "2"=0.579, "3"=0.105, "4"=0.003)
#   res <- try(estimate_mu_sd(pk = pk),silent = TRUE)
#   expect_equal(class(res), "try-error")
# })


