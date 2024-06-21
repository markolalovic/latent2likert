context("Testing simulation")

testthat::test_that("simulate_likert using 4 levels and N(0,1) gives 
  expected result", {
  n_levels <- 4
  cp <- c("mu"=0, "sd"=1, "skew"=0)
  prob <- simulate_likert(n_levels, cp)
  prob <- as.numeric(prob)
  expected_prob <- c(0.163, 0.337, 0.337, 0.163)
  testthat::expect_equal(prob, expected_prob, tolerance = 0.05)
})

testthat::test_that("simulate_likert using 5 levels and N(0,1) gives 
  expected result", {
  n_levels <- 5
  cp <- c("mu"=0, "sd"=1, "skew"=0)
  prob <- simulate_likert(n_levels, cp)
  prob <- as.numeric(prob)
  expected_prob <- c(0.106, 0.244, 0.298, 0.244, 0.106)
  testthat::expect_equal(prob, expected_prob, tolerance = 0.05)
})

testthat::test_that("Correlation input is handled correctly", {
  corr_matrix <- generate_rand_corr_matrix(3)
  corr_inputs <- list(0, "random", 0.5, corr_matrix)
  
  for (i in seq_along(corr_inputs)) {
      corr <- corr_inputs[[i]]
      corr_case <- handle_corr_case(corr)
      testthat::expect_equal(i, corr_case)
  }
})

testthat::test_that("Invalid corr input raises error", {
  res <- try(handle_corr_case("Invalid"), silent = TRUE)
  testthat::expect_equal(class(res), "try-error")
})

testthat::test_that("generate_corr_matrix returns a matrix that 
  resembles a correlation matrix", {
  n_items <- 3
  corr_matrix <- generate_rand_corr_matrix(n_items)
  corr_inputs <- list("random", 0.5, corr_matrix)

  for (i in seq_along(corr_inputs)) {
    corr <- corr_inputs[[i]]
    corr_case <- handle_corr_case(corr)
    res <- generate_corr_matrix(corr, corr_case, n_items)

    testthat::expect_true(all(diag(res) == 1), 
                          info = paste("Failed on input:", i))
    testthat::expect_true(isSymmetric(res), 
                          info = paste("Failed on input:", i))
  }
})

testthat::test_that("proportions of generated responses match actual 
  probabilities for single item", {
  set.seed(12345) # for reproducibility
  size <- 10^6
  n_items <- 1
  n_levels <- 5
  mean <- 0
  sd <- 1
  skew <- 0
  
  data <- rlikert(size, n_items, n_levels, mean, sd, skew)

  cp <- c("mu"=mean, "sd"=sd, "skew"=skew)
  prob <- simulate_likert(n_levels, cp)
  data_prop <- response_prop(data, n_levels)
  testthat::expect_equal(prob, data_prop, tolerance = 0.05)
})

testthat::test_that("proportions of generated responses match actual 
  probabilities for multiple items without corr", {
  set.seed(12345) # for reproducibility
  size <- 10^6
  n_levels <- 5
  n_items <- 3

  mean <- c(-1, 0, 1)
  sd <- c(0.5, 1, 0.5)
  skew <- c(0.5, 0.5, 0.5)
  corr <- 0
  
  data <- rlikert(size, n_items, n_levels, mean, sd, skew, corr)
  for (i in seq_len(n_items)) {
      cp <- c("mu"=mean[i], "sd"=sd[i], "skew"=skew[i])
      prob <- simulate_likert(n_levels, cp)
      data_prop <- response_prop(data[,i], n_levels)
      testthat::expect_equal(prob, data_prop, tolerance = 0.05)
  }
})

testthat::test_that("proportions of generated responses match actual 
  probabilities for multiple items with corr", {
  set.seed(12345) # for reproducibility
  size <- 10^6
  n_levels <- 5
  n_items <- 3

  mean <- c(-1, 0, 1)
  sd <- c(1, 1, 0.5)
  skew <- c(0.5, 0.5, 0.5)
  corr <- 0.5
  
  data <- rlikert(size, n_items, n_levels, mean, sd, skew, corr)
  for (i in seq_len(n_items)) {
      cp <- c("mu"=mean[i], "sd"=sd[i], "skew"=skew[i])
      prob <- simulate_likert(n_levels, cp)
      data_prop <- response_prop(data[,i], n_levels)
      testthat::expect_equal(prob, data_prop, tolerance = 0.05)
  }
})

testthat::test_that("proportions of generated responses match actual 
  probabilities, when using a random corr matrix", {
  set.seed(12345) # for reproducibility    
  size <- 10^6
  n_levels <- 5
  n_items <- 3

  mean <- c(0, -1, -1)
  sd <- c(1, 1, 0.5)
  skew <- c(0, 0, 0)
  corr <- "random"
  
  data <- rlikert(size, n_items, n_levels, mean, sd, skew, corr)
  for (i in seq_len(n_items)) {
      cp <- c("mu"=mean[i], "sd"=sd[i], "skew"=skew[i])
      prob <- simulate_likert(n_levels, cp)
      data_prop <- response_prop(data[,i], n_levels)
      testthat::expect_equal(prob, data_prop, tolerance = 0.05)
  }
})

testthat::test_that("correlations of random responses match actual 
  correlations", {
  set.seed(12345) # for reproducibility    
  size <- 10^6
  n_levels <- 5
  n_items <- 3
  
  mean <- c(0, -1, -1)
  sd <- c(1, 1, 0.5)
  skew <- c(0.5, 0.5, 0.5)
  corr <- 0.5
  
  data <- rlikert(size, n_items, n_levels, mean, sd, skew, corr)
  data_corr_matrix <- cor(data)
  
  actual_corr_matrix <- generate_corr_matrix(corr, 3, n_items)
  dimnames(actual_corr_matrix) <- dimnames(data_corr_matrix)

  testthat::expect_equal(actual_corr_matrix, 
                        data_corr_matrix, tolerance = 0.05)
})

testthat::test_that("correlations of random responses match actual 
  correlations, harder case", {
  set.seed(12345) # for reproducibility
  size <- 10^6
  n_levels <- 6
  n_items <- 3
  
  mean <- c(-0.5, 0, 0.5)
  sd <- c(0.5, 0.5, 0.5)
  skew <- c(-0.3, -0.4, -0.5)
  corr <- 0.7
  
  data <- rlikert(size, n_items, n_levels, mean, sd, skew, corr)
  data_corr_matrix <- cor(data)
  
  actual_corr_matrix <- generate_corr_matrix(corr, 3, n_items)
  dimnames(actual_corr_matrix) <- dimnames(data_corr_matrix)

  testthat::expect_equal(actual_corr_matrix, 
                          data_corr_matrix, tolerance = 0.11)
})
