#' Simulate Likert Scale Item Responses
#' 
#' Simulates Likert scale item responses based on a specified number 
#' of response categories and the centered parameters of the latent variable.
#' 
#' @param n_levels Integer. The number of response categories for the Likert scale item.
#' @param cp Named vector. The centered parameters of the latent variable, including 
#'           mean (`mu`), standard deviation (`sd`), and skewness (`skew`).
#'           Skewness must be between -0.95 and 0.95.
#' @return A named vector of probabilities for each response category.
#' @examples
#' cp <- c(mu = 0, sd = 1, skew = 0.5)
#' simulate_likert(n_levels = 5, cp = cp)
#' cp2 <- c(mu = 1, sd = 2, skew = -0.3)
#' simulate_likert(n_levels = 7, cp = cp2)
#' @details
#' The simulation process uses the following model detailed by Boari and Nai-Ruscone. 
#' Let \eqn{X} be the continuous variable of interest, measured using Likert scale 
#' questions with \eqn{K} response categories. The observed discrete variable \eqn{Y} is 
#' defined as follows:
#' \deqn{
#'   Y = k, \quad \text{ if } \ \ x_{k - 1} < X \leq x_{k}
#'   \quad \text{ for } \ \ k = 1, \dots, K
#' }
#' where \eqn{x_{k}}, \eqn{k = 0, \dots, K} are endpoints defined in the domain
#' of \eqn{X} such that:
#' \deqn{
#'   -\infty = x_{0} < x_{1} < \dots < x_{K - 1} < x_{K} = \infty.
#' }
#' The endpoints dictate the transformation of the density \eqn{f_{X}} of \eqn{X}
#' into a discrete probability distribution:
#' \deqn{
#'   \text{Pr}(Y = k) = \int_{x_{k - 1}}^{x_{k}} f_{X}(x) \, dx
#'   \quad \text{ for } \ \ k = 1, \dots, K.
#' }
#' 
#' The continuous latent variable is modeled using a skew normal distribution.
#' The function \code{simulate_likert} performs the following steps:
#' * Ensures the centered parameters are within the acceptable range.
#' * Converts the centered parameters to direct parameters.
#' * Defines the density function for the skew normal distribution.
#' * Computes the probabilities for each response category using optimal endpoints.
#'
#' @references
#' Boari, G. and Nai Ruscone, M. (2015).
#' A procedure simulating Likert scale item responses. 
#' \emph{Electronic Journal of Applied Statistical Analysis} \bold{8(3)}, 288–297.
#' \doi{10.1285/i20705948v8n3p288} 
#' 
#' @seealso \code{\link{discretize_density}} for details on how to calculate
#' the optimal endpoints.
#' @export
simulate_likert <- function(n_levels, cp) {
  validate_skewness(cp[["skew"]])

  dp <- convert_params(cp)
  density_fn <- function(x) {
    density_sn(x, dp[["xi"]], dp[["omega"]], dp[["alpha"]])
  }
  endp <- calc_endpoints(n_levels, cp[["skew"]])
  prob <- calc_probs(density_fn, endp)
  names(prob) <- paste(seq_len(n_levels))
  return(prob)
}

#' Generate Random Responses
#'
#' Generates an array of random responses to Likert-type questions based on
#' specified latent variables.
#'
#' @param size number of observations.
#' @param n_items number of Likert scale items (number of questions).
#' @param n_levels number of response categories for each item.
#' Integer or vector of integers.
#' @param mean means of the latent variables.
#' Numeric or vector of numerics. Defaults to 0.
#' @param sd standard deviations of the latent variables.
#' Numeric or vector of numerics. Defaults to 1.
#' @param skew marginal skewness of the latent variables.
#' Numeric or vector of numerics. Defaults to 0.
#' @param corr correlations between latent variables.
#' Can be a single numeric value representing the same correlation for
#' all pairs, or an actual correlation matrix. Defaults to 0.
#'
#' @return A matrix of random responses with dimensions \code{size} by
#' \code{n_items}. The column names are \code{Y1, Y2, ..., Yn} where
#' \code{n} is the number of items. Each entry in the matrix represents
#' a Likert scale response, ranging from 1 to \code{n_levels}.
#' @examples
#' # Generate responses for a single item with 5 levels
#' rlikert(size = 10, n_items = 1, n_levels = 5)
#'
#' # Generate responses for three items with different levels and parameters
#' rlikert(
#'   size = 10, n_items = 3, n_levels = c(4, 5, 6),
#'   mean = c(0, -1, 0), sd = c(0.8, 1, 1), corr = 0.5
#' )
#'
#' # Generate responses with a correlation matrix
#' corr <- matrix(c(
#'   1.00, -0.63, -0.39,
#'   -0.63, 1.00, 0.41,
#'   -0.39, 0.41, 1.00
#' ), nrow = 3)
#' data <- rlikert(
#'   size = 1000, n_items = 3, n_levels = c(4, 5, 6),
#'   mean = c(0, -1, 0), sd = c(0.8, 1, 1), corr = corr
#' )
#'
#' @export
rlikert <- function(size, n_items, n_levels,
                    mean = 0, sd = 1, skew = 0, corr = 0) {
  # If there's only one item, generate responses directly
  if (n_items == 1) {
    return(generate_responses(size, n_levels, mean, sd, skew))
  }

  # Ensure input values are vectors of length n_items
  mean <- rep(mean, length.out = n_items)
  sd <- rep(sd, length.out = n_items)
  skew <- rep(skew, length.out = n_items)
  n_levels <- rep(n_levels, length.out = n_items)

  # Determine the correlation case
  corr_case <- handle_corr_case(corr)

  if (corr_case == 1) {
    # Generate multiple univariate item responses without correlations
    data <- matrix(nrow = size, ncol = n_items)
    for (i in seq_len(n_items)) {
      data[, i] <- generate_responses(
        size, n_levels[i],
        mean[i], sd[i], skew[i]
      )
    }
    colnames(data) <- paste0("Y", seq_len(n_items))
    return(data)
  }

  # Generate the correlation matrix
  corr_matrix <- generate_corr_matrix(corr, corr_case, n_items)
  sigma <- cor2cov(corr_matrix, sd)

  # Generate latent variables
  latent_variables <- generate_latent_variables(size, mean, sigma, skew)

  # Discretize latent variables to generate responses
  data <- matrix(nrow = size, ncol = n_items)
  for (i in seq_len(n_items)) {
    endp <- calc_endpoints(n_levels[i], skew[i])
    data[, i] <- findInterval(latent_variables[, i], endp)
  }
  colnames(data) <- paste0("Y", seq_len(n_items))
  return(data)
}

#' Calculate Optimal Endpoints
#'
#' Returns the optimal endpoints of intervals that transform 
#' a neutral density into discrete probability distribution.
#' 
#' @param n_levels integer. Number of response categories.
#' @param skew numeric. Skewness parameter of the latent variable.
#' 
#' @return A numeric vector of optimal endpoints.
#' @noRd
calc_endpoints <- function(n_levels, skew) {
  dp <- convert_params(c("mu" = 0, "sd" = 1, "skew" = skew))

  density_fn <- function(x) {
    density_sn(x, dp[["xi"]], dp[["omega"]], dp[["alpha"]])
  }

  endp <- discretize_density(density_fn, n_levels)[["endp"]]
  return(endp)
}

#' Generate Responses
#'
#' Generates random responses for a single Likert scale item.
#'
#' @param size integer. Number of observations.
#' @param n_levels integer. Number of response categories.
#' @param mean numeric. Mean of the latent variable.
#' @param sd numeric. Standard deviation of the latent variable.
#' @param skew numeric. Skewness parameter of the latent variable.
#' 
#' @return A numeric vector of simulated responses.
#' @noRd
generate_responses <- function(size, n_levels, mean, sd, skew) {
  cp <- c("mu" = mean, "sd" = sd, "skew" = skew)
  prob <- simulate_likert(n_levels, cp)
  sample(seq_len(n_levels), size, replace = TRUE, prob = prob)
}

#' Handle Correlation Input Case
#'
#' Determines the type of correlation input provided.
#'
#' @param corr numeric or character or matrix. Correlation input.
#' 
#' @return An integer representing the correlation case.
#' @noRd
handle_corr_case <- function(corr) {
  if (is.numeric(corr) && !is.matrix(corr)) {
    if (corr == 0) {
      return(1)
    }
    if (corr != 0) {
      return(3)
    }
  }
  if (is.character(corr) && corr == "random") {
    return(2)
  }
  if (is.matrix(corr)) {
    return(4)
  }
  stop("Invalid correlation input")
}

#' Generate Correlation Matrix
#'
#' Generates the correlation matrix based on the correlation case.
#'
#' @param corr numeric or matrix. Correlation input.
#' @param corr_case integer. Correlation case identifier.
#' @param n_items integer. Number of Likert scale items.
#' 
#' @return A numeric matrix representing the correlation matrix.
#' @noRd
generate_corr_matrix <- function(corr, corr_case, n_items) {
  if (corr_case == 2) {
    return(generate_rand_corr_matrix(n_items))
  }
  if (corr_case == 3) {
    if (corr > 1 | corr < -1) stop("Correlation must be between -1 and 1.")
    corr_matrix <- matrix(corr, n_items, n_items)
    diag(corr_matrix) <- 1
    return(corr_matrix)
  }
  if (corr_case == 4) {
    return(corr)
  }
}

#' Generate Latent Variables
#'
#' Generates latent variables based on the specified parameters.
#'
#' @param size number of observations.
#' @param mean means of the latent variables. Numeric vector.
#' @param sigma covariance matrix of the latent variables.
#' @param skew skewness parameters of the latent variables. Numeric vector.
#' 
#' @return A matrix of generated latent variables.
#' @noRd
generate_latent_variables <- function(size, mean, sigma, skew) {
  if (any(skew != 0)) { # at least one latent variable is skewed
    if (!requireNamespace("sn", quietly = TRUE)) {
      stop("Package \"sn\" must be installed.
           Please run:\n\n\tinstall.packages(\"sn\")\n\n")
    }
    cp_list <- list(mean = mean, var.cov = sigma, skew = skew)
    dp_list <- sn::cp2dp(cp_list, family = "SN")
    return(sn::rmsn(size, dp = dp_list))
  } else {
    return(mvtnorm::rmvnorm(size, mean, sigma))
  }
}
