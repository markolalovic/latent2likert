#' Generate Random Responses
#'
#' Generates an array of random responses to Likert-type questions based on
#' specified latent variables.
#'
#' @param size: number of observations.
#' @param n_items: number of Likert scale items (number of questions). 
#' @param n_levels: number of response categories for each item. 
#' Integer or vector of integers.
#' @param mean: means of the latent variables. 
#' Numeric or vector of numerics. Defaults to 0.
#' @param sd: standard deviations of the latent variables. 
#' Numeric or vector of numerics. Defaults to 1.
#' @param skew: marginal skewness of the latent variables.
#' Numeric or vector of numerics. Defaults to 0.
#' @param corr: correlations between latent variables.
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
#' rlikert(size = 10, n_items = 3, n_levels = c(4, 5, 6), mean = c(0, -1, 0), sd = c(0.8, 1, 1), corr = 0.5)
#' 
#' # Generate responses with a correlation matrix
#' corr <- matrix(c(1.00, -0.63, -0.39, -0.63, 1.00, 0.41, -0.39, 0.41, 1.00), nrow = 3)
#' data <- rlikert(size = 1000, n_items = 3, n_levels = c(4, 5, 6), mean = c(0, -1, 0), sd = c(0.8, 1, 1), corr = corr)
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

# Returns a vector of probabilities across response levels
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


# Returns the optimal endpoints of intervals that transform
# a neutral density into discrete probability distribution
calc_endpoints <- function(n_levels, skew) {
  dp <- convert_params(c("mu" = 0, "sd" = 1, "skew" = skew))
  
  density_fn <- function(x) {
    density_sn(x, dp[["xi"]], dp[["omega"]], dp[["alpha"]])
  }
  
  endp <- discretize_density(density_fn, n_levels)[["endp"]]
  return(endp)
}

generate_responses <- function(size, n_levels, mean, sd, skew) {
  cp <- c("mu" = mean, "sd" = sd, "skew" = skew)
  prob <- simulate_likert(n_levels, cp)
  sample(seq_len(n_levels), size, replace = TRUE, prob = prob)
}

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
