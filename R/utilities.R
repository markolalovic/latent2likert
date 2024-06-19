#' Plot Transformation
#'
#' Plots the densities of latent variables in the first row 
#' and transformed discrete probability distributions below.
#'
#' Arguments
#' @param n_items: Integer. The number of Likert scale items (questions). 
#' @param n_levels: Integer or vector of integers. The number of response 
#'              categories (points) for each Likert scale item.
#'
#' Latent Variables Parameters
#' @param mean: Numeric or vector of numerics. Means of the latent variables.
#'           Defaults to 0.
#' @param sd: Numeric or vector of numerics. Standard deviations of the latent variables.
#'           Defaults to 1.
#' @param skew: Numeric or vector of numerics. Marginal skewness of the latent variables.
#'           Defaults to 0.
#'
#' @examples
#' plot_likert_transform(n_items = 3, n_levels = c(3, 4, 5))
#' plot_likert_transform(n_items = 3, n_levels = 5, mean=c(0, 1, 2))
#' plot_likert_transform(n_items = 3, n_levels = 5, sd=c(0.8, 1, 1.2))
#' plot_likert_transform(n_items = 3, n_levels = 5, skew=c(-0.5, 0, 0.5))
#' @export
plot_likert_transform <- function(n_items, n_levels, mean=0, sd=1, skew=0) {
  n_levels <- rep(n_levels, length.out = n_items)
  mean <- rep(mean, length.out = n_items)
  sd <- rep(sd, length.out = n_items)
  skew <- rep(skew, length.out = n_items)
  graphics::layout(matrix(seq_len(n_items * 2), nrow=2, ncol=n_items))
  x <- seq(-3, 3, length = 1000)
  for (i in seq_len(n_items)) {
    # Draw the densities of latent variables
    cp <- c("mu" = mean[i], "sd" = sd[i], "skew" = skew[i])
    dp <- convert_params(cp)
    y <- density_sn(x, dp[["xi"]], dp[["omega"]], dp[["alpha"]])
    graphics::plot(x, y, type="l", lwd = 2, xlab = "", ylab = "", main="")
    graphics::title(paste("X", i, sep = ""))
    
    # Draw the corresponding discrete probability distributions
    prob <- simulate_likert(n_levels[i], cp)
    graphics::barplot(prob)
    graphics::title(paste("Y", i, sep = ""))
  }
}

#' Pad Missing Levels
#'
#' This function takes a vector of proportions or probabilities across possible 
#' responses and pads the missing levels with zeros up to the specified number 
#' of response categories.
#'
#' @param pr proportions or probabilities across possible responses.
#' @param n_levels number of response categories.
#' @return A named vector of proportions across all possible responses.
#' @examples
#' pr <- c("2" = 0.25, "3" = 0.25, "4" = 0.50)
#' pad_levels(pr, 5)
#' @noRd
pad_levels <- function(pr, n_levels) {
  padded_pr <- vapply(seq_len(n_levels), function(k) {
    ifelse(as.character(k) %in% names(pr), pr[as.character(k)], 0)
  }, numeric(1))
  names(padded_pr) <- as.character(seq_len(n_levels))
  return(padded_pr)
}

# Helper function to validate skewness
validate_skewness <- function(skew) {
  if (skew > 0.95 || skew < -0.95) {
    stop("The value of skewness must be in the range -0.95 to 0.95: 
         `skew >= -0.95` and `skew <= 0.95`.")
  }
}

# Helper function to check if package is installed
check_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf('Package "%s" must be installed. Please run:\n\n\tinstall.packages("%s")\n\n', pkg, pkg), call. = FALSE)
  }
}

#' Probability density function of a skew normal distribution
#'
#' @param x variable
#' @param xi determines the location
#' @param omega determines the scale
#' @param alpha determines the shape
#' @return density at x
#' @seealso [sn::dsn()]
#' @noRd
density_sn <- function(x, xi = 0, omega = 1, alpha = 0) {
  return(2 / omega * stats::dnorm((x - xi) / omega) *
    stats::pnorm(alpha * (x - xi) / omega))
}

#' Convert parameters
#'
#' Converts from centered parameters to direct parameters appearing in
#' the skew normal density.
#'
#' @param cp centered parameters c(mu, sd, skew)
#' @return direct parameters c(xi, omega, alpha)
#' @seealso [sn::cp2dp]
#' @noRd
convert_params <- function(cp) {
  mu <- cp[1]
  sd <- cp[2]
  skew <- cp[3]

  b <- sqrt(2 / pi)
  r <- sign(skew) * (2 * abs(skew) / (4 - pi))^(1 / 3)
  delta <- r / (b * sqrt(1 + r^2))

  mu_z <- b * delta
  sd_z <- sqrt(1 - mu_z^2)

  omega <- sd / sd_z
  xi <- mu - omega * mu_z
  alpha <- delta / sqrt(1 - delta^2)
  dp <- as.numeric(c(xi, omega, alpha))
  names(dp) <- c("xi", "omega", "alpha")
  return(dp)
}

# Mean of a skew normal distribution
mean_skew_normal <- function(alpha) {
  return(delta_skew_normal(alpha) * sqrt(2 / pi))
}

# Delta parameter of a skew normal distribution
delta_skew_normal <- function(alpha) {
  return(alpha / (sqrt(1 + alpha^2)))
}

# Variance of a skew normal distribution
var_skew_normal <- function(alpha) {
  return(1 - 2 * (delta_skew_normal(alpha)^2) / pi)
}

#' Scale and shift
#'
#' @param x variable
#' @param dp direct parameters xi, omega, alpha
#' @return shifted and scaled x
#' @noRd
scale_and_shift <- function(x, dp) {
  xi <- dp[["xi"]]
  omega <- dp[["omega"]]
  alpha <- dp[["alpha"]]
  mean_sn <- mean_skew_normal(alpha)
  return((x - mean_sn) / omega + mean_sn - xi / omega)
}

#' Generate a random p x p correlation matrix
#'
#' @param p the size of the correlation matrix
#' @return a random p x p correlation matrix
#' @noRd
generate_rand_corr_matrix <- function(p) {
  corr <- drop(stats::rWishart(1, p, diag(p)))
  corr <- stats::cov2cor(corr)
  return(corr)
}

#' Convert correlation matrix to covariance matrix
#'
#' @param corr correlation matrix
#' @param s vector of standard deviations
#' @return covariance matrix
#' @noRd
cor2cov <- function(corr, s) {
  return(diag(s) %*% corr %*% diag(s))
}

#' Get a table of proportions across each possible response
#'
#' @param data a vector or array of responses
#' @param K number of response categories
#' @return table of proportions
#' @noRd
get_prop_table <- function(data, K) {
  if (is.vector(data)) {
    tab <- pad_levels(prop.table(table(data)), K)
  } else {
    tab <- t(apply(data, 2, function(x_col) {
      pad_levels(prop.table(table(x_col)), K)
    }))
    dimnames(tab) <- list(Item = rownames(tab), Response = colnames(tab))
  }
  return(tab)
}

# Helper function to get random centered parameters `c(mu, sd, gamma1)`
generate_random_cp <- function() {
  mu <- stats::rnorm(1, 0, 1)
  sd <- stats::runif(1, 0.1, 2)
  gamma1 <- stats::runif(1, -0.95, 0.95)
  cp <- c("mu" = mu, "sd" = sd, "gamma1" = gamma1)
  return(cp)
}

# Helper function to get percentage for a given number
percentify <- function(x, digits = 0) {
  percentage <- formatC(x * 100, format = "f", digits = digits)
  return(paste0(percentage, "%"))
}
