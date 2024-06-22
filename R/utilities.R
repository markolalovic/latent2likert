#' Plot Transformation
#'
#' Plots the densities of latent variables and the corresponding
#' transformed discrete probability distributions.
#'
#' @param n_items number of Likert scale items (questions).
#' @param n_levels number of response categories for each Likert item.
#'  Integer or vector of integers.
#' @param mean means of the latent variables.
#'  Numeric or vector of numerics. Defaults to 0.
#' @param sd standard deviations of the latent variables.
#'  Numeric or vector of numerics. Defaults to 1.
#' @param skew marginal skewness of the latent variables.
#'  Numeric or vector of numerics. Defaults to 0.
#' @return NULL. The function produces a plot.
#' @examples
#' plot_likert_transform(n_items = 3, n_levels = c(3, 4, 5))
#' plot_likert_transform(n_items = 3, n_levels = 5, mean = c(0, 1, 2))
#' plot_likert_transform(n_items = 3, n_levels = 5, sd = c(0.8, 1, 1.2))
#' plot_likert_transform(n_items = 3, n_levels = 5, skew = c(-0.5, 0, 0.5))
#' @export
plot_likert_transform <- function(
    n_items, n_levels,
    mean = 0, sd = 1, skew = 0) {
  n_levels <- rep(n_levels, length.out = n_items)
  mean <- rep(mean, length.out = n_items)
  sd <- rep(sd, length.out = n_items)
  skew <- rep(skew, length.out = n_items)
  graphics::layout(matrix(seq_len(n_items * 2), nrow = 2, ncol = n_items))
  x <- seq(-3, 3, length = 1000)
  for (i in seq_len(n_items)) {
    # Draw the densities of latent variables
    cp <- c("mu" = mean[i], "sd" = sd[i], "skew" = skew[i])
    dp <- convert_params(cp)
    y <- density_sn(x, dp[["xi"]], dp[["omega"]], dp[["alpha"]])
    graphics::plot(x, y, type = "l", lwd = 2, xlab = "", ylab = "", main = "")
    graphics::title(paste("X", i, sep = ""))

    # Draw the corresponding discrete probability distributions
    prob <- simulate_likert(n_levels[i], cp)
    graphics::barplot(prob)
    graphics::title(paste("Y", i, sep = ""))
  }
  invisible(NULL)
}

#' Plot Contour
#'
#' Plots the contour of the objective function values over a grid
#' of parameter values. It visualizes the norm of the function \code{fn}
#' for different values of \code{u} (mean) and \code{v} (1/standard deviation)
#' and overlays the trace of parameter updates during the estimation process.
#'
#' @param fn objective function to be minimized.
#' @param endp endpoints of intervals that partition the continuous domain.
#' @param prob discrete probability distribution.
#' @param cdf_X cumulative distribution function of the latent variable.
#' @param trace matrix of parameter updates.
#' @return NULL. The function produces a plot.
#' @noRd
plot_contour <- function(fn, endp, prob, cdf_X, trace) {
  xlen <- 50
  ylen <- 50
  xgrid <- seq(-3, 3, length.out = xlen) # Range for mean (mu)
  ygrid <- seq(0.1, 3, length.out = ylen) # Range for 1/sd
  zvals <- matrix(NA, ncol = xlen, nrow = ylen)
  for (i in seq_len(xlen)) {
    for (j in seq_len(ylen)) {
      zvals[i, j] <- norm(fn(
        matrix(c(xgrid[i], ygrid[j])),
        endp, prob, cdf_X
      ), "2")
    }
  }
  graphics::contour(
    x = xgrid, y = ygrid, z = zvals,
    col = "gray42", xlab = "u = mu", ylab = "v = 1/sd"
  )
  graphics::grid(col = "lightgray", lty = "dotted")
  graphics::points(trace[1, ], trace[2, ], pch = 20, col = "blue")
  invisible(NULL)
}

#' Calculate Response Proportions
#'
#' Returns a table of proportions for each possible response category.
#'
#' @param data numeric vector or matrix of responses.
#' @param n_levels number of response categories.
#' @return A table of response category proportions.
#' @examples
#' data <- c(1, 2, 2, 3, 3, 3)
#' response_prop(data, n_levels = 3)
#'
#' data_matrix <- matrix(c(1, 2, 2, 3, 3, 3), ncol = 2)
#' response_prop(data_matrix, n_levels = 3)
#' @export
response_prop <- function(data, n_levels) {
  if (is.vector(data)) {
    tab <- pad_levels(prop.table(table(data)), n_levels)
  } else {
    tab <- t(apply(data, 2, function(x_col) {
      pad_levels(prop.table(table(x_col)), n_levels)
    }))
    dimnames(tab) <- list(Item = rownames(tab), Response = colnames(tab))
  }
  return(tab)
}

#' Pad Missing Levels
#'
#' Helper function that takes a vector of proportions or probabilities
#' across possible responses and pads the missing levels with zeros up
#' to the specified number of response categories.
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

#' Validate Skewness
#'
#' Checks if the skewness parameter is within the acceptable range.
#'
#' @param skew numeric. Skewness parameter.
#' @noRd
validate_skewness <- function(skew) {
  if (skew > 0.95 || skew < -0.95) {
    stop("The value of skewness must be in the range -0.95 to 0.95:
         `skew >= -0.95` and `skew <= 0.95`.")
  }
}

#' Check Package Installation
#'
#' Checks if a package is installed, and stops with an error message if not.
#'
#' @param pkg character. The name of the package.
#' @noRd
check_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf(
      'Package "%s" must be installed.
         Please run:\n\n\tinstall.packages("%s")\n\n',
      pkg, pkg
    ), call. = FALSE)
  }
}

#' Density of Skew Normal Distribution
#'
#' Computes the probability density function of a skew normal distribution.
#'
#' @param x numeric. Variable.
#' @param xi numeric. Location parameter.
#' @param omega numeric. Scale parameter.
#' @param alpha numeric. Shape parameter.
#' @return numeric. Density at x.
#' @seealso [sn::dsn()]
#' @noRd
density_sn <- function(x, xi = 0, omega = 1, alpha = 0) {
  return(2 / omega * stats::dnorm((x - xi) / omega) *
    stats::pnorm(alpha * (x - xi) / omega))
}

#' Convert Centered Parameters
#'
#' Converts centered parameters to direct parameters used in the
#' skew normal density.
#'
#' @param cp numeric vector. Centered parameters c(mu, sd, skew).
#' @return numeric vector. Direct parameters c(xi, omega, alpha).
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

#' Mean of Skew Normal Distribution
#'
#' Computes the mean of a skew normal distribution.
#'
#' @param alpha numeric. Shape parameter.
#' @return numeric. Mean of the skew normal distribution.
#' @noRd
mean_skew_normal <- function(alpha) {
  return(delta_skew_normal(alpha) * sqrt(2 / pi))
}

#' Delta Parameter of Skew Normal Distribution
#'
#' Computes the delta parameter of a skew normal distribution.
#'
#' @param alpha numeric. Shape parameter.
#' @return numeric. Delta parameter.
#' @noRd
delta_skew_normal <- function(alpha) {
  return(alpha / (sqrt(1 + alpha^2)))
}

#' Variance of Skew Normal Distribution
#'
#' Computes the variance of a skew normal distribution.
#'
#' @param alpha numeric. Shape parameter.
#' @return numeric. Variance of the skew normal distribution.
#' @noRd
var_skew_normal <- function(alpha) {
  return(1 - 2 * (delta_skew_normal(alpha)^2) / pi)
}

#' Scale and Shift
#'
#' Scales and shifts a variable based on direct parameters.
#'
#' @param x numeric. Variable.
#' @param dp numeric vector. Direct parameters xi, omega, alpha.
#' @return numeric. Shifted and scaled variable.
#' @noRd
scale_and_shift <- function(x, dp) {
  xi <- dp[["xi"]]
  omega <- dp[["omega"]]
  alpha <- dp[["alpha"]]
  mean_sn <- mean_skew_normal(alpha)
  return((x - mean_sn) / omega + mean_sn - xi / omega)
}

#' Generate Random Correlation Matrix
#'
#' Generates a random p x p correlation matrix.
#'
#' @param p integer. The size of the correlation matrix.
#' @return numeric matrix. A random p x p correlation matrix.
#' @noRd
generate_rand_corr_matrix <- function(p) {
  corr <- drop(stats::rWishart(1, p, diag(p)))
  corr <- stats::cov2cor(corr)
  return(corr)
}

#' Convert Correlation Matrix to Covariance Matrix
#'
#' Converts a correlation matrix to a covariance matrix.
#'
#' @param corr numeric matrix. Correlation matrix.
#' @param s numeric vector. Standard deviations.
#' @return numeric matrix. Covariance matrix.
#' @noRd
cor2cov <- function(corr, s) {
  return(diag(s) %*% corr %*% diag(s))
}

#' Generate Random Centered Parameters
#'
#' Generates random centered parameters for mu, sd, and skewness.
#'
#' @return numeric vector. Random centered parameters c(mu, sd, gamma1).
#' @noRd
generate_random_cp <- function() {
  mu <- stats::rnorm(1, 0, 1)
  sd <- stats::runif(1, 0.1, 2)
  gamma1 <- stats::runif(1, -0.95, 0.95)
  cp <- c("mu" = mu, "sd" = sd, "gamma1" = gamma1)
  return(cp)
}

#' Percentify
#'
#' Converts a numeric value to a percentage string.
#'
#' @param x numeric. The value to convert.
#' @param digits integer. Number of digits to round to. Defaults to 0.
#' @return character. Percentage string.
#' @noRd
percentify <- function(x, digits = 0) {
  percentage <- formatC(x * 100, format = "f", digits = digits)
  return(paste0(percentage, "%"))
}
