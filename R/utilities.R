# Helper function to check if package is installed
check_package <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(sprintf('Package "%s" must be installed. Please run:\n\n\tinstall.packages("%s")\n\n', pkg, pkg), call. = FALSE)
    }
}

#' Probability density function of a skew normal distribution
#'
#' @export
#' @param x variable
#' @param xi determines the location
#' @param omega determines the scale
#' @param alpha determines the shape
#' @return density at x
#' @seealso [sn::dsn()]
density_sn = function(x, xi=0, omega=1, alpha=0) {
    return(2/omega*stats::dnorm((x - xi)/omega) * 
           stats::pnorm(alpha*(x - xi)/omega))
}

#' Convert parameters
#'
#' Converts from centered parameters to direct parameters appearing in
#' the skew normal density.
#'
#' @export
#' @param cp centered parameters c(mu, sd, skew)
#' @return direct parameters c(xi, omega, alpha)
#' @seealso [sn::cp2dp]
convert_params = function(cp) {
  mu = cp[1]
  sd = cp[2]
  skew = cp[3]

  b = sqrt(2/pi)
  r  = sign(skew)*(2*abs(skew)/(4 - pi))^(1/3)
  delta = r/(b*sqrt(1 + r^2))

  mu_z = b*delta
  sd_z = sqrt(1 - mu_z^2)

  omega = sd / sd_z
  xi = mu - omega * mu_z
  alpha = delta / sqrt(1 - delta^2)
  dp = as.numeric(c(xi, omega, alpha))
  names(dp) = c("xi", "omega", "alpha")
  return(dp)
}

#' The mean of skew normal distribution
#'
#' @param alpha determines the shape
#' @return mean of a skew-normal distribution
mean_skew_normal = function(alpha) {
  return(delta_skew_normal(alpha) * sqrt(2/pi))
}

#' Delta parameter of a skew normal distribution
#'
#' @param alpha determines the shape
#' @return delta of a skew-normal distribution
delta_skew_normal = function(alpha) {
  return(alpha / (sqrt(1 + alpha^2)))
}

#' Variance of a skew normal distribution
#'
#' @param alpha determines the shape
#' @return variance of a skew-normal distribution
var_skew_normal = function(alpha) {
  return(1 - 2*(delta_skew_normal(alpha)^2)/pi)
}

#' Scale and shift
#'
#' @param x variable
#' @param dp direct parameters xi, omega, alpha
#' @return shifted and scaled x
scale_and_shift = function(x, dp) {
  xi = dp[["xi"]]
  omega = dp[["omega"]]
  alpha = dp[["alpha"]]
  mean_sn = mean_skew_normal(alpha)
  return((x - mean_sn)/omega + mean_sn - xi/omega)
}

#' Generate a random p x p correlation matrix
#'
#' @param p the size of the correlation matrix
#' @return a random p x p correlation matrix
generate_rand_corr_matrix = function(p) {
  corr = drop(stats::rWishart(1, p, diag(p)))
  corr = stats::cov2cor(corr)
  return(corr)
}

#' Convert correlation matrix to covariance matrix
#'
#' @param corr correlation matrix
#' @param s vector of standard deviations
#' @return covariance matrix
cor2cov = function(corr, s) {
  return(diag(s) %*% corr %*% diag(s))
}

#' Pad missing levels with zeros
#'
#' @export
#' @param pk proportions or probabilities across possible responses
#' @param K number of response categories
#' @return table of proportions across all possible responses
pad_levels <- function(pk, K) {
  pk <- vapply(as.character(seq_len(K)), function(k) {
    ifelse(k %in% names(pk), pk[[k]], 0)
  }, numeric(1))
  return(pk)
}

#' Get a table of proportions across each possible response
#'
#' @export
#' @param data a vector or array of responses
#' @param K number of response categories
#' @return table of proportions
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

#' Get random centered parameters `c(mu, sd, gamma1)` for testing
#'
#' @export
#' @return random centered parameters
generate_random_cp <- function() {
  mu <- stats::rnorm(1, 0, 1)
  sd <- stats::runif(1, 0.1, 2)
  gamma1 <- stats::runif(1, -0.95, 0.95)
  cp <- c("mu"=mu, "sd"=sd, "gamma1"=gamma1)
  return(cp)
}

#' Return percentage for a given number
#'
#' @export
#' @param x number
#' @param digits number of digits
percentify <- function(x, digits=0) {
  percentage <-formatC(x*100, format="f", digits=digits)
  return(paste0(percentage, "%"))
}
