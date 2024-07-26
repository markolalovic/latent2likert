#' Estimate Latent Parameters
#'
#' Estimates the location and scaling parameters of the latent variables from
#' existing survey data.
#'
#' @param data survey data with columns representing individual items.
#'             Apart from this, \code{data} can be of almost any class such as
#'             "data.frame" "matrix" or "array".
#' @param n_levels number of response categories, a vector or a number.
#' @param skew marginal skewness of latent variables, defaults to 0.
#' @return A table of estimated parameters for each latent variable.
#' @examples
#' data(part_bfi)
#' vars <- c("A1", "A2", "A3", "A4", "A5")
#' estimate_params(data = part_bfi[, vars], n_levels = 6)
#' @details
#' The relationship between the continuous random variable \eqn{X} and the
#' discrete probability distribution \eqn{p_k}, for \eqn{k = 1, \dots, K},
#' can be described by a system of non-linear equations:
#' \deqn{
#'   p_{k} = F_{X}\left( \frac{x_{k - 1} - \xi}{\omega} \right)
#'         - F_{X}\left( \frac{x_{k} - \xi}{\omega} \right)
#'         \quad \text{for} \ k = 1, \dots, K
#' }
#' where:
#' \describe{
#'   \item{\eqn{F_{X}}}{ is the cumulative distribution function of \eqn{X},}
#'   \item{\eqn{K}}{ is the number of possible response categories,}
#'   \item{\eqn{x_{k}}}{ are the endpoints defining the boundaries of the
#' response categories,}
#'   \item{\eqn{p_{k}}}{ is the probability of the \eqn{k}-th
#' response category,}
#'   \item{\eqn{\xi}}{ is the location parameter of \eqn{X},}
#'   \item{\eqn{\omega}}{ is the scaling parameter of \eqn{X}.}
#' }
#' The endpoints \eqn{x_{k}} are calculated by discretizing a
#' random variable \eqn{Z}
#' with mean 0 and standard deviation 1 that follows the same
#' distribution as \eqn{X}.
#' By solving the above system of non-linear equations iteratively,
#' we can find the parameters that best fit the observed discrete
#' probability distribution \eqn{p_{k}}.
#'
#' The function \code{estimate_params}:
#' * Computes the proportion table of the responses for each item.
#' * Estimates the probabilities \eqn{p_{k}} for each item.
#' * Computes the estimates of \eqn{\xi} and \eqn{\omega} for each item.
#' * Combines the estimated parameters for all items into a table.
#'
#' @seealso \code{\link{discretize_density}} for details on calculating
#' the endpoints, and \code{\link{part_bfi}} for example of the survey data.
#' @export
estimate_params <- function(data, n_levels, skew = 0) {
  if (is.vector(data)) {
    prob <- prop.table(table(data))
    estimates <- estimate_mean_and_sd(prob, n_levels, skew)
    tab <- as.table(estimates)
    rownames(tab) <- c("mean", "sd")
  } else { # multiple items
    nitems <- ncol(data)
    skew <- rep(skew, length.out = nitems)
    if (is.numeric(n_levels)) {
      n_levels <- rep(n_levels, nitems)
    }
    mat <- matrix(data = NA, nrow = nitems, ncol = 2)
    for (i in seq_len(ncol(data))) {
      prob <- prop.table(table(data[, i]))
      estimates <- estimate_mean_and_sd(prob, n_levels[i], skew[i])
      mat[i, ] <- estimates
    }
    tab <- as.table(t(mat))
    dimnames(tab) <- list(
      estimates = c("mean", "sd"),
      items = colnames(data)
    )
  }
  return(tab)
}

#' Estimate mean and standard deviation
#'
#' Estimates the mean and standard deviation of a latent variable given the
#' discrete probabilities of its observed Likert scale responses.
#'
#' @param prob named vector of probabilities for each response category.
#' @param n_levels number of response categories for the Likert scale item.
#' @param skew marginal skewness of the latent variable, defaults to 0.
#' @param eps tolerance for convergence, defaults to 1e-6.
#' @param maxit maximum number of iterations, defaults to 100.
#'
#' @return A numeric vector with two elements: the estimated mean and
#' standard deviation.
#' 
#' @examples
#' prob <- c("1" = 0.313, "2" = 0.579, "3" = 0.105, "4" = 0.003)
#' # returns estimates that are close to the actual mean and sd: c(-1, 0.5)
#' estimate_mean_and_sd(prob, 5)
#'
#' @details
#' This function uses an iterative algorithm to solve the system of non-linear
#' equations that describe the relationship between the continuous latent
#' variable and the observed discrete probability distribution of Likert scale
#' responses. The algorithm ensures stability by reparameterizing the system
#' and applying constraints to prevent stepping into invalid regions.
#'
#' @export
estimate_mean_and_sd <- function(prob, n_levels, skew = 0,
                                 eps = 1e-6, maxit = 100) {
  prob <- as.vector(pad_levels(prob, n_levels))
  endp <- calc_endpoints(n_levels, skew)
  dist_funcs <- initialize_distributions(skew)
  x <- matrix(c(0, 1)) # Initial guess
  trace <- matrix(rep(x, maxit), ncol = maxit)

  for (i in seq_len(maxit)) {
    b <- fn(x, endp, prob, dist_funcs$cdf_X)
    A <- jac(x, endp, dist_funcs$pdf_X)
    A_svd <- svd(A)
    dx <- A_svd$v %*% diag(1 / A_svd$d) %*% t(A_svd$u) %*% (-b)

    # Prevent stepping into negative values for v = 1 / sd
    while ((x + dx)[2] < 0) {
      dx <- dx / 2
    }

    x <- x + 0.2 * dx
    trace[, i] <- x

    if (norm(dx, "2") < eps) {
      break
    }
  }

  mean <- x[1]
  sd <- 1 / x[2]

  return(c(mean, sd))
}

#' Initialize CDF and PDF Functions
#'
#' Initializes the cumulative distribution function (CDF) and probability
#' density function (PDF) based on the skewness parameter.
#'
#' @param skew numeric value representing the skewness of the distribution.
#'
#' @return A list containing the CDF and PDF functions appropriate for the
#' given skewness.
#'
#' @noRd
initialize_distributions <- function(skew) {
  if (abs(skew) > 0) {
    check_package("sn")
    cp <- c("mu" = 0, "sd" = 1, "skew" = skew)
    dp <- sn::cp2dp(cp, family = "SN")
    return(list(
      cdf_X = function(x) sn::psn(x, dp = dp),
      pdf_X = function(x) sn::dsn(x, dp = dp)
    ))
  } else {
    return(list(cdf_X = stats::pnorm, pdf_X = stats::dnorm))
  }
}

#' Calculate Differences for Root Finding
#'
#' Computes the differences used in the iterative root-finding process.
#'
#' @param x numeric vector of current estimates for the location and scaling
#' parameters.
#' @param endp numeric vector of endpoints defining the boundaries of the
#' response categories.
#' @param prob numeric vector of probabilities for each response category.
#' @param cdf_X function representing the cumulative distribution function
#' (CDF) of the latent variable.
#'
#' @return A matrix of differences between the CDF evaluated at the endpoints
#' and the observed probabilities.
#'
#' @noRd
fn <- function(x, endp, prob, cdf_X) {
  u <- x[1]
  v <- x[2]
  y <- cdf_X(v * endp - u * v)
  return(matrix(utils::tail(y, -1) - utils::head(y, -1) - prob))
}

# Compute Jacobian Matrix
#'
#' Computes the Jacobian matrix used in the iterative root-finding process.
#'
#' @param x numeric vector of current estimates for the location and scaling
#' parameters.
#' @param endp numeric vector of endpoints defining the boundaries of the
#' response categories.
#' @param pdf_X function representing the probability density function (PDF)
#' of the latent variable.
#'
#' @return A matrix representing the Jacobian of the system of equations with
#' respect to the parameters.
#'
#' @noRd
jac <- function(x, endp, pdf_X) {
  u <- x[1]
  v <- x[2]
  midp <- utils::head(utils::tail(endp, -1), -1)

  du <- pdf_X(v * endp - u * v) * (-v)
  dv <- pdf_X(v * midp - u * v) * (midp - u)

  du <- utils::tail(du, -1) - utils::head(du, -1)
  dv <- c(utils::head(dv, 1), utils::tail(dv, -1)
  - utils::head(dv, -1), -utils::tail(dv, 1))

  return(cbind(du, dv))
}
