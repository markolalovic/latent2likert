#' Estimate Latent Parameters
#'
#' Estimates the location and scaling parameters of the latent variables from
#' existing survey data.
#'
#' @param data survey responses where the columns correspond to individual items.
#'             Apart from this, `data` can be of almost any class such as
#'             "data.frame" "matrix" or "array".
#' @param n_levels number of response categories, a vector or a number.
#' @param skew marginal skewness of latent variables, defaults to 0.
#' @return A table of estimated parameters for each latent variable.
##examples
##TODO: fix
##Warning in data(part_bfi) : data set ‘part_bfi’ not found
##data(part_bfi)
##head(part_bfi)
#' @details
#' The relationship between the continuous random variable \eqn{X} and the 
#' discrete probability distribution \eqn{p_k} can be described by a system 
#' of non-linear equations:
#' \deqn{
#'   p_{k} = F_{X}\left( \frac{x_{k - 1} - \xi}{\omega} \right) 
#'         - F_{X}\left( \frac{x_{k} - \xi}{\omega} \right) 
#'         \quad \text{for} \ k = 1, \dots, K
#' }
#' where:
#' \describe{
#'   \item{\eqn{F_{X}}}{ is the cumulative distribution function (CDF) of \eqn{X},}
#'   \item{\eqn{K}}{ is the number of possible response categories,}
#'   \item{\eqn{x_{k}}}{ are the endpoints defining the boundaries of the response categories,}
#'   \item{\eqn{p_{k}}}{ is the probability of the \eqn{k}-th response category,}
#'   \item{\eqn{\xi}}{ is the location parameter of \eqn{X},}
#'   \item{\eqn{\omega}}{ is the scaling parameter of \eqn{X}.}
#' }
#' The endpoints \eqn{x_{k}} are calculated by discretizing a random variable \eqn{Z} 
#' with mean 0 and standard deviation 1 that follows the same distribution as \eqn{X}. 
#' By solving the above system of non-linear equations iteratively, we can find the 
#' parameters that best fit the observed discrete probability distribution \eqn{p_{k}}.
#' 
#' The function `estimate_params`:
#' \enumerate{ 
#'     \item{}{Computes the proportion table of the responses for each item.}
#'     \item{}{Estimates the probabilities \eqn{p_{k}} for each item.}
#'     \item{}{Computes the estimates of \eqn{\xi} and \eqn{\omega}.}
#'     \item{}{Combines the estimates parameters for each item into a table.}
#' }
#' @seealso \code{\link{discretize_density}} for details on calculating the endpoints,
#' \code{\link{part_bfi}} for example of the survey data.
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
    dimnames(tab) <- list(estimates = c("mean", "sd"), items = colnames(data))
  }
  return(tab)
}

# Solve reparameterized system for stability
estimate_mean_and_sd <- function(prob, n_levels, skew = 0, eps = 1e-6, maxit = 100) {
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
  # list("mean"=mean, "sd"=sd, "trace"=trace)
  return(c(mean, sd))
}

# Initialize CDF and PDF functions based on skew
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

# Function to find roots
fn <- function(x, endp, prob, cdf_X) {
  u <- x[1]
  v <- x[2]
  y <- cdf_X(v * endp - u * v)
  return(matrix(utils::tail(y, -1) - utils::head(y, -1) - prob))
}

# Jacobian column wise
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

plot_contour <- function(fn, endp, prob, cdf_X, trace) {
  xlen <- 50
  ylen <- 50
  xgrid <- seq(-3, 3, length.out = xlen) # -5, 5
  ygrid <- seq(0.1, 3, length.out = ylen) # 0.1, 10
  zvals <- matrix(NA, ncol = xlen, nrow = ylen)
  for (i in seq_len(xlen)) {
    for (j in seq_len(ylen)) {
      zvals[i, j] <- norm(fn(matrix(c(xgrid[i], ygrid[j])), endp, prob, cdf_X), "2")
    }
  }
  graphics::contour(
    x = xgrid, y = ygrid, z = zvals,
    col = "gray42", xlab = "u = mu", ylab = "v = 1/sd"
  )
  graphics::grid(col = "lightgray", lty = "dotted")
  graphics::points(trace[1, ], trace[2, ], pch = 20, col = "blue")
}
