#' Estimate parameters
#'
#' Given survey responses, this function estimates the parameters of the latent
#' variables assuming that the latent variables are normally distributed.
#'
#' @export
#' @param data responses, where the columns correspond to individual items.
#'             Apart from this, `data` can be of almost any class such as
#'             "data.frame" "matrix" or "array".
#' @param K number of response categories, a vector or a number
#' @param gamma1 marginal skewness of latent variables, 0 by default
#' @return a table of estimated parameters for each latent variable
#' @seealso See [responsesR::estimate_mu_sd()] for univariate case.
#' @examples
#' estimate_parameters(data = c(2, 3, 1, 4, 4, 3, 1, 4, 5, 4), K = 5)
estimate_parameters <- function(data, K, gamma1=0) {
  if (is.vector(data)) {
    pk <- prop.table(table(data))
    estimates <- estimate_mu_sd(pk, K, gamma1)
    tab <- as.table(estimates)
    rownames(tab) <- c("mu", "sd")
  } else { # multiple items
    nitems <- ncol(data)
    gamma1 <- rep(gamma1, length.out = nitems)
    if (is.numeric(K)) {
      K <- rep(K, nitems)
    }
    mat <- matrix(data=NA, nrow=nitems, ncol=2)
    for (i in seq_len(ncol(data))) {
      pk <- prop.table(table(data[,i]))
      estimates <- estimate_mu_sd(pk, K[i], gamma1[i])
      mat[i, ] <- estimates
    }
    tab <- as.table(t(mat))
    dimnames(tab) <- list(estimates = c("mu", "sd"), items = colnames(data))
  }
  return(tab)
}

#' Estimate mean and standard deviation
#'
#' Given proportions pk of responses over categories 1, ..., K:
#'   pk = (number of responses with value k) / (the number of all responses)
#' the function estimates the parameters of the latent variable assuming that
#' the latent variable is normal or skew-normal provided the value of `gamma1`.
#'
#' @export
#' @param pk a list or a table of proportions of responses
#' @param K number of response categories
#' @param gamma1 skewness of latent variable, 0 by default
#' @param trace if TRUE, plots contour of the function and trace iteration steps
#' @return estimated mean and standard deviation of the latent variable
#' @examples
#' pk <- list("1" = 0.313, "2" = 0.579, "3" = 0.105, "4" = 0.003)
#' estimate_mu_sd(pk = pk, K = 5)
estimate_mu_sd <- function(pk, K, gamma1=0, trace=FALSE) {
  if(missing(K)) {
    stop('The number of response categories for the item must be provided.')
  }
  x <- matrix(c(0, 1)) # initial guess
  niters <- 100 # number of iterations
  cp <- c("mu"=x[1], "sd"=x[2], "gamma1"=gamma1)
  sim <- simulate_responses(K, cp)
  xk <- c(-Inf, sim$xk, Inf)
  pk <- vapply(as.character(seq_len(K)), function(k) { # pad missing levels
    ifelse(k %in% names(pk), pk[[k]], 0)
  }, numeric(1))

  if (abs(gamma1) > 0) { # X ~ skew normal
    if (!requireNamespace("sn", quietly = TRUE)) {
      stop(
        "Package \"sn\" must be installed to estimate skew normal parameters.
        Please run:\n\n \tinstall.packages(\"sn\")\n\n",
        call. = FALSE)
    }
    dp <- convert_params(cp)
    cdf_X <- function(x) { sn::psn(x, dp = dp) } #TODO: cdf_lat
    pdf_X <- function(x) { sn::dsn(x, dp = dp) } #TODO: lat_pdf
  } else { # X ~ normal
    cdf_X <- function(x) { stats::pnorm(x) }
    pdf_X <- function(x) { stats::dnorm(x) }
  }

  # solve nonlinear system:
  # cdf(v x_(k+1) - v u) - cdf(v x_k - v u) = pk
  # using parameters u=xi, v=1/omega
  hk <- function(x, k) {  # component of nonlinear system
    return(
      cdf_X(x[2]*xk[k + 1] - x[2]*x[1])
      - cdf_X(x[2]*xk[k] - x[2]*x[1]) - pk[k])
  }
  dhk_u <- function(x, k) { # partial derivative wrt u
    if (k == 1) { # edge case of pk
      return(pdf_X(x[2]*xk[k + 1] - x[2]*x[1])*(-x[2]))
    } else if (k == K) { # edge case of pk
      return(-pdf_X(x[2]*xk[k] - x[2]*x[1])*(-x[2]))
    } else {
      return(
        pdf_X(x[2]*xk[k + 1] - x[2]*x[1])*(-x[2])
        - pdf_X(x[2]*xk[k] - x[2]*x[1])*(-x[2])
      )
    }
  }
  dhk_v <- function(x, k) { # partial derivative wrt v
    if (k == 1) { # edge case of pk
      return(pdf_X(x[2]*xk[k + 1] - x[2]*x[1])*(xk[k + 1] - x[1]))
    } else if (k == K) { # edge case of pk
      return(-pdf_X(x[2]*xk[k] - x[2]*x[1])*(xk[k] - x[1]))
    } else {
      return(
        pdf_X(x[2]*xk[k + 1] - x[2]*x[1])*(xk[k + 1] - x[1])
        - pdf_X(x[2]*xk[k] - x[2]*x[1])*(xk[k] - x[1])
      )
    }
  }
  f <- function(x) { # function to find roots
    matrix(vapply(seq_len(K), function(k) { hk(x, k) }, numeric(1)))
  }
  Df <- function(x) { # Jacobian column wise
    matrix(c(vapply(seq_len(K), function(k) { dhk_u(x, k) }, numeric(1)),
             vapply(seq_len(K), function(k) { dhk_v(x, k) }, numeric(1))),
           ncol=2)
  }
  x_trace <- c(x[1])
  y_trace <- c(x[2])
  for (i in seq_len(niters)) {
    b <- f(x) # evaluate f
    A <- Df(x) # evaluate Jacobian
    A.svd <- svd(A) # can be unstable, use SVD
    A_diag <- diag(1 / A.svd$d)
    ## solve linear least squares norm(A*d + b) -> min:
    d <- A.svd$v %*% A_diag %*% t(A.svd$u) %*% (-b)

    ## iteration step adaptive method
    while ((x + d)[2] < 0) { # to prevent stepping into negative values
      d <- d/2 # half the step size
    }
    x <- x + 0.2*d # smoothing
    if (trace) {
      x_trace <- c(x_trace, x[1])
      y_trace <- c(y_trace, x[2])
    }
    if (norm(d, "2") < 1e-15) {
      break
    }
  }
  if (trace) {
    plot_contour(f, x_trace, y_trace)
  }
  mu <- x[1]
  sd <- 1/x[2]
  return(c(mu, sd))
}

#' Plot the contour of function `f` along with provided trace
#'
#' @param f function to find roots
#' @param x_trace x coordinates of the trace
#' @param y_trace y coordinates of the trace
plot_contour <- function(f, x_trace, y_trace) {
  xlen <- 50
  ylen <- 50
  xgrid <- seq(-3, 3, length.out = xlen) # -5, 5
  ygrid <- seq(0.1, 3, length.out = ylen) # 0.1, 10
  zvals <- matrix(NA, ncol = xlen, nrow = ylen)
  for (i in seq_len(xlen)) {
    for (j in seq_len(ylen)) {
      zvals[i, j] <- norm(f(matrix(c(xgrid[i], ygrid[j]))) , "2")
    }
  }
  graphics::contour(x = xgrid, y = ygrid, z = zvals, 
                    col="gray42", xlab = "u", ylab = "v")
  graphics::grid(col = "lightgray", lty = "dotted")
  graphics::points(x_trace, y_trace, pch=20, col="blue")
}
