#' Estimate parameters
#'
#' Given existing survey data, this function estimates the parameters of the latent variables.
#'
#' @export
#' @param data The survey responses where the columns correspond to individual items.
#'             Apart from this, `data` can be of almost any class such as
#'             "data.frame" "matrix" or "array".
#' @param n_levels The number of response categories, a vector or a number.
#' @param skew Marginal skewness of latent variables, defaults to 0.
#' @return A table of estimated parameters for each latent variable.
#' @seealso See also [latent2likert::estimate_mean_and_sd()].
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
  return(matrix(tail(y, -1) - head(y, -1) - prob))
}

# Jacobian column wise
jac <- function(x, endp, pdf_X) {
  u <- x[1]
  v <- x[2]
  midp <- head(tail(endp, -1), -1)

  du <- pdf_X(v * endp - u * v) * (-v)
  dv <- pdf_X(v * midp - u * v) * (midp - u)

  du <- tail(du, -1) - head(du, -1)
  dv <- c(head(dv, 1), tail(dv, -1) - head(dv, -1), -tail(dv, 1))

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
