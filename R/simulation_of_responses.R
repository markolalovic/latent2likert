#' Get responses
#'
#' Returns a sample of random responses based on parameters of latent variables.
#'
#' @export
#' @param n number of observations
#' @param mu means of latent variables, a vector or a number
#' @param sd standard deviations of latent variables, a vector or a number
#' @param gamma1 marginal skewness of latent variables, a vector or a number.
#'               The values must be between -0.95 and 0.95.
#' @param K numbers of response categories, a vector or a number
#' @param R correlations between latent variables, 0 by default.
#'                    The value can be:
#'                        - a number: same correlation is used between all
#'                                     pairs of latent variables;
#'                        - "random": a random correlation matrix is used;
#'                        - a correlation matrix.
#' @return an array of random responses
#' @seealso See [responsesR::get_univariate_responses()] for univariate case.
#' @examples
#' data <- get_responses(n = 5, mu = c(0, 1, 2))
get_responses <- function(n=10, mu=0, sd=1, gamma1=0, K=5, R=0) {
  raw_inputs <- list(mu, sd, gamma1, K)
  nitems <- max(lengths(raw_inputs))

  if (nitems == 1) {
    return(get_univariate_responses(n, mu, sd, gamma1, K))
  }

  # single raw_inputs are repeated to create a vector
  inputs <- vapply(raw_inputs,
                   function(input) { rep(input, length.out=nitems) },
                   numeric(nitems))
  colnames(inputs) <- c("mu", "sd", "gamma1", "K")

  # conditions to deal with correlation input `R`
  # correlation `R` can be: 0, "random", number != 0 or a correlation matrix
  case_1 <- function(R) { sum(!is.character(R) & !is.matrix(R) & (R == 0)) }
  case_2 <- function(R) { sum(R == "random") }
  case_3 <- function(R) { sum(!is.character(R) & !is.matrix(R) & (R != 0)) }
  case_4 <- function(R) { sum(is.matrix(R)) }
  if (case_1(R) == 1) { # return multiple univariate items responses
    data <- apply(inputs, 1, function(input) {
      get_univariate_responses(
        n, input[["mu"]], input[["sd"]], input[["gamma1"]], input[["K"]])
    })
    colnames(data) <- sapply(seq_len(nitems), function(i) paste("Y", i, sep=""))
    return(data)
  }
  if (case_2(R) == 1) { # random correlation matrix is used
    R <- get_rand_corr_matrix(nitems)
  } else if (case_3(R) == 1) {
    if(R > 1 | R < -1) {
      stop('Error: correlation R must be between -1 and 1.')
    }
    R <- matrix(data=R, nrow=nitems, ncol=nitems)
    diag(R) <- rep(1, nitems)
  } else {
    # a correlation matrix must be provided
    if(case_4(R) != 1)
      stop("Error: correlation R can be a number, \"random\", 
           or a correlation matrix.")
  }
  sigma <- cor2cov(R, inputs[, "sd"])

  if (sum(abs(gamma1)) > 0) { # at least one latent variable is skewed
    # use correlated skew-normal latent variables
    if (!requireNamespace("sn", quietly = TRUE)) {
      stop(
        "Package \"sn\" must be installed to get correlated skew responses.
        Please run:\n\n \tinstall.packages(\"sn\")\n\n",
        call. = FALSE)
    }
    cp_list <- list(mean=inputs[, "mu"],
                    var.cov=sigma,
                    gamma1=inputs[, "gamma1"])

    dp_list <- sn::cp2dp(cp_list, family="SN")
    lat <- sn::rmsn(n, dp=dp_list)
  } else { # all latent variables are normal
    # use normal latent variables
    lat <- mvtnorm::rmvnorm(n, inputs[, "mu"], sigma)
  }
  # simulate responses with 0, 1 and skewness gamma1
  sims <- apply(inputs, 1, function(input) {
    simulate_responses(input[["K"]],
                       c("mu"=0, "sd"=1, "gamma1"=input[["gamma1"]]))
  })

  data <- sapply(seq_len(nitems), function(i) {
    findInterval(lat[, i], c(-Inf, sims[[i]]$xk, Inf))
  })
  colnames(data) <- sapply(seq_len(nitems), function(i) paste("Y", i, sep = ""))
  return(data)
}

#' Get univariate responses
#'
#' Generates a sample of random univariate responses.
#'
#' @param n number of observations
#' @param mu mean of latent variable
#' @param sd standard deviation of latent variable
#' @param gamma1 skewness of latent variable, must be between -0.95 and 0.95
#' @param K number of response categories, default is 5
#' @return a vector of random responses
#' @seealso See [responsesR::simulate_responses()] for an explanation of a
#' discrete random variable from which samples are taken.
get_univariate_responses <- function(n=10, mu=0, sd=1, gamma1=0, K=5) {
  sim <- simulate_responses(K, c("mu" = mu, "sd" = sd, "gamma1" = gamma1))
  data <- sample(x = seq_len(K), size = n, replace = TRUE, prob = sim$pk)
  return(data)
}

#' Simulate responses
#'
#' Returns a description of a discrete random variable with `K` possible
#' outcomes that best approximates the latent distribution in the mean-square
#' sense.
#'
#' @export
#' @param K number of response categories
#' @param params skew-normal parameters `xi`, `omega` and `alpha`
#' @return lists of probabilities `pk` and cut points `xk`
#' @examples
#' simulate_responses(K = 5, params = c("mu"=0, "sd"=1, "gamma1"=0))
simulate_responses <- function(K, params) {
  cp_X0 <- c("mu"=0, "sd"=1, "gamma1"=params[["gamma1"]])
  dp_X0 <- convert_params(cp_X0)
  f_X0 <- function(x) {
    d_skew_normal(x, dp_X0[["xi"]], dp_X0[["omega"]], dp_X0[["alpha"]])
  }
  sol_X0 <- run_Lloyd(f_X0, K)
  xk_X0 <- sol_X0$xk_estimates

  # use scaled and shifted density to get pk
  dp_X1 <- convert_params(params)
  f_X1 <- function(x) {
    d_skew_normal(x, dp_X1[["xi"]], dp_X1[["omega"]], dp_X1[["alpha"]])
  }
  pk_X1 <- get_pk(xk_X0, f_X1)
  return(list("pk"=pk_X1, "xk"=xk_X0))
}

#' Implementation of Lloyd's algorithm
#'
#' Given a probability density function `fX` of a random variable `X`,
#' the function returns a discrete random variable with `K` possible outcomes
#' that best approximates `X` in the mean-square or L2 sense by minimizing MSE.
#'
#' @param fX probability density function of the latent variable
#' @param K number of response categories
#' @return xk_estimates list of estimated cut points
#' @return pk_estimates list of estimated probabilities
#' @return rk_estimates list of estimated representatives
#' @return pk_means list of means of estimated probabilities
#' @return pk_MS_errors list of mean squared errors for convergence plots
run_Lloyd <- function(fX, K) {
  rk <- seq(-5, 5, length.out = K) # start with equidistant representatives
  niters <- 100 # enough for convergence
  pk_means <- c()
  pk_MS_errors <- c()
  for (i in seq_len(niters)) {
    xk <- get_new_xk(rk) # calculate the new cut points
    rk <- get_new_rk(xk, fX) # calculate the new representatives
    ## calculate estimated probabilities, means and distortion measure
    pk <- get_pk(xk, fX)
    pk_means <- c(pk_means, get_pk_mean(pk))
    pk_MS_errors <- c(pk_MS_errors, get_MSE(xk, rk, fX))
  }
  return(list("xk_estimates"=xk,
              "pk_estimates"=pk,
              "rk_estimates"=rk,
              "pk_means"=pk_means,
              "pk_MS_errors"=pk_MS_errors))
}

#' Calculate new cut points `xk` from representatives `rk`
#'
#' @param rk vector of representatives
#' @return vector `xk` of cut points
get_new_xk <- function(rk) {
  K <- (length(rk) - 1) # generalized to arbitrary number of points
  xk <- rep(0, K)
  for (k in seq_len(K)) {
    xk[k] <- (rk[k] + rk[k+1])/2
  }
  return(xk)
}

#' Calculate new representatives `rk` from cut points `xk`
#'
#' @param xk cut points
#' @param fX probability density function of the latent variable
#' @return vector `rk` of representatives
get_new_rk <- function(xk, fX) {
  K <- length(xk) + 1
  xk <- c(-Inf, xk, Inf)
  rk <- rep(0, K)
  f_above <- function(x) {
    x*fX(x)
  }
  f_below <- function(x) {
    fX(x)
  }
  for (k in seq_len(K)) {
    result_above <- stats::integrate(f_above, lower=xk[k], upper=xk[k+1])[[1]]
    result_below <- stats::integrate(f_below, lower=xk[k], upper=xk[k+1])[[1]]
    rk[k] <- result_above/result_below
  }
  return(rk)
}

#' Calculate probabilities `pk` from cut points `xk`
#'
#' @param xk cut points
#' @param fX probability density function of the latent variable
#' @return vector of probabilities `pk`
get_pk <- function(xk, fX) {
  K <- length(xk) + 1
  xk <- c(-Inf, xk, Inf)
  pk <- rep(0, K)
  for (k in seq_len(K)) {
    lower_bound <- xk[k]
    upper_bound <- xk[k+1]
    pk[k] <- stats::integrate(fX, lower=lower_bound, upper=upper_bound)[[1]]
  }
  return(pk)
}

#' Calculate the mean of `pk`
#'
#' @param pk vector of probabilities `pk`
#' @return mean of `pk`
get_pk_mean <- function(pk) {
  domain <- seq_len(length(pk))
  return(sum(pk * domain))
}

#' Calculate the mean squared error
#'
#' @param xk vector of cut points
#' @param rk vector of representatives
#' @param fX probability density function of the latent variable
#' @return mean squared error
get_MSE <- function(xk, rk, fX) {
  K <- length(rk) # generalized for testing
  xk <- c(-Inf, xk, Inf)
  mse <- 0
  for (k in seq_len(K)) {
    lower_bound <- xk[k]
    upper_bound <- xk[k+1]
    integrand <- function(x) { ((x - rk[k])^2)*fX(x)  }
    mse <- mse + stats::integrate(integrand, 
                                  lower=lower_bound, upper=upper_bound)[[1]]
  }
  return(mse)
}

#' Probability density function of a skew normal distribution
#'
#' @export
#' @param x variable
#' @param xi determines the location
#' @param omega determines the scale
#' @param alpha determines the shape
#' @return density at `x`
#' @seealso [sn::dsn()]
d_skew_normal <- function(x, xi=0, omega=1, alpha=0) {
  return(2/omega*stats::dnorm((x - xi)/omega) * 
           stats::pnorm(alpha*(x - xi)/omega))
}

#' The mean of skew normal distribution
#'
#' @param alpha determines the shape
#' @return mean of a skew-normal distribution
mean_skew_normal <- function(alpha) {
  return(delta_skew_normal(alpha) * sqrt(2/pi))
}

#' Delta parameter of a skew normal distribution
#'
#' @param alpha determines the shape
#' @return delta of a skew-normal distribution
delta_skew_normal <- function(alpha) {
  return(alpha / (sqrt(1 + alpha^2)))
}

#' Variance of a skew normal distribution
#'
#' @param alpha determines the shape
#' @return variance of a skew-normal distribution
var_skew_normal <- function(alpha) {
  return(1 - 2*(delta_skew_normal(alpha)^2)/pi)
}

#' Convert parameters
#'
#' Converts from centered parameters to direct parameters appearing in
#' the skew-normal density.
#'
#' @export
#' @param cp centered parameters c(mu, sd, gamma1)
#' @return direct parameters c(xi, omega, alpha)
#' @seealso [sn::cp2dp]
convert_params <- function(cp) {
  mu <- cp[1]
  sd <- cp[2]
  gamma1 <- cp[3]

  b <- sqrt(2/pi)
  r  <- sign(gamma1)*(2*abs(gamma1)/(4 - pi))^(1/3)
  delta <- r/(b*sqrt(1 + r^2))

  mu_z <- b*delta
  sd_z <- sqrt(1 - mu_z^2)

  omega <- sd / sd_z
  xi <- mu - omega * mu_z
  alpha <- delta / sqrt(1 - delta^2)
  dp <- as.numeric(c(xi, omega, alpha))
  names(dp) <- c("xi", "omega", "alpha")
  return(dp)
}

#' Scale and shift cut points
#'
#' @param x variable
#' @param dp direct parameters `xi`, `omega`, `alpha`
#' @return shifted and scaled `x`
scale_and_shift <- function(x, dp) {
  xi <- dp[["xi"]]
  omega <- dp[["omega"]]
  alpha <- dp[["alpha"]]
  mean_sn <- mean_skew_normal(alpha)
  return((x - mean_sn)/omega + mean_sn - xi/omega)
}

#' Generate a random `p x p` correlation matrix
#'
#' @param p the size of the correlation matrix
#' @return a random `p x p` correlation matrix
get_rand_corr_matrix <- function(p) {
  R <- drop(stats::rWishart(1, p, diag(p)))
  R <- stats::cov2cor(R)
  return(R)
}

#' Get covariance matrix from a correlation matrix
#'
#' @param R correlation matrix
#' @param s standard deviation vector
#' @return covariance matrix
cor2cov <- function(R, s){
  diag(s) %*% R %*% diag(s)
}
