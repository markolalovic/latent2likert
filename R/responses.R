#' rLikert
#'
#' Generates a sample of simulated Likert scale item responses. It assumes the
#' underlying latent variable is following a skew-normal distribution and
#' performs optimal discretization using Lloyd-Max algorithm to generate
#' probabilities of a discrete random variable from which it draws the sample.
#'
#' @param size (positive, int)   Size of a sample; e.g.: size=100
#' @param items (positive, int)  Number of Likert items; e.g.: items=10
#' @param levels (positive, int) Number of possible responses; e.g.: levels=5
#' @param location (real)        Determines the location or shift; e.g.: location=0
#' @param scale (positive, real) Determines the scale or dispersion; e.g.: scale=1
#' @param shape (real)           Determines the skewness or asymmetry; e.g.: shape=0
#' @return responses (data.frame) Simulated Likert scale item responses; e.g.:
#'     > rLikert(size=6, items=8)
#'       X1 X2 X3 X4 X5 X6 X7 X8
#'     1  5  2  2  2  3  5  3  5
#'     2  3  3  1  3  3  2  3  3
#'     3  3  5  3  4  3  3  1  4
#'     4  3  4  5  3  3  2  3  3
#'     5  3  4  4  3  5  1  2  3
#'     6  2  2  4  4  3  3  3  2
#' @examples
#' rLikert(size=6, items=8)
#' responses <- rLikert(size=6, items=8)
#' @export
rLikert <- function(size=1, items=1, levels=5, location=0, scale=1, shape=0) {
  sim <- simulateLikert(levels, location, scale, shape)
  x <- sample(x = 1:levels, size=size*items, replace = TRUE, prob = sim$pk)
  responses <- matrix(x, nrow = size, ncol = items)
  if (items == 1) {
    colnames(responses) <- 'X'
  }
  return(data.frame(responses))
}

#' simulateLikert
#'
#' Simulates Likert scale responses using skew-normal distribution and optimal
#' discretization.
#'
#' @param K (positive, int)      Number of possible responses; e.g.: K=5
#' @param xi (real)              Determines the location or shift; e.g.: xi=0
#' @param omega (positive, real) Determines the scale or dispersion; e.g.: omega=1
#' @param alpha (real)           Determines the skewness or asymmetry; e.g.: alpha=0
#' @return pk, rk, xk (list)     Manifest probabilities, representatives, cutpoints
simulateLikert <- function(K, xi, omega, alpha) {
  fX <- function(x) { dSN(x + meanSN(alpha), 0, 1, alpha) }
  sol <- applyLloydMax(fX, K)
  xkEst <- sol$xkEst
  pkEst <- sol$pkEst

  ## simpler to apply location and scale to the cutpoints than to the PDF
  xk <- scaleShiftSN(xkEst, omega, xi, alpha)
  pk <- getPk(xk, fX)
  rk <- getNewRk(xk, fX)
  return(list("pk"=pk, "rk"=rk, "xk"=xk))
}

#' applyLloydMax
#'
#' Implementation of Lloyd-Max algorithm for simulation of manifest variables
#' from continuous latent variables.
#'
#' @param fX (function)     Probability density function of the latent variable
#' @param K (positive, int) Possible number of responses
#' @return xkEst (list)     Estimated cutpoints
#' @return pkEst (list)     Estimated probabilities
#' @return rkEst (list)     Estimated representatives
#' @return pkMeans (list)   Mean of estimated probabilities
#' @return pkMSEs  (list)   Mean squared errors for convergence plots
applyLloydMax <- function(fX, K) {
  rk <- seq(-5, 5, length.out = K) # start with K arbitrary representatives
  nIter <- 10 # 10 iterations is fine, since the convergence is fast
  pkMeans <- c()
  pkMSEs <- c()
  for (i in 1:nIter) {
    xk <- getNewXk(rk) # calculate new cutpoints
    rk <- getNewRk(xk, fX) # calculate new representatives
    ## calculate pk's, mean and distortion measure for pk
    pk <- getPk(xk, fX)
    pkMeans <- c(pkMeans, getPkMean(pk))
    pkMSEs <- c(pkMSEs, getMSE(xk, rk, fX))
  }
  return(list("xkEst"=xk, "pkEst"=pk, "rkEst"=rk, "pkMeans"=pkMeans,
    "pkMSEs"=pkMSEs))
}

#' Calculates mean squared error.
#' @param xk (vector)   Cutpoints
#' @param rk (vector)   Representatives
#' @param fX (function) Probability density function of the latent variable
#' @return mse (real)   Mean squared error
getMSE <- function(xk, rk, fX) {
  K <- length(rk) # generalized for testing
  xk <- c(-Inf, xk, Inf)
  mse <- 0
  for (k in 1:K) {
    lowerBound <- xk[k]
    upperBound <- xk[k+1]
    integrand <- function(x) { ((x - rk[k])^2)*fX(x)  }
    mse <- mse + integrate(integrand, lower=lowerBound, upper=upperBound)[[1]]
  }
  return(mse)
}

#' Calculates new cutpoints xk from representatives rk.
#' @param rk (vector)  Representatives
#' @return xk (vector) Cutpoints
getNewXk <- function(rk) {
  K <- (length(rk) - 1) # generalized to arbitrary K
  xk <- rep(0, K)
  for (k in 1:K) {
    xk[k] <- (rk[k] + rk[k+1])/2
  }
  return(xk)
}

#' Calculates new representatives rk.
#' @param xk (vector)   Cutpoints
#' @param fX (function) Probability density function of the latent variable
#' @return rk (vector)  Representatives
getNewRk <- function(xk, fX) {
  K <- length(xk) + 1
  xk <- c(-Inf, xk, Inf)
  rk <- rep(0, K)
  for (k in 1:K) {
    fUpper<- function(x) { x*fX(x) }
    fLower <- function(x) { fX(x) }
    IUpper <- integrate(fUpper, lower=xk[k], upper=xk[k+1])[[1]]
    ILower <- integrate(fLower, lower=xk[k], upper=xk[k+1])[[1]]
    rk[k] <- IUpper/ILower
  }
  return(rk)
}

#' Calculates probabilities pk from cutpoints xk.
#' @param xk (vector)   Cutpoints
#' @param fX (function) Probability density function of the latent variable
#' @return pk (vector)  Probabilities
getPk <- function(xk, fX) {
  K <- length(xk) + 1
  xk <- c(-Inf, xk, Inf)
  pk <- rep(0, K)
  for (k in 1:K) {
    lowerBound <- xk[k]
    upperBound <- xk[k+1]
    pk[k] <- integrate(fX, lower=lowerBound, upper=upperBound)[[1]]
  }
  return(pk)
}

#' Calculates mean of pk
#' @param pk (vector)  Probabilities
#' @return mean (real) Mean of pk
getPkMean <- function(pk) {
  domain <- (1:length(pk)) # generalized it so we can test it
  return(sum(pk * domain))
}

#' Calculates variance of pk
#' @param pk (vector) Probabilities
#' @return var (real) Variance of pk
getPkVar <- function(pk) {
  domain <- (1:length(pk)) # generalized it so we can test it
  m <- getPkMean(pk)
  return(sum(pk * (domain - m)^2))
}

#' Probability density function of a skew-normal distribution.
#' @param x (real)               Variable
#' @param xi (real)              Determines the location or shift; e.g.: xi=0
#' @param omega (positive, real) Determines the scale or dispersion; e.g.: omega=1
#' @param alpha (real)           Determines the skewness or asymmetry; e.g.: alpha=0
#' @return (real) Density at x
dSN <- function(x, xi, omega, alpha) {
  return(2/omega*dnorm((x - xi)/omega)*pnorm(alpha*(x - xi)/omega))
}

#' Delta parameter of skew-normal distribution
#' @param alpha (real) Determines the skewness or asymmetry; e.g.: alpha=0
#' @return (real)      Delta of skew-normal distribution
deltaSN <- function(alpha) {
  return(alpha / (sqrt(1 + alpha^2)))
}

#' Mean of skew-normal distribution
#' @param alpha (real) Determines the skewness or asymmetry; e.g.: alpha=0
#' @return (real)      Mean of skew-normal distribution
meanSN <- function(alpha) {
  return(deltaSN(alpha) * sqrt(2/pi))
}

#' Variance of skew-normal distribution
#' @param alpha (real) Determines the skewness or asymmetry; e.g.: alpha=0
#' @return (real)      Variance of skew-normal distribution
varSN <- function(alpha) {
  return(1 - 2*(deltaSN(alpha)^2)/pi)
}

#' Shifts and scales cutpoints of skew-normal distribution
#' @param x (real)               Variable
#' @param xi (real)              Determines the location or shift; e.g.: xi=0
#' @param omega (positive, real) Determines the scale or dispersion; e.g.: omega=1
#' @param alpha (real)           Determines the skewness or asymmetry; e.g.: alpha=0
#' @return (real) Shifted and scaled x
scaleShiftSN <- function(x, omega, xi, alpha) {
  return((x - meanSN(alpha))/omega + meanSN(alpha) - xi/omega)
}
