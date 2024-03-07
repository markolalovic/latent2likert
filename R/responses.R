#' genLikert
#'
#' Generates a sample of simulated Likert scale item responses. It assumes the
#' underlying latent variable is following a skew-normal distribution and
#' performs optimal discretization using Lloyd-Max algorithm to generate
#' probabilities of a discrete random variable from which it draws the sample.
#'
#' @param size (positive, int)      Size of a sample
#' @param items (positive, int)     Number of Likert items
#' @param correlation (real)        Pairwise Pearson correlation
#' @param levels (positive, int)    Number of possible responses
#' @param location (real)           Determines the location or shift
#' @param scale (positive, real)    Determines the scale or dispersion
#' @param shape (real)              Determines the skewness or asymmetry
#' @return responses (data.frame)   Simulated Likert scale item responses
#' @examples
#' df <- generateData(size=100, items=10)
#' @export
genLikert <- function(size=1, items=1, correlation, 
                    levels=5, location=0, scale=1, shape=0) {
  if (items == 1) {
    sim <- simulateLikert(K=levels, xi=location, omega=scale, alpha=shape)
    x <- sample(x = 1:levels, size=size*items, replace=TRUE, prob=sim$pk)
    responses <- matrix(x, nrow=size, ncol=items)
    colnames(responses) <- 'X'
  } else {
    ## single numbers are used for all items
    if (is.numeric(levels)) {
      levels <- rep(levels, items)
    }
    if (is.numeric(location)) {
      location <- rep(location, items)
    }
    if (is.numeric(scale)) {
      scale <- rep(scale, items)
    }
    if (is.numeric(shape)) {
      shape <- rep(shape, items)
    }
    inputs <- cbind(levels, location, scale, shape)
    sims <- apply(inputs, 1, 
                  function(xRow) simulateLikert(xRow[1], xRow[2], xRow[3], xRow[4]))
    if (missing(correlation)) {
      ## random correlation matrix is used
      correlation <- randcorr(items)
    } else if (!is.matrix(correlation)) { 
      ## the same correlation is used between all pairs of items
      r <- correlation
      r <- sign(r)*min(abs(r), 1) # correlation must be between -1 and 1
      correlation <- matrix(data=r, nrow=items, ncol=items)
      diag(correlation) <- rep(1, items)
    }
    x <- mvtnorm::rmvnorm(n=size, mean=rep(0, items), sigma=correlation)
    responses <- sapply(1:items,
                        function(i) findInterval(x[,i], c(-Inf, sims[[i]]$xk, Inf)))
  }
  return(data.frame(responses))
}

#' estimateParameters
#'
#' Estimates parameters given responses to the Likert-scale survey questions.
#' It assumes that the underlying latent variable follows a normal distribution.
#'
#' @param df (data.frame)           Likert-scale responses
#' @param levels (positive, int)    Number of possible responses
#' @return (table)                  Estimated means and variances
#' @export
estimateParameters <- function(df, levels) {
  items <- ncol(df)
  if (items == 1) {
    pk <- prop.table(table(df))
    estimates <- estimateMeanVariance(pk=pk, K=levels)
    tab <- as.table(estimates)
    rownames(tab) <- c("mean", "variance")
  } else { # multiple items
    mat = matrix(data=NA, nrow=items, ncol=2)
    for (i in 1:ncol(df)) {
      pk <- prop.table(table(df[,i]))
      estimates <- estimateMeanVariance(pk=pk, K=levels[i])
      mat[i, ] <- estimates
    }
    tab <- as.table(t(mat))
    dimnames(tab) <- list(estimates = c("mean", "variance"), items = colnames(df))
  }
  return(tab)
}

#' simulateLikert
#'
#' Simulates Likert scale responses using skew-normal distribution and optimal
#' discretization.
#'
#' @param K (positive, int)      Number of possible responses i.e. levels
#' @param xi (real)              Determines the location i.e. mean
#' @param omega (positive, real) Determines the scale i.e. standard deviation
#' @param alpha (real)           Determines the amount of skewness
#' @return pk, rk, xk (list)     Manifest probabilities, representatives, cut points
simulateLikert <- function(K=5, xi=0, omega=1, alpha=0) {
  fX <- function(x) { dSN(x + meanSN(alpha), 0, 1, alpha) }
  sol <- applyLloydMax(fX, K)
  xkEst <- sol$xkEst
  pkEst <- sol$pkEst

  ## shift by location and scale the cut points accordingly
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
#' @param fX (function)     robability density function of the latent variable
#' @param K (positive, int) Possible number of responses, i.e. levels
#' @return xkEst (list)     Estimated cut points
#' @return pkEst (list)     Estimated probabilities
#' @return rkEst (list)     Estimated representatives
#' @return pkMeans (list)   Mean of estimated probabilities
#' @return pkMSEs  (list)   Mean squared errors for convergence plots
applyLloydMax <- function(fX, K) {
  rk <- seq(-5, 5, length.out = K) # start with K arbitrary representatives
  nIter <- 10 # 10 iterations is enough for convergence
  pkMeans <- c()
  pkMSEs <- c()
  for (i in 1:nIter) {
    xk <- getNewXk(rk) # calculate the new cut points
    rk <- getNewRk(xk, fX) # calculate the new representatives
    ## calculate estimated probabilities, means and distortion measure
    pk <- getPk(xk, fX)
    pkMeans <- c(pkMeans, getPkMean(pk))
    pkMSEs <- c(pkMSEs, getMSE(xk, rk, fX))
  }
  return(list("xkEst"=xk, "pkEst"=pk, "rkEst"=rk, "pkMeans"=pkMeans, "pkMSEs"=pkMSEs))
}

#' Calculates mean squared error.
#' @param xk (vector)   Cut points
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

#' Calculates new cut points xk from representatives rk.
#' @param rk (vector)  Representatives
#' @return xk (vector) Cut points
getNewXk <- function(rk) {
  K <- (length(rk) - 1) # generalized to arbitrary K
  xk <- rep(0, K)
  for (k in 1:K) {
    xk[k] <- (rk[k] + rk[k+1])/2
  }
  return(xk)
}

#' Calculates new representatives rk.
#' @param xk (vector)   Cut points
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

#' Calculates probabilities pk from cut points xk.
#' @param xk (vector)   Cut points
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
  domain <- (1:length(pk)) 
  return(sum(pk * domain))
}

#' Calculates variance of pk
#' @param pk (vector) Probabilities
#' @return var (real) Variance of pk
getPkVar <- function(pk) {
  domain <- (1:length(pk)) 
  m <- getPkMean(pk)
  return(sum(pk * (domain - m)^2))
}

#' Probability density function of a skew-normal distribution.
#' @param x (real)                 Variable
#' @param xi (real)                Determines the location or shift; e.g.: xi=0
#' @param omega (positive, real)   Determines the scale or dispersion; e.g.: omega=1
#' @param alpha (real)             Determines the skewness or asymmetry; e.g.: alpha=0
#' @return (real)                  Density at x
dSN <- function(x, xi=0, omega=1, alpha=0) {
  return(2/omega*dnorm((x - xi)/omega)*pnorm(alpha*(x - xi)/omega))
}

#' Delta parameter of skew-normal distribution
#' @param alpha (real)   Determines the skewness or asymmetry; e.g.: alpha=0
#' @return (real)        Delta of skew-normal distribution
deltaSN <- function(alpha) {
  return(alpha / (sqrt(1 + alpha^2)))
}

#' Mean of skew-normal distribution
#' @param alpha (real)   Determines the skewness or asymmetry; e.g.: alpha=0
#' @return (real)        Mean of skew-normal distribution
meanSN <- function(alpha) {
  return(deltaSN(alpha) * sqrt(2/pi))
}

#' Variance of skew-normal distribution
#' @param alpha (real)   Determines the skewness or asymmetry; e.g.: alpha=0
#' @return (real)        Variance of skew-normal distribution
varSN <- function(alpha) {
  return(1 - 2*(deltaSN(alpha)^2)/pi)
}

#' Shifts and scales cut points of skew-normal distribution
#' @param x (real)                Variable
#' @param xi (real)               Determines the location or shift; e.g.: xi=0
#' @param omega (positive, real)  Determines the scale or dispersion; e.g.: omega=1
#' @param alpha (real)            Determines the skewness or asymmetry; e.g.: alpha=0
#' @return (real)                 Shifted and scaled x
scaleShiftSN <- function(x, omega, xi, alpha) {
  return((x - meanSN(alpha))/omega + meanSN(alpha) - xi/omega)
}

#' Generates a random p x p correlation matrix
#' @param p (positive, int)      Denoting the size of the correlation matrix
#' @return (matrix)              A random p x p correlation matrix
randcorr <- function(p) {
  R <- drop(rWishart(1, p, diag(p)))
  R <- cov2cor(R)
  return(R)
}

#' Estimates mean and variance of latent normal distribution
#' using adaptive Gauss–Newton method
#' @param pk (list)                  Manifest proportions
#' @param K (positive, int)          Number of possible responses
#' @return (vector, real)            Estimated mean and variance
estimateMeanVariance <- function(pk, K, trace=FALSE) {
  x=matrix(c(0, 1)) # initial guess
  nIter=100 # number of iterations
  sim <- simulateLikert(K)
  xk <- c(-Inf, sim$xk, Inf)
  pk <- sapply(as.character(1:K), function(k) # pad missing levels with zeros
    ifelse(k %in% names(pk), pk[[k]], 0))
  
  hk <- function(x, k) { # component of reparameterized nonlinear system
    return(pnorm(x[2]*xk[k + 1] - x[2]*x[1]) - pnorm(x[2]*xk[k] - x[2]*x[1]) - pk[k])
  }
  
  dhk_u <- function(x, k) { # partial derivative of u = location
    if (k == 1) { # edge case of pk
      return(dnorm(x[2]*xk[k + 1] - x[2]*x[1])*(-x[2]))
    } else if (k == K) { # edge case of pk
      return(-dnorm(x[2]*xk[k] - x[2]*x[1])*(-x[2]))
    } else {
      return(
        dnorm(x[2]*xk[k + 1] - x[2]*x[1])*(-x[2])
        - dnorm(x[2]*xk[k] - x[2]*x[1])*(-x[2])
      )
    }
  }
  
  dhk_v <- function(x, k) { # partial derivative of v = 1/scale
    if (k == 1) { # edge case of pk
      return(dnorm(x[2]*xk[k + 1] - x[2]*x[1])*(xk[k + 1] - x[1]))
    } else if (k == K) { # edge case of pk
      return(-dnorm(x[2]*xk[k] - x[2]*x[1])*(xk[k] - x[1]))
    } else {
      return(
        dnorm(x[2]*xk[k + 1] - x[2]*x[1])*(xk[k + 1] - x[1])
        - dnorm(x[2]*xk[k] - x[2]*x[1])*(xk[k] - x[1])
      )
    }
  }
  
  f <- function(x) { # function to find roots
    matrix(sapply(1:K, function(k) hk(x, k)))
  }
  
  Df <- function(x) { # Jacobian column wise
    matrix(c(sapply(1:K, function(k) dhk_u(x, k)),
             sapply(1:K, function(k) dhk_v(x, k))),
           ncol=2)
  }
  
  x_trace <- c(x[1])
  y_trace <- c(x[2])
  for (i in 1:nIter) {
    b <- f(x) # eval f
    A <- Df(x) # eval Jacobian
    A.svd <- svd(A) # can be unstable, use SVD
    A_diag <- diag(1 / A.svd$d) 
    # solve linear least squares norm(A*d + b) -> min:
    d <- A.svd$v %*% A_diag %*% t(A.svd$u) %*% (-b)
    
    # iteration step adaptive method
    while ((x + d)[2] < 0) { # to prevent stepping into negative values
      d <- d/2 # use half the step size
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
  if (trace) { # draw contour
    xlen <- 50
    ylen <- 50
    xgrid <- seq(-3, 3, length.out = xlen) # -5, 5
    ygrid <- seq(0.1, 3, length.out = ylen) # 0.1, 10
    zvals <- matrix(NA, ncol = xlen, nrow = ylen)
    for (i in 1:xlen) {
      for (j in 1:ylen) {
        zvals[i, j] <- norm(f(matrix(c(xgrid[i], ygrid[j]))) , "2")
      }
    }
    contour(x = xgrid, y = ygrid, z = zvals, col="gray42", xlab = "u", ylab = "v")
    grid(col = "lightgray", lty = "dotted")
    points(x_trace, y_trace, pch=20, col="blue")
  }
  return(c(x[1], 1/x[2]))
}
