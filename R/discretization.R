#' Discretize Density
#'
#' Transforms the density function of a continuous random variable into a
#' discrete probability distribution with minimal distortion
#' using the Lloyd-Max algorithm.
#'
#' @param density_fn probability density function of the continuous random variable.
#' @param n_levels cardinality of the set of all possible outcomes.
#' @param eps convergence threshold for the algorithm.
#'
#' @return A list containing:
#' \describe{
#'   \item{prob}{discrete probability distribution.}
#'   \item{endp}{endpoints of intervals that partition the continuous domain.}
#'   \item{repr}{representation points of the intervals.}
#'   \item{dist}{distortion measured as the mean-squared error (MSE).}
#' }
#' @examples
#' discretize_density(density_fn = stats::dnorm, n_levels = 5)
#' discretize_density(density_fn = function(x) {
#'    2 * stats::dnorm(x) * stats::pnorm(0.5 * x)}, n_levels = 4)
#' @details 
#' The function addresses the problem of transforming a continuous random 
#' variable \eqn{X} into a discrete random variable \eqn{Y} with minimal 
#' distortion. Distortion is measured as mean-squared error (MSE):
#' \deqn{
#'   \text{E}\left[ (X - Y)^2 \right] = 
#'   \sum_{k=1}^{K} \int_{x_{k-1}}^{x_{k}} f_{X}(x) 
#'   \left( x - r_{k} \right)^2 \, dx
#' }
#' where:
#' \describe{
#'   \item{\eqn{f_{X}}}{ is the probability density function of \eqn{X},}
#'   \item{\eqn{K}}{ is the number of possible outcomes of \eqn{Y},}
#'   \item{\eqn{x_{k}}}{ are endpoints of intervals that partition the domain of \eqn{X},}
#'   \item{\eqn{r_{k}}}{ are representation points of the intervals.}
#' }
#' This problem is solved using the following iterative procedure:
#' \enumerate{
#'   \item{}{Start with an arbitrary initial set of representation points:
#'                          \eqn{r_{1} < r_{2} < \dots < r_{K}}.}
#'   \item{}{Repeat the following steps until the improvement in MSE falls 
#' below given \eqn{\epsilon}.}
#'   \enumerate{
#'     \item{}{Calculate endpoints as \eqn{x_{k} = (r_{k+1} + r_{k})/2} for each 
#'           \eqn{k = 1, \dots, K-1} and set \eqn{x_{0}} and \eqn{x_{K}} to 
#'           \eqn{-\infty} and \eqn{\infty}, respectively.}
#'     \item{}{Update representation points by setting \eqn{r_{k}} equal to the 
#'           conditional mean of \eqn{X} given \eqn{X \in (x_{k-1}, x_{k})} for 
#'           each \eqn{k = 1, \dots, K}.}
#'  }
#' }
#' 
#' With each execution of steps (a) and (b), the MSE decreases or remains the same. 
#' As MSE is nonnegative, 
#' it approaches a limit. The algorithm terminates when the improvement in MSE is 
#' less than a given \eqn{\epsilon > 0}, ensuring convergence after a finite number 
#' of iterations.
#'   
#' This procedure is known as Lloyd-Max's algorithm, initially used for scalar 
#' quantization and closely related to the k-means algorithm. Local convergence 
#' has been proven for log-concave density functions by Kieffer. Many common 
#' probability distributions are log-concave including the normal and skew 
#' normal distribution, as shown by Azzalini.
#'
#' @references
#' Azzalini, A. (1985). 
#' A class of distributions which includes the normal ones. 
#' \emph{Scandinavian Journal of Statistics} \bold{12(2)}, 171–178.
#' \url{http://www.jstor.org/stable/4615982} 
#' 
#' Kieffer, J. (1983).
#' Uniqueness of locally optimal quantizer for log-concave density and convex 
#' error function.
#' \emph{IEEE Transactions on Information Theory} \bold{29}, 42–47.
#' 
#' Lloyd, S. (1982). 
#' Least squares quantization in PCM.
#' \emph{IEEE Transactions on Information Theory} \bold{28 (2)}, 129–137.
#'
#' @export
discretize_density <- function(density_fn, n_levels, eps = 1e-6) {
  # Initial set of representation points
  repr <- seq(-5, 5, length.out = n_levels)
  midp <- calc_midpoints(repr)
  dist <- compute_distortion(density_fn, midp, repr)

  diff <- Inf
  while (diff > eps) {
    repr <- update_epresentatives(density_fn, midp)
    midp <- calc_midpoints(repr)
    newd <- compute_distortion(density_fn, midp, repr)

    diff <- dist - newd
    dist <- newd
  }

  endp <- c(-Inf, midp, Inf)
  prob <- calc_probs(density_fn, endp)
  return(list(prob = prob, endp = endp, repr = repr, dist = dist))
}

#' Calculate Midpoints
#'
#' Calculate the midpoints of the intervals defined by the representation points.
#'
#' @param repr representation points.
#'
#' @return a vector of midpoints.
#' @noRd
calc_midpoints <- function(repr) {
  n_levels <- length(repr)
  midp <- (repr[2:n_levels] + repr[1:(n_levels - 1)]) / 2
  return(midp)
}

#' Update Representation Points
#'
#' Calculate the representation points for the intervals given the new midpoints.
#'
#' @param density_fn probability density function of the continuous random variable.
#' @param midp midpoints of the intervals.
#'
#' @return a vector of representation points.
#' @noRd
update_epresentatives <- function(density_fn, midp) {
  n_levels <- length(midp) + 1
  endp <- c(-Inf, midp, Inf)
  repr <- numeric(n_levels)
  for (k in seq_len(n_levels)) {
    a <- stats::integrate(function(x) {x * density_fn(x)}, endp[k], endp[k + 1])
    b <- stats::integrate(density_fn, endp[k], endp[k + 1])
    repr[k] <- a[[1]] / b[[1]]
  }
  return(repr)
}

#' Compute Distortion
#'
#' Compute the distortion (mean-squared error) given the midpoints and 
#' representation points.
#'
#' @param density_fn probability density function.
#' @param midp midpoints of the intervals.
#' @param repr representation points of the intervals.
#'
#' @return distortion (mean-squared error).
#' @noRd
compute_distortion <- function(density_fn, midp, repr) {
  n_levels <- length(repr)
  endp <- c(-Inf, midp, Inf)
  mse <- vapply(seq_len(n_levels), function(k) {
    stats::integrate(function(x) {
      density_fn(x) * (x - repr[k])^2}, endp[k], endp[k + 1])[[1]]
  }, numeric(1))
  return(sum(mse))
}

#' Calculate Probabilities
#'
#' Calculate the discrete probabilities for the given endpoints.
#'
#' @param density_fn probability density function of the continuous random variable.
#' @param endp endpoints of the intervals.
#'
#' @return a vector of probabilities for the intervals.
#' @noRd
calc_probs <- function(density_fn, endp) {
  n_levels <- length(endp) - 1
  prob <- vapply(seq_len(n_levels), function(k) {
    stats::integrate(function(x) {
      density_fn(x)}, endp[k], endp[k + 1])[[1]]
  }, numeric(1))
  return(prob)
}
