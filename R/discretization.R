#' Discretize Density
#'
#' Transforms the density function of a continuous random variable into a
#' discrete probability distribution with minimal distortion 
#' using the Lloyd-Max algorithm.
#'
#' @param density_func The probability density function of the continuous random variable.
#' @param n_levels The cardinality of the set of all possible outcomes.
#' @param eps The convergence threshold for the algorithm.
#'
#' @return A list containing:
#' \describe{
#'   \item{prob}{Discrete probability distribution.}
#'   \item{endp}{Optimal end points of intervals that partition the domain.}
#'   \item{repr}{Representation points of the intervals.}
#'   \item{dist}{Distortion measured as the mean-squared error (MSE).}
#' }
#' @export
discretize_density <- function(density_func, n_levels, eps = 1e-6) {
    # Initial set of representation points
    repr <- seq(-5, 5, length.out = n_levels)
    midp <- calc_midpoints(repr)
    dist <- compute_distortion(density_func, midp, repr)

    diff <- Inf
    while (diff > eps) {
        repr <- find_representatives(density_func, midp)
        midp <- calc_midpoints(repr)
        newd <- compute_distortion(density_func, midp, repr)

        diff <- dist - newd
        dist <- newd
    }

    endp <- c(-Inf, midp, Inf)
    prob <- calc_probs(density_func, endp)
    return(list(prob = prob, endp = endp, repr = repr, dist = dist))
}

#' Calculate Midpoints
#'
#' Calculate the midpoints of the intervals defined by the representation points.
#'
#' @param repr The representation points.
#'
#' @return A vector of midpoints.
calc_midpoints <- function(repr) {
    n_levels <- length(repr)
    midp <- (repr[2:n_levels] + repr[1:(n_levels - 1)]) / 2
    return(midp)
}

#' Find Representatives
#'
#' Find the representation points for the intervals defined by the midpoints.
#'
#' @param density_func The probability density function of the continuous random variable.
#' @param midp The midpoints of the intervals.
#'
#' @return A vector of representation points.
find_representatives <- function(density_func, midp) {
    n_levels <- length(midp) + 1
    endp <- c(-Inf, midp, Inf)
    repr <- numeric(n_levels)
    for (k in seq_len(n_levels)) {
        a <- stats::integrate(function(x) x * density_func(x), endp[k], endp[k + 1])
        b <- stats::integrate(density_func, endp[k], endp[k + 1])
        repr[k] <- a[[1]] / b[[1]]
    }
    return(repr)
}

#' Compute Distortion
#'
#' Compute the distortion (mean-squared error) for the given representation points.
#'
#' @param density_func The probability density function of the continuous random variable \code{X}.
#' @param midp The midpoints of the intervals.
#' @param repr The representation points of the intervals.
#'
#' @return The distortion (mean-squared error).
compute_distortion <- function(density_func, midp, repr) {
    n_levels <- length(repr)
    endp <- c(-Inf, midp, Inf)
    mse <- vapply(seq_len(n_levels), function(k) {
        stats::integrate(function(x) density_func(x) * (x - repr[k])^2, endp[k], endp[k + 1])[[1]]
    }, numeric(1))
    return(sum(mse))
}

#' Calculate Probabilities
#'
#' Calculate the discrete probabilities for the given cut points.
#'
#' @param density_func The probability density function of the continuous random variable.
#' @param endp The end points of the intervals.
#'
#' @return A vector of probabilities for the intervals.
calc_probs <- function(density_func, endp) {
    n_levels <- length(endp) - 1
    prob <- vapply(seq_len(n_levels), function(k) {
        stats::integrate(function(x) density_func(x), endp[k], endp[k + 1])[[1]]
    }, numeric(1))
    return(prob)
}
