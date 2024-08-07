% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimation.R
\name{estimate_mean_and_sd}
\alias{estimate_mean_and_sd}
\title{Estimate mean and standard deviation}
\usage{
estimate_mean_and_sd(prob, n_levels, skew = 0, eps = 1e-06, maxit = 100)
}
\arguments{
\item{prob}{named vector of probabilities for each response category.}

\item{n_levels}{number of response categories for the Likert scale item.}

\item{skew}{marginal skewness of the latent variable, defaults to 0.}

\item{eps}{tolerance for convergence, defaults to 1e-6.}

\item{maxit}{maximum number of iterations, defaults to 100.}
}
\value{
A numeric vector with two elements: the estimated mean and
standard deviation.
}
\description{
Estimates the mean and standard deviation of a latent variable given the
discrete probabilities of its observed Likert scale responses.
}
\details{
This function uses an iterative algorithm to solve the system of non-linear
equations that describe the relationship between the continuous latent
variable and the observed discrete probability distribution of Likert scale
responses. The algorithm ensures stability by reparameterizing the system
and applying constraints to prevent stepping into invalid regions.
}
\examples{
prob <- c("1" = 0.313, "2" = 0.579, "3" = 0.105, "4" = 0.003)
# returns estimates that are close to the actual mean and sd: c(-1, 0.5)
estimate_mean_and_sd(prob, 5)

}
