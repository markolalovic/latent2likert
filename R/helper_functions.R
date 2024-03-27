#' Calculate variance of probabilities `pk`
#'
#' @param pk vector of probabilities
#' @return variance of `pk`
get_pk_var <- function(pk) {
  domain <- (1:length(pk))
  m <- get_pk_mean(pk)
  return(sum(pk * (domain - m)^2))
}

#' Variance of a skew normal distribution
#'
#' @param alpha determines the shape
#' @return variance of a skew-normal distribution
var_skew_normal <- function(alpha) {
  return(1 - 2*(delta_skew_normal(alpha)^2)/pi)
}

#' Pad missing levels with zeros
#'
#' @export
#' @param pk proportions or probabilities across possible responses
#' @param K number of response categories
#' @return table of proportions across all possible responses
pad_levels <- function(pk, K) {
  pk <- sapply(as.character(1:K), function(k) {
    ifelse(k %in% names(pk), pk[[k]], 0)
  })
  return(pk)
}

#' Get a table of proportions across each possible response
#'
#' @export
#' @param data a vector or array of responses
#' @param K number of response categories
#' @return table of proportions
get_prop_table <- function(data, K) {
  if (is.vector(data)) {
    tab <- pad_levels(prop.table(table(data)), K)
  } else {
    tab <- t(apply(data, 2, function(x_col) {
      pad_levels(prop.table(table(x_col)), K)
    }))
    dimnames(tab) <- list(Item = rownames(tab), Response = colnames(tab))
  }
  return(tab)
}

#' Get random centered parameters `c(mu, sd, gamma1)` for testing
#'
#' @export
#' @return random centered parameters
get_random_cp <- function() {
  mu <- stats::rnorm(1, 0, 1)
  sd <- stats::runif(1, 0.1, 2)
  gamma1 <- stats::runif(1, -0.95, 0.95)
  cp <- c("mu"=mu, "sd"=sd, "gamma1"=gamma1)
  return(cp)
}

#' Return percentage for a given number
#'
#' @export
#' @param x number
#' @param digits number of digits
percentify <- function(x, digits=0) {
  percentage <-formatC(x*100, format="f", digits=digits)
  return(paste0(percentage, "%"))
}
