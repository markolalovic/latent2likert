library(ggplot2)
library(cowplot)
library(e1071)

dp_test <- function(dp) {
  s <- sn::rsn(n = 10^5, dp = dp)
  sample_params <- c("mu"=mean(s), "sd"=sd(s), "gamma1"=e1071::skewness(s))
  print(round(sample_params, 2))
}

get_pk_skewness <- function(pk) {
  mu <- get_pk_mean(pk)
  m3 <- sum( pk * ((1:K) - mu)^3 )
  s3 <- sum( pk * ((1:K) - mu)^2 )^(3/2)
  return(m3/s3) # estimator of gamma1
}

get_sample_skewness <- function(data) {
  m3 <- (1/size) * sum( (data - mean(data))^3 )
  s3 <- ((1/(size - 1)) * sum( (data - mean(data))^2 ))^(3/2)
  return(m3/s3) # estimator of gamma1
}

plot_skew_density <- function(K, params) {
  sim <- simulate_skew_responses(K, params)

  params_dp <- convert_params(params)
  x <- seq(-3, 3, length = 200)
  y <- d_skew_normal(x, params_dp[["xi"]], params_dp[["omega"]], params_dp[["alpha"]])
  #plot(x, y, type="l", lwd = 2, xlab = "", ylab = "")

  dd <- data.frame(x=x, y=y)
  eps <- 0.008
  dd_subset <- subset(dd, x > sim$xk[2] - eps & x < sim$xk[3] + eps)

  ggplot(dd, aes(x = x, y = y)) +
    xlab("x") + ylab("Density") +
    ggtitle(paste(
      "mu = ", params[["mu"]],
      ", sd = ", params[["sd"]],
      ", gamma1 = ", params[["gamma1"]], sep="")) +
    geom_ribbon(data=dd_subset, aes(ymax=y), ymin=0, fill="#bababaff", colour=NA) +
    geom_vline(xintercept = sim$xk, col="black", linetype="dashed", linewidth=0.5) +
    geom_line() +
    theme_classic()
}

plot_skew_responses <- function(K, params) {
  sim <- simulate_skew_responses(K, params)

  d <- data.frame(Response=as.character(1:K),  Probability=sim$pk)
  ggplot(data = d, aes(x = Response, y = Probability, fill = Response)) +
    geom_col(color = "black",  width=0.8) +
    scale_fill_manual(values = c("white", "white", "#bababaff", "white", "white")) +
    geom_text(aes(label=round(Probability, 3)), vjust=-0.5, size=3) +
    ylim(0, max(max(d$Probability)) + 0.01) +
    theme_classic() +
    theme(legend.position = "none")
}

plot_skew_density_and_responses <- function(K, params) {
  p1 <- plot_skew_density(K, params)
  p2 <- plot_skew_responses(K, params)
  plot_grid(p1, p2,  nrow=2)
}

simulate_skew_responses <- function(K, params) {
  std_cp <- c("mu"=0, "sd"=1, "gamma1"=params[["gamma1"]])
  std_dp <- convert_params(std_cp)
  fX <- function(x) {
    d_skew_normal(x, std_dp[["xi"]], std_dp[["omega"]], std_dp[["alpha"]])
  }
  sol <- run_Lloyd(fX, K)
  xk <- sol$xk_estimates

  params_dp <- convert_params(params)
  # use scaled and shifted density
  fX <- function(x) {
    d_skew_normal(x, params_dp[["xi"]], params_dp[["omega"]], params_dp[["alpha"]])
  }
  pk <- get_pk(xk, fX)
  return(list("pk"=pk, "xk"=xk))
}

# TEST
K <- 5

p1 <- plot_skew_density_and_responses(K, c("mu"=0, "sd"=1, "gamma1"=-0.5))
p2 <- plot_skew_density_and_responses(K, c("mu"=0, "sd"=1, "gamma1"=0))
p3 <- plot_skew_density_and_responses(K, c("mu"=0, "sd"=1, "gamma1"=0.5))
plot_grid(p1, p2, p3, ncol=3)
