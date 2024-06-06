library(ggplot2)
library(cowplot)

plot_density <- function(K, params) {
  sim <- simulate_responses(K, params)

  params_dp <- convert_params(params)
  x <- seq(-3, 3, length = 200)
  y <- d_skew_normal(x, 
                     params_dp[["xi"]], 
                     params_dp[["omega"]], 
                     params_dp[["alpha"]])

  dd <- data.frame(x=x, y=y)
  eps <- 0.008
  dd_subset <- subset(dd, x > sim$xk[2] - eps & x < sim$xk[3] + eps)

  ggplot(dd, aes(x = x, y = y)) +
    xlab("x") + ylab("Density") +
    # ggtitle(paste(
    #   "mu = ", params[["mu"]],
    #   ", sd = ", params[["sd"]],
    #   ", gamma1 = ", params[["gamma1"]], sep="")) +
    geom_ribbon(data=dd_subset, aes(ymax=y), ymin=0, fill="#bababaff", colour=NA) +
    geom_vline(xintercept = sim$xk, col="black", linetype="dashed", linewidth=0.5) +
    geom_line() +
    theme_classic()
}

plot_responses <- function(K, params) {
  sim <- simulate_responses(K, params)

  d <- data.frame(Response=as.character(1:K),  Probability=sim$pk)
  ggplot(data = d, aes(x = Response, y = Probability, fill = Response)) +
    geom_col(color = "black",  width=0.8) +
    scale_fill_manual(values = c("white", "white", "#bababaff", "white", "white")) +
    geom_text(aes(label=round(Probability, 3)), vjust=-0.5, size=3) +
    ylim(0, max(max(d$Probability)) + 0.01) +
    theme_classic() +
    theme(legend.position = "none")
}

plot_density_and_responses <- function(K, params) {
  p1 <- plot_density(K, params)
  p2 <- plot_responses(K, params)
  plot_grid(p1, p2,  nrow=2)
}

# TEST
K <- 5

p1 <- plot_density_and_responses(K, c("mu"=0, "sd"=1, "gamma1"=0))
p2 <- plot_density_and_responses(K, c("mu"=-1, "sd"=1, "gamma1"=0))
p3 <- plot_density_and_responses(K, c("mu"=-1, "sd"=0.5, "gamma1"=0))
plot_grid(p1, p2, p3, ncol=3)

p1 <- plot_density_and_responses(K, c("mu"=0, "sd"=1, "gamma1"=0.5))
p2 <- plot_density_and_responses(K, c("mu"=-1, "sd"=1, "gamma1"=0.5))
p3 <- plot_density_and_responses(K, c("mu"=-1, "sd"=0.5, "gamma1"=-0.5))
plot_grid(p1, p2, p3, ncol=3)

# Skew normal X_0(gamma1) has mu=0 and sd=1:
params <- c("mu"=0, "sd"=1, "gamma1"=-0.5)
params_dp <- convert_params(params)
sample_0 <- sn::rsn(n=10^6, dp=params_dp)
mean(sample_0)
sd(sample_0)

testall <- function() {
  par(mfrow=c(2, 2))
  gamma1 <- 0
  params <- c("mu"=0, "sd"=1, "gamma1"=gamma1)
  params2 <- c("mu"=0.5, "sd"=0.8, "gamma1"=gamma1)
  testplot(params, params2)

  gamma1 <- -0.6
  params <- c("mu"=0, "sd"=1, "gamma1"=gamma1)
  params2 <- c("mu"=0.5, "sd"=0.8, "gamma1"=gamma1)
  testplot(params, params2)
}

testplot <- function(params, params2) {
  params_dp <- convert_params(params)
  params_dp2 <- convert_params(params2)

  sim1 <- simulate_responses(K, params)
  sim2 <- simulate_responses(K, params2)

  #par(mfrow=c(1, 1))
  x <- seq(-3, 3, length = 500)
  y2 <- d_skew_normal(x, params_dp2[["xi"]], params_dp2[["omega"]], params_dp2[["alpha"]])
  y0 <- rep(0, 500)

  plot(x, y2, type = "l", lty=2, lwd=1, col=rgb(red = 0, green = 0, blue = 1, alpha = 1))
  polygon(c(x, rev(x)), c(y2, rev(y0)), lwd=0.01,
          col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2))
  curve(d_skew_normal(x, params_dp[["xi"]], params_dp[["omega"]], params_dp[["alpha"]]), add=TRUE, lty=1, lwd=3)
  abline(v = sim1$xk, col="darkgray", lwd=.5, lty=1)


  data <- rbind(sim1$pk, sim2$pk)
  colnames(data) <- 1:5
  rownames(data) <- c("X", "Y")
  barplot(data,
          col=c(rgb(red = 0, green = 0, blue = 0, alpha = 1),
                rgb(red = 0, green = 0, blue = 1, alpha = 0.3)),
          border=c("black", rgb(red = 0, green = 0, blue = 1, alpha = 1)),
          font.axis=2,
          beside=T,
          xlab="group",
          font.lab=2)
}
testall()
