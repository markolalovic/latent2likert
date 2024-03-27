library(ggplot2)
library(cowplot)

plot_normal_density <- function(K, params) {
  sim_std <- simulate_normal_responses(K, c("mu"=0, "sd"=1))

  x <- seq(-3, 3, length = 200)
  y <- dnorm(x, params[["mu"]], params[["sd"]])

  dd <- data.frame(x=x, y=y)
  eps <- 0.008
  dd_subset <- subset(dd, x > sim_std$xk[2] - eps & x < sim_std$xk[3] + eps)

  ggplot(dd, aes(x = x, y = y)) +
    xlab("x") + ylab("Density") +
    ggtitle(paste(
      "mu = ", params[["mu"]],
      ", sd = ", params[["sd"]], sep="")) +
    geom_ribbon(data=dd_subset, aes(ymax=y), ymin=0, fill="#bababaff", colour=NA) +
    geom_vline(xintercept = sim_std$xk, col="black", linetype="dashed", linewidth=0.5) +
    geom_line() +
    theme_classic()
}

plot_normal_responses <- function(K, params) {
  sim <- simulate_normal_responses(K, params)

  d <- data.frame(Response=as.character(1:K),  Probability=sim$pk)
  ggplot(data = d, aes(x = Response, y = Probability, fill = Response)) +
    geom_col(color = "black",  width=0.8) +
    scale_fill_manual(values = c("white", "white", "#bababaff", "white", "white")) +
    geom_text(aes(label=round(Probability, 3)), vjust=-0.5, size=3) +
    ylim(0, max(max(d$Probability)) + 0.01) +
    theme_classic() +
    theme(legend.position = "none")
}

plot_normal_density_and_responses <- function(K, params) {
  p1 <- plot_normal_density(K, params)
  p2 <- plot_normal_responses(K, params)
  plot_grid(p1, p2,  nrow=2)
}

scale_and_shift_normal <- function(x, params) {
  mu <- params[["mu"]]
  sd <- params[["sd"]]
  return( (x - mu)/sd )
}

simulate_normal_responses <- function(K, params, scale_shift=FALSE) {
  fX <- function(x) { dnorm(x) }
  sol <- run_Lloyd(fX, K)
  xk <- sol$xk_estimates

  # there are 2 ways to get the right probabilities pk
  if (scale_shift) { # scale and shift xk and use standard normal fX
    xk <- scale_and_shift_normal(xk, params)
  } else {
    # 2. option is to keep std normal xk and use scaled and shifted fX
    fX <- function(x) { dnorm(x, params[["mu"]], params[["sd"]]) }
  }
  pk <- get_pk(xk, fX)
  return(list("pk"=pk, "xk"=xk))
}

# TEST
K <- 5
p1 <- plot_normal_density_and_responses(K, c("mu"=0, "sd"=1))
p2 <- plot_normal_density_and_responses(K, c("mu"=-1, "sd"=1))
p3 <- plot_normal_density_and_responses(K, c("mu"=-1, "sd"=0.5))
plot_grid(p1, p2, p3, ncol=3)
