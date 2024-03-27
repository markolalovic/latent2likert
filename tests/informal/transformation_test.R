library(ggplot2)
library("latex2exp")
library(cowplot)
library(RColorBrewer)

density_plot <- function(params1,
                         params2,
                         element_text_size=15,
                         legend_text_size=20,
                         plot_line=TRUE) {
  params_dp1 <- convert_params(params1)
  params_dp2 <- convert_params(params2)

  pdf1 <- function(x) {
    d_skew_normal(x, params_dp1[["xi"]], params_dp1[["omega"]], params_dp1[["alpha"]])
  }
  pdf2 <- function(x) {
    d_skew_normal(x, params_dp2[["xi"]], params_dp2[["omega"]], params_dp2[["alpha"]])
  }

  # data
  xlim <- c(-3.5, 3.5)
  x <- seq(-3.5, 3.5, length = 500)
  d <- data.frame(x=c(x, x),
                  y=c(pdf1(x), pdf2(x)),
                  group=factor(c(rep(1, 500), rep(2, 500))))

  # labels
  density_labels <- c(TeX("$X_{1}:\\, \\, \\mu = 0, \\sigma = 1"),
                      TeX("$X_{2}:\\, \\, \\mu = 0.5, \\sigma = 0.8"))

  if (plot_line) {
    p1 <- ggplot(data=d, aes(x=x, y=y, group=group)) +
      geom_line(aes(linetype=group), linewidth=1) +
      labs(x = "x", y = "Density") +
      scale_x_continuous(breaks = -3:3, labels = -3:3) +
      scale_linetype_manual(values = c("solid", "32"), labels = density_labels) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            legend.position.inside = c(0.22, 0.9),
            legend.title=element_blank(),
            text = element_text(size = element_text_size),
            legend.text=element_text(size=legend_text_size))
  } else { # plot area
    p1 <- ggplot(NULL, aes(xlim)) +
      geom_area(stat="function", fun=pdf1, xlim=xlim, fill=color1, alpha = 0.22) +
      geom_area(stat="function", fun=pdf2, xlim=xlim, fill=color2, alpha = 0.4) +
      labs(x = "x", y = "Density") +
      scale_x_continuous(breaks = -3:3, labels = -3:3) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            text = element_text(size = element_text_size))
  }
  return(p1)
}

probs_plot <- function(params1, params2, K, element_text_size=15, legend_text_size=20) {
  sim1 <- simulate_responses(K, params1)
  sim2 <- simulate_responses(K, params2)

  d <- data.frame(k=c(1:K, 1:K),
                  Probability=c(sim1$pk, sim2$pk),
                  group=factor(c(rep(1, K), rep(2, K))))

  probs_labels <- c(TeX("$Pr(Y_{1} = k)"), TeX("$Pr(Y_{2} = k)"))
  ggplot(data=d, aes(x=k, y=Probability, fill=group)) +
    geom_col(position=position_dodge(0.44), width = 0.4, color="black") +
    scale_fill_manual(values=c(color1, color2), labels = probs_labels) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.position = c(0.22, 0.9),
          legend.title=element_blank(),
          text = element_text(size = element_text_size),
          legend.text=element_text(size=legend_text_size, margin = margin(t = 1)),
          legend.key = element_rect(color = NA, fill = NA),
          legend.key.size = unit(.5, "cm")
    ) +
    guides(fill = guide_legend(byrow = TRUE))
}

draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)

  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}

# Parameters
K <- 5
gamma1 <- 0
element_text_size <- 15
legend_text_size <- 17

params1 <- c("mu"=0, "sd"=1, "gamma1"=gamma1)
params2 <- c("mu"=0.5, "sd"=0.8, "gamma1"=gamma1)

color_palette <- brewer.pal(n=5, name = "BrBG")
color1 <- "#808080" #  "#E5E4E2" # neutral group
color2 <- color_palette[5] # higher mean group
plot_line <- TRUE
p1 <- density_plot(params1, params2, element_text_size, legend_text_size, plot_line = plot_line)
p2 <- probs_plot(params1, params2, K, element_text_size, legend_text_size)

plot_grid(p1, NULL, p2,  ncol=3, rel_widths = c(1.2, 0.05, 0.8),
          labels = c("A", "", "B"), label_size=20, hjust=c(-0.2, -0.2))
