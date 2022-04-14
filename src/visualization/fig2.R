# 0. Working setup ---------------------------
library(ggplot2)
library(sf)
library(tidyverse)
library(fixest)
library(dplyr)
library(reshape2)
library(RColorBrewer)

# 1. Set paths and parameters -----
load("CONFIG.Rspace")
model_data_fpath <- file.path(gdrive_fpath, "Models")
# figure_fpath <- file.path(gdrive_fpath,  "Figures")
figure_fpath_local <- file.path("results", "figures")

load(file.path(model_data_fpath, "modelmatrix.RData"))

# 2. Helper functions to bootstrap and visualize model effects  ---------------

round.choose <- function(x, roundTo, dir = 1) {
  # round x to nearest roundTo up (dir = 1) or down (dir = 0)
  if (dir == 1) { ## ROUND UP
    x + (roundTo - x %% roundTo)
  } else {
    if (dir == 0) { ## ROUND DOWN
      x - (x %% roundTo)
    }
  }
}

bootstrap_coef <- function(formula_eq, data, K = 100) {
  # bootstrap coeffs of fixed effect model using K bootstrap samples
  # returning n_coef x K matrix (or K length vector if only one coef)
  ll <- dim(data)[1]
  replicate(K, coef(feols(formula(formula_eq), data = data[sample(1:ll, size = ll, replace = T), ])), simplify = TRUE)
}

bootstrap_model <- function(formula_eq, data, X, x_center_row = 1, K = 100, level = .95) {
  # bootstraped CIs at given level of fixed effect model using K bootstrap samples
  # with response function recentered at the value x_center_row index of X, and
  # where X is the len(x_grid) x n_coef design matrix to run prediction on
  # returning 3 x len(x_grid) array with upper/lower CIs in the first/third rows
  # and median in the second row
  coef.boot <- bootstrap_coef(formula_eq, data, K)
  y.boot <- X %*% coef.boot
  if (!is.null(x_center_row)) {
    y.boot <- sweep(y.boot, 2, y.boot[x_center_row, ], "-")
  }

  apply(y.boot, 1, function(x) quantile(x, probs = c(1 - level, 0.5, level)))
}

plot_model <- function(confint, x, color, xlab = "", ylab = "", new_plot = T) {
  # y_llim <- c(round.choose(min(confint), 10, 0), round.choose(max(confint), 10, 1))
  y_llim <- c(floor(min(confint)), ceiling(max(confint)))
  if (new_plot == TRUE) {
    # x_llim <- ifelse(l_histogram==TRUE, c(min(x),max(x)) - 15, c(min(x),max(x)))
    x_llim <- c(min(x), max(x))
    plot(1, xlim = x_llim, ylim = y_llim, las = 1, xlab = xlab, ylab = ylab, type = "n", cex.axis = 1.2, axes = FALSE)
    axis(1, las = 1)
    axis(2, col = "black", col.ticks = "black", las = 1)
  }
  polygon(c(x, rev(x)), c(confint[1, ], rev(confint[3, ])), col = adjustcolor(color, alpha.f = 0.4), border = NA)
  lines(x, confint[2, ], col = color, lwd = 2)
}



visualize_coef <- function(CI, x, color = "blue", new_plot = T,
                           xlab = "", ylab = "",
                           y_obs = NULL, hist_base_y = NULL) {
  plot_model(CI, x, color, xlab = xlab, ylab = ylab, new_plot)

  y_min <- min(CI)
  x_res <- diff(x)[1] # get resolution of x
  print(x_res)

  if (!is.null(y_obs)) {
    # Histogram params
    dis <- 0.055
    count_scale <- 15 * x_res
    gap <- .5 * x_res
    ymin_offset <- -.5 * x_res

    base <- ifelse(is.null(hist_base_y), y_min + ymin_offset, hist_base_y)
    # Make histograms
    zz <- hist(y_obs, plot = F, breaks = length(x))
    cts <- zz$counts / max(zz$counts) * count_scale
    rect(zz$breaks, base, zz$breaks + gap, base + cts, col = adjustcolor(color, alpha.f = 0.6), lwd = .1)
  }

  # if(l_histogram==TRUE){
  #   bins = seq(0, 700, 2)
  #   hist.hom = data$hom_rate_100k
  #
  #   # Histogram params
  #   dis = 0.055
  #   base= -15
  #
  #   # Make histograms
  #   yy <- hist(hist.hom, plot=F,breaks=bins)
  #   cts = yy$counts/max(yy$counts)*10
  #   rect(xleft=base,ybottom=bins-0.1, xright=base+cts,ytop=bins,col="red")
  # }
}


# 3. Create Main Effect & Rural/Urban Plot  --------------------------
# setup color pallettes
purp_pal <- brewer.pal(name = "Purples", n = 9)
blue_pal <- brewer.pal(name = "Blues", n = 9)

# plot main effect
x <- 0:85
pdf(file.path(figure_fpath, "main_effect.pdf"), height = 5, width = 4.5)
CI <- bootstrap_model(
  formula_eq = "hom_rate_100k ~ mean_vhi |  munic_dep + Year",
  data = full_df, X = matrix(x), K = 500, x_center_row = 42
)
visualize_coef(
  CI = CI, y_obs = full_df$mean_vhi, color = purp_pal[9],
  x = x, xlab = "VHI", ylab = "Homicide Rate"
)
dev.off()

# plot rural and urban models
x <- 0:85
CI_urban <- bootstrap_model(
  formula_eq = "hom_rate_100k ~ mean_vhi |  munic_dep + Year",
  data = full_df[full_df$urban_2013 == 1, ],
  X = matrix(x), K = 500, x_center_row = 42
)
CI_rural <- bootstrap_model(
  formula_eq = "hom_rate_100k ~ mean_vhi |  munic_dep + Year",
  data = full_df[full_df$urban_2013 == 0, ],
  X = matrix(x), K = 500, x_center_row = 42
)

pdf(file.path(figure_fpath, "rural_urban_effect.pdf"), height = 5, width = 4.5)
visualize_coef(
  CI = CI_urban, y_obs = full_df$mean_vhi, color = blue_pal[8],
  x = x, xlab = "VHI", ylab = "Homicide Rate"
)

visualize_coef(
  CI = CI_rural, y_obs = full_df$mean_vhi, color = blue_pal[4],
  x = x, xlab = "VHI", ylab = "Homicide Rate",
  new_plot = F, hist_base_y = -40
)
dev.off()

# 3. Create Marginal Ag Effect Plot  --------------------------
green_pal <- brewer.pal(name = "Greens", n = 9)
x <- 0:64 / 100
# get Matrix of [1, X] to multiply times [beta1, beta_2]^T
X <- cbind(rep(1, length(x)), x)

CI <- bootstrap_model(
  formula_eq = "hom_rate_100k ~ mean_vhi*cropland|  munic_dep + Year",
  data = full_df, K = 500,
  X = X, x_center_row = NULL
)

# plot marginal effect for ag
pdf(file.path(figure_fpath, "cropland_effect.pdf"), height = 5, width = 4.5)
visualize_coef(
  CI = CI, color = green_pal[8], x = x, y_obs = full_df$cropland,
  xlab = "% Cropland", ylab = "Homicide Rate Marginal Effect",
)
col <- "gray20"
abline(h = 0, col = col, lwd = 1, lty = 3)
dev.off()