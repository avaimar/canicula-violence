# 0. Working setup ---------------------------
library(ggplot2)
library(sf)
library(tidyverse)
library(fixest)
library(dplyr)
library(reshape2)
library(RColorBrewer)

#1. Set paths and parameters -----
load('CONFIG.Rspace')
model_data_fpath <- file.path(gdrive_fpath, "Models")
#figure_fpath <- file.path(gdrive_fpath,  "Figures")
figure_fpath_local <- file.path("results", "figures")

load(file.path(model_data_fpath, "modelmatrix.RData"))

# 2. Helper functions to bootstrap and visualize model effects  ---------------

round.choose <- function(x, roundTo, dir = 1) {
  # round x to nearest roundTo up (dir = 1) or down (dir = 0)
  if(dir == 1) {  ##ROUND UP
    x + (roundTo - x %% roundTo)
  } else {
    if(dir == 0) {  ##ROUND DOWN
      x - (x %% roundTo)
    }
  }
}

bootstrap_coef <- function(formula_eq, data, K = 100) {
  # bootstrap coeffs of fixed effect model using K bootstrap samples
  # returning n_coef x K matrix (or K length vector if only one coef)
  ll = dim(data)[1]
  replicate(K, coef(feols(formula(formula_eq), data=data[sample(1:ll,size=ll,replace=T),])), simplify = TRUE)
}

bootstrap_model <- function(formula_eq, data, X, x_center_row = 1, K = 100, level = .95) {
  # bootstraped CIs at given level of fixed effect model using K bootstrap samples
  # with response function recentered at the value x_center_row index of X, and 
  # where X is the len(x_grid) x n_coef design matrix to run prediction on
  # returning 3 x len(x_grid) array with upper/lower CIs in the first/third rows
  # and median in the second row
  coef.boot <- bootstrap_coef(formula_eq, data, K)
  y.boot <- X %*% coef.boot
  #y.boot <- sweep(y.boot, 2, y.boot[x_center_row,], '-') 
 
  apply(y.boot,1,function(x) quantile(x,probs=c(1-level,0.5,level)))
}

 plot_model <- function(confint, x, color, xlab = '', ylab= '', new_plot =T) {
  #y_llim <- c(round.choose(min(confint), 10, 0), round.choose(max(confint), 10, 1))
  y_llim <- c(floor(min(confint)), ceiling(max(confint)))
  if (new_plot == TRUE){
    #x_llim <- ifelse(l_histogram==TRUE, c(min(x),max(x)) - 15, c(min(x),max(x)))
    x_llim <- c(min(x),max(x))
    plot(1,xlim=x_llim, ylim = y_llim, las=1,xlab=xlab,ylab=ylab, type='n', cex.axis=1.2, axes=FALSE)
    axis(1)
    axis(2, col = "black", col.ticks = "black")
  }
  polygon(c(x,rev(x)),c(confint[1,],rev(confint[3,])),col = adjustcolor(color,alpha.f=0.4),border = NA)
  lines(x,confint[2,], col = color, lwd = 2)
}


visualize_coef <- function(CI, x, color = 'blue',  new_plot = T, y_obs = NULL, hist_base_y = NULL) {
  
  plot_model(CI, x, color, xlab='VHI', ylab='Homicide Rate', new_plot)
  
  #y_min <- round.choose(min(CI), 10, 0)
  y_min <- floor(min(CI))

  if (!is.null(y_obs)){
    # Histogram params
    dis = 0.055
    base <- ifelse(is.null(hist_base_y), y_min - .5, hist_base_y)
    
    # Make histograms
    zz <- hist(y_obs,plot=F,breaks=max(x))
    cts = zz$counts/max(zz$counts)*15
    rect(zz$breaks, base, zz$breaks+.5, base+cts, col=adjustcolor(color,alpha.f=0.6), lwd = .1)
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

# 1. Create Main Effect & Rural/Urban Plot  ------------------------------------
#setup color pallettes
purp_pal <- brewer.pal(name = 'Purples', n = 9)
blue_pal <- brewer.pal(name = 'Blues', n = 9)
green_pal <- brewer.pal(name = 'Greens', n = 9)

pdf(file.path(figure_fpath, 'main_effect.pdf'), height = 7, width = 6)

x = 0:85
CI <- bootstrap_model(formula_eq='hom_rate_100k ~ mean_vhi |  munic_dep + Year',
                      data=full_df, X= matrix(x), K = 100, x_center_row =42 )
visualize_coef(CI = CI, y_obs = full_df$mean_vhi, color=purp_pal[9], x = x)
dev.off()

pdf(file.path(figure_fpath, 'rural_urban_effect.pdf'), width = 5, height = 4.5)

CI <- bootstrap_model(formula_eq='hom_rate_100k ~ mean_vhi |  munic_dep + Year',
                      data=full_df[full_df$urban_2013==1,], X= matrix(x), K = 100, x_center_row =42 )
visualize_coef(CI = CI, y_obs = full_df$mean_vhi, color=blue_pal[8], x = x)

CI <- bootstrap_model(formula_eq='hom_rate_100k ~ mean_vhi |  munic_dep + Year',
                      data=full_df[full_df$urban_2013==0,], X= matrix(x), K = 100, x_center_row =42 )
visualize_coef(CI = CI, y_obs = full_df$mean_vhi, color=blue_pal[4], x = x, new_plot = F, hist_base_y = -40)



###TODO: finish for marginal effects
x <- 0:64/100
X <- cbind(rep(1, length(x)), x)
CI <- bootstrap_model(formula_eq = 'hom_rate_100k ~ mean_vhi*cropland|  munic_dep + Year', 
                      data = full_df, K = 500, 
                      X  = X, x_center_row = 1)

visualize_coef(CI = CI, color=green_pal[8], x = x)




plot_f <- function(x, xlim) {
  CI <- bootstrap_model(formula_eq = 'hom_rate_100k ~ mean_vhi*cropland|  munic_dep + Year', data = full_df, K = 100, x_center = NA, x = x)
  mod <- feols(hom_rate_100k ~ mean_vhi*cropland|  munic_dep + Year, data = full_df)
  coef(mod)[1]
  l_histogram<- FALSE
  pal <- brewer.pal(name = 'Greens', n = 9)
  color=pal[8]
  ylim=c(-2.5, .5)
  xlab='% Cropland'
  ylab='Homicide Rate'


  plot(1,xlim=xlim,ylim=ylim,las=1,xlab=xlab,ylab=ylab, type='n', cex.axis=1.2, axes=FALSE)
  axis(1)
  axis(2, col = "black", col.ticks = "black")
  polygon(c(x,rev(x)),c(coef(mod)[1] +CI[1,2,],rev(coef(mod)[1] +CI[3,2,])),col=adjustcolor(color,alpha.f=0.4),border = NA)
  lines(x,coef(mod)[1] + CI[2,2,], col = color)
  hist.VHI = full_df$cropland
  # Histogram params
  dis = 0.055
  base = -2.49
  # Make histograms
  zz<-hist(hist.VHI,plot=F, breaks = 64)
  cts = zz$counts/max(zz$counts)/2
  rect(zz$breaks, base, zz$breaks+.005, base+cts, col=adjustcolor(color,alpha.f=0.6), border=FALSE)
}

pdf(file.path(figure_fpath, 'legend.pdf'), width = 5.5, height = 4.5)
plot(0,0, col = 'indianred3')
legend(0, 0, legend=c("Urban", "Rural"),
       col=c('indianred3', '#a050a3'), lty=1:2, cex=0.8)
dev.off()
pdf(file.path(figure_fpath, 'cropland_effect.pdf'), width = 5, height = 4.5)
plot_f(x = 0:64/100, xlim = c(0, .64))
dev.off()
