library(raster)
library(sf)
library(sp)
library(exactextractr)
library(tidyverse)
library(RColorBrewer)
library(fixest)
library(units)

#1. Set paths and parameters -----
#gdrive data paths
load('CONFIG.Rspace')
raw_data_fpath <- file.path(gdrive_fpath, "Data", "Raw")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
model_data_fpath <- file.path(gdrive_fpath, "Models")
#repo data paths
#figure_fpath <- file.path(gdrive_fpath, "Figures")
figure_fpath <- file.path("results", "figures")

# CRS
proj_crs <- 26716
latlon_crs <- 4326

#2. Read in Data -----
admin_bndry_sf <- st_read(
  file.path(processed_data_fpath, "admin_bndry_cropland.geojson"))
vhi_df <- read.csv(
  file.path(processed_data_fpath, "Canicula_index_2010_2020.csv"))
#convert canicula index to ordered factor
vhi_df$Canicula_Label <- factor(vhi_df$Canicula_Label,
  levels = c("Extreme", "Severe", "Moderate", "Mild", "None"))
hom_df <- read.csv(file.path(processed_data_fpath, "homicide_rates.csv"))

#3. Join Canicula Data, Homicide Data, and Shapefiles -----
canicula_sf <- left_join(
  admin_bndry_sf,
  vhi_df,
  by = c("shapeID", "Departamento", "Municipio", "shapeGroup"))

full_sf <- left_join(
  canicula_sf,
  hom_df,
  by = c("Departamento", "Municipio", "Year"))

# Add areas to compute population densities
full_sf <- full_sf %>%
  sf::st_transform(full_sf, crs = proj_crs) %>%
  mutate(area = st_area(geometry))  %>%
  mutate(area = set_units(area, km^2))

# Compute population density
full_sf <- full_sf %>%
  mutate(pop_density = Population / area, 
         urban = ifelse(pop_density > set_units(300, 1/km^2), 1, 0))

# Keep density from 2013 for heterogeneous effects
full_sf <- full_sf %>%
  group_by(Departamento, Municipio) %>%
  arrange(Year) %>%
  mutate(urban_2013 = dplyr::first(urban))

#4. Prepare data for FE Model -----
#transform variables
#make vhi on scale from 1-100 to make coefs easier to interpret
full_sf$mean_vhi = full_sf$mean_vhi * 100
#make unique identifier for each spatial unit as Municipality_Department
full_sf$munic_dep <- paste(full_sf$Municipio, full_sf$Departamento, sep = "_")

#get non-spatial DF so that we can determine duplicates
#without considering geometry
full_df <- st_drop_geometry(full_sf)

#get the 3 munic-departments that are non-unique due to 
# Null Municipality or weird multiple shapeID issue
#TODO:for now we will drop these, but need to investigate these later
nonunique_munic_dept <- full_df[duplicated(full_df[, c("munic_dep", "Year")]),] %>%
  select("munic_dep") %>%
  unique() %>%
  unlist()
full_df <- full_df %>% filter(!(munic_dep %in% nonunique_munic_dept))

# Subset to Dry Corridor
full_df <- filter(full_df, in_dry_corridor==TRUE)

#5. Run FE Model -----

#look at data coverage by country/year
full_df %>% filter(!is.na(hom_rate_100k)) %>%
  group_by(shapeGroup) %>%
  summarise(min_year = min(Year),
            max_year = max(Year))

#We only have data for >= 3 countries for 2013-2020 so subset to this period
# full_df <-full_df %>% filter(Year >= 2011) #period with data for >=2 countries
full_df <- full_df %>% filter(Year >= 2013) #period with data for >=3 countries
#full_df <- full_df %>% filter(Year >= 2014 & Year <= 2019) #period with data for all countries
#full_df <-full_df %>% filter(shapeGroup != "NIC") # subset to countries with munic-level data 

#run FE model with municipality FE (significant effects)
m1.vhi <- feols(hom_rate_100k ~ mean_vhi |  munic_dep + Year, data = full_df)
m2.CI <- feols(hom_rate_100k ~ as.factor(Canicula_Index) | munic_dep + Year, data = full_df)

# Pick model for visualization purposes
selected.MM.rhs <- model.matrix(m1.vhi, type='rhs')
selected.MM.lhs <- model.matrix(m1.vhi, type='lhs')
selected.MM.fixef <- model.matrix(m1.vhi, type='fixef')
save(full_df, selected.MM.rhs, selected.MM.lhs, selected.MM.fixef, m1.vhi, 
     file = file.path(model_data_fpath, "modelmatrix.RData"))

#no significant effects using department or country FE or country*year department*year FE
feols(hom_rate_100k ~ mean_vhi |  Departamento + Year, data = full_df)
feols(hom_rate_100k ~ as.factor(Canicula_Index) |  Departamento + Year, data = full_df)
feols(hom_rate_100k ~ mean_vhi | shapeGroup + Year, data = full_df)
feols(hom_rate_100k ~ as.factor(Canicula_Index) |  shapeGroup + Year, data = full_df)
feols(hom_rate_100k ~ mean_vhi | shapeGroup[Year] + Year, data = full_df)
feols(hom_rate_100k ~ as.factor(Canicula_Index) | shapeGroup[Year] + Year, data = full_df)
feols(hom_rate_100k ~ mean_vhi |  Departamento[Year] + Year, data = full_df)
feols(hom_rate_100k ~ as.factor(Canicula_Index) |  Departamento[Year] + Year, data = full_df)


#number of years of data per munic-dep
full_df %>% filter(!is.na(hom_rate_100k)) %>%
  group_by(munic_dep) %>%
  summarise(n_year = n()) %>% 
  group_by(n_year)%>%
  summarise(n_munic = n())

full_df <- full_df %>% filter(!is.na(hom_rate_100k)) %>%
  group_by(munic_dep) %>%
  mutate(n_year = n())

#most municipalities have at least 6 years of data, remove those 19 that do not
full_df <- full_df %>% filter(n_year > 5)

#no major change when removing these
feols(hom_rate_100k ~ mean_vhi | munic_dep + Year, data = full_df)
feols(hom_rate_100k ~ as.factor(Canicula_Index) | munic_dep + Year, data = full_df)
feols(hom_rate_100k ~ mean_vhi |  Departamento + Year, data = full_df)
feols(hom_rate_100k ~ as.factor(Canicula_Index) |  Departamento + Year, data = full_df)

##FE with lags + leads... not sure how to interpret these
feols(hom_rate_100k ~ l(mean_vhi, 0:2) | munic_dep + Year, data = full_df, panel.id = ~munic_dep + Year )
feols(hom_rate_100k ~ l(Canicula_Index, 0:2) | munic_dep + Year, data = full_df, panel.id = ~munic_dep + Year )
feols(hom_rate_100k ~ f(mean_vhi, 0:2)|  munic_dep + Year, data = full_df, panel.id = ~munic_dep + Year )
feols(hom_rate_100k ~ f(Canicula_Index, 0:2)| munic_dep + Year, data = full_df, panel.id = ~munic_dep + Year )

# Functional form
m.quad <- feols(hom_rate_100k ~ poly(mean_vhi,2) |  munic_dep + Year, data = full_df, subset=!is.na(full_df$mean_vhi))
m.log <- feols(hom_rate_100k ~ log(mean_vhi) |  munic_dep + Year, data = full_df)

# 6. Heterogeneous effects -------------------------------------
# * Population density
mod.pop_density <- feols(hom_rate_100k ~ mean_vhi*urban_2013 |  munic_dep + Year, data = full_df)
mod.urban <- feols(hom_rate_100k ~ mean_vhi |  munic_dep + Year, data = full_df, subset=full_df$urban==1)
mod.rural <- feols(hom_rate_100k ~ mean_vhi |  munic_dep + Year, data = full_df, subset=full_df$urban==0)

mod.pop_density.ag <- feols(hom_rate_100k ~ mean_vhi + mean_vhi : urban_2013: cropland |  munic_dep + Year, data = full_df)

# * Cropland 
mod.crop <- feols(hom_rate_100k ~ mean_vhi*cropland |  munic_dep + Year, data = full_df)
mod.grass <- feols(hom_rate_100k ~ mean_vhi*grassland |  munic_dep + Year, data = full_df)

# * Cropland vs Pop Density
mod.crop.urban <- feols(hom_rate_100k ~ mean_vhi*cropland |  munic_dep + Year, data = full_df, subset=full_df$urban==1)
mod.crop.rural <- feols(hom_rate_100k ~ mean_vhi*cropland |  munic_dep + Year, data = full_df, subset=full_df$urban==0)

# * Country
mod.GTM <- feols(hom_rate_100k ~ mean_vhi |  Departamento + Year, data = full_df, subset=full_df$Country=='Guatemala')
mod.ELS <- feols(hom_rate_100k ~ mean_vhi |  Departamento + Year, data = full_df, subset=full_df$Country=='El Salvador')
mod.HOND <- feols(hom_rate_100k ~ mean_vhi |  Departamento + Year, data = full_df, subset=full_df$Country=='Honduras')
mod.NIC <- feols(hom_rate_100k ~ mean_vhi |  Departamento + Year, data = full_df, subset=full_df$Country=='Nicaragua')

# 7. Create Figure 2 Plots ------------------------------------
# Helper functions to bootstrap and visualize models
bootstrap_model <- function(formula_eq, data, K, x_center) {
  x <- 0:100
  
  boots <- matrix(nrow=K,ncol=length(x))
  ll = dim(data)[1]
  for (k in 1:K) {
    if (k %% 10 == 0) print(i)
    
    # Fit model on bootstrapped dataset
    samp <- sample(1:ll, size=ll,replace=T)
    newdata = data[samp,]
    mod <- feols(formula(formula_eq), data = newdata)
    
    # Predict and save
    y <- x*coef(mod)
    y <- y - y[x=x_center]
    boots[k,] <- y
  }
  
  confint <- apply(boots,2,function(x) quantile(x,probs=c(0.05,0.5,0.95)))
  confint
}

plot_model <- function(confint, xlab, ylab, new_plot, color, l_histogram, ylim) {
  x=0:100
  
  if (new_plot == TRUE){
    x_llim <- ifelse(l_histogram==TRUE, -15, 0)
    plot(1,xlim=c(x_llim,100),ylim=ylim,las=1,xlab=xlab,ylab=ylab, type='n', cex.axis=1.2, axes=FALSE)
    axis(1)
    axis(2, col = "black", col.ticks = "black")
  }
  polygon(c(x,rev(x)),c(confint[1,],rev(confint[3,])),col=color,border = NA)
  lines(x,confint[2,])
}

visualize_model <- function(formula_eq, data, K, l_histogram, b_histogram, x_center, new_plot, color, ylim) {
  CI <- bootstrap_model(formula_eq, data, K, x_center)
  
  plot_model(CI, xlab='VHI', ylab='Homicide Rate', new_plot, color, l_histogram, ylim)
  
  if (b_histogram==TRUE){
    bins = seq(0,100,1)
    hist.VHI = data$mean_vhi
    
    # Histogram params
    dis = 0.055
    base = -50
    
    # Make histograms
    zz <- hist(hist.VHI,plot=F,breaks=bins)
    cts = zz$counts/max(zz$counts)*15
    rect(bins, base, bins+0.5, base+cts, col="red")
  }
  
  if(l_histogram==TRUE){
    bins = seq(0, 700, 2)
    hist.hom = data$hom_rate_100k
    
    # Histogram params
    dis = 0.055
    base= -15
    
    # Make histograms
    yy <- hist(hist.hom, plot=F,breaks=bins)
    cts = yy$counts/max(yy$counts)*15
    rect(xleft=base,ybottom=bins-0.1, xright=base+cts,ytop=bins,col="red")
  }
}

X_CENTER <- 42

visualize_model(
  formula_eq='hom_rate_100k ~ mean_vhi |  munic_dep + Year', 
  data=full_df, K=100, l_histogram=F, b_histogram=T, x_center=X_CENTER, new_plot=TRUE, 
  color='azure2', ylim=c(-50, 50))

visualize_model(
  formula_eq='hom_rate_100k ~ mean_vhi |  munic_dep + Year', 
  data=full_df[full_df$urban==1,], K=100, l_histogram=F, b_histogram=F, x_center=X_CENTER, 
  new_plot=TRUE, color='indianred3', ylim=c(-80, 70))

visualize_model(
  formula_eq='hom_rate_100k ~ mean_vhi |  munic_dep + Year', 
  data=full_df[full_df$urban==0,], K=100, l_histogram=F, b_histogram=F, x_center=X_CENTER,
  new_plot=FALSE, color='azure2', ylim=c(-80, 70))


# References:
# Urbanization: https://blogs.worldbank.org/sustainablecities/how-do-we-define-cities-towns-and-rural-areas
# Burke, Hsiang, Miguel (2015)
