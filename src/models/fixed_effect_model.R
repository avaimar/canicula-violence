# 0. Working set up ---------------------------
library(raster)
library(sf)
library(sp)
library(exactextractr)
library(tidyverse)
library(RColorBrewer)
library(fixest)
library(units)

# 1. Set paths and parameters -----
load("CONFIG.Rspace") # gdrive data paths
raw_data_fpath <- file.path(gdrive_fpath, "Data", "Raw")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
model_data_fpath <- file.path(gdrive_fpath, "Models")
# repo data paths
# figure_fpath <- file.path(gdrive_fpath, "Figures")
figure_fpath <- file.path("results", "figures")

source(file.path("src", "models", "panel_preprocessing_funcs.R"))

# CRS
proj_crs <- 26716
latlon_crs <- 4326

# 2. Read in panel -----
full_df <- read.csv(
  file.path(processed_data_fpath, "panel_2013_2020.csv")
)

# 3. Main model (used in first draft) -----

# run FE model with municipality FE (significant effects)
m1.vhi <- feols(hom_rate_100k ~ mean_vhi | munic_dep + Year, data = full_df)
m2.CI <- feols(hom_rate_100k ~ as.factor(Canicula_Index) | munic_dep + Year, data = full_df)

# Pick model for visualization purposes
selected.MM.rhs <- model.matrix(m1.vhi, type = "rhs")
selected.MM.lhs <- model.matrix(m1.vhi, type = "lhs")
selected.MM.fixef <- model.matrix(m1.vhi, type = "fixef")
save(full_sf, selected.MM.rhs, selected.MM.lhs, selected.MM.fixef, m1.vhi,
  file = file.path(model_data_fpath, "modelmatrix.RData")
)

# 4. Spatial unit fixed effects ----------
# no significant effects using department or country FE or country*year department*year FE
feols(hom_rate_100k ~ mean_vhi | Departamento + Year, data = full_df)
feols(hom_rate_100k ~ as.factor(Canicula_Index) | Departamento + Year, data = full_df)
feols(hom_rate_100k ~ mean_vhi | shapeGroup + Year, data = full_df)
feols(hom_rate_100k ~ as.factor(Canicula_Index) | shapeGroup + Year, data = full_df)
feols(hom_rate_100k ~ mean_vhi | shapeGroup[Year] + Year, data = full_df)
feols(hom_rate_100k ~ as.factor(Canicula_Index) | shapeGroup[Year] + Year, data = full_df)
feols(hom_rate_100k ~ mean_vhi | Departamento[Year] + Year, data = full_df)
feols(hom_rate_100k ~ as.factor(Canicula_Index) | Departamento[Year] + Year, data = full_df)

# 5. Lagged models ----------------
## FE with lags + leads... not sure how to interpret these
feols(hom_rate_100k ~ l(mean_vhi, 0:2) | munic_dep + Year, data = full_df, panel.id = ~ munic_dep + Year)
feols(hom_rate_100k ~ l(Canicula_Index, 0:2) | munic_dep + Year, data = full_df, panel.id = ~ munic_dep + Year)
feols(hom_rate_100k ~ f(mean_vhi, 0:2) | munic_dep + Year, data = full_df, panel.id = ~ munic_dep + Year)
feols(hom_rate_100k ~ f(Canicula_Index, 0:2) | munic_dep + Year, data = full_df, panel.id = ~ munic_dep + Year)

# one period lag - less strong/significant of an effect
feols(hom_rate_100k ~ l(mean_vhi, 1) | munic_dep + Year, data = full_df, panel.id = ~ munic_dep + Year)

# 6. Different Functional forms --------------
m.quad <- feols(hom_rate_100k ~ poly(mean_vhi, 2) | munic_dep + Year, data = full_df, subset = !is.na(full_df$mean_vhi))
m.log <- feols(hom_rate_100k ~ log(mean_vhi) | munic_dep + Year, data = full_df)

# 7. Some Robustness Checks -----
# number of years of data per munic-dep
# full_df %>%
#   filter(!is.na(hom_rate_100k)) %>%
#   group_by(munic_dep) %>%
#   summarise(n_year = n()) %>%
#   group_by(n_year) %>%
#   summarise(n_munic = n())

# full_df <- full_df %>%
#   filter(!is.na(hom_rate_100k)) %>%
#   group_by(munic_dep) %>%
#   mutate(n_year = n())

# most municipalities have at least 6 years of data, remove those 19 that do not
full_df_test <- full_df %>% filter(n_year > 5)

# #no major change when removing these
feols(hom_rate_100k ~ mean_vhi | munic_dep + Year, data = full_df_test)
feols(hom_rate_100k ~ as.factor(Canicula_Index) | munic_dep + Year, data = full_df_test)
feols(hom_rate_100k ~ mean_vhi | Departamento + Year, data = full_df_test)
feols(hom_rate_100k ~ as.factor(Canicula_Index) | Departamento + Year, data = full_df_test)

# nicaragua spatial units at lower spatial res
full_df_test <- full_df %>% filter(Country == "Nicaragua")

# #no major change when removing these
feols(hom_rate_100k ~ mean_vhi | munic_dep + Year, data = full_df_test)
feols(hom_rate_100k ~ as.factor(Canicula_Index) | munic_dep + Year, data = full_df_test)
feols(hom_rate_100k ~ mean_vhi | Departamento + Year, data = full_df_test)
feols(hom_rate_100k ~ as.factor(Canicula_Index) | Departamento + Year, data = full_df_test)


# 8. Heterogeneous effects -------------------------------------
# * Population density
mod.pop_density <- feols(hom_rate_100k ~ mean_vhi * urban_2013 | munic_dep + Year, data = full_df)
mod.urban <- feols(hom_rate_100k ~ mean_vhi | munic_dep + Year, data = full_df, subset = full_df$urban == 1)
mod.rural <- feols(hom_rate_100k ~ mean_vhi | munic_dep + Year, data = full_df, subset = full_df$urban == 0)

mod.pop_density.ag <- feols(hom_rate_100k ~ mean_vhi + mean_vhi:urban_2013:cropland | munic_dep + Year, data = full_df)

# * Cropland
mod.crop <- feols(hom_rate_100k ~ mean_vhi * cropland | munic_dep + Year, data = full_df)
mod.grass <- feols(hom_rate_100k ~ mean_vhi * grassland | munic_dep + Year, data = full_df)

# * Cropland vs Pop Density
mod.crop.urban <- feols(hom_rate_100k ~ mean_vhi * cropland | munic_dep + Year, data = full_df, subset = full_df$urban == 1)
mod.crop.rural <- feols(hom_rate_100k ~ mean_vhi * cropland | munic_dep + Year, data = full_df, subset = full_df$urban == 0)

# * Country
mod.GTM <- feols(hom_rate_100k ~ mean_vhi | Departamento + Year, data = full_df, subset = full_df$Country == "Guatemala")
mod.ELS <- feols(hom_rate_100k ~ mean_vhi | Departamento + Year, data = full_df, subset = full_df$Country == "El Salvador")
mod.HOND <- feols(hom_rate_100k ~ mean_vhi | Departamento + Year, data = full_df, subset = full_df$Country == "Honduras")
mod.NIC <- feols(hom_rate_100k ~ mean_vhi | Departamento + Year, data = full_df, subset = full_df$Country == "Nicaragua")

# 9. Precipitation / Temperature ----------------
# Canicular precipitation
feols(hom_rate_100k ~ precip_canicular | munic_dep + Year, data = full_df)

# Annual precipitation
feols(hom_rate_100k ~ precip_full | munic_dep + Year, data = full_df)

# Canicular temperature

# Annual temperature
