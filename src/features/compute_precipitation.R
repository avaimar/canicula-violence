# 0. Working set up ---------------------------
library(raster)
library(sf)
library(sp)
library(exactextractr)
library(tidyverse)
library(RColorBrewer)
library(zoo)
library(stringr)
library(reshape2)

#1. Set paths and parameters -----
load("CONFIG.Rspace") # load gdrive data path
raw_data_fpath <- file.path(gdrive_fpath, "Data", "Raw")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
data_raw_CHIRPS_fpath <- file.path("Data", "Raw")
# projection CRS globals
proj_crs <- 26716
latlon_crs <- 4326

year_range <- c(2013:2020)
pentad_full_range <- c(1:72)
pentad_canicular_range <- c(38, 49)
#canicula_pentad_range <- round(c((27*7/5),(35*7/5)), 0)

# 2. Read in municipality/department shapefile  ----
admin_bndry_sf <- st_read(
  file.path(processed_data_fpath, "cleaned_admin_boundaries.geojson")
)

# 3.  Read In  & Preprocess CHIRPS Raster Data -----
# Chirps for the full year
chirps_full <- stack(
  list.files(file.path(data_raw_CHIRPS_fpath, 'CHIRPS_FULL'), 
             pattern = ".bil$", full.names = T, recursive = T)
) %>%
  crop(extent(filter(admin_bndry_sf, in_dry_corridor == T)))
chirps_full <- reclassify(chirps_full, cbind(-9999, NA)) # map -9999 -> NULL

# Chirps for the canicular period of each year
chirps_canicular <- stack(
  list.files(file.path(data_raw_CHIRPS_fpath, 'CHIRPS_CANICULAR'), 
             pattern = ".bil$", full.names = T, recursive = T)
) %>%
  crop(extent(filter(admin_bndry_sf, in_dry_corridor == T)))
chirps_canicular <- reclassify(chirps_canicular, cbind(-9999, NA)) # map -9999 -> NULL

# For each year, get mean value for each gridcell over pentad_range
chirps_full_mean_by_year <- stackApply(
  chirps_full,
  indices =  rep(seq(length(year_range)), each = length(pentad_full_range)),
  fun = "mean", na.rm = T)

chirps_canicular_mean_by_year <- stackApply(
  chirps_canicular,
  indices =  rep(seq(length(year_range)), each = length(pentad_canicular_range)),
  fun = "mean", na.rm = T)

names(chirps_full_mean_by_year) <-  paste("precip_full", year_range, sep = ".")
names(chirps_canicular_mean_by_year) <-  paste("precip_canicular", year_range, sep = ".")

plot(chirps_full_mean_by_year$precip.2018) #make sure this looks right

#4. Compute Average year precipitation over spatial units----
# get spatial average
chirps_full_admin_bndry <- exact_extract(
  chirps_full_mean_by_year, admin_bndry_sf, fun = "mean")

chirps_canicular_admin_bndry <- exact_extract(
  chirps_canicular_mean_by_year, admin_bndry_sf, fun = "mean")

# Join full and canicular averages
chirps_admin_bndry <- cbind(chirps_full_admin_bndry, chirps_canicular_admin_bndry)

admin_bndry_chirps_sf <- st_sf(cbind(admin_bndry_sf, chirps_admin_bndry))

#reshape from wide to long format (precipitation by year stored in last cols other than geom col)
admin_bndry_chirps_sf <- gather(
  admin_bndry_chirps_sf, Measure_type, precip, (ncol(admin_bndry_sf)+1):ncol(admin_bndry_chirps_sf) - 1)

#extract portion of string containing the year and pricipitation type
admin_bndry_chirps_sf$Year <- as.numeric(str_sub(admin_bndry_chirps_sf$Measure_type,-4,-1))
precip_type <- str_split_fixed(admin_bndry_chirps_sf$Measure_type, '[.]', n=3)
admin_bndry_chirps_sf$Precip_period <- precip_type[,2]


head(admin_bndry_chirps_sf)
plot(filter(admin_bndry_chirps_sf, Year == 2018 & Precip_period == 'precip_full')['precip'])

# Reshape
admin_bndry_chirps_sf <- st_drop_geometry(select(admin_bndry_chirps_sf, -c(in_dry_corridor)))
admin_bndry_chirps_sf <- dcast(
  data=admin_bndry_chirps_sf, 
  formula=shapeID+shapeGroup+Departamento+Municipio+Year~Precip_period, 
  value.var='precip')

#5. Write out CSV ----
write.csv(
  admin_bndry_chirps_sf, 
  file.path(processed_data_fpath, "Precipitation_2013_2020.csv"))
