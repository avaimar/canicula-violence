# 0. Working set up ---------------------------
library(raster)
library(sf)
library(exactextractr)
library(tidyverse)
library(exactextractr)

# 1. Set paths and parameters -----
load("CONFIG.Rspace") # load gdrive_fpath
raster_data_fpath <- file.path(gdrive_fpath, "Data", "Raw", "Rasters")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
# repo data paths
data_raw_VHI_fpath <- file.path("data_raw_local", "VHI")
figure_fpath <- file.path("results", "figures")

proj_crs <- 26716
latlon_crs <- 4326

# 2. Read in spatial unit shapefiles & crop/grassland rasters ----
admin_bndry_sf <- st_read(
  file.path(processed_data_fpath, "cleaned_admin_boundaries.geojson")
)

# Downloaded from: https://data.review.fao.org/map/catalog/srv/api/records/ba4526fd-cdbf-4028-a1bd-5a559c4bff38
cropland <- raster(file.path(raster_data_fpath, "GlcShare_v10_02", "glc_shv10_02.Tif")) %>%
  crop(extent(admin_bndry_sf))
grassland <- raster(file.path(raster_data_fpath, "GlcShare_v10_03", "glc_shv10_03.Tif")) %>%
  crop(extent(admin_bndry_sf))

# sum(is.na(admin_bndry_sf$cropland))
# plot(cropland)
# plot(grassland)

# 3. Compute Average cropland and grassland over spatial units----
admin_bndry_sf$cropland <- exact_extract(
  cropland, admin_bndry_sf,
  fun = "mean"
) / 100
admin_bndry_sf$grassland <- exact_extract(
  grassland, admin_bndry_sf,
  fun = "mean"
) / 100

# plot(admin_bndry_sf['cropland'])
# plot(admin_bndry_sf['grassland'])


# 3. Write out shapefile with ag/grassland percent columns----
st_write(admin_bndry_sf,
  file.path(processed_data_fpath, "admin_bndry_cropland.geojson"),
  delete_dsn = TRUE
)