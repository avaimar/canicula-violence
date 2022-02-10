library(raster)
library(sf)
library(sp)
library(exactextractr)
library(tidyverse)
library(RColorBrewer)

# 1. Set paths and parameters -----
# gdrive data paths
gdrive_fpath <- file.path( ## change this to point at google drive path
  "/Volumes/GoogleDrive-112161833434429421879/My Drive/Project"
)

raw_data_fpath <- file.path(gdrive_fpath, "Data", "Raw")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
data_raw_CHIRPS_fpath <- file.path("data_raw_local", "CHIRPS")
# projection CRS globals
proj_crs <- 26716
latlon_crs <- 4326

# 2. Read in municipality/department shapefile  ----
admin_bndry_sf <- st_read(
  file.path(processed_data_fpath, "cleaned_admin_boundaries.geojson")
)

#make country shapefile
#@Ben: modify this if you want to plot time series at diff spatial agg
country_sf <- admin_bndry_sf %>%
  sf::st_transform(admin_bndry_sf, crs = proj_crs) %>%
  group_by(shapeGroup) %>%
  summarize(geometry = st_union(geometry)) %>%
  sf::st_transform(admin_bndry_sf, crs = latlon_crs)

# 3.  Read In  & Preprocess CHIRPS Raster Data -----
# read in chirps data from .bil files (NOT THE .hdr file!!) 
#a nd crop to our area of interest
chirps <- stack(
  list.files(data_raw_CHIRPS_fpath, pattern = ".bil$", full.names = T, recursive = T)) %>%
  crop(extent(country_sf))
chirps <- reclassify(chirps, cbind(-9999, NA)) # map -9999 -> NULL

nlayers(chirps) # number of 5 day intervals you have
plot(chirps[[1:2]]) # visualize first two image to make sure it looks right

# get mean precip over country for each pentad-year
chirps_country <- exact_extract(chirps, country_sf, fun = "mean")
chirps_country_sf <- st_sf(cbind(country_sf, chirps_country))
# reshape from wide to long format
chirps_country_sf <- gather(
  chirps_country_sf, layer_str, mean_precip, 
  (ncol(country_sf) + 1):ncol(chirps_country_sf) - 1)

#Get year and pentad num
# layer strings are coded like "mean.v2p0chirps201001" 
n_chars <- nchar(chirps_country_sf$layer_str[1])
# extract portion of string containing the year
chirps_country_sf$year <- as.numeric(
  substr(chirps_country_sf$layer_str, n_chars - 5, n_chars - 2))
# extract portion of string containing the pentad
chirps_country_sf$pentad <- as.numeric(
  substr(chirps_country_sf$layer_str, n_chars - 1, n_chars))
#drop layer string col
chirps_country_sf <- chirps_country_sf%>% select(-layer_str)

head(chirps_country_sf)

# 5.  Get yearly time series -----
# plot time series for
st_drop_geometry(chirps_country_sf) %>%
  mutate(country.year = paste(shapeGroup, year, sep = ".")) %>%
  ggplot(
    aes(x = pentad, y = mean_precip,group = country.year,
    colour = year, linetype = shapeGroup)) +
  geom_line()

