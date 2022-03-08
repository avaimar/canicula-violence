# 0. Working set up ---------------------------
library(basemaps)
library(ggmap)
library(sf)
library(ggplot2)
library(ggspatial)
library(scales)

#1. Set paths and parameters -----
load("CONFIG.Rspace")
raw_data_fpath <- file.path(gdrive_fpath, "Data", "Raw")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
figure_fpath <- file.path(gdrive_fpath,  "Figures")
#figure_fpath_local <- file.path("results", "figures")

# 2. Read in shapefiles-----

# dry corridor
dry_corridor <- sf::st_read(
  file.path(raw_data_fpath, "Shapefiles", "corredor_seco.geojson")
)

# 3. Plot Dry Corridor -----
bbox <- st_bbox(dry_corridor)
names(bbox) <- c('left', 'bottom', 'right', 'top')

options <- c('toner-hybrid', 'terrain-lines')
purp_pal <- brewer.pal(name = 'Purples', n = 9)
blue_pal <- brewer.pal(name = 'Blues', n = 9)

map <- ggmap::get_stamenmap(bbox=bbox, maptype='toner-lite', crop=FALSE, zoom=7)
ggmap(map)+ 
  geom_sf(data=dry_corridor, fill = muted(blue_pal[3]), color = NA, alpha = .5,  inherit.aes = FALSE) +
  coord_sf(datum = NA) +
  theme_void()
ggsave(file.path(figure_fpath, "dry_corridor.pdf"))
