# 0. Working set up ---------------------------
library(basemaps)
library(sf)
library(ggplot2)
library(ggspatial)
library(scales)

#1. Set paths and parameters -----
load("CONFIG.Rspace")
raw_data_fpath <- file.path(gdrive_fpath, "Data", "Raw")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
# figure_fpath <- file.path(gdrive_fpath,  "Figures")
figure_fpath_local <- file.path("results", "figures")

# 2. Read in shapefiles-----

# dry corridor
dry_corridor <- sf::st_read(
  file.path(raw_data_fpath, "Shapefiles", "corredor_seco.geojson")
)

# 3. Plot Dry Corridor -----

p <- ggplot(dry_corridor) +
  annotation_map_tile(zoom = 7, type = "hotstyle") +
  geom_sf(fill = muted("blue"), color = NA, alpha = .5) +
  coord_sf(datum = NA) +
  theme_void()

ggsave(file.path(figure_fpath_local, "dry_corridor.pdf"))