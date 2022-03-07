# 0. Working set up ---------------------------
library(raster)
library(sf)
library(sp)
library(exactextractr)
library(tidyverse)
library(RColorBrewer)
library(zoo)

#1. Set paths and parameters -----
load("CONFIG.Rspace") # load gdrive data path
raw_data_fpath <- file.path(gdrive_fpath, "Data", "Raw")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
data_raw_CHIRPS_fpath <- file.path("data_raw_local", "CHIRPS")
# projection CRS globals
proj_crs <- 26716
latlon_crs <- 4326

year_range <- c(2010:2020)
pentad_range <- c(1:72)

# 2. Read in municipality/department shapefile  ----
admin_bndry_sf <- st_read(
  file.path(processed_data_fpath, "cleaned_admin_boundaries.geojson")
)


filter(admin_bndry_sf, in_dry_corridor == T)

# 3.  Read In  & Preprocess CHIRPS Raster Data -----
chirps <- stack(
  list.files(data_raw_CHIRPS_fpath, pattern = ".bil$", full.names = T, recursive = T)
) %>%
  crop(extent(filter(admin_bndry_sf, in_dry_corridor == T)))
chirps <- reclassify(chirps, cbind(-9999, NA)) # map -9999 -> NULL

prcp_pentad_year <- data.frame(rowMeans(t(getValues(chirps)), na.rm = TRUE))
names(prcp_pentad_year) <- c("prcp")
prcp_pentad_year$pentad <- rep(seq(length(pentad_range)), length(year_range))
prcp_pentad_year$year <- rep(year_range, each = length(pentad_range))
head(prcp_pentad_year)

prcp_pentad_year$week <- round(prcp_pentad_year$pentad * 5 / 7)

blue <- brewer.pal(9, name = "Blues")[9]

p <- prcp_pentad_year %>%
  filter(year > 2012) %>%
  ggplot(
    mapping = aes(x = week, y = prcp), color = blue, size = 1
  ) +
  stat_smooth(
    method = "loess", span = 0.1, se = TRUE,
    alpha = 0.2, color = blue, fill = blue
  ) +
  annotate("rect",
    xmin = 26, xmax = 36, ymin = -Inf, ymax = Inf,
    fill = "gray60", alpha = .3
  )
p <- p + labs(x = "Week", y = "Precipitation (mm)")
p + theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
ggsave(file.path(figure_fpath, "prcp_canicula_2012_2020.pdf"), height = 3, width = 5)