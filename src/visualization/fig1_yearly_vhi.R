# 0. Working set up ---------------------------
library(raster)
library(sf)
library(exactextractr)
library(tidyverse)
library(RColorBrewer)
library(cartography)
library(scales)

#1. Set paths and parameters -----
load("CONFIG.Rspace") # load gdrive data path
raw_data_fpath <- file.path(gdrive_fpath, "Data", "Raw")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")

figure_fpath <- file.path("results", "figures")

# repo data paths
data_raw_VHI_fpath <- file.path("data_raw_local", "VHI_all_years")
figure_fpath <- file.path("results", "figures")
year_range <- c(2011:2020)
week_range <- c(1:52)
proj_crs <- 26716
latlon_crs <- 4326

# 2. Read in municipality/department shapefile  ----
admin_bndry_sf <- st_read(
  file.path(processed_data_fpath, "cleaned_admin_boundaries.geojson")
)

country_sf <- st_read(
  file.path(processed_data_fpath, "country_outlines.geojson")
)

# 3.  Preprocess VHI Raster Data -----
# read in vhi data during year/week_range and crop to 4 countries of interest
vhi <- stack(
  list.files(data_raw_VHI_fpath, pattern = ".tif$", full.names = T)
) %>% crop(extent(filter(admin_bndry_sf, in_dry_corridor == T)))
nlayers(vhi)
vhi <- reclassify(vhi, cbind(-9999, NA)) # map -9999 to NULL
# for each year, get mean value for each gridcell over week_range

vhi_week_year <- data.frame(rowMeans(t(getValues(vhi)), na.rm = TRUE))
names(vhi_week_year) <- c("vhi")
vhi_week_year$week <- rep(seq(length(week_range)), length(year_range))
vhi_week_year$year <- rep(year_range, each = length(week_range))
head(vhi_week_year)


getPalette <- colorRampPalette(brewer.pal(9, name = "YlGnBu"))
pal <- carto.pal(pal1 = "turquoise.pal", n1 = 8)

vhi_week_year %>%
  filter(year > 2012) %>%
  ggplot(
    aes(x = week, y = 100 - vhi, group = factor(year))
  ) +
  geom_line(aes(color = factor(year))) +
  scale_color_manual(values = pal) +
  theme_minimal()

getPalette <- colorRampPalette(brewer.pal(9, name = "Blues"))


p <- vhi_week_year %>%
  mutate(l = year %in% c(2015, 2017)) %>%
  filter(year > 2012) %>%
  ggplot() +
  annotate("rect",
    xmin = 26, xmax = 36, ymin = -Inf, ymax = Inf,
    fill = "gray60", alpha = .3
  ) +
  geom_line(aes(x = week, y = vhi, group = factor(year), color = factor(year)), linetype = 3, size = .8) +
  scale_color_manual(values = pal, name = "Year")
p <- p + labs(x = "Week", y = "VHI")
p + theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

ggsave(file.path(figure_fpath, "vhi_yearly.pdf"), height = 3, width = 6)