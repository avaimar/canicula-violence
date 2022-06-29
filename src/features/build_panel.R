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

# 2. Read in Data -----
admin_bndry_sf <- st_read(
  file.path(processed_data_fpath, "admin_bndry_cropland.geojson")
)
vhi_df <- read.csv(
  file.path(processed_data_fpath, "Canicula_index_1982_2020.csv")
)
precip_df <- read.csv(
  file.path(processed_data_fpath, "Precipitation_2013_2020.csv")
)
temp_df <- read.csv(
  file.path(processed_data_fpath, "Temperature_2000_2021.csv")
)

# convert canicula index to ordered factor
vhi_df$Canicula_Label <- factor(vhi_df$Canicula_Label,
                                levels = c("Extreme", "Severe", "Moderate", "Mild", "None")
)
hom_df <- read.csv(file.path(processed_data_fpath, "Homicides", "homicide_rates.csv"))

# 3. Build Clean Panel Data -----
full_sf <- build_clean_panel_sf(admin_bndry_sf, vhi_df, hom_df, panel_start_year = 2013)

# Add precipitation data
full_sf <- left_join(
  full_sf, 
  select(precip_df, Departamento, Municipio, Year, precip_canicular, precip_full), 
  by = c("Departamento", "Municipio", "Year")
)

# Add temperature data
full_sf <- left_join(
  full_sf, 
  select(temp_df, Departamento, Municipio, Year, temp_annual, temp_canicular), 
  by = c("Departamento", "Municipio", "Year")
)

# # # look at data coverage by country/year
# full_sf %>% st_drop_geometry() %>%
#   filter(!is.na(hom_rate_100k)) %>%
#   group_by(shapeGroup) %>%
#   summarise(
#     min_year = min(Year),
#     max_year = max(Year)
#   )
# # subset to panel period
# full_sf <- full_sf %>% filter(Year >= 2013) # period with data for >=3 countries
# Subset to Dry Corridor
full_sf <- filter(full_sf, in_dry_corridor == TRUE)

# Add areas to compute population densities
full_sf <- full_sf %>%
  sf::st_transform(full_sf, crs = proj_crs) %>%
  mutate(area = st_area(geometry)) %>%
  mutate(area = set_units(area, km^2))

# Compute population density
full_sf <- full_sf %>%
  mutate(
    pop_density = Population / area,
    urban = ifelse(pop_density > set_units(300, 1 / km^2), 1, 0)
  )

# Keep density from 2013 for heterogeneous effects
full_sf <- full_sf %>%
  group_by(Departamento, Municipio) %>%
  arrange(Year) %>%
  mutate(urban_2013 = dplyr::first(urban))

# 4. Prepare data for FE Model -----
full_df <- st_drop_geometry(full_sf)

# 5. Save as CSV -------------------
write.csv(
  full_df, 
  file.path(processed_data_fpath, "panel_2013_2020.csv"))
