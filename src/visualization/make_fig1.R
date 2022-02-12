library(sf)
library(tidyverse)

#1. Set paths and parameters -----
#gdrive data paths
gdrive_fpath <- file.path( ##change this to point at google drive path
  "/Volumes/GoogleDrive-112161833434429421879/My Drive/Project")
raw_data_fpath <- file.path(gdrive_fpath, "Data", "Raw")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
#repo data paths
#figure_fpath <- file.path(gdrive_fpath, "Figures")
figure_fpath <- file.path("results", "figures" )

#2. Read in Data -----
admin_bndry_sf <- st_read(
  file.path(processed_data_fpath, "cleaned_admin_boundaries.geojson"))
vhi_df <- read.csv(
  file.path(processed_data_fpath, "Canicula_index_2010_2020.csv"))
#convert canicula index to ordered factpr
vhi_df$Canicula_Label <- factor(vhi_df$Canicula_Label,
  levels = c("Extreme", "Severe", "Moderate", "Mild", "None"))
hom_df <- read.csv(file.path(processed_data_fpath, "homicide_rates.csv"))
hom_df <- hom_df[!duplicated(hom_df), ]

#3. Join Canicula Data, Homicide Data, and Shapefiles -----
canicula_sf <- left_join(
  admin_bndry_sf,
  vhi_df,
  by = c("shapeID", "Departamento", "Municipio", "shapeGroup"))

full_sf <- left_join(
  canicula_sf,
  hom_df,
  by = c("Departamento", "Municipio", "Year"))

y <- 2015
pdf(file.path(figure_fpath, paste("homicide_rate_ceil", y, "pdf", sep = ".")))
subset(full_sf, Year == y) %>%
  ggplot() +
    geom_sf(
      aes(fill = pmin(hom_rate_100k, 200)), lwd = 0.1) +
    scale_fill_viridis_c(option = "magma", na.value = "grey") +
    guides(fill = guide_colorbar(
      title = "Homicides per 100K",
      label.theme = element_text(colour = "gray10", angle = 0, size = 8),
      title.theme = element_text(colour = "gray10", angle = 0, size = 10))) +
    theme_void()
dev.off()


pdf(file.path(figure_fpath, paste("Canicula_Label", y, "pdf", sep = ".")))
subset(full_sf, Year == y) %>%
  ggplot() +
    geom_sf(
      aes(fill = Canicula_Label), lwd = 0) +
    scale_fill_viridis_d(option = "magma", begin = 1, end = 0) +
    guides(fill = guide_legend(
      title = "Canicula Intensity", 
      label.theme = element_text(colour = "gray10", angle = 0, size = 8),
      title.theme = element_text(colour = "gray10", angle = 0, size = 10))) +
    theme_void()
dev.off()