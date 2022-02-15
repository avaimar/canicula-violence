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
country_sf <- st_read(
  file.path(processed_data_fpath, "country_outlines.geojson")
)
admin_bndry_sf <- st_read(
  file.path(processed_data_fpath, "cleaned_admin_boundaries.geojson"))
vhi_df <- read.csv(
  file.path(processed_data_fpath, "Canicula_index_2010_2020.csv"))
#convert canicula index to ordered factor
vhi_df$Canicula_Label <- factor(vhi_df$Canicula_Label,
  levels = c("Extreme", "Severe", "Moderate", "Mild", "None"))
hom_df <- read.csv(file.path(processed_data_fpath, "homicide_rates.csv"))
# sum(duplicated(hom_df))

#3. Join Canicula Data, Homicide Data, and Shapefiles -----
canicula_sf <- left_join(
  admin_bndry_sf,
  vhi_df,
  by = c("shapeID", "Departamento", "Municipio", "shapeGroup"))

full_sf <- left_join(
  canicula_sf,
  hom_df,
  by = c("Departamento", "Municipio", "Year"))


y <- 2015 #choosing 2015 to display
#Homicide rate map (with max val of 200)
pdf(file.path(figure_fpath, paste("Homicide_Rate_Ceil", y, "pdf", sep = ".")))
ggplot() +
  geom_sf(data = subset(full_sf, Year == y),
    aes(fill = pmin(hom_rate_100k, 200)), lwd = 0.1) +
  geom_sf(data = country_sf, fill = NA, lwd = 0.1, color = "white") +
  scale_fill_viridis_c(option = "magma", na.value = "grey") +
  guides(fill = guide_colorbar(
    title = "Homicides per 100K",
    label.theme = element_text(colour = "gray10", angle = 0, size = 8),
    title.theme = element_text(colour = "gray10", angle = 0, size = 10))) +
  theme_void()
dev.off()

#Canicula Index Map with Labels
pdf(file.path(figure_fpath, paste("Canicula_Label", y, "pdf", sep = ".")))
ggplot() +
  geom_sf(data = subset(full_sf, Year == y),
    aes(fill = Canicula_Label), lwd = 0) +
  geom_sf(data = country_sf, fill = NA, lwd = 0.1, color = "white") +
  scale_fill_viridis_d(option = "magma", begin = 1, end = 0) +
  guides(fill = guide_legend(
    title = "Canicula Intensity", 
    label.theme = element_text(colour = "gray10", angle = 0, size = 8),
    title.theme = element_text(colour = "gray10", angle = 0, size = 10))) +
  theme_void()
dev.off()

#Mean VHI (over 9 week period in summer) Map
pdf(file.path(figure_fpath, paste("Canicula_Period_VHI", y, "pdf", sep = ".")))
ggplot() +
  geom_sf(data = subset(full_sf, Year == y),
    aes(fill = mean_vhi), lwd = 0) +
  geom_sf(data = country_sf, fill = NA, lwd = 0.1, color = "white") +
  scale_fill_viridis_c(option = "magma") +
  guides(fill = guide_legend(
    title = "VHI (July-Aug)", 
    label.theme = element_text(colour = "gray10", angle = 0, size = 8),
    title.theme = element_text(colour = "gray10", angle = 0, size = 10))) +
  theme_void()
dev.off()