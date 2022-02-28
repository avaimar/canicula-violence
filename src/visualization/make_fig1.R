library(sf)
library(tidyverse)
library(cartography)
library(RColorBrewer)

#1. Set paths and parameters -----
#gdrive data paths
gdrive_fpath <- file.path( ##change this to point at google drive path
  "/Volumes/GoogleDrive/My Drive/Project")
raw_data_fpath <- file.path(gdrive_fpath, "Data", "Raw")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
#repo data paths
figure_fpath <- file.path(gdrive_fpath, "Figures")
#figure_fpath <- file.path("results", "figures" )

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

getPalette = colorRampPalette(brewer.pal(9, name = "BuPu"))
purp_20 <- getPalette(20)
purp5 <- c(purp_20[1], purp_20[7], purp_20[11], purp_20[16], purp_20[20])

y <- 2015 #choosing 2015 to display

#Homicide rate map (with max val of 200)
hom_rate_ceil <- 200
ggplot() +
  geom_sf(data = country_sf, fill = "gray90", lwd = 0.1, color = "gray20") +
  geom_sf(data = subset(full_sf, Year == y & in_dry_corridor == T),
    aes(fill = pmin(hom_rate_100k, hom_rate_ceil )), lwd = 0.02, color = "gray60") +
  geom_sf(data = country_sf, fill = NA, lwd = 0.1, color = "gray20") +
  scale_fill_gradientn(colours = purp5,
         na.value = "grey60")+
  # scale_fill_viridis_c(option = "magma", na.value = "grey40", begin = 1, end = .5) +
  guides(fill = guide_colorbar(
    title = "Homicide Rate (per 100,000)",
    label.theme = element_text(colour = "gray20", angle = 0, size = 8),
    title.theme = element_text(colour = "gray20", angle = 0, size = 10))) +
  theme_void()
ggsave(file.path(figure_fpath, paste("Homicide_Rate_Ceil_Dry_", y, ".pdf", sep = "")))


#Canicula Index Map with Labels
ggplot() +
  geom_sf(data = country_sf, fill = "gray90", lwd = 0.1, color = "gray20") +
  geom_sf(data = subset(full_sf, Year == y & in_dry_corridor == T),
    aes(fill = Canicula_Label), lwd = 0.02, color = "gray60") +
  scale_fill_manual(values=rev(purp5), na.value = "grey80") +
  geom_sf(data = country_sf, fill = NA, lwd = 0.12, color = "gray20") +
  guides(fill = guide_legend(
    title = "Canicula Intensity", 
    label.theme = element_text(colour = "gray20", angle = 0, size = 8),
    title.theme = element_text(colour = "gray20", angle = 0, size = 10))) +
  theme_void()
ggsave(file.path(figure_fpath, paste("Canicula_Label_Dry_", y, ".pdf", sep = "")))

# #Mean VHI (over 9 week period in June/July) Map
# ggplot() +
#   # geom_sf(data = subset(full_sf, Year == y),
#    geom_sf(data = subset(crossection_sf, in_dry_corridor == T),
#     aes(fill = mean_vhi), lwd = 0) +
#   geom_sf(data = country_sf, fill = NA, lwd = 0.1, color = "white") +
#   scale_fill_viridis_c(option = "magma", begin = 1, end = 0) +
#   # guides(fill = guide_legend(
#   #   title = "VHI (July-Aug)", 
#   #   label.theme = element_text(colour = "gray10", angle = 0, size = 8),
#   #   title.theme = element_text(colour = "gray10", angle = 0, size = 10))) +
#   theme_void()
# ggsave(file.path(figure_fpath, paste("Canicula_Period_VHI", y, "pdf", sep = ".")))