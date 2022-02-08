library(raster)
library(sf)
library(sp)
library(exactextractr)
library(tidyverse)
library(RColorBrewer)

#1. Set paths and parameters -----
#gdrive data paths
gdrive_fpath <- file.path("/Volumes/GoogleDrive-112161833434429421879/My Drive/Project") ##change this to point at google drive path
raw_data_fpath <- file.path(gdrive_fpath, 'Data', 'Raw')
processed_data_fpath <- file.path(gdrive_fpath, 'Data', 'Processed')
#repo data paths
figure_fpath <- file.path("results", "figures" )
YEAR_RANGE <- c(2010:2020)
WEEK_RANGE <- c(27:35)

admin_bndry_sf <- st_read(file.path(processed_data_fpath, 'cleaned_admin_boundaries.geojson'))
vhi_df <- read.csv(file.path(processed_data_fpath, 'Canicula_index_2010_2020.csv')) %>% rename(Year = year)
hom_df <- read.csv(file.path(processed_data_fpath, 'homicide_rates.csv'))

head(vhi_df)

hom_sf <- merge(admin_bndry_sf, hom_df, by= c("Departamento", "Municipio"))
canicula_sf <- merge(admin_bndry_sf, vhi_df, by= c("shapeID"))

pdf(file.path(figure_fpath, 'hom_2019_binned.pdf'))
hom_sf %>% 
  filter(Year == 2019) %>%
  ggplot()  + 
  geom_sf(aes(fill = pmin(hom_rate_100k, 200)), lwd = 0.1) +
  scale_fill_viridis_c(option = "magma") +
  ggtitle("2019 Homicide Rate") + 
  theme_void()
dev.off()


pdf(file.path(figure_fpath, 'VHI_2019.pdf'))
canicula_sf %>% 
  filter(Year == 2019) %>%
  ggplot()  + 
  geom_sf(aes(fill = mean_vhi), lwd = 0) +
  scale_fill_viridis_c(option = "magma", begin = 1, end = 0)+
  ggtitle("2019 VHI during Canicula Period") + 
  theme_void()
dev.off()

# pdf(file.path(figure_fpath, 'VHI_2018.pdf'))
# admin_bndry_vhi_sf %>% 
#   filter(year == 2018) %>%
#   ggplot()  + 
#   geom_sf(aes(fill = mean_vhi), lwd = 0) +
#   scale_fill_viridis_c(option = "magma", begin = 1, end = 0)+
#   ggtitle("2018 VHI during Canicula Period") + 
#   theme_void()
# dev.off()

admin_bndry_vhi_sf$mean_vhi



