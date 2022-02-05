library(raster)
library(sf)
library(sp)
library(exactextractr)
library(tidyverse)
library(RColorBrewer)


google_drive_fpath <- file.path("/Volumes/GoogleDrive-112161833434429421879/My Drive/Project")
shapefile_fpath <- file.path(google_drive_fpath, "Data", "Raw", "Shapefiles")
data_raw_VHI_fpath <- file.path("data_raw_local", "VHI" )
NULL_VAL <- -9999

#read in municipality shapefiles for countries of interest
shp_municipality_fpaths <- list.files(shapefile_fpath, pattern = '_ADM2.geojson$', full.names = T)
munic_sf <- do.call(rbind, lapply(shp_municipality_fpaths, st_read))

# #read in crop/pastureland raster and crop to region of interest
# crop_pasture_land <- stack(list.files(file.path(shapefile_fpath, "CroplandPastureArea2000_Geotiff"),
#                                       pattern = '.tif$', full.names = T)) %>% crop(extent(munic_sf))
# #sum crop+ pasturland percentage for each gridcell
# agricultural_land <- stackApply(crop_pasture_land, indices = rep(1, nlayers(crop_pasture_land)), fun = "sum", na.rm = T)
# names(agricultural_land ) <- "percent_ag"
# #plot(agricultural_land)

#read in vhi data and crop to 4 countries of interest
vhi <- stack(list.files(data_raw_VHI_fpath, pattern = '.tif$', full.names = T)) %>% crop(extent(munic_sf))
vhi  <- reclassify(vhi, cbind(NULL_VAL, 0)) 

#for each year, get mean value for each gridcell over 9 week period 
vhi_mean_by_year <- stackApply(vhi, indices =  rep(seq(8), each = 9), fun = "mean", na.rm = T)
names(vhi_mean_by_year) <- paste("mean_vhi", seq(2013, 2020), sep = "_")
plot(vhi_mean_by_year$mean_vhi_2018)

#reproject 10km ag percent grid  to 4km resolution of VHI using nearest neighbor method
#agricultural_land <- projectRaster(agricultural_land, vhi_mean_by_year, method = 'ngb') 
# plot(agricultural_land)


#### Compute Average VHI for each municipality ######
vhi_munic <- exact_extract(vhi_mean_by_year,munic_sf,fun="mean")/100
munic_sf<- st_sf(cbind(munic_sf, vhi_munic))

names(munic_sf)[(ncol(munic_sf) - 7):ncol(munic_sf)-1] <- seq(2013, 2020)
#reshape from wide to long format
munic_sf <- gather(munic_sf, year, mean_vhi, (ncol(munic_sf) - 7):ncol(munic_sf)-1)
munic_sf$year <- as.numeric(munic_sf$year)

munic_sf <- munic_sf %>% 
  mutate(VHI_bin = 
           as.numeric(cut(mean_vhi, breaks = c(0,.25,.35, .38, .42, 1))))

pdf('VHI_2018_binned.pdf')
ggplot(subset(munic_sf, year == 2018)) + 
  geom_sf(aes(fill = VHI_bin), lwd = 0) +
  theme_minimal()
dev.off()

# #get average (1-VHI) weighted by ag percentage -- using (1 - VHI) so 
# vhi_agg_weighted <- (1-vhi_mean_by_year/100)*agricultural_land$percent_ag #compute as 1-VHI 
# names(vhi_agg_weighted ) <- paste("mean_ag_vhi", seq(2013, 2020), sep = "_")
# plot(vhi_agg_weighted$mean_ag_vhi_2018)
# vhi_agg_weighted  <- 1 - reclassify(vhi_agg_weighted, cbind(0, NA)) 
# plot(vhi_agg_weighted$mean_ag_vhi_2018)
# 
# 
# ##bin canicula index by intensity according to FAO paper
# reclass_df <- c(0, .25, 4,
#                 .25, .35, 3,
#                 .35, .38, 2,
#                 .38, .42, 1, 
#                 .42, 1, 0)
# 
# reclass_m <- matrix(reclass_df,
#                     ncol = 3,
#                     byrow = TRUE)
# 
# chm_classified <- reclassify(vhi_agg_weighted,
#                              reclass_m)
# plot(chm_classified$mean_ag_vhi_2018)


#munic_sf <- munic_sf %>% mutate(VHI_bin = as.numeric(cut(mean_vhi, breaks = c(0,.25,.35, .38, .42, 1), labels = c(4,3,2,1,0) )))



