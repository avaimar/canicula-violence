library(raster)
library(sf)
library(exactextractr)
library(tidyverse)

#1. Set paths and parameters -----
load('CONFIG.Rspace') #load gdrive_fpath
raw_data_fpath <- file.path(gdrive_fpath, "Data", "Raw")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
#repo data paths
data_raw_VHI_fpath <- file.path("data_raw_local", "VHI" )
year_range <- c(1982:2020)
week_range <- c(27:35)


#2. Read in municipality/department shapefiles ----
admin_bndry_sf <- st_read(
  file.path(processed_data_fpath, "cleaned_admin_boundaries.geojson"))

#3.  Preprocess VHI Raster Data -----
#r ead in vhi data during year/week_range and crop to 4 countries of interest
# NOTE: No file exists for 'VHP.G04.C07.NL.P2004029.VH.VHI.tif' so just copied 
# 'VHP.G04.C07.NL.P2004030.VH.VHI.tif' and renamed 
vhi <- stack(
  list.files(data_raw_VHI_fpath, pattern = "*.VH.VHI.tif$", full.names = T)) %>% 
  crop(extent(admin_bndry_sf))
vhi  <- reclassify(vhi, cbind(-9999, NA)) #map -9999 to NULL

#for each year, get mean value for each gridcell over week_range
vhi_mean_by_year <- stackApply(
  vhi,
  indices =  rep(seq(length(year_range)), each = length(week_range)),
  fun = "mean",
  na.rm = T)
names(vhi_mean_by_year) <-  paste("vhi", year_range, sep = ".")
plot(vhi_mean_by_year$vhi.2018) #make sure this looks right

#4. Compute Average year VHI over spatial units----
#get spatial average and convert VHI to between 0 and 1
vhi_admin_bndry <- exact_extract(
  vhi_mean_by_year, admin_bndry_sf, fun = "mean") / 100
admin_bndry_vhi_sf <- st_sf(cbind(admin_bndry_sf, vhi_admin_bndry))
#reshape from wide to long format (vhi by year stored in last cols other than geom col)
admin_bndry_vhi_sf <- gather(
  admin_bndry_vhi_sf, Year, mean_vhi, (ncol(admin_bndry_sf)+1):ncol(admin_bndry_vhi_sf) - 1)
#extract portion of string containing the year
admin_bndry_vhi_sf$Year <- as.numeric(substr(admin_bndry_vhi_sf$Year, 10, 14))
admin_bndry_vhi_sf
plot(filter(admin_bndry_vhi_sf, Year == 2018)['mean_vhi'])

#5. Create Canicula Index Columns----
#get canicula index by binning VHI by intensity according to FAO document
# Index. Label (bin):
## 5. Extreme (<0.25)
## 4. Severe (0.25 - 0.35)
## 3. Moderate (0.35 - 0.38)
## 2. Mild (0.38 - 0.42)
## 1. None (>0.42)
# create 3 columns for Index, Label and bin
## VHI_Bin: Interval of bin (factor)
## Canicula_Index: Canicula intensity index (1-5) smallest to largest (numerical)
## Canicula_Label: Canicula index label from "None" to "Extreme" (ordered factor)
admin_bndry_vhi_sf <- admin_bndry_vhi_sf %>%
  mutate(
    VHI_Bin = cut(mean_vhi, breaks = c(0,.25,.35, .38, .42, 1)),
    Canicula_Index = 5-as.numeric(cut(
      mean_vhi, 
      breaks = c(0, .25, .35, .38, .42, 1))),
    Canicula_Label = factor(
      cut(
        mean_vhi,
        breaks = c(0, .25, .35, .38, .42, 1),
        labels = c("Extreme", "Severe", "Moderate", "Mild", "None")),
      levels = c("Extreme", "Severe", "Moderate", "Mild", "None")))

#6. Write out CSV ----
write.csv(
  st_drop_geometry(select(admin_bndry_vhi_sf, -c(in_dry_corridor))), 
  file.path(processed_data_fpath, "Canicula_index_1982_2020.csv"))
