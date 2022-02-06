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
data_raw_VHI_fpath <- file.path("data_raw_local", "VHI" )
figure_fpath <- file.path("results", "figures" )
YEAR_RANGE <- c(2010:2020)
WEEK_RANGE <- c(27:35)


#2. Preprocess municipality/department shapefiles ----
#create sf for all countries at spatial aggregation of interest (municipality for HND, ELS, GTM and dept for NIC)
# read in all 4 shapefiles
shape_fpaths <- list.files(file.path(raw_data_fpath, "Shapefiles"), pattern = 'geoBoundaries.*', full.names = T)
#(shapeType = 'ADM1' denotes department, shapeType = 'ADM2' denotes municipality)
#read in boundary shapefile and drop "shapeISO" column if it exists (only exists for department-level files)
read_boundary_shapefile <- function(f) {
  s = st_read(f)
  return(s[,!names(s) == "shapeISO"])
}
admin_bndry_sf <- do.call(rbind, lapply(boundary_shape_fpaths, read_boundary_shapefile))
#replace all names to match homicide files
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "Ahuachpan", "AHUACHAPAN"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "Chiltiupán", "CHITIUPAN"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "Ciudad Ilobasco", "ILOBASCO"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "Ciudad Barrios", "CUIDAD BARRIOS"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "Delgado", "CIUDAD DELGADO"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "Yucuaiquin", "YUCUALQUIN"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == 'Tepetitan', "TEPETITLAN"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == 'San Raimundo', "San Raymundo"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == 'Teujutepeque', "TEJUTEPEQUE"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == 'San Marcos de Sierra', "SAN MARCOS DE LA SIERRA"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == 'Juan Francisco  Bulnes', "JUAN FRANCISCO BULNES"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "San Jose Cancasque", "SAN JOSE CONCASQUE"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "Quelepa", "QUELAPA"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "Concepcion Quezaltepeque", "CONCEPCION QUEZALTEPQUE"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "Chalatenago", "CHALATENANGO"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "Jocoaitique", "JOCOATIQUE"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "San Francsico Chinameca", "SAN FRANCISCO CHINAMECA"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "Opico", "SAN JUAN OPICO"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "Meanguera del Golfo", "MEANGARA DEL GOLFO"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "San Ildefonso", "SAN IDELFONSO"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "Ramón Villeda Morales", "VILLEDA MORALES"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "Guacoteci", "GUACOTECTI"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "Potonico", "POTANICO"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "San Antonia Los Ranchos", "SAN ANTONIO LOS RANCHOS"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "San Emigdo", "SAN EMIGDIO"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "San Luis de la Reina", "SAN LUIS REINA"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "Matagalpa (Departemento)","MATAGALPA"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "North Carribean Coast Autonomous Region","RACCN"))
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(shapeName, shapeName == "South Atlantic Autonomous Region", "RACCS" ))
admin_bndry_sf <- admin_bndry_sf  %>% mutate(shapeName = chartr("ÁÉÍÓÚ", "AEIOU", toupper(shapeName) ))

#write this cleaned shapefile out so we don't have to do this again
st_write(admin_bndry_sf, file.path(processed_data_fpath, 'cleaned_admin_boundaries.geojson'))

#3.  Preprocess VHI Raster Data -----
#read in vhi data during YEAR_RANGE/, WEEK_RANGE and crop to 4 countries of interest 
vhi <- stack(list.files(data_raw_VHI_fpath, pattern = '.tif$', full.names = T)) %>% crop(extent(admin_bndry_sf))
vhi  <- reclassify(vhi, cbind(-9999, NA)) #map -9999 -> NULL
#for each year, get mean value for each gridcell over WEEK_RANGE
vhi_mean_by_year <- stackApply(vhi, indices =  rep(seq(length(YEAR_RANGE)), each = length(WEEK_RANGE)), fun = "mean", na.rm = T)
names(vhi_mean_by_year) <-  paste("vhi", YEAR_RANGE, sep = ".")
plot(vhi_mean_by_year$vhi.2011)

#4. Compute Average year VHI over spatial units----
#get spatial average and convert VHI to between 0 and 1
vhi_admin_bndry <- exact_extract(vhi_mean_by_year,admin_bndry_sf,fun="mean")/100 
admin_bndry_vhi_sf <- st_sf(cbind(admin_bndry_sf, vhi_admin_bndry))
#reshape from wide to long format
admin_bndry_vhi_sf <- gather(admin_bndry_vhi_sf, year, mean_vhi, (ncol(admin_bndry_sf) + 1):ncol(admin_bndry_vhi_sf)-1)
#extract portion of string containing the year
admin_bndry_vhi_sf$year <- as.numeric(substr(admin_bndry_vhi_sf$year, 10, 14)) 

#5. Create Canicula Index Columns----
#get canicula index by binning VHI by intensity according to FAO document
# Index. Label (bin):
## 5. Extreme (<0.25)
## 4. Severe (0.25 - 0.35)
## 3. Moderate (0.35 - 0.38)
## 2. Mild (0.38 - 0.42)
## 1. None (>0.42)
# create 3 columns for Index, Label and bin
#### VHI_Bin: Interval of bin (factor)
#### Canicula_Index: Numerical Canicula intensity index (1-5) where 1 corresponds to largest bin and 5 to smallest (numerica)
#### Canicula_Label: Label for Canicula intensity index from 'None' to 'Extreme' (factor)
admin_bndry_vhi_sf <- admin_bndry_vhi_sf%>% 
  mutate(VHI_Bin =cut(mean_vhi, breaks = c(0,.25,.35, .38, .42, 1)), 
         Canicula_Index = 5-as.numeric(cut(mean_vhi, breaks = c(0,.25,.35, .38, .42, 1))), 
         Canicula_Label = cut(mean_vhi, breaks = c(0,.25,.35, .38, .42, 1), 
                              labels = c('Extreme', 'Severe', 'Moderate', 'Mild', 'None')))


#6. Write out CSV ----
write.csv(st_drop_geometry(admin_bndry_vhi_sf), file.path(processed_data_fpath, 'Canicula_index_2010_2020.csv'))

# pdf(file.path(figure_fpath, 'VHI_2018_binned.pdf'))
# admin_bndry_vhi_sf %>% 
#   filter(year == 2018) %>%
#   ggplot()  + 
#   geom_sf(aes(fill = VHI_Bin), lwd = 0) +
#   scale_fill_viridis_d( begin = 1, end = 0)+
#   ggtitle("2018 binned VHI during Canicula Period") + 
#   theme_void()
# dev.off()



                              