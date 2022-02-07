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


#2. Preprocess municipality/department shapefiles ----
#create sf for all countries at spatial aggregation of interest (municipality for HND, ELS, GTM and dept for NIC)
# read in all 4 shapefiles
read_boundary_shapefile <- function(f) {
  s = st_read(f)
  return(s[,!names(s) == "shapeISO"])
}
dept_fpaths <- list.files(file.path(raw_data_fpath, "Shapefiles"), pattern = 'geoBoundaries.*ADM1', full.names = T)[1:4]
dept_sf <- do.call(rbind, lapply(dept_fpaths, read_boundary_shapefile))
munic_fpaths <- list.files(file.path(raw_data_fpath, "Shapefiles"), pattern = 'geoBoundaries.*ADM2', full.names = T)
munic_sf <- do.call(rbind, lapply(munic_fpaths, st_read))

munic_centroid <-munic_sf %>%
  mutate(geometry  =st_centroid(geometry))

munic_dept_join <- dept_sf  %>% 
  select(c(shapeName, geometry)) %>%
  st_join(st_centroid(munic_sf), suffix = c('.Departamento', '.Municipio')) %>%
  rename(Departamento = shapeName.Departamento, 
         Municipio = shapeName.Municipio) %>%
  select(Departamento, Municipio, shapeID) %>%
  st_drop_geometry()

#municipality sf with department column (not including )
munic_sf <- left_join(munic_sf, munic_dept_join, by = 'shapeID') %>%
  select(-shapeName)

st_write(munic_sf, file.path(processed_data_fpath, 'munic_with_dept.geojson'))

nic_dept_sf <- dept_sf %>% 
  filter(shapeGroup == 'NIC') %>%
  mutate(Municipio = NA) %>%
  rename(Departamento = shapeName )

munic_sf_no_nic <- munic_sf %>% 
  filter(shapeGroup != 'NIC')
  
admin_bndry_sf<- rbind(munic_sf_no_nic, nic_dept_sf)


#replace all names to match homicide files
admin_bndry_sf <- mutate(admin_bndry_sf, shapeName = replace(Municipio, Municipio == "Ahuachpan", "AHUACHAPAN"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "Chiltiupán", "CHITIUPAN"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "Ciudad Ilobasco", "ILOBASCO"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "Ciudad Barrios", "CUIDAD BARRIOS"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "Delgado", "CIUDAD DELGADO"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "Yucuaiquin", "YUCUALQUIN"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == 'Tepetitan', "TEPETITLAN"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == 'San Raimundo', "San Raymundo"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == 'Teujutepeque', "TEJUTEPEQUE"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == 'San Marcos de Sierra', "SAN MARCOS DE LA SIERRA"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == 'Juan Francisco  Bulnes', "JUAN FRANCISCO BULNES"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "San Jose Cancasque", "SAN JOSE CONCASQUE"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "Quelepa", "QUELAPA"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "Concepcion Quezaltepeque", "CONCEPCION QUEZALTEPQUE"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "Chalatenago", "CHALATENANGO"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "Jocoaitique", "JOCOATIQUE"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "San Francsico Chinameca", "SAN FRANCISCO CHINAMECA"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "Opico", "SAN JUAN OPICO"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "Meanguera del Golfo", "MEANGARA DEL GOLFO"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "San Ildefonso", "SAN IDELFONSO"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "Ramón Villeda Morales", "VILLEDA MORALES"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "Guacoteci", "GUACOTECTI"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "Potonico", "POTANICO"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "San Antonia Los Ranchos", "SAN ANTONIO LOS RANCHOS"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "San Emigdo", "SAN EMIGDIO"))
admin_bndry_sf <- mutate(admin_bndry_sf, Municipio = replace(Municipio, Municipio == "San Luis de la Reina", "SAN LUIS REINA"))
admin_bndry_sf <- mutate(admin_bndry_sf, Departamento = replace(Departamento, Departamento == "North Carribean Coast Autonomous Region","RACCN"))
admin_bndry_sf <- mutate(admin_bndry_sf, Departamento = replace(Departamento, Departamento == "South Atlantic Autonomous Region", "RACCS" ))
admin_bndry_sf <- mutate(admin_bndry_sf, Departamento = replace(Departamento, Departamento == "Bay Islands", "ISLAS DE LA BAHIA" ))
admin_bndry_sf$Departamento <- gsub(" \\(Departamento)", "", admin_bndry_sf$Departamento)
admin_bndry_sf$Departamento <- gsub(" \\(Departemento)", "", admin_bndry_sf$Departamento)
admin_bndry_sf$Departamento <- gsub("Departamento de ", "", admin_bndry_sf$Departamento)

#remove accents
admin_bndry_sf <- admin_bndry_sf  %>% 
  mutate(Municipio = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Municipio)), 
        Departamento = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Departamento))) %>%
  select(c('shapeID', 'shapeGroup', 'Departamento', 'Municipio', 'geometry'))


#write this cleaned shapefile out so we don't have to do this again
st_write(admin_bndry_sf, file.path(processed_data_fpath, 'cleaned_admin_boundaries.geojson'))


