library(raster)
library(sf)
library(exactextractr)
library(tidyverse)

#1. Set paths and parameters -----
#gdrive data paths
gdrive_fpath <- file.path(##change this to point at google drive path
  "/Volumes/GoogleDrive-112161833434429421879/My Drive/Project")

raw_data_fpath <- file.path(gdrive_fpath, "Data", "Raw")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
#projection CRS globals
proj_crs <- 26716
latlon_crs <- 4326

#2. Preprocess municipality/department shapefiles ----
#function to deal w the fact that NIC Dept file is dumb 
#and has different columns than the others 
read_boundary_shapefile <- function(f) {
  s <- sf::st_read(f)
  return(s[, !names(s) == "shapeISO"])
}
# read in all shapefiles for 4 countries at municipality and dept admin level
dept_fpaths <- list.files(
    file.path(raw_data_fpath, "Shapefiles"),
    pattern = "geoBoundaries.*ADM1_simplified*", full.names = T)
munic_fpaths <- list.files(
    file.path(raw_data_fpath, "Shapefiles"),
    pattern = "geoBoundaries.*ADM2_simplified*", full.names = T)

#create munic and dept sdf
dept_sf <- do.call(rbind, lapply(dept_fpaths, read_boundary_shapefile))
munic_sf <- do.call(rbind, lapply(munic_fpaths, st_read))

#3. Add Department column to municipality sdf ----
#reproject to projection CRS for central america so we can take centroids
dept_sf <- sf::st_transform(dept_sf, crs = proj_crs)
munic_sf <- sf::st_transform(munic_sf, crs = proj_crs)

#join dept sdf to munic sdf based on which
#department the polygon munic centroid falls into
munic_dept_join <- dept_sf  %>% 
  select(c(shapeName, geometry)) %>%
  st_join(st_centroid(munic_sf), suffix = c(".Departamento", ".Municipio")) %>%
  rename(Departamento = shapeName.Departamento, 
         Municipio = shapeName.Municipio) %>%
  select(Departamento, Municipio, shapeID)

#add the department column to the munic sdf based on previous join
munic_sf <- left_join(
  munic_sf, st_drop_geometry(munic_dept_join), by = "shapeID") %>%
    select(-shapeName)

#make sure these look right
ggplot() +
  geom_sf(data = munic_sf, aes(fill = Departamento)) +
  guides(fill = "none")

#reproject to lat/lon CRS
munic_sf <- sf::st_transform(munic_sf, crs = latlon_crs)
dept_sf <- sf::st_transform(dept_sf, crs = latlon_crs)

#write out munic sdf with department column
st_write(munic_sf,
  file.path(processed_data_fpath, "munic_with_dept.geojson"),
  delete_dsn = TRUE)

#4. Create shapefile at desired administrative levels for homicide join ----
# Create sdf with one row for each NIC department with a null Municip
# subset department sdf to nicaragua
nic_dept_sf <- dept_sf %>%
  filter(shapeGroup == "NIC") %>%
  mutate(Municipio = NA) %>%
  rename(Departamento = shapeName)
# subset munic sdf with department col to not include nicaragua
munic_sf_no_nic <- munic_sf %>%
  filter(shapeGroup != "NIC")
# combine
admin_bndry_sf <- rbind(munic_sf_no_nic, nic_dept_sf)

#5. Clean names  ----
#replace all names to match homicide files
admin_bndry_sf <- admin_bndry_sf  %>%
  mutate(
    Municipio = case_when(
      Municipio == "Ahuachpan" ~ "AHUACHAPAN",
      Municipio == "Chiltiupán" ~ "CHITIUPAN",
      Municipio == "Ciudad Ilobasco" ~ "ILOBASCO",
      Municipio == "Ciudad Barrios" ~ "CUIDAD BARRIOS",
      Municipio == "Delgado" ~ "CIUDAD DELGADO",
      Municipio == "Yucuaiquin" ~ "YUCUALQUIN",
      Municipio == "Tepetitan" ~ "TEPETITLAN",
      Municipio == "San Raimundo" ~ "San Raymundo",
      Municipio == "Teujutepeque" ~ "TEJUTEPEQUE",
      Municipio == "San Marcos de Sierra" ~ "SAN MARCOS DE LA SIERRA",
      Municipio == "Juan Francisco  Bulnes" ~ "JUAN FRANCISCO BULNES",
      Municipio == "San Jose Cancasque" ~ "SAN JOSE CONCASQUE",
      Municipio == "Quelepa" ~ "QUELAPA",
      Municipio == "Concepcion Quezaltepeque" ~ "CONCEPCION QUEZALTEPQUE",
      Municipio == "Chalatenago" ~ "CHALATENANGO",
      Municipio == "Jocoaitique" ~ "JOCOATIQUE",
      Municipio == "San Francsico Chinameca" ~ "SAN FRANCISCO CHINAMECA",
      Municipio == "Opico" ~ "SAN JUAN OPICO",
      Municipio == "Meanguera del Golfo" ~ "MEANGARA DEL GOLFO",
      Municipio == "San Ildefonso" ~ "SAN IDELFONSO",
      Municipio == "Ramón Villeda Morales" ~ "VILLEDA MORALES",
      Municipio == "Guacoteci" ~ "GUACOTECTI",
      Municipio == "Potonico" ~ "POTANICO",
      Municipio == "San Antonia Los Ranchos" ~ "SAN ANTONIO LOS RANCHOS",
      Municipio == "San Emigdo" ~ "SAN EMIGDIO",
      Municipio == "San Luis de la Reina" ~ "SAN LUIS REINA",
      TRUE ~ Municipio),
    Departamento = case_when(
      Departamento == "North Carribean Coast Autonomous Region" ~"RACCN",
      Departamento == "South Atlantic Autonomous Region" ~ "RACCS",
      Departamento == "Bay Islands" ~ "ISLAS DE LA BAHIA",
      TRUE ~ Departamento),
)

#remove (Departmento de *) and (* Departamento) patterns
admin_bndry_sf <- admin_bndry_sf %>%
  mutate(
    Departamento = gsub(" \\(Departamento)", "", Departamento),
    Departamento = gsub(" \\(Departemento)", "", Departamento),
    Departamento = gsub("Departamento de ", "", Departamento))

#remove accents
admin_bndry_sf <- admin_bndry_sf  %>%
  mutate(Municipio = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Municipio)),
        Departamento = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Departamento))) %>%
  select(c("shapeID", "shapeGroup", "Departamento", "Municipio", "geometry"))

#write out cleaned shapefile
st_write(admin_bndry_sf,
  file.path(processed_data_fpath, "cleaned_admin_boundaries.geojson"),
  delete_dsn = T)

### Tests for same columns as homicide Df
# hom_df <- read.csv(file.path(processed_data_fpath, 'homicide_rates.csv'))
# setdiff(hom_df$Municipio, admin_bndry_sf$Municipio)
# setdiff(admin_bndry_sf$Municipio, hom_df$Municipio)

# setdiff(hom_df$Departamento,admin_bndry_sf$Departamento)
# setdiff(admin_bndry_sf$Departamento, hom_df$Departamento)
