library(raster)
library(sf)
library(exactextractr)
library(tidyverse)

# 1. Set paths and parameters -----
# gdrive data paths
load("CONFIG.Rspace")
raw_data_fpath <- file.path(gdrive_fpath, "Data", "Raw")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
# projection CRS globals
proj_crs <- 26716
latlon_crs <- 4326

# 2. Preprocess municipality/department shapefiles ----
# function to deal w the fact that some Dept files have extra columns
# and has different columns than the others
read_boundary_shapefile <- function(f) {
  s <- sf::st_read(f)
  return(s[, !names(s) == "shapeISO"])
}
# read in all shapefiles for 3 countries at municipality and dept admin level
dept_fpaths <- list.files(
  file.path(raw_data_fpath, "Shapefiles"),
  pattern = "geoBoundaries.*ADM1_simplified*", full.names = T
)
munic_fpaths <- list.files(
  file.path(raw_data_fpath, "Shapefiles"),
  pattern = "geoBoundaries.*ADM2_simplified*", full.names = T
)

# create munic and dept sdf
dept_sf <- do.call(rbind, lapply(dept_fpaths, read_boundary_shapefile))
munic_sf <- do.call(rbind, lapply(munic_fpaths, st_read))

# create dept sf for Nicaragua (from different source in since first source didnt include Lago Nicaragua
nic_dept_sf <- sf::st_read(file.path(raw_data_fpath, "Shapefiles", "nic_department.json"))

# remove Lago Nicaragua from dep shapefile,
# rename/subset columns to match other format,
# and add a NULL munic column
nic_dept_sf <- nic_dept_sf %>%
  filter(type_1 != "Water body") %>%
  rename(
    shapeID = id,
    shapeGroup = iso,
    Departamento = name_1
  ) %>%
  select(shapeID, shapeGroup, Departamento) %>%
  mutate(Municipio = NA)

# 3.  Create shapefiles at desired administrative levels for homicide join ----

## a) Add Department column to municipality sdf ----
# reproject to projection CRS for central america so we can take centroids
dept_sf <- st_transform(dept_sf, crs = proj_crs)
munic_sf <- st_transform(munic_sf, crs = proj_crs)

# join dept sdf to munic sdf based on which
# department the polygon munic centroid falls into
munic_dept_join <- dept_sf %>%
  dplyr::select(c(shapeName, geometry)) %>%
  st_join(st_centroid(munic_sf), suffix = c(".Departamento", ".Municipio")) %>%
  rename(
    Departamento = shapeName.Departamento,
    Municipio = shapeName.Municipio
  ) %>%
  dplyr::select(Departamento, Municipio, shapeID)

# add the department column to the munic sdf based on previous join
munic_sf <- left_join(
  munic_sf, st_drop_geometry(munic_dept_join),
  by = "shapeID"
) %>%
  dplyr::select(-shapeName)

# reproject to lat/lon CRS, and subset to columns of interest
munic_sf <- sf::st_transform(munic_sf, crs = latlon_crs) %>%
  select(c("shapeID", "shapeGroup", "Departamento", "Municipio", "geometry"))

## b) Combine Nic Dept + Munic Df to create sdf at for desired spatial units ----

# combine this with nic dept file
admin_bndry_sf <- rbind(munic_sf, nic_dept_sf)

# make sure these look right
ggplot() +
  geom_sf(data = admin_bndry_sf, aes(fill = Departamento)) +
  guides(fill = "none")

## c) Add column for whether admin unit is in dry corridor  ----

dry_corridor <- sf::st_read(
  file.path(raw_data_fpath, "Shapefiles", "corredor_seco.geojson")
)

dry_corridor <- st_transform(dry_corridor, crs = proj_crs)

# add column 'in_dry_corridor'
admin_bndry_sf <- admin_bndry_sf %>% mutate(
  in_dry_corridor = lengths(st_intersects(
    st_transform(admin_bndry_sf, crs = proj_crs),
    dry_corridor
  )) > 0
)

# 5. Clean names to match homicide file ----
# replace all names to match homicide files
admin_bndry_sf <- admin_bndry_sf %>%
  mutate(
    Municipio = case_when(
      Municipio == "Ahuachpan" ~ "AHUACHAPAN",
      # Municipio == "Chiltiupán" ~ "CHITIUPAN",
      Municipio == "Ciudad Ilobasco" ~ "ILOBASCO",
      # Municipio == "Ciudad Barrios" ~ "CUIDAD BARRIOS",
      # Municipio == "Delgado" ~ "CIUDAD DELGADO",
      # Municipio == "Yucuaiquin" ~ "YUCUALQUIN",
      # Municipio == "Tepetitan" ~ "TEPETITLAN",
      Municipio == "San Raimundo" ~ "San Raymundo",
      Municipio == "Teujutepeque" ~ "TEJUTEPEQUE",
      Municipio == "San Marcos de Sierra" ~ "SAN MARCOS DE LA SIERRA",
      Municipio == "Juan Francisco  Bulnes" ~ "JUAN FRANCISCO BULNES",
      Municipio == "San Jose Cancasque" ~ "SAN JOSE CONCASQUE",
      # Municipio == "Quelepa" ~ "QUELAPA",
      # Municipio == "Concepcion Quezaltepeque" ~ "CONCEPCION QUEZALTEPQUE",
      Municipio == "Chalatenago" ~ "CHALATENANGO",
      # Municipio == "Jocoaitique" ~ "JOCOATIQUE",
      Municipio == "San Francsico Chinameca" ~ "SAN FRANCISCO CHINAMECA",
      Municipio == "Opico" ~ "SAN JUAN OPICO",
      Municipio == "Meanguera del Golfo" ~ "MEANGUERA DEL GOLFO",
      # Municipio == "San Ildefonso" ~ "SAN IDELFONSO",
      Municipio == "Ramón Villeda Morales" ~ "VILLEDA MORALES",
      Municipio == "Guacoteci" ~ "GUACOTECTI",
      # Municipio == "Potonico" ~ "POTANICO",
      Municipio == "San Antonia Los Ranchos" ~ "SAN ANTONIO LOS RANCHOS",
      Municipio == "San Emigdo" ~ "SAN EMIGDIO",
      # Municipio == "San Luis de la Reina" ~ "SAN LUIS REINA",
      Departamento == "San Marcos" & Municipio == "Ayutla" ~ "AYUTLA O TECUN UMAN",
      Departamento == "Quetzaltenango" & Municipio == "San Miguel Sigüila" ~ "SAN MIGUEL SIGUILA",
      Departamento == "Chiquimula" & Municipio == "San Juan Ermita" ~ "SAN JUAN ERMINTA",
      Departamento == "Chiquimula" & Municipio == "Quezaltepeque" ~ "QUETZALTEPEQUE",
      Departamento == "Departamento de Sonsonate" & Municipio == "Santo Domingo" ~ "SANTO DOMINGO DE GUZMAN",
      Departamento == "La Libertad" & Municipio == "Antiguo Cuscatlan" ~ "ANTIGUO CUSCATAN",
      Departamento == "Departamento de San Miguel" & Municipio == "San Rafael" ~ "SAN RAFAEL ORIENTE",
      Departamento == "Departamento de Chalatenango" & Municipio == "San Ingnacio" ~ "SAN IGNACIO",
      Departamento == "Copán" & Municipio == "Cabana" ~ "CABAÑAS",
      Departamento == "La Paz" & Municipio == "Cabanas" ~ "CABAÑAS",
      Departamento == "Departamento de Chalatenango" & Municipio == "San Jose Las Flores" ~ "LAS FLORES",
      TRUE ~ Municipio
    ),
    Departamento = case_when(
      Departamento == "Atlántico Norte" ~ "RACCN",
      Departamento == "Atlántico Sur" ~ "RACCS",
      Departamento == "Bay Islands" ~ "ISLAS DE LA BAHIA",
      TRUE ~ Departamento
    ),
  )

# remove (Departmento de *) and (* Departamento) patterns
admin_bndry_sf <- admin_bndry_sf %>%
  mutate(
    Departamento = gsub(" \\(Departamento)", "", Departamento),
    Departamento = gsub(" \\(Departemento)", "", Departamento),
    Departamento = gsub("Departamento de ", "", Departamento)
  )

# remove accents
admin_bndry_sf <- admin_bndry_sf %>%
  mutate(
    Municipio = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Municipio)),
    Departamento = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Departamento))
  )

# write out cleaned shapefile
st_write(admin_bndry_sf,
  file.path(processed_data_fpath, "cleaned_admin_boundaries.geojson"),
  delete_dsn = T
)

### Tests for same columns as homicide Df
hom_df <- read.csv(file.path(processed_data_fpath, "homicide_rates.csv"))
pending_hom <- setdiff(hom_df$Municipio, admin_bndry_sf$Municipio)
pending_admin <- setdiff(admin_bndry_sf$Municipio, hom_df$Municipio)

# length(setdiff(hom_df$Municipio, admin_bndry_sf$Municipio))
# length(setdiff(admin_bndry_sf$Municipio, hom_df$Municipio))

# setdiff(hom_df$Departamento,admin_bndry_sf$Departamento)
# setdiff(admin_bndry_sf$Departamento, hom_df$Departamento)

# Note: We have missing data for 14 municipalities in El Salvador, and
# 7 from Guatemala
salvador_missings <- c(
  "SANTA ANA", "SONSONATE", "SANTA TECLA",
  "SAN SALVADOR", "LA UNION", "NUEVA ESPERANZA", "SAN MIGUEL",
  "ZACATECOLUCA", "COJUTEPEQUE", "SAN FRANCISCO GOTERA",
  "SENSUNTEPEQUE", "SAN VICENTE", "SAN JOSE CONCASQUE",
  "USULUTAN"
)
guatemala_missings <- c(
  "SIPACATE", "SAN JOSE LA MAQUINA", "LA BLANCA", "PETATAN",
  "LAS CRUCES", "EL CHAL", "SAN JORGE"
)
pending_admin <- setdiff(pending_admin, salvador_missings)
pending_admin <- setdiff(pending_admin, guatemala_missings)

print(paste0("[INFO] Unmatched municipalities from homicide data: ", length(pending_hom)))
print(paste0("[INFO] Unmatched municipalities from admin data: ", length(pending_admin)))


# 6. Create country shapefile to be used later  ----

country_sf <- admin_bndry_sf %>%
  sf::st_transform(admin_bndry_sf, crs = proj_crs) %>%
  group_by(shapeGroup) %>%
  summarize(geometry = st_union(geometry)) %>%
  sf::st_transform(dmin_bndry_sf, crs = latlon_crs)

ggplot() +
  geom_sf(data = country_sf, aes(fill = shapeGroup))

st_write(country_sf,
  file.path(processed_data_fpath, "country_outlines.geojson"),
  delete_dsn = TRUE
)