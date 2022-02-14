library(raster)
library(sf)
library(sp)
library(exactextractr)
library(tidyverse)
library(RColorBrewer)
library(fixest)

#1. Set paths and parameters -----
#gdrive data paths
gdrive_fpath <- file.path(##change this to point at google drive path
  "/Volumes/GoogleDrive-112161833434429421879/My Drive/Project")
raw_data_fpath <- file.path(gdrive_fpath, "Data", "Raw")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
model_data_fpath <- file.path(gdrive_fpath, "Models")
#repo data paths
#figure_fpath <- file.path(gdrive_fpath, "Figures")
figure_fpath <- file.path("results", "figures")

#2. Read in Data -----
admin_bndry_sf <- st_read(
  file.path(processed_data_fpath, "cleaned_admin_boundaries.geojson"))
vhi_df <- read.csv(
  file.path(processed_data_fpath, "Canicula_index_2010_2020.csv"))
#convert canicula index to ordered factor
vhi_df$Canicula_Label <- factor(vhi_df$Canicula_Label,
  levels = c("Extreme", "Severe", "Moderate", "Mild", "None"))
hom_df <- read.csv(file.path(processed_data_fpath, "homicide_rates.csv"))

#3. Join Canicula Data, Homicide Data, and Shapefiles -----
canicula_sf <- left_join(
  admin_bndry_sf,
  vhi_df,
  by = c("shapeID", "Departamento", "Municipio", "shapeGroup"))

full_sf <- left_join(
  canicula_sf,
  hom_df,
  by = c("Departamento", "Municipio", "Year"))


#4. Prepare data for FE Model -----
#transform variables
#make vhi on scale from 1-100 to make coefs easier to interpret
full_sf$mean_vhi = full_sf$mean_vhi * 100
#make unique identifier for each spatial unit as Municipality_Department
full_sf$munic_dep <- paste(full_sf$Municipio, full_sf$Departamento, sep = "_")

#get non-spatial DF so that we can determine duplicates
#without considering geometry
full_df <- st_drop_geometry(full_sf)

#get the 3 munic-departments that are non-unique due to 
# Null Municipality or weird multiple shapeID issue
#TODO:for now we will drop these, but need to investigate these later
nonunique_munic_dept <- full_df[duplicated(full_df[, c("munic_dep", "Year")]),] %>%
  select("munic_dep") %>%
  unique() %>%
  unlist()
full_df <- full_df %>% filter(!(munic_dep %in% nonunique_munic_dept))

#5. Run FE Model -----

#look at data coverage by country/year
full_df %>% filter(!is.na(hom_rate_100k)) %>%
  group_by(shapeGroup) %>%
  summarise(min_year = min(Year),
            max_year = max(Year))

#We only have data for >= 3 countries for 2013-2020 so subset to this period
# full_df <-full_df %>% filter(Year >= 2011) #period with data for >=2 countries
full_df <- full_df %>% filter(Year >= 2013) #period with data for >=3 countries
#full_df <- full_df %>% filter(Year >= 2014 & Year <= 2019) #period with data for all countries
#full_df <-full_df %>% filter(shapeGroup != "NIC") # subset to countries with munic-level data 

#run FE model with municipality FE (significant effects)
m1.vhi <- feols(hom_rate_100k ~ mean_vhi |  munic_dep + Year, data = full_df)
m2.CI <- feols(hom_rate_100k ~ as.factor(Canicula_Index) | munic_dep + Year, data = full_df)

# Pick model for visualization purposes
selected.MM <- model.matrix(m1.vhi)
save(selected.MM, m1.vhi, file = file.path(model_data_fpath, "modelmatrix.RData"))

#no significant effects using department or country FE or country*year department*year FE
feols(hom_rate_100k ~ mean_vhi |  Departamento + Year, data = full_df)
feols(hom_rate_100k ~ as.factor(Canicula_Index) |  Departamento + Year, data = full_df)
feols(hom_rate_100k ~ mean_vhi | shapeGroup + Year, data = full_df)
feols(hom_rate_100k ~ as.factor(Canicula_Index) |  shapeGroup + Year, data = full_df)
feols(hom_rate_100k ~ mean_vhi | shapeGroup[Year] + Year, data = full_df)
feols(hom_rate_100k ~ as.factor(Canicula_Index) | shapeGroup[Year] + Year, data = full_df)
feols(hom_rate_100k ~ mean_vhi |  Departamento[Year] + Year, data = full_df)
feols(hom_rate_100k ~ as.factor(Canicula_Index) |  Departamento[Year] + Year, data = full_df)


#number of years of data per munic-dep
full_df %>% filter(!is.na(hom_rate_100k)) %>%
  group_by(munic_dep) %>%
  summarise(n_year = n()) %>% 
  group_by(n_year)%>%
  summarise(n_munic = n())

full_df <- full_df %>% filter(!is.na(hom_rate_100k)) %>%
  group_by(munic_dep) %>%
  mutate(n_year = n())

#most municipalities have at least 6 years of data, remove those 19 that do not
full_df <- full_df %>% filter(n_year > 5)

#no major change when removing these
feols(hom_rate_100k ~ mean_vhi | munic_dep + Year, data = full_df)
feols(hom_rate_100k ~ as.factor(Canicula_Index) | munic_dep + Year, data = full_df)
feols(hom_rate_100k ~ mean_vhi |  Departamento + Year, data = full_df)
feols(hom_rate_100k ~ as.factor(Canicula_Index) |  Departamento + Year, data = full_df)

##FE with lags + leads... not sure how to interpret these
feols(hom_rate_100k ~ l(mean_vhi, 0:2) | munic_dep + Year, data = full_df, panel.id = ~munic_dep + Year )
feols(hom_rate_100k ~ l(Canicula_Index, 0:2) | munic_dep + Year, data = full_df, panel.id = ~munic_dep + Year )
feols(hom_rate_100k ~ f(mean_vhi, 0:2)|  munic_dep + Year, data = full_df, panel.id = ~munic_dep + Year )
feols(hom_rate_100k ~ f(Canicula_Index, 0:2)| munic_dep + Year, data = full_df, panel.id = ~munic_dep + Year )
