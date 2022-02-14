# 0. Working set up ----------------------------
library(readxl)
library(reshape2)
library(tidyverse)

# Parameters
temporal_period <- c(2010:2020)
gdrive_fpath <- file.path("/Volumes/GoogleDrive-112161833434429421879/My Drive/Project") ##change this to point at google drive path
raw_data_fpath <- file.path(gdrive_fpath, 'Data', 'Raw')
processed_data_fpath <- file.path(gdrive_fpath, 'Data', 'Processed')
output_path <- file.path(processed_data_fpath, 'homicide_rates.csv')

# 1. Load data ---------------------------------
# * 1.1 Nicaragua Departamento-Annual Homicide Rates -----------
nicaragua_rates <- read_csv(
  file.path(processed_data_fpath, 'nicaragua_departamento_year_2007_2020.csv'))

# Preprocess
nicaragua_rates <- nicaragua_rates %>% 
  filter(Year %in% temporal_period) %>% 
  mutate(Municipio=NA) %>%
  mutate(Population=NA) %>%
  mutate(Departamento=Departamentos) %>% select(-Departamentos) %>%
  mutate(
    Country='Nicaragua', 
    Departamento = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Departamento)))

# Add a department code
nicaragua_rates <- mutate(nicaragua_rates, code = NA) # TODO

# * 1.2 El Salvador Municipality-Annual Homicide Rates (preprocessed) -----------
elsalvador_rates <- read_csv(file.path(processed_data_fpath, 'elsalvador_homicide_rates.csv'))

# * 1.3 Guatemala Municipality-Annual Homicide Rates (preprocessed) -----------
guatemala_rates <- read.csv(file.path(processed_data_fpath, 'guatemala_homicide_rates.csv'))

# * 1.4 Honduras Municipality-Annual Homicide Rates (2013-2020) -----------
honduras_rates <- read.csv(
  file.path(processed_data_fpath, 'honduras_homicide_rates.csv'))

# 3. Consolidate -----------------------------
guatemala_rates <- guatemala_rates %>% mutate(Country ='Guatemala')
elsalvador_rates <- elsalvador_rates %>% mutate(Country ='El Salvador')
honduras_rates <- honduras_rates %>% mutate(Country ='Honduras')

homicide_rates <- rbind(
  select(nicaragua_rates, c(code, Country, Departamento, Municipio, Year, hom_rate_100k, Population)),
  select(guatemala_rates, c(code, Country, Departamento, Municipio, Year, hom_rate_100k, Population)),
  select(honduras_rates, c(code, Country, Departamento, Municipio, Year, hom_rate_100k, Population)),
  select(elsalvador_rates, c(code, Country, Departamento, Municipio, Year, hom_rate_100k, Population)))

# 5. Save ------------------------------------
write.csv(homicide_rates, output_path, row.names = FALSE)

