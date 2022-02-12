# 0. Working set up ----------------------------
library(ggplot2)
library(plyr)
library(readxl)
library(reshape2)
library(tidyverse)

# Parameters
temporal_period <- c(2010:2020)
gdrive_fpath <- file.path("/Volumes/GoogleDrive-112161833434429421879/My Drive/Project") ##change this to point at google drive path
raw_data_fpath <- file.path(gdrive_fpath, 'Data', 'Raw')
processed_data_fpath <- file.path(gdrive_fpath, 'Data', 'Processed')
output_path <- file.path(processed_data_fpath, 'honduras_homicide_rates.csv')

# 1. Load and preprocess data ---------------------------------
# * Honduras Municipality-Annual Homicide Counts (2013-2020)
honduras <- read.csv(
  file.path(raw_data_fpath,'Homicides', 'Honduras', 'honduras_homicides.csv'))

# Get rid of accents to merge with population data
honduras <- honduras %>% mutate(
  Municipio = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Mun)), 
  Departamento = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Dep))) %>%
  select(-c(Mun, Dep))

# * Honduras population data
honduras_pop <- read_excel(
  file.path(raw_data_fpath, 'Population', 'Honduras', 'reporte.xls'), skip=11, 
  col_names=c('Mun', 2013:2030, 'Total'), n_max=2280)

# Preprocess population data
honduras_pop <- honduras_pop %>% filter(!is.na(Mun)) %>% 
  filter(!Mun %in% c('Area', 'Urbano', 'Rural'))

honduras_pop <- cbind(filter(honduras_pop, Mun != 'Total') %>% mutate(Municipio =`2013`) %>% select(Mun, Municipio), 
                      filter(honduras_pop, !is.na(`2014`)) %>% select(-Mun))
honduras_pop <- honduras_pop %>% select(
  -c(`2021`, `2022`, `2023`, `2024`, `2025`, `2026`, `2027`, `2028`, `2029`, `2030`, 'Total'))

# Melt
honduras_pop <- reshape2::melt(honduras_pop, id.vars = c('Mun', 'Municipio'), variable.name='Year', value.name='Population')
honduras_pop <- honduras_pop %>% 
  mutate(code = gsub("AREA # ","", Mun)) %>%
  select(-Mun) %>% mutate(Year = as.numeric(levels(Year))[Year]) %>%
  mutate(Population = as.numeric(Population)) %>%
  mutate(
    Municipio = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Municipio)))

# Drop non-municipality rows
honduras_pop <- filter(honduras_pop, code != 'RESUMEN')
honduras_pop <- mutate(honduras_pop, dep_code = as.numeric(substr(code, 1, 2)))

# Get departamentos for population data
honduras_dept_list <- unique(honduras$Departamento)
honduras_pop <- mutate(honduras_pop, Departamento = (function(x) {honduras_dept_list[dep_code]})(.))

# 2. Merge homicide and population data -----------------------------------
honduras_dict <- distinct(honduras_pop, code, Municipio, Departamento)

# Get municipality codes for homicide counts
honduras_temp <- left_join(honduras, honduras_dict, by=c('Municipio', 'Departamento'))
colSums(is.na(honduras_temp))
distinct(filter(honduras_temp, is.na(code)), Municipio, Departamento )

# Correct the 2 municipalities
honduras <- honduras %>%
  mutate(
    Municipio = case_when(
      Departamento == 'COPAN' & Municipio == "SAN PEDRO DE COPAN" ~ "SAN PEDRO",
      Departamento == 'EL PARAISO' & Municipio == "GÜINOPE" ~ "GUINOPE",
      TRUE ~ Municipio
    )
  )

# Join and check missing values, unique values
honduras <- left_join(honduras, honduras_dict, by=c('Municipio', 'Departamento'))
colSums(is.na(honduras))
length(unique(honduras$code)) == dim(honduras_dict)[1]

# Merge with population data
honduras_rates <- left_join(
  honduras, select(honduras_pop, code, Year, Population), by=c('code', 'Year'))

# No missing population values
colSums(is.na(honduras_rates))

# 3. Compute rates ---------------------------
honduras_rates <- honduras_rates %>%
    mutate(hom_rate_100k = Count / Population * 100000) %>%
    select(code, Year, Municipio, Departamento, hom_rate_100k, Population, Count)

# 4. Save ------------------------------------
write.csv(honduras_rates, output_path, row.names = FALSE)

# 5. Validate --------------------------------
national_trend <- honduras_rates %>% group_by(Year) %>% 
  dplyr::summarise(nat_rate = weighted.mean(hom_rate_100k, Population))
ggplot(national_trend, aes(x=Year, y=nat_rate)) + geom_line()
# Note: trend follows national reports (p. 2) found at
# https://www.sepol.hn/artisistem/images/sepol-images/files/PDF/Tasa%20de%20homicidios%20por%20cada%20100%20mil%20habitantes%202011-2022(1).pdf

# * Check outliers
head(arrange(honduras_rates, desc(hom_rate_100k)))
