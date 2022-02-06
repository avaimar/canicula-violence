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
nicaragua_rates <- read_csv(file.path(processed_data_fpath, 'nicaragua_departamento_year_2007_2020.csv'))
nicaragua_rates <- filter(nicaragua_rates, Year %in% temporal_period)
nicaragua_rates <- nicaragua_rates %>% mutate(Municipio=NA) %>%
  mutate(Departamento=Departamentos) %>% select(-Departamentos) %>%
  mutate(Country='Nicaragua')

# * 1.2 El Salvador Municipality-Annual Homicide Counts -----------
elsalvador <- read_csv(file.path(processed_data_fpath, 'elsalvador_municipality_homicidecount_2011_2020.csv'))
elsalvador <- elsalvador %>% mutate(Municipio = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Municipio)) )
elsalvador <- elsalvador %>% mutate(Departamento = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Departamento) ))

# * 1.3 Guatemala Municipality-Annual Homicide Counts -----------
guatemala <- read_excel(
  file.path(raw_data_fpath, 'Homicides', 'Guatemala', 
            'consolidado homicidios por municipios por tipo de arma y sexo del 2001 al 2019 PNC (version corregida 18nov 2020).xlsx'),
  sheet=2)

# Select homicide totals
guatemala <- filter(guatemala, `medio/sexo` == 'todos') %>% select(-`medio/sexo`)

# Melt
guatemala <- reshape2::melt(guatemala, id.vars = c('code', 'dpto', 'muni'), variable.name='Year', value.name = 'homicide_count')

# Select period
guatemala <- filter(guatemala, Year %in% temporal_period)
guatemala <- mutate(guatemala, Municipio = toupper(muni))

# Remove accents
guatemala <- guatemala %>% mutate(dpto=chartr("ÁÉÍÓÚ", "AEIOU", dpto)) %>%
  mutate(Municipio=chartr("ÁÉÍÓÚ", "AEIOU", Municipio))

# * 1.4 Honduras Municipality-Annual Homicide Counts (2013-2020) -----------
honduras <- read.csv(file.path(raw_data_fpath,'Homicides', 'Honduras', 'honduras_homicides.csv'))

# Get rid of accents to merge with population data
honduras <- honduras %>% mutate(Mun = chartr("ÁÉÍÓÚ", "AEIOU", Mun) )

# 2. Load required population data -----------
# * 2.1 El Salvador -----------
elsalvador_pop <- read_csv(file.path(raw_data_fpath, 'Population', 'El_Salvador', 'population_estimates.csv'))
elsalvador_pop <- elsalvador_pop %>% 
  mutate(Municipio = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Municipality)) ) %>% 
  mutate(Departamento = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Departamento) )) %>%
  select(-Municipality) %>%
  filter(Year %in% temporal_period)

# * 2.2 Guatemala -----------
guatemala_pop <- read_excel(
  file.path(raw_data_fpath, 'Population', 'Guatemala', 'proyecciones-poblacion-2014-2022v3.xlsx'),
  skip=4, n_max=3050, range='A5:F3055')
guatemala_pop <- guatemala_pop %>% filter(!is.na(`Año`)) %>% 
  select(-`Area de Salud`) %>%
  mutate(Year = as.factor(`Año`), dpto=Departamento) %>% 
  select(-c(`Año`, Departamento, `Cod.\r\nDepto.`))

# Corrections
guatemala_pop <- mutate(guatemala_pop, Municipio = replace(Municipio, Municipio == "SAN AGUSTÍN ACASAGUSTLÁN", "SAN AGUSTÍN ACASAGUASTLÁN"))
guatemala_pop <- mutate(guatemala_pop, Municipio = replace(Municipio, Municipio == "SAN JUAN OSTUNCALCO", "OSTUNCALCO"))
guatemala_pop <- mutate(guatemala_pop, Municipio = replace(Municipio, Municipio == "SAN MIGUEL PETAPA", "PETAPA"))
guatemala_pop <- mutate(guatemala_pop, Municipio = replace(Municipio, Municipio == "SAN PEDRO SOLOMA", "SOLOMA"))
guatemala_pop <- mutate(guatemala_pop, Municipio = replace(Municipio, Municipio == "SAN MIGUEL USPANTÁN", "USPANTÁN"))
guatemala_pop <- mutate(guatemala_pop, Municipio = replace(Municipio, Municipio == "SANTA CRUZ BARILLAS", "BARILLAS"))
guatemala_pop <- mutate(guatemala_pop, dpto = replace(dpto, dpto == "EL PETEN", "PETEN"))

# Remove accents
guatemala_pop <- guatemala_pop %>% mutate(dpto=chartr("ÁÉÍÓÚ", "AEIOU", dpto)) %>%
  mutate(Municipio=chartr("ÁÉÍÓÚ", "AEIOU", Municipio))

# * 2.3 Honduras -----------
honduras_pop <- read_excel(
  file.path(raw_data_fpath, 'Population', 'Honduras', 'reporte.xls'), skip=11, 
  col_names=c('Mun', 2013:2030, 'Total'), n_max=2280)
honduras_pop <- honduras_pop %>% filter(!is.na(Mun)) %>% 
  filter(!Mun %in% c('Area', 'Urbano', 'Rural'))

honduras_pop <- cbind(filter(honduras_pop, Mun != 'Total') %>% mutate(Municipio =`2013`) %>% select(Mun, Municipio), 
                      filter(honduras_pop, !is.na(`2014`)) %>% select(-Mun))
honduras_pop <- honduras_pop %>% select(
  -c(`2021`, `2022`, `2023`, `2024`, `2025`, `2026`, `2027`, `2028`, `2029`, `2030`, 'Total'))

# Melt
honduras_pop <- reshape2::melt(honduras_pop, id.vars = c('Mun', 'Municipio'), variable.name='Year', value.name='Population')
honduras_pop <- honduras_pop %>% 
  mutate(Dep = gsub("AREA # ","", Mun)) %>%
  mutate(Mun = Municipio) %>% 
  select(-Municipio) %>% mutate(Year = as.numeric(levels(Year))[Year]) %>%
  mutate(Population = as.numeric(Population))

# Drop non-municipality rows
honduras_pop <- filter(honduras_pop, Dep != 'RESUMEN')
honduras_pop <- mutate(honduras_pop, Dep = as.numeric(substr(Dep, 1, 2)))

# Get departamentos for population data
honduras_dept_list <- unique(honduras$Dep)
honduras_pop <- mutate(honduras_pop, Dep = (function(x) {honduras_dept_list[Dep]})(.))

# 3. Merge -----------------------------------
# * 3.1 El Salvador -----------
elsalvador_rates <- left_join(elsalvador, elsalvador_pop, by = c('Departamento', 'Municipio', 'Year'))
elsalvador_rates <- mutate(elsalvador_rates, hom_rate_100k = hom_count / Population * 100000)

# * 3.2 Guatemala -----------
guatemala_rates <- left_join(guatemala, guatemala_pop, by = c('dpto', 'Municipio', 'Year'))
guatemala_rates <- guatemala_rates %>% filter(Year %in% 2014:2020)
guatemala_rates <- filter(guatemala_rates, !is.na(Total)) # TODO check remaining municipalities
guatemala_rates <- mutate(guatemala_rates, hom_rate_100k = homicide_count / Total * 100000)

# * 3.3 Honduras -----------
honduras_rates <- left_join(honduras, honduras_pop, by=c('Mun','Dep', 'Year'))
honduras_rates <- filter(honduras_rates, !is.na(Population))
honduras_rates <- mutate(honduras_rates, hom_rate_100k = Count / Population * 100000)

# 4. Consolidate -----------------------------
guatemala_rates <- guatemala_rates %>% mutate(Country ='Guatemala') %>%
  mutate(Departamento = dpto, Municipio=muni)

elsalvador_rates <- elsalvador_rates %>% mutate(Country ='El Salvador')

honduras_rates <- honduras_rates %>% mutate(Country ='Honduras') %>%
  mutate(Departamento = Dep, Municipio=Mun)

homicide_rates <- rbind(
  select(nicaragua_rates, c(Country, Departamento, Municipio, Year, hom_rate_100k)),
  select(guatemala_rates, c(Country, Departamento, Municipio, Year, hom_rate_100k)),
  select(honduras_rates, c(Country, Departamento, Municipio, Year, hom_rate_100k)),
  select(elsalvador_rates, c(Country, Departamento, Municipio, Year, hom_rate_100k)))

# 5. Save ------------------------------------
write.csv(homicide_rates, output_path, row.names = FALSE)
