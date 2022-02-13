# 0. Working set up ----------------------------
library(ggplot2)
library(readxl)
library(reshape2)
library(tidyverse)

# Parameters
temporal_period <- c(2010:2020)
gdrive_fpath <- file.path("/Volumes/GoogleDrive-112161833434429421879/My Drive/Project") ##change this to point at google drive path
raw_data_fpath <- file.path(gdrive_fpath, 'Data', 'Raw')
processed_data_fpath <- file.path(gdrive_fpath, 'Data', 'Processed')
output_path <- file.path(processed_data_fpath, 'elsalvador_homicide_rates.csv')

# 1. Load data ---------------------------------
# * El Salvador Municipality-Annual Homicide Counts
elsalvador <- read_csv(
  file.path(processed_data_fpath, 'elsalvador_municipality_homicidecount_2011_2020.csv'))

# Preprocess
elsalvador <- elsalvador %>% 
  mutate(Municipio = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Municipio)), 
         Departamento = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Departamento)))

# * Population data
elsalvador_pop <- read_csv(
  file.path(raw_data_fpath, 'Population', 'El_Salvador', 'population_estimates.csv'))

# Preprocess
elsalvador_pop <- elsalvador_pop %>% 
  mutate(Municipio = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Municipality)) ) %>% 
  mutate(Departamento = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Departamento) )) %>%
  select(-Municipality) %>%
  filter(Year %in% temporal_period)

# Add department code
dept_list <- c("AHUACHAPAN", "SANTA ANA", "SONSONATE", "CHALATENANGO", 
               "LA LIBERTAD", "SAN SALVADOR", "CUSCATLAN", "LA PAZ", "CABAÑAS",   
               "SAN VICENTE" , "USULUTAN", "SAN MIGUEL", "MORAZAN", "LA UNION")
elsalvador_pop <- elsalvador_pop %>% 
  mutate(dep_code = stringr::str_replace_all(
    Departamento, setNames(as.character(1:14), dept_list)))

# Make full municipality code
elsalvador_pop <- elsalvador_pop %>%
  mutate(code = paste0(dep_code, Mun_code))

# 2. Merge -----------------------------------
elsalvador_dict <- distinct(elsalvador_pop, code, Municipio, Departamento)

elsalvador_temp <- left_join(
  elsalvador, elsalvador_dict, by = c('Departamento', 'Municipio'))
colSums(is.na(elsalvador_temp))
length(unique(filter(elsalvador_temp, is.na(code))$Municipio))
missings <- distinct(filter(elsalvador_temp, is.na(code)), Municipio, Departamento)

# Correct 30 municipalities
elsalvador <- elsalvador %>%
  mutate(
    Municipio = case_when(
      Departamento == 'LA LIBERTAD' & Municipio == "ANTIGUO CUSCATLAN" ~ "ANTIGUO CUSCATAN",
      Departamento == 'LA LIBERTAD' & Municipio == "CHITIUPAN" ~ "CHILTIUPAN",
      Departamento == 'SAN SALVADOR' & Municipio == "CIUDAD DELGADO" ~ "DELGADO",
      Departamento == 'LA UNION' & Municipio == "MEANGARA DEL GOLFO" ~ "MEANGUERA DEL GOLFO",
      Departamento == 'LA UNION' & Municipio == "YUCUALQUIN" ~ "YUCUAIQUIN",
      Departamento == 'SAN MIGUEL' & Municipio == "SAN RAFAEL" ~ "SAN RAFAEL ORIENTE",
      Departamento == 'SAN MIGUEL' & Municipio == "SAN LUIS REINA" ~ "SAN LUIS DE LA REINA",
      Departamento == 'SAN MIGUEL' & Municipio == "SAN ANTONIO DEL MOSCO" ~ "SAN ANTONIO",
      Departamento == 'SAN MIGUEL' & Municipio == "CUIDAD BARRIOS" ~ "CIUDAD BARRIOS",
      Departamento == 'SAN MIGUEL' & Municipio == "QUELAPA" ~ "QUELEPA",
      Departamento == 'MORAZAN' & Municipio == "JOCOATIQUE" ~ "JOCOAITIQUE",
      Departamento == 'SAN VICENTE' & Municipio == "TEPETITLAN" ~ "TEPETITAN",
      Departamento == 'SAN VICENTE' & Municipio == "SAN IDELFONSO" ~ "SAN ILDEFONSO",
      Departamento == 'CHALATENANGO' & Municipio == "POTANICO" ~ "POTONICO",
      Departamento == 'CHALATENANGO' & Municipio == "SAN JOSE LAS FLORES" ~ "LAS FLORES",
      Departamento == 'CHALATENANGO' & Municipio == "CONCEPCION QUEZALTEPQUE" ~ "CONCEPCION QUEZALTEPEQUE",
      TRUE ~ Municipio
    )
  )

# Join homicides and code
elsalvador <- left_join(
  elsalvador, elsalvador_dict, by = c('Departamento', 'Municipio'))

# Check missing values and unique codes
colSums(is.na(elsalvador))

# Unable to find the following 14 municipalities
length(unique(filter(elsalvador, is.na(code))$Municipio))
print('[INFO] Unable to find 14 municipalities: ')
print(unique(filter(elsalvador, is.na(code))$Municipio))

# Drop these 14 municipalities
elsalvador <- filter(elsalvador, !is.na(code))

# Merge homicides and rates
elsalvador_rates <- left_join(
  elsalvador, 
  select(elsalvador_pop, code, Year, Population),
  by = c('code', 'Year'))

# Check
colSums(is.na(elsalvador_rates))

# 3. Compute rates --------------------------
elsalvador_rates <- mutate(
  elsalvador_rates, hom_rate_100k = hom_count / Population * 100000)

# 4. Save ------------------------------------
write.csv(elsalvador_rates, output_path, row.names = FALSE)

# 5. Validate --------------------------------
# * Check national trends
national_trend <- elsalvador_rates %>% group_by(Year) %>% 
  dplyr::summarise(nat_rate = weighted.mean(hom_rate_100k, Population))
ggplot(national_trend, aes(x=Year, y=nat_rate)) + geom_line()
# 2015 data in line with https://www.swissinfo.ch/spa/el-salvador-homicidios_homicidios-se-elevan-en-el-salvador-m%C3%A1s-del-10---en-primer-semestre-de-2021/46793546#:~:text=En%202015%20el%20pa%C3%ADs%20centroamericano,bajaron%20a%203.346%20en%202018.
# Trend generally matches World Bank data: https://data.worldbank.org/indicator/VC.IHR.PSRC.P5?end=2018&locations=SV&start=1994&view=chart

# * Check outliers
head(arrange(elsalvador_rates, desc(hom_rate_100k)))

# Comacaran: https://www.laprensagrafica.com/elsalvador/Estructura-vinculada-con-14-homicidios-20190610-0432.html