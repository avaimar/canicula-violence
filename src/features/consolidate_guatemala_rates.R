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
output_path <- file.path(processed_data_fpath, 'guatemala_homicide_rates.csv')
pop_projection_type <- c('old', 'new')

# 1. Load and prep homicide data ---------------------------------
# * 1.1 Guatemala Municipality-Annual Homicide Counts (2001-2019)
guatemala_2001_2019 <- read_excel(
  file.path(raw_data_fpath, 'Homicides', 'Guatemala', 
            'consolidado homicidios por municipios por tipo de arma y sexo del 2001 al 2019 PNC (version corregida 18nov 2020).xlsx'),
  sheet=2)

# Select homicide totals
guatemala_2001_2019 <- filter(guatemala_2001_2019, `medio/sexo` == 'todos') %>% 
  select(-`medio/sexo`)

# Melt
guatemala_2001_2019 <- reshape2::melt(
  guatemala_2001_2019, id.vars = c('code', 'dpto', 'muni'), variable.name='Year', 
  value.name = 'homicide_count')

# Select period
guatemala_2001_2019 <- filter(guatemala_2001_2019, Year %in% temporal_period)

# Remove accents
guatemala_2001_2019 <- guatemala_2001_2019 %>% 
  mutate(Departamento=chartr("ÁÉÍÓÚ", "AEIOU", toupper(dpto))) %>%
  mutate(Municipio=chartr("ÁÉÍÓÚ", "AEIOU", toupper(muni))) %>%
  select(-c(dpto, muni)) %>%
  mutate(Year = as.numeric(levels(Year)[Year]))

# * 1.2 Guatemala Municipality-Annual Homicide Counts (2020)
guatemala2020 <- read.csv(file.path(processed_data_fpath, "guatemala_homicides_2020.csv"))

# Add 2020 homicide counts
guatemala2020 <- guatemala2020 %>%
  mutate(Departamento = Departament, Municipio=Municipality, homicide_count=Homicide) %>%
  select(Year, homicide_count, Departamento, Municipio)

# 2. Merge 2020 and pre-2020 homicide data --------------------
guatemala_dict <- distinct(guatemala_2001_2019, code, Departamento, Municipio)

# Note: there are 340 municipalities in both the dictionary and the 2020 data
guatemala2020_temp <- left_join(
  guatemala2020, guatemala_dict, by = c('Departamento', 'Municipio'))

# Check missing municipalities and correct
#colSums(is.na(guatemala2020_temp))
missings <- filter(guatemala2020_temp, is.na(code))

guatemala2020 <- guatemala2020 %>%
  mutate(
    Municipio = case_when(
      Departamento == 'GUATEMALA' & Municipio == "SAN MIGUEL PETAPA" ~ "PETAPA",
      Departamento == 'QUICHE' & Municipio == "SANTO TOMAS CHICHICASTENANGO" ~ "CHICHICASTENANGO",
      Departamento == 'HUEHUETENANGO' & Municipio == "SANTA CRUZ BARILLAS" ~ "BARILLAS",
      Departamento == 'QUICHE' & Municipio == "PLAYA GRANDE IXCAN" ~ "IXCAN",
      Departamento == 'QUICHE' & Municipio == "SANTA MARIA NEBAJ" ~ "NEBAJ",
      Departamento == 'QUICHE' & Municipio == "SAN MIGUEL USPANTAN" ~ "USPANTAN",
      Departamento == 'ALTA VERAPAZ' & Municipio == "SANTA MARIA CAHABON" ~ "CAHABON",
      Departamento == 'HUEHUETENANGO' & Municipio == "SAN PEDRO SOLOMA" ~ "SOLOMA",
      Departamento == 'QUETZALTENANGO' & Municipio == "SAN JUAN OSTUNCALCO" ~ "OSTUNCALCO",
      Departamento == 'CHIMALTENANGO' & Municipio == "SAN JUAN COMALAPA" ~ "COMALAPA",
      Departamento == 'QUETZALTENANGO' & Municipio == "COLOMBA COSTA CUCA" ~ "COLOMBA",
      Departamento == 'HUEHUETENANGO' & Municipio == "SAN ILDEFONSO IXTAHUACAN" ~ "IXTAHUACAN",
      Departamento == 'ALTA VERAPAZ' & Municipio == "SAN MIGUEL TUCURU" ~ "TUCURU",
      Departamento == 'SAN MARCOS' & Municipio == "AYUTLA" ~ "AYUTLA O TECUN UMAN",
      Departamento == 'CHIMALTENANGO' & Municipio == "SAN PEDRO YEPOCAPA" ~ "YEPOCAPA",
      Departamento == 'QUETZALTENANGO' & Municipio == "SAN JUAN OLINTEPEQUE" ~ "OLINTEPEQUE",
      Departamento == 'CHIQUIMULA' & Municipio == "QUEZALTEPEQUE" ~ "QUETZALTEPEQUE",
      Departamento == 'SACATEPEQUEZ' & Municipio == "SAN JUAN ALOTENANGO" ~ "ALOTENANGO",
      Departamento == 'ALTA VERAPAZ' & Municipio == "SAN AGUSTIN LANQUIN" ~ "LANQUIN",
      Departamento == 'SAN MARCOS' & Municipio == "SAN JOSE EL RODEO" ~ "EL RODEO",
      Departamento == 'CHIQUIMULA' & Municipio == "SAN JUAN ERMITA" ~ "SAN JUAN ERMINTA",
      Departamento == 'TOTONICAPAN' & Municipio == "SAN BARTOLO AGUAS CALIENTES" ~ "SAN BARTOLO",
      Departamento == 'CHIMALTENANGO' & Municipio == "SAN MIGUEL POCHUTA" ~ "POCHUTA",
      Departamento == 'BAJA VERAPAZ' & Municipio == "SANTA CRUZ EL CHOL" ~ "EL CHOL",
      TRUE ~ Municipio
    )
  )

guatemala2020 <- left_join(
  guatemala2020, guatemala_dict, by = c('Departamento', 'Municipio'))
guatemala2020 <- select(guatemala2020, code, Year, homicide_count, Departamento, Municipio)

# Check missings and uniqueness
colSums(is.na(guatemala2020))
length(unique(guatemala2020$code)) == dim(guatemala_dict)[1]

# Concatenate 
guatemala <- rbind(guatemala_2001_2019, guatemala2020)
rm(guatemala_2001_2019, guatemala2020, guatemala2020_temp)

# 3. Load and process required population data -----------

if (pop_projection_type == 'old') {
  guatemala_pop <- read.csv(file.path(processed_data_fpath, 'guatemala_population_counts.csv'))
  print('[INFO] Old population projections only include 333 municipalities.')
} else if (pop_projection_type == 'new') {
  stop('[ERROR] Usage of new population counts have not been updated.')
  guatemala_pop <- read_excel(
    file.path(raw_data_fpath, 'Population', 'Guatemala', 'proyecciones-poblacion-2014-2022v3.xlsx'),
    skip=4, n_max=3050, range='A5:F3055')
  
  guatemala_pop <- guatemala_pop %>% filter(!is.na(`Año`)) %>% 
    select(-`Area de Salud`) %>%
    mutate(Year = as.factor(`Año`), dpto=Departamento) %>% 
    select(-c(`Año`, Departamento, `Cod.\r\nDepto.`))
  
  # Corrections
  guatemala_pop <- guatemala_pop %>% 
    mutate(Municipio = replace(Municipio, Municipio == "SAN AGUSTÍN ACASAGUSTLÁN", "SAN AGUSTÍN ACASAGUASTLÁN"))
  guatemala_pop <- mutate(guatemala_pop, Municipio = replace(Municipio, Municipio == "SAN JUAN OSTUNCALCO", "OSTUNCALCO"))
  guatemala_pop <- mutate(guatemala_pop, Municipio = replace(Municipio, Municipio == "SAN MIGUEL PETAPA", "PETAPA"))
  guatemala_pop <- mutate(guatemala_pop, Municipio = replace(Municipio, Municipio == "SAN PEDRO SOLOMA", "SOLOMA"))
  guatemala_pop <- mutate(guatemala_pop, Municipio = replace(Municipio, Municipio == "SAN MIGUEL USPANTÁN", "USPANTÁN"))
  guatemala_pop <- mutate(guatemala_pop, Municipio = replace(Municipio, Municipio == "SANTA CRUZ BARILLAS", "BARILLAS"))
  guatemala_pop <- mutate(guatemala_pop, dpto = replace(dpto, dpto == "EL PETEN", "PETEN"))
  
  # Remove accents
  guatemala_pop <- guatemala_pop %>% mutate(dpto=chartr("ÁÉÍÓÚ", "AEIOU", toupper(dpto))) %>%
    mutate(Municipio=chartr("ÁÉÍÓÚ", "AEIOU", toupper(Municipio)))
  
} else {
  stop('[ERROR] Select population projections.')
}

# Filter temporal period
guatemala_pop <- filter(guatemala_pop, Year %in% temporal_period)

# Get municipality codes
guatemala_pop_temp <- distinct(guatemala_pop, Departamento, Municipio)
guatemala_pop_temp <- left_join(
  guatemala_pop_temp, guatemala_dict, by = c('Departamento', 'Municipio'))

#colSums(is.na(guatemala_pop_temp))
missings <- filter(guatemala_pop_temp, is.na(code))

guatemala_pop <- guatemala_pop %>%
  mutate(
    Municipio = case_when(
      Departamento == 'GUATEMALA' & Municipio == "CHINUAUTLA" ~ "CHINAUTLA",
      Departamento == 'GUATEMALA' & Municipio == "SAN RAIMUNDO" ~ "SAN RAYMUNDO",
      Departamento == 'EL PROGRESO' & Municipio == "SAN AGUSTIN ACASAGUSTLAN" ~ "SAN AGUSTIN ACASAGUASTLAN",
      Departamento == 'SACATEPEQUEZ' & Municipio == "SANTIAGO SACTEPEQUEZ" ~ "SANTIAGO SACATEPEQUEZ",
      Departamento == 'SACATEPEQUEZ' & Municipio == "SAN BARTOLOME" ~ "SAN BARTOLOME MILPAS ALTAS",
      Departamento == 'ESCUINTLA' & Municipio == "TIQUIZATE" ~ "TIQUISATE",
      Departamento == 'SOLOLA' & Municipio == "SANTA CATALINA IXTAHUACAN" ~ "SANTA CATARINA IXTAHUACAN",
      Departamento == 'QUETZALTENANGO' & Municipio == "SAN JUAN OSTUNCALCO" ~ "OSTUNCALCO",
      Departamento == 'SAN MARCOS' & Municipio == "AYUTLA" ~ "AYUTLA O TECUN UMAN",
      Departamento == 'HUEHUETENANGO' & Municipio == "SAN IDELFONSO IXTAHUACAN" ~ "IXTAHUACAN",
      Departamento == 'HUEHUETENANGO' & Municipio == "CONCEPCION" ~ "CONCEPCION HUISTA",
      Departamento == 'HUEHUETENANGO' & Municipio == "SANTA CRUZ BARILLAS" ~ "BARILLAS",
      Departamento == 'HUEHUETENANGO' & Municipio == "SANTIAGO CHIMALTENANANGO" ~ "SANTIAGO CHIMALTENANGO",
      Departamento == 'ALTA VERAPAZ' & Municipio == "LA TINTA" ~ "SANTA CATALINA LA TINTA",
      Departamento == 'CHIQUIMULA' & Municipio == "SAN JUAN ERMITA" ~ "SAN JUAN ERMINTA",
      Departamento == 'CHIQUIMULA' & Municipio == "QUEZALTEPEQUE" ~ "QUETZALTEPEQUE",
      Departamento == 'JUTIAPA' & Municipio == "ACATEMPA" ~ "SAN JOSE ACATEMPA",
      Departamento == 'JUTIAPA' & Municipio == "QUEZADA" ~ "QUESADA",
      TRUE ~ Municipio
    )
  )

guatemala_pop <- left_join(
  guatemala_pop, guatemala_dict, by = c('Departamento', 'Municipio'))

# Check missings and uniqueness
colSums(is.na(guatemala_pop))
length(unique(guatemala_pop$code))

# 4. Merge homicide and population data -----------------------------------
guatemala_rates <- left_join(
  guatemala, 
  select(guatemala_pop, code, Year, Population), by = c('code', 'Year'))

# Note: 77 missing values are related to the 7 municipalities that were
# not included in the population projections. We drop these.
length(unique(filter(guatemala_rates, is.na(Population))$code))
guatemala_rates <- filter(guatemala_rates, !is.na(Population))

# Compute homicide rate per 100,000 inhabitants
guatemala_rates <- guatemala_rates %>%
  mutate(guatemala_rates, hom_rate_100k = homicide_count / Population * 100000) %>%
  select(-c(homicide_count))

# 5. Save ------------------------------------
write.csv(guatemala_rates, output_path, row.names = FALSE)

# 6. Validate --------------------------------
# * Check national trends
national_trend <- guatemala_rates %>% group_by(Year) %>% 
  summarise(nat_rate = weighted.mean(hom_rate_100k, Population))
ggplot(national_trend, aes(x=Year, y=nat_rate)) + geom_line()
# Note: trend follows national reports (p. 2) found at
# https://mingob.gob.gt/wp-content/uploads/2021/02/Infografi%CC%81a-Ana%CC%81lisis-de-Seguridad-Ciudadana-2019.pdf

# * Check outliers
head(arrange(guatemala_rates, desc(hom_rate_100k)))

# SAN JOSE (PETEN) rate of 533. This is directly in line with police
# reports for 2020 (see raw data, page. 103). Note that rates in reports
# are computed per 10k inhabitants. Slight difference stems from use of 
# old population projections for 2020. 

# Numbers for San Jose (Escuintla) and Nueva Concepcion match those of Dialogos
# https://www.dialogos.org.gt/sites/default/files/2020-03/Informe-SEMESTRAL-sobre-la-Violencia-Homicida-en-Guatemala-2018-ver-FINAL.pdf
