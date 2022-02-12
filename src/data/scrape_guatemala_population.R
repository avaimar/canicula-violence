# 0. Working set up ----------------------------------------
library(pdftools)
library(stringr)
library(reshape)
library(tidyverse)

# Parameters
gdrive_fpath <- file.path("/Volumes/GoogleDrive-112161833434429421879/My Drive/Project")
raw_data_fpath <- file.path(gdrive_fpath, '/Data/Raw/Population/Guatemala/Population_estimates.pdf')
output_fpath <- file.path(gdrive_fpath, 'Data/Processed/guatemala_population_counts.csv')

# 1. Load data ----------------------------------
data <- pdftools::pdf_text(raw_data_fpath)

# 2. Process each page ------------------------------------
table.names <- c('Municipio', 2008:2020, 'Departamento', 'Unit_type')
population <- data.frame()

for (data_page in data) {
  # Extract the tables
  page_table <- strsplit(data_page, split='2020\n\n')[[1]][2]
  
  # Get each row
  rows <- strsplit(page_table, "\n")
  
  # Get data.frame
  df <- as_tibble(data.frame(str_split_fixed(rows[[1]], '[ \t]{2,}' ,n=Inf)))
  
  # Extract departamento 
  df$Departamento <- NA
  df$Unit_type <- NA
  cur_departamento <- df$X1[1]
  white_space <- TRUE
  
  for (i in 1:length(df$Departamento)) {
    if (df$X1[i] != "") {
      if (white_space == FALSE) {
        df$Departamento[i] <- cur_departamento
        df$Unit_type[i] <- 'Municipio'
      } else {
        cur_departamento <- df$X1[i]
        df$Unit_type[i] <- 'Departamento'
        white_space <- FALSE
      }
    } else {
      white_space <- TRUE
    }
  }
  
  # Drop empty rows and departamento rows
  df <- df %>% filter(X1 != "") %>% filter(Unit_type != 'Departamento')
  
  # Attach to homicide table
  population <- rbind(population, df)
}

colnames(population) <- table.names

# Reshape
population <- reshape2::melt(
  population, id.vars=c('Departamento', 'Municipio', 'Unit_type'), variable.name='Year')

# Reformat department, municipality names, add year and format population counts
population <- population %>% 
  mutate(Departamento = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Departamento))) %>%
  mutate(Municipio = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Municipio))) %>%
  mutate(Population = as.numeric(gsub(',', '', value))) %>%
  select(-c(value, Unit_type))

# Check for missing values
colSums(is.na(population))

# Save
write.csv(population, output_fpath, row.names=FALSE)

# Note: Projections include only 333 municipalities vs 340 in total
