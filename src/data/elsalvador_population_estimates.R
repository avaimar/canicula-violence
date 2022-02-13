# 0. Working set up ----------------------------------------
library(stringr)
library(reshape)
library(pdftools)
library(tidyverse)
library(tidyr)

# 1. Load data ----------------------------------
gdrive_fpath <- file.path("/Volumes/GoogleDrive-112161833434429421879/My Drive/Project") ##change this to point at google drive path
raw_data_fpath <- file.path(gdrive_fpath, 'Data', 'Raw')
processed_data_fpath <- file.path(gdrive_fpath, 'Data', 'Processed')
homicides <- read_csv(file.path(processed_data_fpath, 'elsalvador_municipality_homicidecount_2011_2020.csv'))
data <- pdftools::pdf_text(file.path(raw_data_fpath, 'Population', 'El_Salvador', 'Proyecciones_Municipales_tables.pdf'))

# 2. Process ------------------------------------
departments <- unique(homicides$Departamento)
population.table <- data.frame()
row_split <- c("\n\n\n", "\n\n\n", "\n\n\n", '\n', '\n\n', '\n\n', '\n\n\n', '\n\n', '\n\n\n\n', '\n\n\n', '\n\n', '\n\n', '\n\n', '\n\n')

counter <- 1
for (page in data){
  # Get department
  dep <- strsplit(page, 'DEPARTAMENTO DE ')[[1]][2]
  dep <- strsplit(dep, '\\.')[[1]][1]
  print(dep)
  
  # Extract the table
  population <- strsplit(page, '2005 - 2020.\n\n')[[1]][2]
  
  # Get each row
  rows <- strsplit(population, row_split[counter])
  rows <- rows[[1]][4:length(rows[[1]])]
  
  # Get table 
  table.names <- c('Municipality', 2005:2020)
  tab <- str_split_fixed(rows, '[ \t]{2,}' ,n=Inf)
  
  # Apply the column names from the first row
  table <- data.frame(tab)
  colnames(table) <- table.names
  
  # Fix Municipality, drop non-municipality columns
  table <- filter(table, `2010` != '')
  table <- table %>% tidyr::separate(Municipality, c("Mun_code", "Municipality"), sep='-')
  
  # Reshape / melt
  table <- reshape::melt(
    data=table, id.vars=c('Municipality', 'Mun_code'), variable_name='Year')
  table <- mutate(table, Population = as.numeric(gsub(',', '', value)))
  table <- select(table, -value)
  
  # Append
  table$Departamento <- dep
  population.table <- rbind(population.table, table)
  
  # Update counter
  counter <- counter + 1
}

# 3. Save --------------------
write.csv(population.table, file.path(raw_data_fpath, 'Population', 'El_Salvador', 'population_estimates.csv'),
          row.names=FALSE)


