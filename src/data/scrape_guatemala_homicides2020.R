# 0. Working set up ----------------------------------------
library(pdftools)
library(stringr)
library(reshape)
library(tidyverse)

# Parameters
gdrive_fpath <- file.path("/Volumes/GoogleDrive-112161833434429421879/My Drive/Project")
raw_data_fpath <- file.path(gdrive_fpath, 'Data/Raw/Homicides/Guatemala/Compendio-estadístico-MINGOB-2020.pdf')
output_fpath <- file.path(gdrive_fpath, 'Data/Processed/guatemala_homicides_2020.csv')

# 1. Load data ----------------------------------
data <- pdftools::pdf_text(raw_data_fpath)
data <- data[94:104]

# 2. Process each page ------------------------------------
table.names <- c('Departament', 'Municipality', 'Homicide', 'Population', 'Rate')
tables_per_page <- list(3, c(1, 2), 1, c(1, 2), 1, 1, 1, 1, 1, 1, 1)
homicides <- data.frame()

counter <- 1
for (data_page in data) {
  # Extract the tables
  page_table <- strsplit(data_page, split='\nCOMPENDIO ESTADÍSTICO 2020  ')[[1]][1]
  page_table <- strsplit(page_table, 'Tasa')[[1]]
  page_table <- page_table[tables_per_page[[counter]]]

  for (table in page_table){
    # Get each row
    rows <- strsplit(table, "\n")
    
    # Get data.frame
    df <- as_tibble(data.frame(str_split_fixed(rows[[1]], '[ \t]{2,}' ,n=Inf)))
    
    # Drop empty column and rows, and header row
    df <- df %>% select(-X1) %>% filter(X6 != "")
    
    # Attach to homicide table
    homicides <- rbind(homicides, df)
  }
  
  counter <- counter + 1
}

colnames(homicides) <- table.names

# Reformat department, municipality names, add year and format homicide counts
homicides <- homicides %>% 
  mutate(Departament = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Departament))) %>%
  mutate(Municipality = chartr("ÁÉÍÓÚ", "AEIOU", toupper(Municipality))) %>%
  mutate(Year = 2020) %>%
  mutate(Homicide = as.numeric(Homicide)) %>%
  select(-c(Rate, Population))

# Check for missing values
colSums(is.na(homicides))

# Save
write.csv(homicides, output_fpath, row.names=FALSE)
