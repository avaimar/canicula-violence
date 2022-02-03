# 0. Working set up ----------------------------------------
library(stringr)
library(reshape)
library(pdftools)

# 1. Load data ----------------------------------
data <- pdftools::pdf_text('Data/Homicides/Raw/Nicaragua/Anuario-PN-2020-marzo.pdf')

# 2. Process ------------------------------------
homicides <- strsplit(data, split='Homicidios por cada 100 mil habitantes por departamentos')[[35]][2]
rm(data)

# Extract the table
homicides <- strsplit(homicides, ' del país\n     Años 2007-2020\n\n\n ')[[1]][2]
homicides <- strsplit(homicides, '\n\nA partir')[[1]][1]

# Get each row
rows <- strsplit(homicides, "\n")
rows <- rows[[1]][1:22]

# Get the column names and the rows for the provinces. We skip row 3 as it 
# contains the national aggregates
tabnames <- str_split_fixed(rows[1], '[ \t]{1,}' ,n=Inf)
tab <- str_split_fixed(rows[4:22], '[ \t]{2,}' ,n=Inf)

# Apply the column names from the first row
table <- data.frame(tab)
colnames(table) <- tabnames[1,]

# Reshape / melt
table <- reshape::melt(data=table, id.vars=c('Departamentos'), variable_name='Year')
names(table)[names(table) == 'value'] <- 'hom_rate_100k'

# Save
write.csv(table, 'Data/Homicides/Processed/nicaragua_departamento_year_2007_2020.csv', row.names=FALSE)


# Honduras
data <- pdftools::pdf_text('Data/Homicides/Raw/Honduras/DEPARTAMENTO Y MUNICIPIO HOMICIDIOS(1).pdf')
homicides <- strsplit(data, split='INCIDENCIA DE HOMICIDIOS POR MUNICIPIO 2020')[[1]][2]

