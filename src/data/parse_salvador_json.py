import json
import pandas as pd
import pathlib as plib

gdrive_fpath = plib.Path('/Volumes/GoogleDrive-112161833434429421879/My Drive/Project/')
outpath = gdrive_fpath / 'Data' / 'Processed' / 'Homicides' /'elsalvador_municipality_homicidecount_2011_2020.csv'


with open (gdrive_fpath / 'Data'/ 'Raw'/ 'ElSalvador'/ 'el_salvador_homicide.json') as f:
    d = json.load(f)

municipality_data_list = [munic['metadata'] for munic in d['choropleth'] ]

metadata_colnames = ["Municipio","Departamento","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","TOTAL"]
df_homicide_salvador = pd.DataFrame(municipality_data_list, columns= metadata_colnames)
df_homicide_salvador.drop('TOTAL', axis = 1, inplace=True)

df_long = df_homicide_salvador.melt(id_vars=["Municipio","Departamento"], var_name='Year', value_name='hom_count')
df_long.to_csv(outpath, index = False)
print(f"Data written to {outpath}")
