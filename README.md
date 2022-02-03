canicula-violence
==============================

## Data

Stored in [Google Drive](https://drive.google.com/drive/folders/1QNyZ89ooJamt69vxnnY5jXXc8HD3Q_pz?usp=sharing)

#### Homicide Data Resources:

[El Salvador](https://flo.uri.sh/visualisation/6005450/embed?auto=1) : Online viz  
[Honduras](https://www.sepol.hn/sepol-estadisticas-incidencia-municipio.php) : Online query  
Guatemala [[1](https://cien.org.gt/index.php/cantidad-de-homicidios-por-municipio/), [2](https://mingob.gob.gt/indicadores-de-seguridad-ciudadana-y-acciones-policiales/), [2015-21 monthly](https://www.dialogos.org.gt/index.php/publicaciones/informe-sobre-la-violencia-homicida-en-guatemala-diciembre-2021)](https://www.dialogos.org.gt/index.php/publicaciones/informe-sobre-la-violencia-homicida-en-guatemala-diciembre-2021) : Tableau, PDF, Excel  
[Nicaragua](https://www.policia.gob.ni/wp-content/uploads/2021/05/Anuario-PN-2020-marzo.pdf) : PDF table

#### Canicula Data

[4km weekly VHI data in GEO-TIFF format (1981-2021) -- NOAA STAR](https://www.star.nesdis.noaa.gov/smcd/emb/vci/VH/vh_ftp.php)

[Method for computing canicula index from VHI -- UN FAO](https://www.fao.org/publications/card/en/c/CB1818EN/) 

Project Organization
------------

    ├── data (stored in google drive)
    │   ├── Processed      <- The final, canonical data sets for modeling.
    │   └── Raw            <- The original, immutable data dump.
    ├── data_raw_local  <- raw data so fat it seems a bit dumb to store in google drive...
    │   └── VHI      <- Raw VHI data populated from running `src/data/pull_VHI_data.sh`
    │
    │
    ├── notebooks          <- Rmarkdown Notebooks for tooling around
    │
    ├── results            
    │   ├── tables         <- csv outputs
    │   └── figures        <- generated figs
    │
    └── src                <- Source code for use in this project.
        │
        ├── data           <- Scripts to download/generate data (populate data/Raw or data/Processed from external source)
        │   ├── 
        │   └── scrape_honduras_homicides.py
        │
        ├── features       <- Scripts to turn raw data into features for modeling (data/raw -> data/processed)
        │   └── scrape_Nicaragua_provinces.R
        │
        ├── models         <- Scripts to for model training/inference
        │
        └── visualization  <- Scripts to create exploratory and results oriented visualizations--------

<p><small>Project based on the <a target="_blank" href="https://drivendata.github.io/cookiecutter-data-science/">cookiecutter data science project template</a>. #cookiecutterdatascience</small></p>
