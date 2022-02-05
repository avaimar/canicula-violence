import json
import requests
import pathlib as plib


gdrive_fpath = plib.Path('/Volumes/GoogleDrive-112161833434429421879/My Drive/Project/')
shape_fpath = gdrive_fpath / 'Data' / 'Raw' / 'Shapefiles'
shape_fpath.mkdir(exist_ok=True)

iso_list = ['NIC', 'HND', 'SLV', 'GTM']
base_url = "https://www.geoboundaries.org/gbRequest.html?"

for iso in iso_list:
    for level in [1,2]:
        r = requests.get(f"{base_url}ISO={iso}&ADM=ADM{level}")
        if len(r.json()) > 0:
            dlPath = r.json()[0]['gjDownloadURL']
            print(f'Downloading {dlPath}')
            geoBoundary = requests.get(dlPath).json()
            outpath = shape_fpath / f'{iso}_ADM{level}.geojson'
            with open(outpath, 'w') as f:
                json.dump(geoBoundary, f)
            print(f'Written to {outpath}')
        else:
            print(f"No shapefile for {iso} at admin level {level}")
