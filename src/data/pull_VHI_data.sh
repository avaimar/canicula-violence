#!/bin/bash
## Downloads VHI data for 2011-2020 for weeks 26-36 (June, July)
# Run this script from project directory :
# > sh src/data/pull_VHI_data.sh

cd data_raw_local/VHI
for YEAR in {11..20}
do
  for WEEK_NUM in {26..35}
  do
    curl --remote-name https://www.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/geo_TIFF/VHP.G04.C07.npp.P20${YEAR}0${WEEK_NUM}.VH.VHI.tif
  done
done
cd ../..
