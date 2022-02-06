#!/bin/bash
## Downloads VHI data for 2010-2020 for weeks 27-36 (June, July)
# Run this script from project directory :
# > sh src/data/pull_VHI_data.sh

cd data_raw_local/VHI
for YEAR in {2010..2011}
do
  if ((YEAR <= 2010))
  then
      FILE_PREFIX=NN
  elif ((YEAR <= 2012))
  then
      FILE_PREFIX=NP
  else
      FILE_PREFIX=npp
  fi
  #[[ "$YEAR" < 2013 ]] && FILE_PREFIX=NP || FILE_PREFIX=npp
  for WEEK_NUM in {27..35}
  do
    curl --remote-name https://www.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/geo_TIFF/VHP.G04.C07.${FILE_PREFIX}.P${YEAR}0${WEEK_NUM}.VH.VHI.tif
  done
done

cd ../..
