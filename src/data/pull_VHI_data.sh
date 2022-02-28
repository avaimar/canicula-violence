#!/bin/bash
## Downloads VHI data for 2010-2020 for weeks 27-36 (June, July)
# Run this script from project directory :
# > sh src/data/pull_VHI_data.sh

if [ ! -d "data_raw_local/VHI" ]
then
  mkdir data_raw_local/VHI
fi 

cd data_raw_local/VHI

for YEAR in {2011..2020}
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

  for WEEK_NUM in {27..35}
  # for WEEK_NUM in {1..52}
  do
    printf -v WEEK_NUM_PADDED "%02d" $WEEK_NUM #zero pad pentad to be 2 digits
    curl --remote-name https://www.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/geo_TIFF/VHP.G04.C07.${FILE_PREFIX}.P${YEAR}0${WEEK_NUM_PADDED}.VH.VHI.tif
  done
done

cd ../..
