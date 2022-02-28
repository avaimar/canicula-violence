#!/bin/bash
## Downloads CHIRPS .bil files for 2010-2020 for all 72 5-day periods
# and writes them to data_raw_local/CHIRPS with 

# Run this script from project directory :
# > sh src/data/pull_CHIRPS_data.sh

if [ ! -d "data_raw_local/CHIRPS" ]
then
  mkdir data_raw_local/CHIRPS
fi 

cd data_raw_local/CHIRPS

for YEAR in {2013..2020}
do 
  for PENTAD in {1..72}
  do
    printf -v PENTAD_PADDED "%02d" $PENTAD #zero pad pentad to be 2 digits
    URL=https://data.chc.ucsb.edu/products/CHIRPS-2.0/camer-carib_pentad/bils/v2p0chirps${YEAR}${PENTAD_PADDED}.tar.gz
    curl --remote-name $URL
  done
done

for f in *.tar.gz; do tar xf "$f"; done #extract all tar.gz files
rm *.tar.gz #delete compressed files

cd ../..
