#!/bin/bash

TABLEFILE="table.levels"
OUTPUTFILE="output_file.nc"
GRIBDIR=/gpfs/projects/bsc21/bsc21908/FALL3D/testsuite/Meteo/20190626-00z-ulawun-gfs-0p50-grib

for i in {000..024..3} 
do
#    GRIBFILE="gfs.t00z.pgrb2.0p50.f${i}"
    GRIBFILE="${GRIBDIR}/${i}-20190626-00z.grb"
    echo "$GRIBFILE"
    if [ $i -eq 0 ]
    then
        wgrib2 $GRIBFILE -nc_table $TABLEFILE -nc3 -netcdf $OUTPUTFILE
    else
        wgrib2 $GRIBFILE -nc_table $TABLEFILE -append -nc3 -netcdf $OUTPUTFILE
    fi
done
#wgrib2 $GRIBFILE -nc_table level.table -nc3 -netcdf output.nc
