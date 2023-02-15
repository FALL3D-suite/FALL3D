#!/usr/bin/env python
# -*- coding: UTF-8 -*-
""" 
Download ERA5 data in pressure levels required by FALL3D model. 
"""

import sys
import argparse
import configparser
from datetime import datetime, timedelta

from fall3dutil.ecmwf import ERA5sfc

def lat_type(str):
    try:
        lat = float(str)
    except:
        raise argparse.ArgumentTypeError("invalid float value: '{0}'".format(str))

    if lat < -90 or lat > 90:
        raise argparse.ArgumentTypeError('latitude not in range -90..90')
    else:
        return lat

def lon_type(str):
    try:
        lon = float(str)
    except:
        raise argparse.ArgumentTypeError("invalid float value: '{0}'".format(str))

    if lon < -180 or lon > 360:
        raise argparse.ArgumentTypeError('longitude not in range -180..180 or 0..360')
    else:
        return lon

def date_type(s):
    try:
        return datetime.strptime(s, "%Y%m%d")
    except ValueError:
        msg = "Not a valid date: '{0}'.".format(s)
        raise argparse.ArgumentTypeError(msg)

def main():
    # Input parameters and options
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('-x', '--lon',     help='longitude range [Default: %(default)s]',          default=(-180., 180.), nargs=2, type=lon_type,  metavar=('lonmin', 'lonmax'))
    parser.add_argument('-y', '--lat',     help='latitude range [Default: %(default)s]',           default=(-90., 90.),   nargs=2, type=lat_type,  metavar=('latmin', 'latmax'))
    parser.add_argument('-r', '--res',     help='spatial resolution (deg) [Default: %(default)s]', default=0.25,                   type=float,     choices=(0.25, 0.5) )
    parser.add_argument('-s', '--step',    help='temporal resolution (h) [Default: %(default)s]',  default=1,                      type=int,       choices=(1, 3, 6, 12))
    parser.add_argument('-a', '--area',    help='area name [Default: %(default)s]',                                                type=str,       metavar=('Area'))
    parser.add_argument('-i', '--input',   help='area definition file [Default: %(default)s]',     default='areas.def',            type=str,       metavar=('AreaFile'))
    parser.add_argument('-o', '--output',  help='output file [Default: date1-date2-sfc.nc]',       default='')
    parser.add_argument('-v', '--verbose', help="increase output verbosity", action="store_true")
    parser.add_argument('date_start',      help='Initial date in format YYYYMMDD',                                                 type=date_type, metavar='Start_Date')
    parser.add_argument('date_end',        help='End date in format YYYYMMDD',                                                     type=date_type, metavar='End_Date')
    args = parser.parse_args()

    if args.area:
        print("Reading coordinates of {} from input file: {}".format(args.area,args.input))
        config = configparser.ConfigParser()
        config.read(args.input)
        block = config[args.area]
        args.lonmin = lon_type( block["lonmin"] )
        args.lonmax = lon_type( block["lonmax"] )
        args.latmin = lat_type( block["latmin"] )
        args.latmax = lat_type( block["latmax"] )
    else:
        args.lonmin = args.lon[0]
        args.lonmax = args.lon[1]
        args.latmin = args.lat[0]
        args.latmax = args.lat[1]

    if args.latmin > args.latmax:
        sys.exit("Error: Use '{-y,--lat} latmin latmax' or edit the area definition file "+args.input)

    if args.output:
        if not args.output.endswith('.nc'):
            args.output = args.output.strip() + '.nc'
    else:
        args.output = "{date1}-{date2}-sfc.nc".format(date1 = args.date_start.strftime("%Y%m%d"), 
                                                      date2 = args.date_end.strftime(  "%Y%m%d") ) 


    if args.date_start.year != args.date_end.year or args.date_start.month != args.date_end.month:
        print("Only is possible to download dates from the same month")
        if args.date_start.month == 12:
            args.date_end = datetime(args.date_start.year+1,1,1)
        else:
            args.date_end = datetime(args.date_start.year,args.date_start.month+1,1)
        args.date_end -= timedelta(days=1)
        print("Using end_date:", args.date_end)

    request = ERA5sfc(args)
    request.retrieve()

if __name__ == '__main__':
    main()
