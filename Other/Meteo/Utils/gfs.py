#!/usr/bin/env python
# -*- coding: UTF-8 -*-
""" 
Download GFS data required by FALL3D model. 
The NCEP operational Global Forecast System 
analysis and forecast grids are on a 0.25 by 0.25 
global latitude longitude grid. 
"""

import sys
import argparse
import configparser
from datetime import datetime 
from fall3dutil.opendap import GFS

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
    parser.add_argument('-t', '--time',    help='time steps [Default: %(default)s]',               default=(0, 6),        nargs=2, type=int,       metavar=('tmin',   'tmax'))
    parser.add_argument('-r', '--res',     help='spatial resolution (deg) [Default: %(default)s]', default=0.25,                   type=float,     choices=(0.25, 0.5, 1.0) )
    parser.add_argument('-c', '--cycle',   help='cycle [Default: %(default)s]',                    default=0,                      type=int,       choices=(0,6,12,18))
    parser.add_argument('-s', '--step',    help='temporal resolution (h) [Default: %(default)s]',  default=1,                      type=int,       choices=(1, 3, 12))
    parser.add_argument('-a', '--area',    help='area name [Default: %(default)s]',                                                type=str,       metavar=('Area'))
    parser.add_argument('-i', '--input',   help='area definition file [Default: %(default)s]',     default='areas.def',            type=str,       metavar=('AreaFile'))
    parser.add_argument('-o', '--output',  help='output file [Default: YYYYMMDD_HHz.nc]',          default='')
    parser.add_argument('-v', '--verbose', help="increase output verbosity", action="store_true")
    parser.add_argument('date',            help='Initial date in format YYYYMMDD',                                                 type=date_type, metavar='start_date')

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

    if args.time[0] > args.time[1]:
        sys.exit("Error: Use '{-t,--time}' tmin tmax")

    if args.output:
        if not args.output.endswith('.nc'):
            args.output = args.output.strip() + '.nc'
    else:
        args.output = "{date}-{cycle:02d}z.nc".format(date  = args.date.strftime("%Y%m%d"), 
                                                      cycle = args.cycle)

    if args.res==0.25:
        if args.step != 1 and args.step != 3:
            print("wrong time step. setting to step=3")
            args.step=3
    elif args.res==0.5:
        if args.step != 3: 
            print("wrong time step. setting to step=3")
            args.step=3
    elif args.res==1.0:
        if args.step != 12:
            print("wrong time step. setting to step=12")
            args.step=12

    request = GFS(args)
    request.open_remote()
    request.open_local()
    request.save_data()

if __name__ == '__main__':
    main()
