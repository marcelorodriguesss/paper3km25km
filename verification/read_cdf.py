# -*- coding: utf-8 -*-

from netCDF4 import Dataset
import numpy as np
from datetime import timedelta, date

np.set_printoptions(suppress=True, threshold=np.nan, precision=2)

# http://goo.gl/frxo0
def daterange(start_date, end_date):
    for n in range(int ((end_date - start_date).days)):
        yield start_date + timedelta(n)

resol = 3  # 3 ou 25

if resol == 25:
    myvar = 'total_accum_precip'
    nlats, nlons = 103, 103
elif resol == 3:
    myvar = 'total_resolved_precip'
    nlats, nlons = 222, 222
else:
    exit()

res24 = np.full((59, nlats, nlons), np.nan)
res48 = np.full((59, nlats, nlons), np.nan)
res72 = np.full((59, nlats, nlons), np.nan)

start_date = date(2009, 2, 1)
end_date = date(2009, 4, 1)

for i, single_date in enumerate(daterange(start_date, end_date)):

    nc_path = "/mnt/hdext/002/marcelo/cdfpaper/{0}km/pcp.rgkf.{1}00.{0}km.cdf"\
        .format(resol, single_date.strftime("%Y%m%d"))

    print nc_path

    nc_data = Dataset(nc_path, 'r')

    # latitude e longitudes 
    lats = nc_data.variables['y'][:]
    lons = nc_data.variables['x'][:]

    # accum 24h
    pcp24a = nc_data.variables[myvar][10, ...]
    pcp24b = nc_data.variables[myvar][34, ...]

    # accum 48h
    pcp48a = nc_data.variables[myvar][34, ...]
    pcp48b = nc_data.variables[myvar][58, ...]

    # accum 72h
    pcp72a = nc_data.variables[myvar][58, ...]
    pcp72b = nc_data.variables[myvar][82, ...]

    nc_data.close()

    res24[i, ...] = pcp24b - pcp24a
    res48[i, ...] = pcp48b - pcp48a
    res72[i, ...] = pcp72b - pcp72a

    name24 = 'pcp-rgkf-weeksst-{0}km-2009020100-24h'.format(resol)
    name48 = 'pcp-rgkf-weeksst-{0}km-2009020100-48h'.format(resol)
    name72 = 'pcp-rgkf-weeksst-{0}km-2009020100-72h'.format(resol)

    np.save(name24, res24)
    np.save(name48, res48)
    np.save(name72, res72)

namelat = 'lats-{0}km'.format(resol)
namelon = 'lons-{0}km'.format(resol)

np.save(namelat, lats)
np.save(namelon, lons)

