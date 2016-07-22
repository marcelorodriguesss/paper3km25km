# -*- coding: utf-8 -*-

from netCDF4 import Dataset
import numpy as np
from datetime import timedelta, date

np.set_printoptions(suppress=True, threshold=np.nan, precision=2)

# dias de previsao faltando
# problema no dado do gfs
daysmiss = [date(2009, 5, 4), date(2009, 5, 5)]

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

res24 = np.full((120, nlats, nlons), np.nan)
res48 = np.full((120, nlats, nlons), np.nan)
res72 = np.full((120, nlats, nlons), np.nan)
acc3d = np.full((120, nlats, nlons), np.nan)

start_date = date(2009, 2, 1)
end_date = date(2009, 6, 1)

for i, single_date in enumerate(daterange(start_date, end_date)):

    nc_path = "/mnt/hdext/002/marcelo/cdfpaper/{2}/{0}km/pcp.rgkf.{1}00.{0}km.cdf"\
        .format(resol, single_date.strftime("%Y%m%d"),
                single_date.strftime("%Y"))

    if single_date in daysmiss:

        print nc_path, 'miss!'

        res24[i, ...] = -999.
        res48[i, ...] = -999.
        res72[i, ...] = -999.
        acc3d[i, ...] = -999.

    else:

        print nc_path

        nc_data = Dataset(nc_path, 'r')

        # latitudes e longitudes 
        lats = nc_data.variables['y'][:]
        lons = nc_data.variables['x'][:]

        # accum 24h de 7h-7h
        pcp24a = nc_data.variables[myvar][10, ...]
        pcp24b = nc_data.variables[myvar][34, ...]

        # accum 48h de 7h-7h
        pcp48a = nc_data.variables[myvar][34, ...]
        pcp48b = nc_data.variables[myvar][58, ...]

        # accum 72h de 7h-7h
        pcp72a = nc_data.variables[myvar][58, ...]
        pcp72b = nc_data.variables[myvar][82, ...]

        nc_data.close()

        res24[i, ...] = pcp24b - pcp24a
        res48[i, ...] = pcp48b - pcp48a
        res72[i, ...] = pcp72b - pcp72a
        acc3d[i, ...] = res24[i, ...] + res48[i, ...] + res72[i, ...]

name24 = 'pcp-rgkf-weeksst-{0}km-{1}020200-{1}060100-24h'\
    .format(resol, single_date.strftime("%Y"))
name48 = 'pcp-rgkf-weeksst-{0}km-{1}020300-{1}060200-48h'\
    .format(resol, single_date.strftime("%Y"))
name72 = 'pcp-rgkf-weeksst-{0}km-{1}020400-{1}060300-72h'\
    .format(resol, single_date.strftime("%Y"))
name3d = 'pcp-rgkf-weeksst-{0}km-{1}020400-{1}060300-acc3d'\
    .format(resol, single_date.strftime("%Y"))

np.save(name24, res24)
np.save(name48, res48)
np.save(name72, res72)
np.save(name3d, acc3d)

namelat = 'lats-{0}km'.format(resol)
namelon = 'lons-{0}km'.format(resol)

np.save(namelat, lats)
np.save(namelon, lons)

