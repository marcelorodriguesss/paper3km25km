# -*- coding: utf-8 -*-

import numpy as np
from PyFuncemeClimateTools import Thiessen as th

myfiles = ['pcp-rgkf-weeksst-25km-2009020200-2009060100-24h.npy',
           'pcp-rgkf-weeksst-25km-2009020300-2009060200-48h.npy',
           'pcp-rgkf-weeksst-25km-2009020400-2009060300-72h.npy',
           'pcp-rgkf-weeksst-25km-2009020400-2009060300-acc3d.npy']

flats = 'lats-25km.npy'
flons = 'lons-25km.npy'

# myfiles = ['pcp-rgkf-weeksst-3km-2009020200-2009060100-24h.npy',
#            'pcp-rgkf-weeksst-3km-2009020300-2009060200-48h.npy',
#            'pcp-rgkf-weeksst-3km-2009020400-2009060300-72h.npy',
#            'pcp-rgkf-weeksst-3km-2009020400-2009060300-acc3d.npy']

# flats = 'lats-3km.npy'
# flons = 'lons-3km.npy'

for fdata in myfiles:

    mydata = np.load(fdata)
    lats = np.load(flats)
    lons = np.load(flons)

    # 12 num bacias
    # 120 num dias
    resth = np.full((12, 120), np.nan)

    for i, bacia in enumerate(range(1, 13)):
        rh = 'reg_hidro/reg{0}.txt'.format(bacia)
        print rh, fdata
        nfig = 'bacia{0}.png'.format(bacia)
        res = th.thiessen(mydata, lats, lons, rh, -1, usenc=True, figname=nfig)
        resth[i, ...] = res[0, ...]

    # salva todas as bacias
    fout = 'thiessen-{0}'.format(fdata)
    np.save(fout, resth)

