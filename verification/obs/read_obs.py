# -*- coding: utf-8 -*-

import numpy as np

np.set_printoptions(suppress=True, threshold=np.nan, precision=2)

bacias = ['pr_daily_funceme_obs_20090201_20090630_thiessen_acarau.asc',
          'pr_daily_funceme_obs_20090201_20090630_thiessen_alto_jagua.asc',
          'pr_daily_funceme_obs_20090201_20090630_thiessen_baixo_jagua.asc',
          'pr_daily_funceme_obs_20090201_20090630_thiessen_banabuiu.asc',
          'pr_daily_funceme_obs_20090201_20090630_thiessen_coreau.asc',
          'pr_daily_funceme_obs_20090201_20090630_thiessen_curu.asc',
          'pr_daily_funceme_obs_20090201_20090630_thiessen_litoral.asc',
          'pr_daily_funceme_obs_20090201_20090630_thiessen_medio_jagua.asc',
          'pr_daily_funceme_obs_20090201_20090630_thiessen_metropolitana.asc',
          'pr_daily_funceme_obs_20090201_20090630_thiessen_sertoes_de_crateus.asc',
          'pr_daily_funceme_obs_20090201_20090630_thiessen_salgado.asc',
          'pr_daily_funceme_obs_20090201_20090630_thiessen_serra_ibiapaba.asc']

# num bacias 12
res24 = np.full((12, 120), np.nan)
res48 = np.full((12, 120), np.nan)
res72 = np.full((12, 120), np.nan)
acc3d = np.full((12, 148), np.nan)

for i, bac in enumerate(bacias):

    print bac
    obsdata = np.genfromtxt(bac)
    res24[i, ...] = obsdata[1:121, 3]
    res48[i, ...] = obsdata[2:122, 3]
    res72[i, ...] = obsdata[3:123, 3]
    acc3d[i, ...] = np.convolve(obsdata[:, 3], np.ones(3, dtype=float), 'valid')

# dia 20090202: accum 20090201 7h -> 20090202 7h
np.save('pcp-obs-bacias-20090202-20090601-24h.npy', np.where(res24 > 0, res24, -999.))

# dia 20090203: accum 20090202 7h -> 20090203 7h
np.save('pcp-obs-bacias-20090203-20090602-48h.npy', np.where(res48 > 0, res48, -999.))

# dia 20090204: accum 20090203 7h -> 20090204 7h
np.save('pcp-obs-bacias-20090204-20090603-72h.npy', np.where(res72 > 0, res72, -999.))

# dia 20090203: accum 20090201 7h -> 20090203 7h
aux_acc3d =  np.where(acc3d > 0, acc3d, -999.)
np.save('pcp-obs-bacias-20090204-20090603-acc3d.npy', aux_acc3d[:, 1:121])

