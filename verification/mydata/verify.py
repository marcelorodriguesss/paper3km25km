# -*- coding: utf-8 -*-

import numpy as np
from PyFuncemeClimateTools import VerificationWeather as vw

np.set_printoptions(suppress=True, threshold=np.nan, precision=2)

fmod = 'thiessen-pcp-rgkf-weeksst-25km-2009020200-2009060100-24h.npy'
fobs = 'pcp-obs-bacias-20090202-20090601-24h.npy'

mod = np.load(fmod)
obs = np.load(fobs)

print mod.shape, obs.shape

tp, fp, tn, fn = \
    vw.cont_table(np.where(obs[0, :] >= 0., obs[0, :], np.nan),
                  np.where(mod[0, :] >= 0., mod[0, :], np.nan),
                  1.)

print tp
print fp
print tn
print fn

res = vw.bias(tp, fp, tn)

print res

