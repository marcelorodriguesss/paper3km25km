# -*- coding: utf-8 -*-

import numpy as np
from PyFuncemeClimateTools import VerificationWeather as vw

np.set_printoptions(suppress=True, threshold=np.nan, precision=2)

fmod = 'thiessen-pcp-rgkf-weeksst-25km-2009020200-2009060100-24h.npy'
fobs = 'pcp-obs-bacias-20090202-20090601-24h.npy'

mod = np.load(fmod)
obs = np.load(fobs)

print mod.shape, obs.shape

ct_a, ct_b, ct_c, ct_d = vw.contingency_table(obs[0, :], mod[0, :], [1])

print ct_a
print ct_b
print ct_c
print ct_d

res = vw.bias(ct_a, ct_b, ct_c)

print res

