# -*- coding: utf-8 -*-

import numpy as np
from PyFuncemeClimateTools import VerificationWeather as vw

np.set_printoptions(suppress=True, threshold=np.nan, precision=2)

# list regioes
regs = ['acarau', 'altojagua', 'baixojagua', 'banabuiu', 'coreau',
        'curu', 'litoral', 'medjagua', 'metro', 'sert_crateus',
        'salgado', 'serraibiapaba']

# list prev
prevs = ['24h', '48h', '72h', 'acc3d']

# list obs
lobs = ['pcp-obs-bacias-20090202-20090601-24h.npy',
        'pcp-obs-bacias-20090203-20090602-48h.npy',
        'pcp-obs-bacias-20090204-20090603-72h.npy',
        'pcp-obs-bacias-20090204-20090603-acc3d.npy']

# list 3km
l3km = ['thiessen-pcp-rgkf-weeksst-3km-2009020200-2009060100-24h.npy',
         'thiessen-pcp-rgkf-weeksst-3km-2009020300-2009060200-48h.npy',
         'thiessen-pcp-rgkf-weeksst-3km-2009020400-2009060300-72h.npy',
         'thiessen-pcp-rgkf-weeksst-3km-2009020400-2009060300-acc3d.npy']

# list 25km
l25km = ['thiessen-pcp-rgkf-weeksst-25km-2009020200-2009060100-24h.npy',
        'thiessen-pcp-rgkf-weeksst-25km-2009020300-2009060200-48h.npy',
        'thiessen-pcp-rgkf-weeksst-25km-2009020400-2009060300-72h.npy',
        'thiessen-pcp-rgkf-weeksst-25km-2009020400-2009060300-acc3d.npy']


lim = 20.

i = 0
for fmod, fobs in zip(lobs, l3km):

    print fmod
    print fobs

    mod = np.load(fmod)
    obs = np.load(fobs)

    # 12 regioes
    result = np.full((12, 15), np.nan)

    for r in range(12):

        scores = vw.verif_index(np.where(obs[r, :] >= 0., obs[r, :], np.nan),
                                np.where(mod[r, :] >= 0., mod[r, :], np.nan),
                                lim)

        print '\n- Limiar:', lim, \
              '\n- Valid events:', scores[0], \
              '\n- Bias:', scores[1], \
              '\n- Hit rate:', scores[2], \
              '\n- False alarm ratio:', scores[4], \
              '\n- Accuracy:', scores[5], \
              '\n- Heidke:', scores[9], \
              '\nFalse alarme rate:', scores[3], \
              '\nAccuracy rain:', scores[6], \
              '\nAccuracy no rain:', scores[7], \
              '\nThreat Score:', scores[8], \
              '\nTrue Positive:', scores[10], \
              '\nTrue Negative:', scores[12], \
              '\nFalse Positive:', scores[11], \
              '\nFalse Negative:', scores[13], \
              '\nDias com chuva:', scores[14]

        # foco: bias, hit rate, fa ratio, accuracy, heidke

        result[r, ...] = scores

    filename = 'scores-3km-{0}-{1}mm'.format(prevs[i], int(lim))

    np.save(filename, result)

    i += 1

i = 0
for fmod, fobs in zip(lobs, l25km):

    print fmod
    print fobs

    mod = np.load(fmod)
    obs = np.load(fobs)

    # 12 regioes
    result = np.full((12, 15), np.nan)

    for r in range(12):
        scores = vw.verif_index(np.where(obs[r, :] >= 0., obs[r, :], np.nan),
                                np.where(mod[r, :] >= 0., mod[r, :], np.nan),
                                lim)

        print '\n- Limiar:', lim, \
            '\n- Valid events:', scores[0], \
            '\n- Bias:', scores[1], \
            '\n- Hit rate:', scores[2], \
            '\n- False alarm ratio:', scores[4], \
            '\n- Accuracy:', scores[5], \
            '\n- Heidke:', scores[9], \
            '\nFalse alarme rate:', scores[3], \
            '\nAccuracy rain:', scores[6], \
            '\nAccuracy no rain:', scores[7], \
            '\nThreat Score:', scores[8], \
            '\nTrue Positive:', scores[10], \
            '\nTrue Negative:', scores[12], \
            '\nFalse Positive:', scores[11], \
            '\nFalse Negative:', scores[13], \
            '\nDias com chuva:', scores[14]

            # foco: bias, hit rate, fa ratio, accuracy, heidke

        result[r, ...] = scores

    filename = 'scores-25km-{0}-{1}mm'.format(prevs[i], int(lim))

    np.save(filename, result)

    i += 1
