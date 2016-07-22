# -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt

np.set_printoptions(suppress=True, threshold=np.nan, precision=2)

ind = np.arange(5)

menMeans = np.load('scores-3km-acc3d-20mm.npy')
womenMeans = np.load('scores-25km-acc3d-20mm.npy')

res1 = menMeans[0, ...]
res2 = womenMeans[0, ...]

index = [1, 2, 4, 5, 9]

a = []
for i in index:
    a.append(res1[i])

b = []
for i in index:
    b.append(res2[i])

# [1][2][4][5][9]

width = 0.35

fig, ax = plt.subplots()

rects1 = ax.bar(ind, a, width, color='#9c9c9c')

rects2 = ax.bar(ind + width, b, width, color='#cfcfcf')

ax.set_xlim(-width, len(ind) + width)

ax.set_ylim(-0.1, 2)

# ax.set_ylabel('Y TITLE')
ax.set_title('ACCUM 3 DIAS - 20 MM')

ax.set_xticks(ind + width)

ax.set_xticklabels(('Bias', 'Hit Rate', 'False Alarm', 'Prob Detection', 'Heidke'))

ax.legend((rects1[0], rects2[0]), ('3km', '25km'))

plt.tight_layout()

plt.show()
