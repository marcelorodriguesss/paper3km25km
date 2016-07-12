# -*- coding: utf-8 -*-

#import ipdb; ipdb.set_trace()
import numpy as np
from pylab import *
#import matplotlib.pyplot as plt
import sys
import os

if len(sys.argv) != 5:
    print len(sys.argv)
    print "\n => Use: indices-25km.py reg mm prev tipo \n"
    sys.exit(1)

reg = str(sys.argv[1])
mm = sys.argv[2]
prev = sys.argv[3]
tipo = str(sys.argv[4])

prob_file = "lista-prev{0}-{1}-cat{2}-25km.txt".format(prev, tipo, mm)

if os.path.isfile(prob_file):
    a1, b1, c1, d1, dia1 = np.loadtxt(prob_file, unpack=True)
else:
    print "\n => Arquivo {0} não encontrado!\n".format(prob_file)
    sys.exit(1)

prob1 = sum(a1)
prob2 = sum(b1)
prob3 = sum(c1)
prob4 = sum(d1)
qtd_dias = sum(dia1)

# Índice de acertos (ia)
test_ia = (prob1 + prob2 + prob3 + prob4)
if test_ia == 0:
    print "-999"
    ia = -999
else:
    ia = (prob1 + prob4)/(prob1 + prob2 + prob3 + prob4)

# Índice de acerto com chuva (ir)
test_ir = ((prob1 + prob3)*(prob1 + prob2))
if test_ir == 0:
    print "-999"
    ir = -999
else:
    ir = ((prob1)**2)/((prob1 + prob3)*(prob1 + prob2))

# Índice de acerto sem chuva (iss)
test_iss = ((prob2 + prob4)*(prob3 + prob4))
if test_iss == 0:
    print "-999"
    iss = -999
else:
    iss = ((prob4)**2)/((prob2 + prob4)*(prob3 + prob4))

# Threat Score (ts)
test_ts = (prob1 + prob3)
if test_ts == 0:
    print "-999"
    ts = -999
else:
    ts = (prob1)/(prob1 + prob2 + prob3)

# Probabilidade de detecção (pod)
test_pod = (prob1 + prob3)
if test_pod == 0:
    print "-999"
    pod = -999
else:
    pod = (prob1)/(prob1 + prob3)

# Razão de alarme falso (far)
test_far = (prob1 + prob2)
if test_far == 0:
    print "-999"
    far = -999
else:
    far = (prob2)/(prob1 + prob2)

# Taxa de alarme falso (farr)
test_farr = (prob4 + prob2)
if test_farr == 0:
    print "-999"
    farr = -999
else:
    farr = (prob2)/(prob4 + prob2)

# Bias (bias)
test_bias = (prob1 + prob3)
if test_bias == 0:
    print "-999"
    bias = -999
else:
    bias = (prob1 + prob2)/(prob1 + prob3)

# Indice de Heidke (ih)
test_ih = ((prob1 + prob3)*(prob3 + prob4)+(prob1 + prob2)*(prob2 + prob4))
if test_ih == 0:
    print "-999"
    ih = -999
else:
    ih = (2*((prob1*prob4)-(prob2*prob3)))/((prob1 + prob3)*(prob3 + prob4)+(prob1 + prob2)*(prob2 + prob4))

# Dias com chuva
dias_chuva = prob1 + prob3

name_aval_file = "aval-prev{0}-cat{1}-regiao{2}-25km.txt".format(prev, mm, reg)

if os.path.isfile(name_aval_file):
    print " \n => Removendo arquivo: {0}\n".format(name_aval_file)
    os.remove(name_aval_file)

aval_file = open(name_aval_file, "w")
aval_file.write("{0}    {1}    {2}    {3}    {4}    {5}    {6}    {7}    {8}    {9}    {10}\n"
                .format(ia, ir, iss, ts, pod, far, farr, bias, ih, qtd_dias, dias_chuva))
aval_file.close()

print "\n => Arquivo de saída: {0} \n".format(name_aval_file)

### Plot

t = ia, ir, iss, ts, pod, far, farr = np.loadtxt(name_aval_file, unpack=True, usecols=(0, 1, 2, 3, 4, 5, 6))

n = len(t)

index = arange(n)
width = 0.40       # the width of the bars

p1 = bar(index, t, width, color='red')[0]
p2 = bar(index+width, t, width, color='blue')[0]

xlim(-0.5, n+0.5)
ylim(0, 1)

ylabel(' ')
title("Categoria 1mm - Previsao 24h\nRegiao 1 - 25km", fontstyle='italic')
xticks(index+width, ['IA', 'ICC', 'ISC', 'TS', 'PDD', 'RAF', 'TAF'])

legend((p1, p2), ('12:30', '23:30'), loc='upper left', ncol=1)

plot_name = "test1.png"

savefig(plot_name)

'''
t = ia, ir, iss, ts, pod, far, farr = np.loadtxt(name_aval_file, unpack=True, usecols=(0, 1, 2, 3, 4, 5, 6))
N = 7
ind = np.arange(N)
width = 0.35
p1 = plt.bar(ind, t, color='b')
plt.ylabel('Scores')
plt.title('Indices resolucao 25km')
#plt.xticks(ind+width/0.9, ('G1', 'G2', 'G3', 'G4', 'G5'))
plt.yticks(np.arange(0.9, 1.1, 0.2))
plt.yticks(np.arange(0, 1.1, 0.2))
# plt.legend('25km')
plt.show()
'''

'''
fig = p.figure()
ax = fig.add_subplot(1, 1, 1)
t = ia, ir, iss, ts, pod, far, farr = np.loadtxt(name_aval_file, unpack=True, usecols=(0, 1, 2, 3, 4, 5, 6))
N = 7
ind = range(N)
#ax.bar(ind, y, facecolor='#777777', align='center', yerr=err, ecolor='black')
ax.bar(ind, t, facecolor='gray', align='center')
ax.set_ylabel(' ')
p.ylim([0, 1])
ax.set_title('Categoria 1mm - Previsao 24h - Regiao 1 - 25km', fontstyle='italic')
ax.set_xticks(ind)
group_labels = ['IA', 'ICC', 'ISC', 'TS', 'PDD', 'RAF', 'TAF']
ax.set_xticklabels(group_labels)
fig.autofmt_xdate()
p.savefig('t2.png')
p.show()
'''
