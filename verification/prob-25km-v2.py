# -*- coding: utf-8 -*-

#import ipdb
#ipdb.set_trace()
import numpy as np
import sys
import time
import os

if len(sys.argv) != 5:
    #print len(sys.argv)
    print "\n => Use: prob-25km.py reg mm prev tipo \n"
    sys.exit(1)

reg = str(sys.argv[1])
mm = float(sys.argv[2])
prev = str(sys.argv[3])
tipo = str(sys.argv[4])

####################################################################

# Arquivo previsao
prev_txt = "previsao/regiao{0}_mod_{1}{2}.txt".format(reg, tipo, prev)
prev_ano, prev_mes, prev_dia, prev_pcp = np.loadtxt(prev_txt, unpack=True)

# Arquivo obs/pluv
regiao_obs_txt = "observado/RH{0}.txt".format(reg)
#obs_ano, obs_mes, obs_dia, obs_pcp = np.loadtxt(regiao_obs_txt, unpack=True, usecols=(1, 2, 3, 4))
obs_ano, obs_mes, obs_dia, obs_pcp = np.loadtxt(regiao_obs_txt, unpack=True, usecols=(0, 1, 2, 3))

# Arquivo de saida
n_out_file_txt = "lista-prev{1}-{2}-cat{0}-reg{3}.txt".format(mm, prev, tipo, reg)

if os.path.isfile(n_out_file_txt):
    print "\nApagando arquivo: {0}\n".format(n_out_file_txt)
    os.remove(n_out_file_txt)
    time.sleep(2)

prob1 = []
prob2 = []
prob3 = []
prob4 = []
#dia = []

qtd_dias_prev = len(open(prev_txt).readlines())
qtd_dias_obs = len(open(regiao_obs_txt).readlines())

#if qtd_dias_prev != qtd_dias_obs:
#    print "Linhas arquivo -> {0}: {1}".format(prev_txt, str(qtd_dias_prev))
#    print "Linhas arquivo -> {0}: {1}".format(regiao_obs_txt, str(qtd_dias_obs))
#    sys.exit(1)


def escreve_resultado():
    print prev_ano[I], prev_mes[I], prev_dia[I], prev_pcp[I]
    print obs_ano[A], obs_mes[A], obs_dia[A], obs_pcp[A]
    print "\n"
    dia = prob1[I] + prob2[I] + prob3[I] + prob4[I]
    out_file_txt = open(n_out_file_txt, "a")
    out_file_txt.write("{0}    {1}    {2}    {3}    {4}\n".format(prob1[I], prob2[I], prob3[I], prob4[I], dia))
    out_file_txt.close()

for I in range(qtd_dias_prev):

    if prev == "24":
        A = I + 1
    if prev == "48":
        A = I + 2
    if prev == "72":
        A + I + 3

    if prev_pcp[I] == -999:
        print "Valor prev = -999"
        prob1.append(0)
        prob2.append(0)
        prob3.append(0)
        prob4.append(0)
        escreve_resultado()

    elif obs_pcp[A] == -999:
        print "Valor obs = -999"
        prob1.append(0)
        prob2.append(0)
        prob3.append(0)
        prob4.append(0)
        escreve_resultado()

    else:
        print prev_pcp[I], obs_pcp[A], mm
        if prev_pcp[I] > mm and obs_pcp[A] > mm:
            print "Prev maior que {0} e obs maior que {0}".format(mm)
            prob1.append(1)
        else:
            prob1.append(0)

        if prev_pcp[I] > mm and obs_pcp[A] <= mm:
            print "Prev maior que {0} e obs menor ou igual a {0}".format(mm)
            prob2.append(1)
        else:
            prob2.append(0)

        if prev_pcp[I] <= mm and obs_pcp[A] > mm:
            print "Prev menor ou igual a {0} e obs maior que {0}".format(mm)
            prob3.append(1)
        else:
            prob3.append(0)

        if prev_pcp[I] <= mm and obs_pcp[A] <= mm:
            print "Prev menor ou igual a {0} e obs menor ou igual a {0}".format(mm)
            prob4.append(1)
        else:
            prob4.append(0)

        escreve_resultado()

print " => Arquivos de entrada: {0}, {1}".format(prev_txt, regiao_obs_txt)
print " => Arquivo de saida: {0}\n".format(n_out_file_txt)
