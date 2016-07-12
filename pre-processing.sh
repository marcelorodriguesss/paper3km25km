#!/bin/bash
# 
# Este script deve rodar na p11
#
# Marcelo Rodrigues - 29/Jun/2016
#
# 20160202 00 86

D=$1
HORA_INI=00
HORA_FINAL=84
MACH_RUN="psico32"
MACH_RUN_DIR="/mnt/dados/mayte/gfs/paper"
GFS_DIR="/mnt/hdext/001/paper-tempo"

SDIR="/home/rede/tempo/modelos_psico11/trunk/rams-gfs-kf"
WORDKDIR="/mnt/hdext/001/paper-tempo/${D}${HORA_INI}"

SESSION_NAME="${D}${HORA_INI}"

function grib2grib(){

   echo " +++ Convertendo grib2 para grib +++"
   echo
   sleep 1

   cd ${GFS_DIR}/${D}${HORA_INI} || exit 1

   sleep 2

   rm -rf dp*

   cp $SDIR/{cnvgrib,fdgrib,rams_wgrib,DGRIB_IN} . || exit 1

   for J in `seq -w 0 3 84`; do
       ./cnvgrib -g21 gfs_4_${D}_0000_0${J}.grb2 gfs.t00z.pgrbf$J || exit 1
   done

   for H in `seq -w 0 3 84`; do
       sed -i "3s/.*/filein='gfs.t00z.pgrbf$H',/" DGRIB_IN
       ./fdgrib
   done

   # clear temp files
   rm -f cnvgrib DGRIB_IN dump fdgrib rams_wgrib gfs.*

}

function copydp(){

    echo " +++ Copiando dps +++"
    echo ; sleep 1

    cd ${GFS_DIR}/${D}${HORA_INI}

    ssh ${MACH_RUN} "mkdir -p /mnt/dados/mayte/gfs/paper/${D}${HORA_INI}/"

    ls -1 dp-p* || exit 1

    rsync --progress -avzhe ssh dp-p* \
    ${MACH_RUN}:${MACH_RUN_DIR}/${D}${HORA_INI}/

    # rm dp-p*

}

function runmodel(){


    tmux start-server

    tmux new-session -d -s "${SESSION_NAME}" -n ${MACH_RUN}

    tmux send-keys -t ${SESSION_NAME}:0 "ssh ${MACH_RUN}" C-m

    sleep 5

    tmux send-keys -t ${SESSION_NAME}:0 "cd /mnt/dados/mayte/gfs/paper/${D}${HORA_INI}" C-m
    
    sleep 4
    
    tmux send-keys -t ${SESSION_NAME}:0 "cp ${HOME}/quali/trunk/scripts/2016/executa-rams-gfs-kf.p32.sh ." C-m
    
    sleep 4

    tmux send-keys -t ${SESSION_NAME}:0 "bash -x executa-rams-gfs-kf.p32.sh ${D} ${HORA_INI} ${HORA_FINAL}" C-m

    sleep 4

    # tmux select-window -t ${SESSION_NAME}:0

    # tmux attach-session -d -t ${SESSION_NAME}

}

grib2grib
# copydp
# runmodel

