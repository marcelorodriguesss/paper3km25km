#!/bin/bash
#
# Reformulacao do script opera_cptec2008.sh.
#
# Cleiton Silveira - Marcelo Rodrigues - 28/Set/2009
#
# Migrado para a nova psico11 em 28/11/2013
# Marcelo - marcelorodriguesss@gmail.com
#

D=$1
HORA_INI=00
HORA_FINAL=84
START_DATE=$(date -d "$D" +%Y%m%d)
DATA_ANO=$(echo $START_DATE | cut -c 3-4)
DATA_MES=$(echo $START_DATE | cut -c 5-6)
DATA_DIA=$(echo $START_DATE | cut -c 7-8)
ANOLONGO=$(echo $START_DATE | cut -c 1-4)
DATA_ATUAL=$START_DATE
SI=$(date +%c)

# SDIR="/home/rede/tempo/modelos_psico11/trunk/rams-gfs-kf"
SDIR="/home/rede/tempo/quali/trunk/scripts/2016"
WORDKDIR="/home/dados/mayte/gfs/paper/${D}${HORA_INI}"

echo
echo " => Simulacao iniciada em `date` !"
echo


function copydp(){

    echo " +++ Copiando dps +++"
    echo ; sleep 1

    mkdir -p ${WORDKDIR}
    
    cd ${WORDKDIR} || exit 1

    scp psico11:/mnt/hdext/001/paper-tempo/${D}${HORA_INI}/dp-p* ${WORDKDIR}

    ls -1 dp-p* || exit 1
    
    rm dp-p*.tag
    
    echo "Corrigindo dps..."
    
    for F in `ls -1 dp-p????-??-??-????` ; do

        echo ${F}

        YEA=$(echo $F | cut -c5-8)
        MON=$(echo $F | cut -c10-11)
        DAY=$(echo $F | cut -c13-14)
        HOU=$(echo $F | cut -c16-17)

        sed -i "2s/.*/${YEA} ${MON} ${DAY} ${HOU}00 0 26 720 361/" ${F}

    done

}


function makesurface(){

   cd ${WORDKDIR}/../../

   unlink DEM30s 
   unlink FAOdata-h5 
   unlink ndvi
   unlink ogedata
   unlink sst-h5

   ln -s $SDIR/DEM30s DEM30s
   ln -s $SDIR/FAOdata-h5 FAOdata-h5
   ln -s $SDIR/ndvi ndvi
   ln -s $SDIR/ogedata ogedata
   # ln -s $SDIR/sst-clim sst-h5  # climatology
   ln -s $SDIR/sst sst-h5  # oisst

   cd $WORDKDIR || exit

   mkdir surfbr62

   rm -rf surfbr62/*

   cp $SDIR/RAMSIN6-25km RAMSIN6 || exit
   # cp $SDIR/rams2008 rams || exit
   cp $SDIR/rams60-marcelo/rams60/build/60/rams-6.0-opt rams || exit
   cp $SDIR/grade01.dat.25km grade01.dat || exit

   sed -i "11s/.*/   RUNTYPE  = 'MAKESFC',/" RAMSIN6
   sed -i "15s/.*/   TIMMAX   = ${HORA_FINAL},/" RAMSIN6
   sed -i "21s/.*/   IMONTH1  = ${DATA_MES},/" RAMSIN6
   sed -i "22s/.*/   IDATE1   = ${DATA_DIA},/" RAMSIN6
   sed -i "23s/.*/   IYEAR1   = ${ANOLONGO},/" RAMSIN6
   sed -i "24s/.*/   ITIME1   = ${HORA_INI}00,/" RAMSIN6

   /home/rede/mpich2111p1/bin/mpiexec ./rams -f RAMSIN6
   # ./rams -f RAMSIN6

   rm RAMSIN6

}


function makevfile(){

   cd $WORDKDIR || exit

   mkdir isan

   rm -rf isan/*

   cp $SDIR/RAMSIN6-25km RAMSIN6 || exit
   # cp $SDIR/rams60_mkvfile rams || exit
   cp $SDIR/rams60-marcelo/rams60/build/60/rams-6.0-opt rams || exit
   cp $SDIR/grade01.dat.25km grade01.dat || exit

   sed -i "11s/.*/   RUNTYPE  = 'MAKEVFILE',/" RAMSIN6
   sed -i "15s/.*/   TIMMAX   = ${HORA_FINAL},/" RAMSIN6
   sed -i "21s/.*/   IMONTH1  = ${DATA_MES},/" RAMSIN6
   sed -i "22s/.*/   IDATE1   = ${DATA_DIA},/" RAMSIN6
   sed -i "23s/.*/   IYEAR1   = ${ANOLONGO},/" RAMSIN6
   sed -i "24s/.*/   ITIME1   = ${HORA_INI}00,/" RAMSIN6

   /home/rede/mpich2111p1/bin/mpiexec ./rams -f RAMSIN6
   # ./rams -f RAMSIN6

   rm RAMSIN6

   cd isan ; ls *isan* || exit 1

}


function initial(){

   cd $WORDKDIR || exit

   mkdir out

   rm -rf out/*

   cp $SDIR/RAMSIN6-25km RAMSIN6 || exit
   # cp $SDIR/rams-6.0-opt rams || exit
   cp $SDIR/rams60-marcelo/rams60/build/60/rams-6.0-opt rams || exit
   cp $SDIR/grade01.dat.25km grade01.dat || exit


   sed -i "11s/.*/   RUNTYPE  = 'INITIAL',/" RAMSIN6
   sed -i "15s/.*/   TIMMAX   = ${HORA_FINAL},/" RAMSIN6
   sed -i "21s/.*/   IMONTH1  = ${DATA_MES},/" RAMSIN6
   sed -i "22s/.*/   IDATE1   = ${DATA_DIA},/" RAMSIN6
   sed -i "23s/.*/   IYEAR1   = ${ANOLONGO},/" RAMSIN6
   sed -i "24s/.*/   ITIME1   = ${HORA_INI}00,/" RAMSIN6

   GET_PROC=$(cat /proc/cpuinfo | grep processor | tail -1)
   NPROC="${GET_PROC: -2}"

   /home/rede/mpich2111p1/bin/mpiexec -n $NPROC ./rams -f RAMSIN6

   rm RAMSIN6

}


function runrevu(){

   cd $WORDKDIR || exit

   cp $SDIR/revu-2.5 . || exit
   # cp $SDIR/rams60-marcelo/rams60/build/revu25/revu-2.5 . || exit
   cp $SDIR/REVU_IN.g1.sigma.25km REVU_IN.g1.sigma || exit

   rm arquivo_cdf.cdf

   ./revu-2.5 -f REVU_IN.g1.sigma

   [ -s arquivo_cdf.cdf ] && mv arquivo_cdf.cdf pcp.rgkf.${D}${HORA_INI}.25km.cdf || exit

   rm REVU_IN.g1.sigma
   
}


function makesurface3km(){

   cd ${WORDKDIR}/../../

   rm -rf DEM30s FAOdata-h5 ndvi ogedata sst sst-h5

   ln -s $SDIR/DEM30s DEM30s
   ln -s $SDIR/FAOdata-h5 FAOdata-h5
   ln -s $SDIR/ndvi ndvi
   ln -s $SDIR/ogedata ogedata
   # ln -s $SDIR/sst-clim sst-h5  # climatology
   ln -s $SDIR/sst sst-h5  # oisst

   cd $WORDKDIR || exit

   mkdir surfbr62

   rm -rf surfbr62/*

   cp $SDIR/RAMSIN6-3km RAMSIN6 || exit
   # cp $SDIR/rams2008 rams || exit
   cp $SDIR/rams60-marcelo/rams60/build/60/rams-6.0-opt rams || exit
   cp $SDIR/grade01.dat.3km grade01.dat || exit
   cp $SDIR/grade02.dat.3km grade02.dat || exit

   sed -i "11s/.*/   RUNTYPE  = 'MAKESFC',/" RAMSIN6
   sed -i "15s/.*/   TIMMAX   = ${HORA_FINAL},/" RAMSIN6
   sed -i "21s/.*/   IMONTH1  = ${DATA_MES},/" RAMSIN6
   sed -i "22s/.*/   IDATE1   = ${DATA_DIA},/" RAMSIN6
   sed -i "23s/.*/   IYEAR1   = ${ANOLONGO},/" RAMSIN6
   sed -i "24s/.*/   ITIME1   = ${HORA_INI}00,/" RAMSIN6

   /home/rede/mpich2111p1/bin/mpiexec ./rams -f RAMSIN6
   # ./rams -f RAMSIN6

   rm RAMSIN6

}


function makevfile3km(){

   cd $WORDKDIR || exit

   mkdir isan

   rm -rf isan/*

   cp $SDIR/RAMSIN6-3km RAMSIN6 || exit
   # cp $SDIR/rams60_mkvfile rams || exit
   cp $SDIR/rams60-marcelo/rams60/build/60/rams-6.0-opt rams || exit
   cp $SDIR/grade01.dat.3km grade01.dat || exit
   cp $SDIR/grade02.dat.3km grade02.dat || exit

   sed -i "11s/.*/   RUNTYPE  = 'MAKEVFILE',/" RAMSIN6
   sed -i "16s/.*/   TIMMAX   = ${HORA_FINAL},/" RAMSIN6
   sed -i "22s/.*/   IMONTH1  = ${DATA_MES},/" RAMSIN6
   sed -i "23s/.*/   IDATE1   = ${DATA_DIA},/" RAMSIN6
   sed -i "24s/.*/   IYEAR1   = ${ANOLONGO},/" RAMSIN6
   sed -i "25s/.*/   ITIME1   = ${HORA_INI}00,/" RAMSIN6

   /home/rede/mpich2111p1/bin/mpiexec ./rams -f RAMSIN6
   # ./rams -f RAMSIN6

   rm RAMSIN6

   cd isan ; ls *isan* || exit 1

}


function initial3km(){

   cd $WORDKDIR || exit

   mkdir out

   rm -rf out/*

   cp $SDIR/RAMSIN6-3km RAMSIN6 || exit
   # cp $SDIR/rams-6.0-opt rams || exit
   cp $SDIR/rams60-marcelo/rams60/build/60/rams-6.0-opt rams || exit
   cp $SDIR/grade01.dat.3km grade01.dat || exit
   cp $SDIR/grade02.dat.3km grade02.dat || exit

   sed -i "11s/.*/   RUNTYPE  = 'INITIAL',/" RAMSIN6
   sed -i "16s/.*/   TIMMAX   = ${HORA_FINAL},/" RAMSIN6
   sed -i "22s/.*/   IMONTH1  = ${DATA_MES},/" RAMSIN6
   sed -i "23s/.*/   IDATE1   = ${DATA_DIA},/" RAMSIN6
   sed -i "24s/.*/   IYEAR1   = ${ANOLONGO},/" RAMSIN6
   sed -i "25s/.*/   ITIME1   = ${HORA_INI}00,/" RAMSIN6

   GET_PROC=$(cat /proc/cpuinfo | grep processor | tail -1)
   NPROC="${GET_PROC: -2}"

   /home/rede/mpich2111p1/bin/mpiexec -n ${NPROC} ./rams -f RAMSIN6

   rm RAMSIN6

}


function runrevu3km(){

   cd $WORDKDIR || exit

   cp $SDIR/revu-2.5 . || exit
   cp $SDIR/REVU_IN.g1.sigma.3km REVU_IN.g1.sigma || exit

   rm arquivo_cdf.cdf

   ./revu-2.5 -f REVU_IN.g1.sigma

   [ -s arquivo_cdf.cdf ] && mv arquivo_cdf.cdf pcp.rgkf.${D}${HORA_INI}.3km.cdf || exit

   rm REVU_IN.g1.sigma

}


function cleanrun(){

    cd $WORDKDIR || exit

    rm rams2008 erros_interp.dat teste.cdf tsm0.dat
    rm rams60_mkvfile rams-6.0-opt grade01.dat RAMSIN6 rams2.0
    rm revu-2.5 REVU_IN.g1.sigma arquivo_cdf_solo3d.cdf
    rm -rf surfbr62 isan rams out

    [ -s pcp.rgkf.${D}${HORA_INI}.25km.cdf ] && [ -s pcp.rgkf.${D}${HORA_INI}.3km.cdf ] && rm -rf dp-p* out || exit 

    ls -l pcp.rgkf.${D}${HORA_INI}.25km.cdf pcp.rgkf.${D}${HORA_INI}.3km.cdf

}

copydp
makesurface
makevfile
initial
runrevu
makesurface3km
makevfile3km
initial3km
runrevu3km
cleanrun

echo
echo "ini:   ${SI}"
echo "fim: " $(date +%c)
echo


