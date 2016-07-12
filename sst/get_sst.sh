#! /bin/sh

# Trazido do MASTER, de /p1-ipu/adwgandu/input_brams42/sst-week
# modificado gandu para trabalhar na p30 da FUNCEME
# Este script puxa a sst no novo formato (oisst.????????, versao 2) e executa
# o programa sst2RAMS.x o qual escreve no formato do RAMS/BRAMS.

# sintaxe:  get_sst.sh data DIR_out 
#
# onde:    data = Data no formato YYYYMMDD, onde o dia (DD) eh sempre numa quarta-feira. 
#               = auto => Pega o arquivo mais recente que encontra-se no site. 
#       DIR_out = Diretorio de saida da sst no formato RAMS.
#               = auto => $DIR

DIR=/home/rede/tempo/quali/trunk/scripts/2016/sst
DIR2=/home/rede/tempo/quali/trunk/scripts/2016/sst

data=$1
# DIR_out=$2
DIR_out=/home/rede/tempo/quali/trunk/scripts/2016/sst
gunzip=/bin/gunzip
rm=/bin/rm
cp=/bin/cp
mv=/bin/mv

echo "MOVENDO ARQUIVOS ANTIGOS (2 MESES ATRAS)..."
DATA2M_AGO=`date +%Y%m --date '2 months ago'`

ANO2=`echo $DATA2M_AGO | awk '{print substr($1,1,4)}'`
MES2=`echo $DATA2M_AGO | awk '{print substr($1,5,2)}'`

cd $DIR/RAMS

if test ! -s $ANO2; then
  mkdir $ANO2
  echo
  echo "Criando o diretorio: $DIR/RAMS/"$ANO2" ..."
  echo
else
  echo
  echo "Diretorio $DIR/RAMS/"$ANO2 "ja existe..."  
  echo
fi

TEM=`ls -1 W${ANO2}${MES2}* |wc -l`

if [ $TEM -gt 0 ]; then
  echo "Movendo arquivos de ${ANO2}${MES2} para dir $DIR/RAMS/$ANO2"
  echo ""
  mv W${ANO2}${MES2}* $DIR/RAMS/$ANO2
else
  echo "Nenhum arquivo a ser movido"
  echo ""
fi

cd $DIR

if [ x$data = "x" ]; then
  echo -n "Entre com a data (YYYYMMDD, DD-> quarta-feira)=> "
  read data 
fi

if [ x$DIR_out = "x" ]; then
  echo -n "Entre com o diretorio de saida=> "
  read DIR_out 
fi


if [ $DIR_out = "auto" ]; then DIR_out=$DIR/RAMS;else DIR=$DIR_out; fi
if test ! -s $DIR_out; then mkdir $DIR_out; fi
if test ! -s $DIR_out; then echo "Nao existe o diretorio $DIR_out, saindo..."; exit; fi


if [ $data = "auto" ]; then
cat << Eof | ftp -in ftp.emc.ncep.noaa.gov > $DIR/list
passive
user anonymous adilsonwagner.gandu@funceme.br
cd /cmb/sst/oisst_v2/
epsv
lcd $DIR
dir oisst.*
bye
Eof
file=`tail -1 $DIR/list | awk '{print $9}'`
data=`echo $file | awk '{print substr($1,7,8)}'`
fi

if test ! -s $DIR2/oisst.${data}*; then
echo "Nao existe o arquivo $DIR2/oisst.${data}* tentando baixar do site"
cat << Eof | ftp -in ftp.emc.ncep.noaa.gov > $DIR/list
user anonymous adilsonwagner.gandu@funceme.br
passive
cd /cmb/sst/oisst_v2/
epsv
lcd $DIR
bin
mget oisst.${data}* 
dir
bye
Eof
else
  if [ $DIR = $DIR_out ]; then ${cp} $DIR2/oisst.${data}* $DIR; fi
fi

if test ! -s $DIR/oisst.${data}*; then
  echo "Nao existe o arquivo $DIR/oisst.${data}, saindo..."
  echo
  echo "Arquivos existentes no site:"
  cat $DIR/list | grep oisst
  echo
  echo "Arquivos existentes em $DIR2:"
  ls $DIR2 | grep oisst
  exit
fi   

if [ $DIR != $DIR_out ]; then ${cp} $DIR/oisst.${data}* $DIR_out; fi
if test -s $DIR_out/oisst.${data}*.gz; then ${gunzip} $DIR_out/oisst.${data}*.gz; fi

cd $DIR_out
echo "Gerando a sst do dia=>" $data
echo
mes=`echo $data | awk '{print substr($1,5,2)}'`
if [ $mes = "01" ]; then mes="JAN"; fi
if [ $mes = "02" ]; then mes="FEB"; fi
if [ $mes = "03" ]; then mes="MAR"; fi
if [ $mes = "04" ]; then mes="APR"; fi
if [ $mes = "05" ]; then mes="MAY"; fi
if [ $mes = "06" ]; then mes="JUN"; fi
if [ $mes = "07" ]; then mes="JUL"; fi
if [ $mes = "08" ]; then mes="AUG"; fi
if [ $mes = "09" ]; then mes="SEP"; fi
if [ $mes = "10" ]; then mes="OCT"; fi
if [ $mes = "11" ]; then mes="NOV"; fi
if [ $mes = "12" ]; then mes="DEC"; fi

echo "Arquivo do mes de "$mes
echo
$DIR2/sst2RAMS.x $DIR_out/oisst.${data} ${data}


#if test -s $DIR_out/oisst.${data}; then ${rm} -f $DIR_out/oisst.${data}; fi

echo "Criando o arquivo WHEADER..."

ls -1 *90S180W > $DIR_out/list
nsem=`cat $DIR_out/list | wc -l`
nsem=`expr $nsem + 0`

echo "  180  181  -90 -180 -0.500000 -0.500000" > $DIR_out/WHEADER
echo "     $nsem" >> $DIR_out/WHEADER
i=1
while [ $i -le $nsem ]; do
  data2=`head -$i $DIR_out/list | tail -1 | awk '{print substr($1,2,8)}'`
  ano=`echo $data2 | awk '{print substr($1,1,4)}'`
  mes=`echo $data2 | awk '{print substr($1,5,2)}'`
  dia=`echo $data2 | awk '{print substr($1,7,2)}'`
  #echo "$data2 $ano   $mes   $dia   00" >> $DIR_out/WHEADER
  echo "$data2 0000   $mes   $dia   00" >> $DIR_out/WHEADER  #ateh corrigir o bug da SSTUPDATE
  i=`expr $i + 1`
done 

 #Apago os arquivos "log" antigos...
find $DIR -name "*.out_*" -mtime +2 -exec rm -f {} \;

echo "" >> $DIR_out/WHEADER
echo "(bksz   NO  lat  lon   iofflat   iofflon)" >> $DIR_out/WHEADER
echo "(ntimes)" >> $DIR_out/WHEADER
echo "(fpfx year  mon date hour)" >> $DIR_out/WHEADER
#meses="

#cat << Eof | ftp -in 143.107.15.35
#user trans in&out!
#cd in
#mkdir sst-week
#cd sst-week
#lcd $DIR/RAMS
#put WHEADER
#mput W$ano*
#bye
#Eof

