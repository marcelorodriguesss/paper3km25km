#
# make file for wgrib2
# 
# compiles jasper and grib2c libraries
# then tries to compile wgrib2
#
# mod 1/07 M. Schwarb (libgrib2c name change)
#
# this version uses netcdf libraries -- compile C only version
#  get from UCAR if compile doesn't work
#

j=jasper-1.900.1
jlib=$j/src/libjasper/.libs/libjasper.a

g=g2clib-1.0.5
glib=$g/libgrib2c.a

w=wgrib2
prog=$w/wgrib2

n=netcdf-3.6.2
nlib=$n/libsrc/.libs/libnetcdf.a


all:	$j ${jlib} ${glib} ${nlib} ${prog}

$j:	$j.zip
	unzip $j.zip

${jlib}:	$j
	cd $j;./configure;make

${glib}:	${jlib}
	cd $g;make

$n:	netcdf.tar.gz
	cp netcdf.tar.gz tmp.tar.gz
	gunzip tmp.tar.gz
	tar -xvf tmp.tar
	rm tmp.tar

${nlib}:	$n
	cd $n ; ./configure --enable-c-only ; make check

${prog}:	$w/wgrib2.c $w/*.c $w/*.h ${glib} ${jlib} ${nlib}
	cd $w; make -f makefile

clean:
	cd $w; make clean
	cd $g ; touch junk.a junk.o ; rm *.o *.a
	cd $n ; make clean

