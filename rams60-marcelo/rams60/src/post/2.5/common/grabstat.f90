!
! Copyright (C) 1991-2003  ; All Rights Reserved ; ATMET, LLC
! 
! This file is free software; you can redistribute it and/or modify it under the
! terms of the GNU General Public License as published by the Free Software 
! Foundation; either version 2 of the License, or (at your option) any later version.
! 
! This software is distributed in the hope that it will be useful, but WITHOUT ANY 
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with this 
! program; if not, write to the Free Software Foundation, Inc., 
! 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!======================================================================================
!############################# Change Log ##################################
! 2.5.0
!###########################################################################

subroutine STAT_init (fobs,pred,fpos,ngdb,ngde)

! subroutine to fill array with obs locations

integer :: ngdb,ngde
real :: fobs(numtime,numpts,numfore)  &
       ,pred(numtime,numpts,numfore)  &
       ,fpos(numtime,numpts,8)

include 'vcomm2.h'

character flnm*128,sfcfile*128,rwnfile*128,cdate*15

print*
!print*,'STAT_init: ',ngdb,ngde

!...file names
call RAMS_get_cdata(0,nfile,flnm,nval)
lenhf=index(flnm,' ')-1
print*,'header file: ',flnm(1:lenhf)
cdate=flnm(lenhf-19:lenhf-4)
lensp=index(SFCPREF,' ')-1
SFCFILE=SFCPREF(1:lensp)//cdate//NOQ//' '
lenrp=index(RWNPREF,' ')-1
RWNFILE=RWNPREF(1:lenrp)//cdate//NOQ//' '
inquire(file=SFCFILE,exist=XST_SFC)
inquire(file=RWNFILE,exist=XST_RWN)
lensf=index(SFCFILE,' ')-1
print*,'surface file: ',SFCFILE(1:lensf),' ',XST_SFC
lenrf=index(RWNFILE,' ')-1
print*,'rawinsonde file: ',RWNFILE(1:lenrf),' ',XST_RWN
print*

! get obs data
if(cmode.eq.'sfc'.or.cmode.eq.'SFC'.or.  &
   cmode.eq.'all'.or.cmode.eq.'ALL') then
   if(XST_SFC) then
      !call surface_obs(fobs,fpos,ngdb,ngde,SFCFILE)
   else
      STOP 'STAT_init-sfc'
   endif
endif
if(cmode.eq.'rawin'.or.cmode.eq.'RAWIN'.or.  &
   cmode.eq.'all'.or.cmode.eq.'ALL') then
   if(XST_RWN) then
      !call pressure_obs(fobs,fpos,ngdb,ngde,RWNFILE)
   else
      STOP 'STAT_init-rwn'
   endif
endif
if(cmode.eq.'OTHER'.or.cmode.eq.'other') then
   print*,'EVENTUAL CODE HERE WILL INGEST DATA FOR'
   print*,'STATISTICAL COMPARISON FROM A SOURCE OTHER'
   print*,'THAN RAMS RALPH II IR/IS FILES. CURRENTLY'
   print*,'THIS CODE IS UNAVAILABLE.'
   STOP 'STAT_init-other'
endif
if(cmode.eq.'RAMS'.or.cmode.eq.'rams') then
   print*,'EVENTUAL CODE HERE WILL INGEST DATA FROM'
   print*,'RAMS ANALYSIS FILES FOR STATISTICAL COMPARISON.'
   print*,'NOT CURRENTLY IMPLEMENTED.'
   STOP 'STAT_init-rams'
endif

print*,'total obs found: ',istnloc(nfndx)
print*

! stop if too many obs
if(istnloc(nfndx).gt.MAXLOC) then
   print*,'istnloc(nfndx)=',istnloc(nfndx),'MAXLOC=',MAXLOC
   print*,'adjust MAXLOC in vcomm2.h'
   stop 'grab_obs: too many observations'
endif

!...should do a date and time check on the data here - something like...
!...but some times may not match exactly!
!      CALL RAMS_GET_IDATA(2,nfndx,1,JDATE,NVAL)
!      CALL RAMS_GET_IDATA(3,nfndx,1,JTIME,NVAL)
!      do i1=1,istnloc(nfndx)
!         print*,'kdate,ktime,jdate,jtime',kdate,ktime,jdate,jtime
!         if(kdate.ne.jdate.or.ktime.ne.jtime) then
!            do i3=1,NUMFORE
!               fobs(nfndx,i1,i3)=-999.
!            enddo
!            do i3=1,8
!               fpos(nfndx,i1,i3)=-999.
!            enddo
!         endif
!      enddo

return
end

!***************************************************************************

subroutine GRAB_init (pred,fpos,ngdb,ngde)

! fill obs locations for grabber

integer :: ngdb,ngde
real :: pred(numtime,numpts,numfore)  &
       ,fpos(numtime,numpts,8)

include 'vcomm2.h'

character line*256,idsta*8,tokens(20)*32

! input grabber points are located in the file GRABIN, with
!   5 columns of data: date, time, lat, lon, elev(agl).
!   lines starting with '#' or '!' are excluded

print*
!print*,'GRAB_init: ',ngdb,ngde,nfndx,nfile,' ',grabin(1:lastchar(grabin)+1)

open(unit=31,file=GRABIN,status='old')

ndxplev(1)=0
indx=0
i=0
10 continue
read(31,'(a)',end=99,err=99) line
if(line(1:1).eq.'#'.or.line(1:1).eq.'!') goto 10
call parse(line,tokens,ntok)
if(tokens(1).eq.'#'.or.tokens(1).eq.'!') goto 10
if(ntok.ne.1) then
   iver=1
   rewind(31)
else
   read(tokens(1),*) iver
endif
11 continue
read(31,'(a)',end=99,err=99) line
if(line(1:1).eq.'#'.or.line(1:1).eq.'!') goto 11
call parse(line,tokens,ntok)
if(tokens(1).eq.'#'.or.tokens(1).eq.'!') goto 11

if(iver.eq.1) then
   if(ntok.ne.5.and.(tokens(6).ne.'#'.and.tokens(6).ne.'!')) then
      print*,'iver:',iver
      print*,'ntok:',ntok
      STOP 'GRAB_init: incorrect number of tokens'
   endif
   !read(tokens(1),'(a)') idsta ! cannot do this with mem alloc until f90
   read(tokens(1),*) iidsta
   read(tokens(2),*) flat
   read(tokens(3),*) flon
   read(tokens(4),*) elev
   read(tokens(5),*) itype
   write(*,'(i10,3f11.5,i3)') iidsta,flat,flon,elev,itype
endif

! see if within ngdb to ngde
if(itype.eq.1)then
   zfz=elev
elseif(itype.eq.2)then
   zfz=0.
else
   print*,'illegal type',itype,' -> must be = 1 (pt) or 2 (prof)'
   goto 11
endif
! grid no??
call ll_xy (flat,flon,platn(1),plonn(1),xfx,yfy)
call findgrid (xfx,yfy,zfz,ngrid,xmn,nxpmax,nnxp,ymn,nypmax  &
              ,nnyp,zmn,nzpmax,nnzp,ngdb,ngde)
if(nint(flat).eq.-999.or.nint(flon).eq.-999.or.ngrid.eq.-1) then
   if(nint(flat).eq.-999.or.nint(flon).eq.-999) print*,'lat or lon = -999.'
   if(ngrid.eq.-1) print*,'  station outside requested grids'
   goto 11
endif

if(itype.eq.1)then
   indx=indx+1
   fpos(nfndx,indx,1)=flat
   fpos(nfndx,indx,2)=flon
   fpos(nfndx,indx,3)=xfx
   fpos(nfndx,indx,4)=yfy
   fpos(nfndx,indx,5)=elev
   fpos(nfndx,indx,6)=float(ngrid)
   ! this needs the station height ASL - interplooated to the location
   fpos(nfndx,indx,7)=-999.
   fpos(nfndx,indx,8)=float(iidsta)
   igrabgrd(ngrid)=1
   write(*,'(2(a,i3))') ' station found ',indx,' on grid ',ngrid
elseif(itype.eq.2)then
   do ii=1,nnzp(ngrid)-1
      indx=indx+1
      fpos(nfndx,indx,1)=flat
      fpos(nfndx,indx,2)=flon
      fpos(nfndx,indx,3)=xfx
      fpos(nfndx,indx,4)=yfy
      fpos(nfndx,indx,5)=zmn(ii,ngrid)
      fpos(nfndx,indx,6)=float(ngrid)
      ! this needs the station height ASL - interplooated to the location
      fpos(nfndx,indx,7)=-999.
      fpos(nfndx,indx,8)=float(iidsta)
      igrabgrd(ngrid)=1
   enddo
   write(*,'(2(a,i3))') ' levels found on grid ',ngrid,' for station ',indx
endif
goto 11

99 continue
close(unit=31)

istnloc(nfndx)=indx
ndxplev(1)=indx
print*,ndxplev(1),' locations total'
print*

! stop if too many obs
if(istnloc(nfndx).gt.maxloc) then
   print*,'istnloc(nfndx)=',istnloc(nfndx),'maxloc=',maxloc
   print*,'adjust maxloc in vcomm2.h'
   stop 'grab_obs: too many observations'
endif

return
end

!***************************************************************************

subroutine RAMS_grab (ivtype,a,coor,topo,n1,n2,n3,pred,fpos,nngd)

! subroutine will take the obs station location info and put it into grabber

integer :: ivtype,n1,n2,n3,nngd
real :: a(n1,n2,n3),coor(n1,n2,n3,3),topo(n1,n2)  &
       ,pred(numtime,numpts,numfore)  &
       ,fpos(numtime,numpts,8)

include 'vcomm2.h'

do nl=1,istnloc(nfndx)
   if(nint(fpos(nfndx,nl,6)).eq.nngd)  &
      call interp (ivtype,n1,n2,n3,a,topo,coor  &
                  ,zmn(nnzp(1)-1,1),ztn(1,nngd),vctr1,vctr2  &
                  ,fpos(nfndx,nl,3),fpos(nfndx,nl,4)  &
                  ,fpos(nfndx,nl,5),pred(nfndx,nl,nplot))
enddo

return
end

!***************************************************************************

subroutine GRAB_output (iun,pred,fpos)

integer :: iun
real :: pred(numtime,numpts,numfore)  &
       ,fpos(numtime,numpts,8)

include 'vcomm2.h'

call rams_get_idata (2,nfile,1,jdate,nval)
call rams_get_idata (3,nfile,1,jtime,nval)

do i2=1,istnloc(nfndx)
   write(iun,'(3i10,6e15.6,i10,30e15.6)')  &
     jdate,jtime,nint(fpos(nfndx,i2,8)),(fpos(nfndx,i2,i3),i3=1,2)  &
    ,fpos(nfndx,i2,7),(fpos(nfndx,i2,i3),i3=3,5)  &
    ,nint(fpos(nfndx,i2,6)),(pred(nfndx,i2,i3),i3=1,numfore)
enddo

return
end

!***************************************************************************

subroutine write_ralph (iun,pred,fpos)

integer :: iun
real :: pred(numtime,numpts,numfore)  &
       ,fpos(numtime,numpts,8)

include 'vcomm2.h'

dimension iflags(3,5)
character flnm*128,cdate*17,idsta*8
data rmiss /-999./
data irmiss /-999/
data iflags /15*0/

call RAMS_get_cdata(0,nfile,flnm,nval)
lenhf=len_trim(flnm)
cdate=flnm(lenhf-25:lenhf-9)
read(cdate(1:4),*) jyear
read(cdate(6:7),*) jmonth
read(cdate(9:10),*) jday
! jtime doesn't include seconds
read(cdate(12:15),*) jtime
read(cdate(16:17),*) jsec
jtime=jtime+nint(float(jsec)/60.)

! don't have this yet
slp=rmiss
pcp6=rmiss
pcp24=rmiss
sno=rmiss
cf=rmiss

do i2=1,istnloc(nfndx)
   write(idsta,'(i8)') nint(fpos(nfndx,i2,8))
   write(iun,5) jyear,jmonth,jday,jtime,idsta  &
               ,fpos(nfndx,i2,1),fpos(nfndx,i2,2)  &
               ,fpos(nfndx,i2,7)  &
               ,pred(nfndx,i2,1),(iflags(n,1),n=1,3)  &
               ,pred(nfndx,i2,2),(iflags(n,2),n=1,3)  &
               ,pred(nfndx,i2,3),(iflags(n,3),n=1,3)  &
               ,pred(nfndx,i2,4),(iflags(n,4),n=1,3)  &
               ,pred(nfndx,i2,5),(iflags(n,5),n=1,3)  &
               ,slp,999  &
               ,pcp6,999,pcp24,999,sno,999  &
               ,cf,999
enddo
5 format(i4.4,2x,2(i2.2,2x),i4.4,1x,a8  &
        ,2f9.3,f6.0  &
        ,f9.2,2x,3i1  &
        ,f7.0,2x,3i1  &
        ,2(f8.1,2x,3i1)  &
        ,f10.1,2x,3i1  &
        ,f10.1,2x,i3.3  &
        ,3(f7.1,2x,i3.3),2x  &
        ,f5.0,2x,i3.3)

return
end

!***************************************************************************

subroutine STATS_output (fobs,pred,fpos)

! print out statistical information

real :: fobs(numtime,numpts,numfore)  &
       ,pred(numtime,numpts,numfore)  &
       ,fpos(numtime,numpts,8)

include 'vcomm2.h'

character cparam*4,clevel*7,flnm*128,apref*80
character coutfile*128,cstatfile*128,cdate*17

print*,'STATS_output: '
print*

call RAMS_get_cdata(0,nfile,flnm,nval)
lenhf=len_trim(flnm)
cdate=flnm(lenhf-25:lenhf-9)
lasts=lastslash(flnm)
apref=flnm(lasts+1:lenhf-20)
lenap=len_trim(apref)
cstatfile='stats-'//apref(1:lenap)//cdate//NOQ//' '
coutfile='vals-'//apref(1:lenap)//cdate//NOQ//' '

print*,'printing out raw info in file ',coutfile
open(unit=45,file=coutfile,status='unknown')
write(45,*)'POSITIONS'
write(45,7) 'lat','lon','x (m)','y (m)','z (m agl)','grid'
do j=1,istnloc(nfndx)
   write(45,9) j,(fpos(nfndx,j,j1),j1=1,6)
enddo
write(45,*)' '
write(45,*)'OBSERVATIONAL DATA'
write(45,8) (STCHAR(j1),j1=1,numfore)
do j=1,istnloc(nfndx)
   write(45,9) j,(fobs(nfndx,j,j1),j1=1,numfore)
enddo
write(45,*)' '
write(45,*)'MODEL DATA'
write(45,8) (STCHAR(j1),j1=1,numfore)
do j=1,istnloc(nfndx)
   write(45,9) j,(pred(nfndx,j,j1),j1=1,numfore)
enddo
close(unit=45)
7 format(20a15)
8 format(10x,20a15)
9 format(i4,20f15.5)

print*,'printing out stats info in file ',cstatfile
open(unit=44,file=cstatfile,status='unknown')

if(CMODE.eq.'all'.or.CMODE.eq.'rawin') then
   do i1=1,NUMFORE
      if(i1.eq.1) cparam='U   '
      if(i1.eq.2) cparam='V   '
      if(i1.eq.3) cparam='TEMP'
      if(i1.eq.4) cparam='RH  '
      if(i1.eq.5) cparam='Q   '
      if(i1.eq.6) cparam='PRES'
      write(44,107)
      write(44,108)
      write(44,109) cparam
      do k=1,nplevs+1
         if(k.eq.1) then
            clevel='SURFACE'
         else
            write(clevel,'(i4,a3)') iplevs(k-1),' MB'
         endif
         write(44,110) clevel,(stat_dat(i1,j1,k),j1=1,9)
      enddo
   enddo
   107 format(50x,'MEAN')
   108 format(17x,'#',5x,'MEAN',5x,'MEAN',15x,'ABS',6x,'REL',23x,'CORR')
   109 format(a4,9x,'PAIRS',6x,'OBS',5x,'PRED',5x,'RMSE',6x,'ERR'  &
             ,6x,'ERR',5x,'BIAS',4x,'RMSVE',4x,'COEFF')
   110 format(a7,2x,9f9.2)
elseif(CMODE.eq.'sfc') then
   write(44,111)
   write(44,112)
   write(44,113)
   do i1=1,NUMFORE
      if(i1.eq.1) cparam='U   '
      if(i1.eq.2) cparam='V   '
      if(i1.eq.3) cparam='TEMP'
      if(i1.eq.4) cparam='RH  '
      if(i1.eq.5) cparam='Q   '
      if(i1.eq.6) cparam='PRES'
      write(44,114) cparam,(stat_dat(i1,j1,1),j1=1,9)
   enddo
   111 format(50x,'MEAN')
   112 format(17x,'#',5x,'MEAN',5x,'MEAN',15x,'ABS',6x,'REL',23x,'CORR')
   113 format(13x,'PAIRS',6x,'OBS',5x,'PRED',5x,'RMSE',6x,'ERR'  &
             ,6x ,'ERR',5x,'BIAS',4x,'RMSVE',4x,'COEFF')
   114 format(a4,5x,9f9.2)
endif

close(unit=44)

return
end

!***************************************************************************

subroutine interp (idim,n1,n2,n3,arr,topo,coor,ztop,zt,scr1,scr2  &
                  ,xfx,yfy,zfz,fval)
                  
dimension arr(n1,n2,n3),topo(n1,n2),vt2ds(4,4),coor(n1,n2,n3,3)  &
     ,scr1(*),scr2(*),zt(*)

dxx=coor(3,1,1,1)-coor(2,1,1,1)
dyy=coor(1,3,1,2)-coor(1,2,1,2)
xiloc=1.+(xfx-coor(1,1,1,1))/dxx
yiloc=1.+(yfy-coor(1,1,1,2))/dyy
xnni=mod(xiloc,1.)+2.
ynni=mod(yiloc,1.)+2.
nni=nint(xiloc)
nnj=nint(yiloc)

if(idim.eq.3) then
   do j=nnj-1,nnj+2
      do i=nni-1,nni+2
         top=valugp(n1,n2,1,i,j,1,topo)
         rtg=1.-top/ztop
         do k=1,n3
            scr1(k)=top+rtg*zt(k)
            scr2(k)=arr(i,j,k)
         enddo
         ! zfz should be height above surface...
         call htint (n3,scr2,scr1,1,vt2ds(i-nni+2,j-nnj+2),zfz+top)
      enddo
   enddo
elseif(idim.eq.2) then
   do j=nnj-1,nnj+2
      do i=nni-1,nni+2
         vt2ds(i-nni+2,j-nnj+2)=arr(i,j,1)
      enddo
   enddo
else
   stop 'interp- idim (first arg) can = 2 or 3'
endif

call gdtost (vt2ds,4,4,xnni,ynni,fval)
call gdtost (topo,n1,n2,xiloc,yiloc,topval)

return
end

!***************************************************************************

subroutine post_grab_sort (fobs,pred,fpos)

! subroutine so sort out obs, post-grabber, into
! pressure level groupings for statistical analysis

real :: fobs(numtime,numpts,numfore)  &
       ,pred(numtime,numpts,numfore)  &
       ,fpos(numtime,numpts,8)

include 'vcomm2.h'

real :: dummyo(maxfore),dummyp(maxfore),dummy(8)
integer :: iobs(nplevs+1)

!print*,'post_grid_sort:',numtime,numpts,numfore
!print*,'in pts:  ',nfndx,istnloc(nfndx)
!print*

! first sort by elevation (sfc obs have zero elevation)

do i1=istnloc(nfndx),2,-1
   do i2=1,i1
      if(fpos(nfndx,i2,5).gt.fpos(nfndx,i1,5)) then
         do i3=1,NUMFORE
            dummyo(i3)=fobs(nfndx,i2,i3)
            fobs(nfndx,i2,i3)=fobs(nfndx,i1,i3)
            fobs(nfndx,i1,i3)=dummyo(i3)
         enddo
         do i3=1,NUMFORE
            dummyp(i3)=pred(nfndx,i2,i3)
            pred(nfndx,i2,i3)=pred(nfndx,i1,i3)
            pred(nfndx,i1,i3)=dummyp(i3)
         enddo
         do i3=1,8
            dummy(i3)=fpos(nfndx,i2,i3)
            fpos(nfndx,i2,i3)=fpos(nfndx,i1,i3)
            fpos(nfndx,i1,i3)=dummy(i3)
         enddo
! need to sort out stations
      endif
   enddo
enddo

! zero out levels array
do i1=1,nplevs+1
  ndxplev(i1)=0
enddo

! check total and no of sfc levels
if(XST_SFC) then
   do i1=1,istnloc(nfndx)
      if(fpos(nfndx,i1,5).eq.0.0) ndxplev(1)=ndxplev(1)+1
   enddo
   !print*,'surface pts: ',ndxplev(1)
endif

! sort sounding data by pressure (leave sfc obs at beg of array)

if(XST_RWN) then
   do i1=istnloc(nfndx),ndxplev(1)+2,-1
      do i2=ndxplev(1)+1,i1
         if(fobs(nfndx,i2,6).lt.fobs(nfndx,i1,6)) then
            do i3=1,NUMFORE
               dummyo(i3)=fobs(nfndx,i2,i3)
               fobs(nfndx,i2,i3)=fobs(nfndx,i1,i3)
               fobs(nfndx,i1,i3)=dummyo(i3)
            enddo
            do i3=1,NUMFORE
               dummyp(i3)=pred(nfndx,i2,i3)
               pred(nfndx,i2,i3)=pred(nfndx,i1,i3)
               pred(nfndx,i1,i3)=dummyp(i3)
            enddo
            do i3=1,8
               dummy(i3)=fpos(nfndx,i2,i3)
               fpos(nfndx,i2,i3)=fpos(nfndx,i1,i3)
               fpos(nfndx,i1,i3)=dummy(i3)
           enddo
         endif
      enddo
   enddo
endif

! indexing between pressure levels (check levels with no data)

do i=1,nplevs+1
   iobs(i)=0
   ndxplev(i)=-1
enddo

do i=1,istnloc(nfndx)
   if(nint(fpos(nfndx,i,5)).eq.0) then
      iobs(1)=iobs(1)+1
   else
      do n=1,nplevs
         if(nint(fobs(nfndx,i,6)).eq.iplevs(n)) iobs(n+1)=iobs(n+1)+1
      enddo
   endif
enddo

ndxplev(1)=iobs(1)
!print*,iobs(1),' surface observations:   1 ',ndxplev(1)
itot=0
do n=1,nplevs
   ndxplev(n+1)=ndxplev(n)+iobs(n+1)
   !print*,iobs(n+1),iplevs(n),' mb obs:',ndxplev(n)+1,ndxplev(n+1)
   itot=itot+iobs(n+1)
enddo
!print*,itot,' upper air obs'
!print*

return
end

!***************************************************************************

subroutine store3d (n1,n2,n3,k,i,j,vvv,a)

dimension a(n1,n2,n3)

a(k,i,j)=vvv

return
end

!***************************************************************************

subroutine findgrid (xfx,yfy,zfz,ngr,xmn,maxx,nx,ymn,maxy,ny  &
                    ,zmn,maxz,nz,ngdb,ngde)
                   
dimension xmn(maxx,*),ymn(maxy,*),nx(*),ny(*),zmn(maxz,*),nz(*)

do ng=ngde,ngdb,-1
   if(xfx.le.xmn(nx(ng)-1,ng).and.xfx.ge.xmn(1,ng)  &
     .and.yfy.le.ymn(ny(ng)-1,ng).and.yfy.ge.ymn(1,ng)  &
     .and.zfz.le.zmn(nz(ng)-1,ng).and.zfz.ge.zmn(1,ng)) then
      ngr=ng
      return
   endif
   nl=ng
enddo

!print*,'in findgrid: point outside grid',nl
!print*,'findgrid: ymin=',ymn(1,nl),'ymax=',ymn(ny(nl)-1,nl),ny(nl)
!print*,'findgrid: xmin=',xmn(1,nl),'xmax=',xmn(nx(nl)-1,nl),nx(nl)
!print*,'findgrid: zmin=',zmn(1,nl),'zmax=',zmn(nz(nl)-1,nl),nz(nl)
!print*,'location x,y,z:',xfx,yfy,zfz
ngr=-1

return
end
