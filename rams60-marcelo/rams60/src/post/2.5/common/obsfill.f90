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

subroutine surface_obs (fobs,fpos,ngdb,ngde,SFCFILE)

include 'vcomm2.h'
dimension fobs(numtime,numpts,numfore),fpos(numtime,numpts,8)
character SFCFILE*128,line*256,asta*5,tokens(20)*32
dimension iflags(5),iflagr(4)

print*,'surface_obs: ',nfndx,numtime,numpts,numfore
print*

open(unit=32,file=SFCFILE,status='old')

! READ HEADER INFO AT TOP OF SURFACE FILE
! THIS IS A QUICK-FIX HEADER READ-DOES NOT INSPECT VARIABLES...
print*,'reading header...'
read(32,'(a)') line
call parse(line,tokens,ntok)
read(tokens(1),*) ivflag
if(ivflag.eq.999999) then
   backspace(unit=32)
   read(32,*) isflag,ivernum
   read(32,*) iparam
else
   iparam=ivflag
   isflag=111111
   ivernum=0
endif

print*,ivflag,isflag,ivernum

do i1=1,iparam
   read(32,'(a)')line
   print*,line(1:30)
enddo

print*
print*,'reading the obs...'
if(ivernum.eq.0) read(32,*)

! attempt to read new station - end if no more records
indx=0
10 continue
read(32,'(a256)',end=50,err=50) line

if(ivernum.eq.0) then

   ! OLD STYLE READ - data, with quality flags at end of line
   call parse(line,tokens,ntok)
   read(tokens(1),*) jdate
   read(tokens(2),*) jtime
   read(tokens(3),'(a)') asta
   read(tokens(4),*) flat
   read(tokens(5),*) flon
   read(tokens(6),*) elev
   read(tokens(7),*) wspd
   read(tokens(8),*) wdir
   read(tokens(9),*) tmpc
   read(tokens(10),*) dewpt
   read(tokens(11),*) pres
   print*,jdate,jtime,' ',asta,' ',flat,flon,elev,wspd,wdir,tmpc,dewpt,pres
else

   ! NEW STYLE READ - data and quality flags in pairs
   ! MODIFIED TO READ RALPH II FORMAT...
   call parse(line,tokens,ntok)
   read(tokens(1),*) jyear
   read(tokens(2),*) jmonth
   read(tokens(3),*) jday
   read(tokens(4),*) jtime
   read(tokens(5),'(a)') asta
   read(tokens(6),*) flat
   read(tokens(7),*) flon
   read(tokens(8),*) elev
   read(tokens(9),*) wspd
   read(tokens(10),*) iflags(1)
   read(tokens(11),*) wdir
   read(tokens(12),*) iflags(2)
   read(tokens(13),*) tmpc
   read(tokens(14),*) iflags(3)
   read(tokens(15),*) dewpt
   read(tokens(16),*) iflags(4)
   read(tokens(17),*) pres
   read(tokens(18),*) iflags(5)
   print*,jyear,jmonth,jday,jtime,' ',asta,' ',flat,flon,elev  &
         ,wspd,iflags(1),wdir,iflags(2),tmpc,iflags(3),dewpt  &
         ,iflags(4),pres,iflags(5)

   ! put in some code to use quality flags...
   do i1=1,5
      if(iflags(i1).gt.90) then
         iflag1=iflags(i1)/100
         iflag2=mod(iflags(i1)/10,10)
         iflag3=mod(iflags(i1),10)
      elseif(iflags(i1).gt.9) then
         iflag1=0
         iflag2=iflags(i1)/10
         iflag3=mod(iflags(i1),10)
      else
         iflag1=0
         iflag2=0
         iflag3=iflags(i1)
      endif

      ! use of quality flags can vary-can use all flags, just one of the
      ! checks, or any combination...

      ! NOT USING FLAG3 (GRID CHECK) RIGHT NOW...
      if(iflag1.eq.9.or.iflag1.eq.1.or.iflag3.eq.9) then
         if(i1.eq.1) wspd=-999.
         if(i1.eq.2) wdir=-999.
         if(i1.eq.3) tmpc=-999.
         if(i1.eq.4) dewpt=-999.
         if(i1.eq.5) pres=-999.
      endif
   enddo
endif

300 continue

! see if within ngdb to ngde
! grid no??
call LL_XY(flat,flon,platn(1),plonn(1),xfx,yfy)
CALL FINDGRID(XFX,YFY,0.,ngrid,xmn,nxpmax,nnxp,ymn,nypmax  &
             ,nnyp,zmn,nzpmax,nnzp,ngdb,ngde)
if(nint(flat).eq.-999.or.nint(flon).eq.-999.or.ngrid.eq.-1) then
   if(nint(flat).eq.-999.or.nint(flon).eq.-999) print*,'lat or lon = -999.'
   if(ngrid.eq.-1) print*,'station outside requested grids'
   goto 10
endif

indx=indx+1
print*,'station found',ngrid,indx

fpos(nfndx,indx,1)=flat
fpos(nfndx,indx,2)=flon
fpos(nfndx,indx,3)=xfx
fpos(nfndx,indx,4)=yfy
fpos(nfndx,indx,5)=0.
fpos(nfndx,indx,6)=float(ngrid)
fpos(nfndx,indx,7)=elev
! this needs the station id as a real # - until can alloc char
fpos(nfndx,indx,8)=0.
igrabgrd(ngrid)=1

! WINDS u and v from speed and dir
if(nint(wspd).ne.-999.and.nint(wdir).ne.-999) then
   call ddff2uv(wspd,wdir,uu,vv)
   fobs(nfndx,indx,1)=uu
   fobs(nfndx,indx,2)=vv
else
   fobs(nfndx,indx,1)=-999.
   fobs(nfndx,indx,2)=-999.
endif

! TEMPERATURE
fobs(nfndx,indx,3)=tmpc

! RELATIVE HUMIDITY from temp and dew pt
if(nint(tmpc).ne.-999.and.nint(dewpt).ne.-999) then
   call getrh(tmpc,dewpt,rh)
   fobs(nfndx,indx,4)=rh
else
   fobs(nfndx,indx,4)=-999.
endif

! SPECIFIC HUMIDITY
! determine saturation mixing ratio
if((nint(tmpc).ne.-999).and.(nint(pres).ne.-999).and.(nint(rh).ne.-999)) then
   w=rh*r_sub_s(pres,tmpc+273.16)/100.
   ! use w to determine q, specific humidity (NOTE: this is specific humidity
   ! for VAPOR only, not all species of water, vapor+liquid+ice)
   spechum=1000.*w/(1.0+w)
   if(spechum.lt.0.0) print*,'OBSFILL ERROR IN Q: RH=',rh,'W=',w,'WSUBS=',wsubs
else
   spechum=-999.
endif
fobs(nfndx,indx,5)=spechum

! PRESSURE
if(pres.gt.0.0) then
   fobs(nfndx,indx,6)=pres/100.
else
   fobs(nfndx,indx,6)=pres
endif

! get another station
goto 10

50 continue
close(unit=32)

istnloc(nfndx)=indx
ndxplev(1)=indx
print*,istnloc(nfndx),' surface stations'
print*

return
end

!***************************************************************************

subroutine pressure_obs(fobs,fpos,ngdb,ngde,RWNFILE)

include 'vcomm2.h'
dimension fobs(numtime,numpts,numfore),fpos(numtime,numpts,8)
dimension windbuf(MAXLEV,3)
character RWNFILE*128,line*80,asta*5,ciden*5,tokens(20)*32

print*,'pressure_obs: ',nfndx,numtime,numpts,numfore
print*

open(unit=33,file=RWNFILE,status='old')

!...MORE QUICK-FIX READ FOR EVER-CHANGING DPREP FORMATS...
read(33,*)isflag,ivernum
print*,isflag,ivernum
if(isflag.ne.999999) then
   isflag=111111
   ivernum = 0
endif

if(ivernum.eq.0) backspace(unit=33)

   ! separate treatment for rawin and surface data
   ! this is the treatment for rawin data. sfc subroutine is cws_obsfill

   ! read a value from obs file
   indx2=ndxplev(1)
   indxold=ndxplev(1)
   istns=0
   istnobs=0
   10 read(33,'(a)',end=99,err=99) line

   call parse(line,tokens,ntok)
   if(ivernum.eq.0)then
      read(tokens(1),*) idate
      read(tokens(2),*) itime
      read(tokens(3),'(a)') ciden
      read(tokens(4),*) iplev
      read(tokens(5),*) iwlev
      read(tokens(6),*) flat
      read(tokens(7),*) flon
      read(tokens(8),*) elev
      print*,idate,itime,' ',ciden,' ',iplev,iwlev,flat,flon,elev
   else
      read(tokens(1),*) ijyear
      read(tokens(2),*) ijmonth
      read(tokens(3),*) ijday
      read(tokens(4),*) ijtime
      read(tokens(5),'(a)') ciden
      read(tokens(6),*) iplev
      read(tokens(7),*) iwlev
      read(tokens(8),*) flat
      read(tokens(9),*) flon
      read(tokens(10),*) elev
      print*,ijyear,ijmonth,ijday,ijtime,' ',ciden,' ',iplev  &
            ,iwlev,flat,flon,elev
endif

! stop if too many levels
if(iplev.gt.maxlev.or.iwlev.gt.maxlev) then
   print*,'iplev=',iplev,'iwlev=',iwlev,'MAXLEV=',MAXLEV
   print*,'adjust MAXLEV in vcomm.h - see also MAXLEV in ascoms.h'
   stop 'pressure_obs: too many levels in profile '
endif

! see if within ngdb to ngde - if not, read past data
! grid no??
call LL_XY(flat,flon,platn(1),plonn(1),xfx,yfy)
CALL FINDGRID(XFX,YFY,0.,ngrid,xmn,nxpmax,nnxp,ymn,nypmax  &
             ,nnyp,zmn,nzpmax,nnzp,ngdb,ngde)

if(nint(flat).eq.-999.or.nint(flon).eq.-999.or.iplev.eq.0.or.ngrid.eq.-1) then
   if(nint(flat).eq.-999.or.nint(flon).eq.-999) print*,'lat or lon = -999.'
   if(ngrid.eq.-1) print*,'station outside requested grids'
   if(iplev.eq.0) print*,'no thermodynamic levels'
   if(iplev.ne.0) then
      do i1=1,iplev
         read(33,*)
      enddo
   endif
   if(iwlev.ne.0) then
      do i1=1,iwlev
         read(33,*)
      enddo
   endif
   goto 10
endif
istns=istns+1

do i1=1,iplev
   read(33,'(a)') line
   if(ivernum.eq.0) then
      read(line,*) pres,thgt,tmpc,relhum
   else
      read(line,*) pres,iflag1,thgt,iflag2,tmpc,iflag3,relhum,iflag4
   endif

   ! make sure that sounding z-level is not too high for the model grid
   zfz=thgt-elev
   call findgrid(xfx,yfy,zfz,ngrid,xmn,nxpmax,nnxp  &
                ,ymn,nypmax,nnyp,zmn,nzpmax,nnzp,ngdb,ngde)
   ! got this level?
   plok=0
   do ipl=1,nplevs
      if(nint(pres/100.).eq.iplevs(ipl)) plok=1
   enddo

   if(plok.eq.0.or.ngrid.eq.-1) then
      if(ngrid.eq.-1) PRINT*,'height above grid top'
      goto 20
   else
      indx2=indx2+1
      istnobs=istnobs+1
   endif

   ! put locations into array
   fpos(nfndx,indx2,1)=flat
   fpos(nfndx,indx2,2)=flon
   fpos(nfndx,indx2,3)=xfx
   fpos(nfndx,indx2,4)=yfy
   fpos(nfndx,indx2,5)=zfz
   fpos(nfndx,indx2,6)=float(ngrid)
   fpos(nfndx,indx2,7)=elev
   ! this needs the station id as a real # - until can alloc char
   fpos(nfndx,indx2,8)=0.
   igrabgrd(ngrid)=1

   !print*,'PRESSURE LEVEL:',pres,thgt,tmpc,relhum

   ! mess with quality flags - don't look at pressure or rh
   zflag = iflag2/100
   zflag1 = mod(iflag2,10)
   tflag = iflag3/100
   tflag1 = mod(iflag3,10)

   if(zflag.eq.1.or.zflag1.eq.9) thgt=-999.
   if(tflag.eq.1.or.tflag1.eq.9) tmpc=-999.

   if((nint(relhum).ne.-999).and.(nint(tmpc).ne.-999)) then
      fobs(nfndx,indx2,4)=relhum*100.
      wsubs=r_sub_s(pres,tmpc+273.16)
      w=relhum*wsubs
      fobs(nfndx,indx2,5)=1000.*w/(1.0+w)
   else
      fobs(nfndx,indx2,4)=-999.
      fobs(nfndx,indx2,5)=-999.
   endif

   fobs(nfndx,indx2,3)=tmpc
   fobs(nfndx,indx2,6)=pres/100.

20      continue
enddo

! do wind - WINDBUF is the temporary array to hold wind info for reading
!   into the FOBS array.
! WINDBUF(1)=height, WINDBUF(2)=windspeed, and WINDBUF(3)=direction.

if(iwlev.ne.0) then
   do i2=1,iwlev
      read(33,'(a)') line
      if(ivernum.eq.0) then
         read(line,*) windbuf(i2,1),windbuf(i2,2),windbuf(i2,3)
      else
         read(line,*) windbuf(i2,1),iflag1,windbuf(i2,2),iflag1  &
                     ,windbuf(i2,3),iflag3
         ! utilize quality flags
         if(iflag2/100.eq.1.or.mod(iflag2,10).eq.9.or.  &
            iflag3/100.eq.1.or.mod(iflag3,10).eq.9) then
            windbuf(i2,2)=-999.
            windbuf(i2,3)=-999.
         endif
      endif
   enddo

   do i1=indxold+1,indx2
      fobs(nfndx,i1,1)=-999.
      fobs(nfndx,i1,2)=-999.
   enddo

   do i1=indxold+1,indx2
      hdif=100.
      do i2=1,iwlev
         if(nint(fpos(nfndx,i1,1)).ne.-999.and.  &
            nint(fpos(nfndx,i1,2)).ne.-999.and.  &
          abs(fpos(nfndx,i1,5)-(windbuf(i2,1)-elev)).lt.hdif) then
            hdif=abs(fpos(nfndx,i1,5)-(windbuf(i2,1)-elev))
            call ddff2uv(windbuf(i2,2),windbuf(i2,3),uu,vv)
            fobs(nfndx,i1,1)=uu
            fobs(nfndx,i1,2)=vv
         endif
      enddo
      !print*,'winds',i1,hdif,fobs(nfndx,i1,1),fobs(nfndx,i1,2)
   enddo
else
   do i1=indxold+1,indx2
      fobs(nfndx,i1,1)=-999.
      fobs(nfndx,i1,2)=-999.
   enddo
endif

print*,'station found',ngrid,istns,istnobs,indx2
indxold=indx2
istnobs=0
goto 10

99 close(33)

istnloc(nfndx)=indx2
ndxplev(11)=indx2
print*,ndxplev(11)-ndxplev(1),' upper air obs at',istns,' stations'
print*

return
end

!****************************************************************************

subroutine fndln(text,ltext,order)

character*(*) text
integer ltext,order
if(order.eq.1) then
   do i=1,len(text)
      if(text(i:i).ne.' ') then
         ltext=i
         goto 10
      endif
   enddo
   10 continue
else
   do i=len(text),1,-1
      if (text(i:i).ne.' ') then
         ltext=i
         goto 20
      endif
   enddo
   20 continue
endif
return
end

!***************************************************************************

subroutine ddff2uv(speed,dir,u,v)

real speed,dir,u,v

! convert dir to radian
dir2=dir*(3.14159/180.)
if (speed.eq.0.) then
   u=0.
   v=0.
   return
else
   u=-speed*(sin(dir2))
   v=-speed*(cos(dir2))
endif
if((dir.lt.0.0.or.dir.gt.360.0).or.  &
  (speed.lt.0.0.or.speed.gt.300.0))then
   u=-999.
   v=-999.
endif

return
end

!***************************************************************************

subroutine getrh(tmpc,dewp,rh)

! use 100*e/es to get percentage, other line for decimal

real tmpc, dewp, rh, vap

t =tmpc+273.16
td=dewp+273.16
es=vap(t)
e =vap(td)
rh=100.*e/es
if(rh.lt.0.0) print*,'OBSFILL ERROR IN RH: T=',t,'TD=',td,'ES=',es,'E=',e

return
end

!***************************************************************************

FUNCTION VAP(t)
   e=(-2937.4*log(10.)/t)-(4.9283*log(t))+23.5470*log(10.)
   vap=exp(e)
END

!***************************************************************************

function r_sub_s (pres_Pa,tmpc_K)

! this calls function e_sub_s which computes saturation
! vapor pressure (Pa) and converts to sat. mixing ratio (kg/kg)
!    pres_Pa - pressure (Pa)
!    tmpc_K - tmpcerature (k)

es=e_sub_s(tmpc_K)
r_sub_s=0.622*es/(pres_Pa-es)

return
end

!***************************************************************************

function e_sub_s (tmpc_K)

! compute saturation vapor pressure (Pa) over liquid with
! polynomial fit of Goff-Gratch (1946) formulation. (Walko, 1991)

dimension c(0:8)
data c/610.5851,44.40316,1.430341,0.2641412e-1,0.2995057e-3  &
      ,0.2031998e-5,0.6936113e-8,0.2564861e-11,-0.3704404e-13/

x=max(-80.,tmpc_K-273.16)
e_sub_s=c(0)+x*(c(1)+x*(c(2)+x*(c(3)+x*(c(4)+x*(c(5)  &
       +x*(c(6)+x*(c(7)+x*c(8))))))))

return
end
