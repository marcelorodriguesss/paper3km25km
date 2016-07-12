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

subroutine RAMS_medoc (a,coor,itrans,ivtype,nngd,n1,n2,n3  &
                      ,zlevs,iyear1,imonth1,idate1,itime1  &
                      ,fcstsec,dx,dy,x1,y1,alat1,alon1  &
                      ,revpref,cframe_a,maxfore,ihtflx)

!-----------------------------------------------------------------
! Arguments:
! ----------
! a - data
! itrans - type of vertical transformation 1-sigma-z, 2-Cartesian, 3-pressure
! ivtype - type of variable 2-2d surface, 3-3d atmospheric
! nngd - grid number
! coor - coordinates of RAMS grid
! n1,n2,n3 - actual dimensions of field
! n3 - number of atmospheric height levels
! zlevs(n3) - atmospheric coordinate levels
! iyear1 - year of model start
! imonth1 - month of model start
! idate1 - date of model start
! itime1 - time of model start (hhmm)
! fcstsec - seconds into run
! cframe_a array of variables
! maxfore - maximum number of foregrounds

! Included in window.h
! -------------------
! islab - 1 = x-z, 2 = y-z, 3 = x-y slab
! nib,nie,njb,nje - horizontal or vertical "window" as chosen in namelist.
! icoord - slab value as chosen in namelist.
! nii,njj - number of windowed points

! Included in frame.h
! -------------------
! cvar - variable
!
!-----------------------------------------------------------------

use an_header

implicit none

integer :: itrans,ivtype,n1,n2,n3,iyear1,imonth1,idate1,itime1,nngd  &
          ,maxfore,ihtflx
real :: fcstsec,dx,dy,x1,y1,alat1,alon1
real :: a(n1,n2,n3),coor(n1,n2,n3,*),zlevs(*)
character(len=*) :: revpref,cframe_a(*)

include 'frame.h'
include 'window.h'

character(len=256),save :: flnm='none'
character(len=128) :: cname(maxfore)
character(len=16) :: frtokens(50)
character(len=3) :: csuff,cmon(12)
character(len=1) :: toksep
character(len=8), save :: namdum
character(len=8), dimension(maxframes), save :: namv3d,namv2d,unitv3d  &
                                               ,unitv2d
integer :: iyear2,imonth2,idate2,itime2,itimeh,itimem,itimes,idum=0  &
          ,n,nplot,i,j,k,nv,nreper,ntokfr,lc
real :: adum=0.
integer, save :: ncall(20),n3d=0,n2d=0,iun
real, save :: timesav=-1.

data cmon /'jan','feb','mar','apr','may','jun'  &
          ,'jul','aug','sep','oct','nov','dec'/
data namdum /'dum     '/
data nreper /0/
data toksep /'/'/
data ncall/20*0/

if(ncall(nngd)==0) then

   ! First time into this routine for this grid
 
   ! Sort through variables
   do nplot=1,maxfore
     if(cframe_a(nplot)(1:1).ne.'/') goto 10
     call tokenize1(cframe_a(nplot),frtokens,ntokfr,toksep)
     cname(nplot)=frtokens(1)
   enddo
   10 continue
   nplot=nplot-1

   do n=1,nplot
      lc=len_trim(cname(n))
      if(cname(n)(1:lc)=='ue_avg') then
         n3d=n3d+1
         namv3d(n3d) ='U       '
         unitv3d(n3d)='M/S     '
      elseif(cname(n)(1:lc)=='ve_avg') then
         n3d=n3d+1
         namv3d(n3d) ='V       '
         unitv3d(n3d)='M/S     '
      elseif(cname(n)(1:lc)=='tempk') then
         n3d=n3d+1
         namv3d(n3d) ='TA      '
         unitv3d(n3d)='Kelvin  '
      elseif(cname(n)(1:lc)=='theta') then
         n3d=n3d+1
         namv3d(n3d) ='T       '
         unitv3d(n3d)='Kelvin  '
      elseif(cname(n)(1:lc)=='press') then
         n3d=n3d+1
         namv3d(n3d) ='PRES    '
         unitv3d(n3d)='mb  '
      elseif(cname(n)(1:lc)=='w_avg') then
         n3d=n3d+1
         namv3d(n3d) ='W       '
         unitv3d(n3d)='M/S     '
      elseif(cname(n)(1:lc)=='relhum') then
         print*,'MEDOC format needs moisture in gm/gm'
         print*,'replace relhum with vapor in REVU_IN namelist'
         stop 'in medocout.f: improper field specified'
      elseif(cname(n)(1:lc)=='vapor') then
         n3d=n3d+1
         namv3d(n3d) ='H       '
         unitv3d(n3d)='gm/gm   '
      elseif(cname(n)(1:lc)=='topo') then
         n2d=n2d+1
         namv2d(n2d) ='TOPO    '
         unitv2d(n2d)='meters  '
      elseif(cname(n)(1:lc)=='pbl_ht') then
         n2d=n2d+1
         namv2d(n2d) ='ZI      '
         unitv2d(n2d)='meters  '
      elseif(cname(n)(1:lc)=='sens_flux') then
         n2d=n2d+1
         namv2d(n2d) ='SFC_HTFX'
         unitv2d(n2d)='Watt/m2 '
      else
         print*,'variable not allowed for MEDOC output, skipping: '  &
               ,cname(n)(1:lc)
      endif
   enddo
 
   ! Make and open the file
   print*
   print*,'===='
   write(flnm,'(a,a5,i4.4,2(a1,i2.2),a1,i6.6,a2,i1,a4)' )   &
      revpref(1:len_trim(revpref)),'RAMS-'  &
     ,iyear1,'-',imonth1,'-',idate1,'-',itime1*100,'-g',nngd,'.fmt'
   print*,'filename:',flnm(1:len_trim(flnm))
 
   iun=79+nngd
   open(iun,file=flnm,status='unknown')
   rewind iun
 
   timesav=-1.
   ncall(nngd)=1
endif

call date_add_to (iyear1,imonth1,idate1,itime1*100,fcstsec,'s'  &
                 ,iyear2,imonth2,idate2,itime2)
itimeh=int(itime2/10000)
itimem=int(mod(itime2,10000)/100)   
itimes=mod(itime2,100) 
if(fcstsec > timesav) then

   ! Write the time header if starting a new analysis time

   write(iun,9001) 'FFFFFFFF'
   write(iun,9001) 'codename'
   write(iun,9002) idate2,imonth2,iyear2,itimeh,itimem,itimes
   write(iun,9002) idum,idum,idum,idum,idum,idum
   write(iun,9002) n1,n2,n3-1,nreper,n3d,n2d
   write(iun,9002) idum,idum,idum,idum,idum,idum
   write(iun,9002) idum,idum,idum
   write(iun,9003) (zlevs(k),k=2,n3),dx,dy,x1/1000.,y1/1000.  &
                  ,alat1,alon1,adum,adum,adum,adum,adum
   write(iun,9001) (namdum,n=1,nreper)  &
                 ,(namv3d(n),n=1,n3d),(unitv3d(n),n=1,n3d)  &
                 ,(namv2d(n),n=1,n2d),(unitv2d(n),n=1,n2d)
   timesav=fcstsec
endif

if (itrans==1.or.itrans==2) then
   
   ! If this is a 3-dimensional variable
   if(ivtype==3) then
   
      if(cvar(1)=='vapor') call ae1t0 (n1*n2*n3,a,a,1.e-3)
     
      ! Write out the 3D array
      write(iun,9003) (((a(i,j,k),i=1,n1),j=1,n2),k=2,n3)

   ! If this is a 2-dimensional variable
   elseif(ivtype==2) then

      ! Check 2D variables to see if surface heat flux is on the tape
      if(cvar(1)=='sens_flux'.and.ihtflx==0) call ae0 (n1*n2,a,0.)
     
      ! Write out the 2D array
      write(iun,9003) ((a(i,j,1),i=1,n1),j=1,n2)

   endif

else
   stop 'MEDOC output must be in terrain following coordinates'
endif

9001 format(6(a8,1x))
9002 format(6(i12,1x))
9003 format(6(f12.4,1x))

return
end
