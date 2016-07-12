!
! Copyright (C) 1991-2004  ; All Rights Reserved ; Colorado State University
! Colorado State University Research Foundation ; ATMET, LLC
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
! 5.0.0
!
!###########################################################################


subroutine varf_read(ivflag)

use mem_grid
use mem_varinit

implicit none

integer :: ivflag

character(len=14)  :: itotdate_current
integer :: iyears,imonths,idates,ihours,nf,ifm  &
          ,ivar_wait,nwaits,nw,ivwait1,irsleep,irslp,ifileok,icm

! See if we want to possibly wait for files to be available. 
!    This will control some logic...

ivar_wait=0
if (vwait1 > 0.0 .and. vwaittot > 0.0) ivar_wait=1
if(ivar_wait == 0) then
   print*
   print*,' Will not wait for varfiles'
   print*
   nwaits=1
elseif (ivar_wait == 1) then
   print*
   print*,' Will wait for varfiles if not present'
   print*,'  Check interval:',vwait1,' Fail time:',vwaittot
   print*
   nwaits=int(vwaittot/vwait1)+1
endif

if (ivflag == 0) then   ! Initialization of initial fields

   call date_make_big(iyear1,imonth1,idate1,itime1*100  &
                ,itotdate_current)

   wait: do nw = 1, nwaits
   
      ! Inventory all varf files. 
      call varf_file_inv (varfpfx,iyear1,imonth1,idate1,itime1)
  

      ! The initial time must have an exact time match. 
      nvarffl=0
      do nf=1,nvarffiles
         if(itotdate_current == itotdate_varf(nf)) then
            nvarffl=nf
            exit wait
         endif
      enddo
      if (ivar_wait == 0 ) then
         print*
         print*,'No initial varfiles found with prefix: ',trim(varfpfx)
         print*
         stop 'no initial varfile'
      elseif(ivar_wait == 1 .and. nw < nwaits) then
         print*,'No initial varfiles found: ',trim(varfpfx)
         print*,'    Waiting:', vwait1,' seconds.   Total wait:',nw*vwait1
         ivwait1=nint(vwait1)
         irslp = irsleep(ivwait1)
      elseif(ivar_wait == 1 .and. nw == nwaits) then
         print*,'No initial varfiles found: ',trim(varfpfx)
         print*,'    Waited too long:', vwaittot,' seconds.'
         stop ' No initial varfile after wait.'
      endif
      
   enddo wait
   
   ! Now do actual initialization for the coarse grid 
   !     and find 1D reference state
   call newgrid(1)
   call varf_update(0,ifileok,1)
   
   !  On all fine grids, initialize the 1-D reference state arrays, 
   !  the 3-D reference state arrays,
   !  and the prognostic atmospheric fields by interpolation.

   call fmrefs1d(2,ngrids)

   do ifm = 2,ngrids
      icm = nxtnest(ifm)
      
      ! Get 3D reference state for this grid
      call fmrefs3d(ifm)
      
      ! Interpolate prognostic fields. These will be overwritten if the varfile
      !    exists
      call prgintrp(nnzp(icm),nnxp(icm),nnyp(icm)  &
          ,nnzp(icm),nnxp(icm),nnyp(icm),0,0,ifm,1,0)

      call newgrid(ifm)
      
      ! See if this grid's varfile is created.
      call varf_update(0,ifileok,1)
          
      if (ifileok  ==  1) then
         ! Everything's cool...
         print*,'Initial varfile read of grid-',ifm
      else
         ! Using interpolated nudging arrays from parent grid.
         print*,'Initial interpolation of grid-',ifm
      endif

      call fmdn0(ifm)

   enddo
   
   return
         
elseif (ivflag == 1) then   ! Fill nudging arrays and compute weights

   ! If a history start, we will assume a past time file is there.
   !  Take closest past time.

   call date_add_to(iyear1,imonth1,idate1,itime1*100  &
                ,time,'s',iyears,imonths,idates,ihours)
   call date_make_big(iyears,imonths,idates,ihours  &
                ,itotdate_current)
   
   call varf_file_inv (varfpfx,iyear1,imonth1,idate1,itime1)
   
   nvarffl=0
   do nf=nvarffiles,1,-1
      if(itotdate_varf(nf) <= itotdate_current ) then
         nvarffl=nf
         exit
      endif
   enddo
   if (nvarffl == 0 ) then
      print*
      print*,'No past varfiles found on nudge fill:',trim(varfpfx)
      print*
      stop 'no past varfile on fill'
   endif

   ! Compute weighting factors for grid 1
   call varweight(nnzp(1),nnxp(1),nnyp(1),varinit_g(1)%varwts(1,1,1)  &
                 ,grid_g(1)%topt(1,1),grid_g(1)%rtgt(1,1))

   
   ! Read files

   do ifm = 1,ngrids
      icm = nxtnest(ifm)

      call newgrid(ifm)
      
      ! Interpolate weights to all other grids
      if (ifm > 1) call vfintrpf(ifm,1)

      ! See if this grid's varfile is created.
      call varf_update(0,ifileok,0)

      if (ifileok  ==  1) then
         ! Everything's cool...
         print*,'Varfile read of grid-',ifm
      else
         ! Using interpolated nudging arrays from parent grid.
         call vfintrpf(ifm,2)
         print*,'Interpolation of grid-',ifm
      endif

   enddo  

   vtime2=varf_times(nvarffl)
   print*,'New varfile times:',nvarffl,vtime2

elseif (ivflag == 2) then   ! Runtime file increment
   
   ! Find current date/time
   call date_add_to(iyear1,imonth1,idate1,itime1*100  &
                   ,time,'s',iyears,imonths,idates,ihours)
   call date_make_big(iyears,imonths,idates,ihours  &
                   ,itotdate_current)

endif

! Find the next varfile in the list, waiting for it if necessary


wait2: do nw = 1, nwaits
   
   ! Redo the inventory in case new files showed up
   call varf_file_inv (varfpfx,iyear1,imonth1,idate1,itime1)

   nvarffl=0
   do nf=1,nvarffiles
      if(itotdate_varf(nf) > itotdate_current) then
         nvarffl=nf
         exit wait2
      endif
   enddo
   if (ivar_wait == 0 ) then
      print*
      print*,'No future varfiles found with prefix: ',trim(varfpfx)
      print*
      stop 'no future varfile'
   elseif(ivar_wait == 1 .and. nw < nwaits) then
      print*,'No future varfiles found: ',trim(varfpfx)
      print*,'    Waiting:', vwait1,' seconds.   Total wait:',nw*vwait1
      ivwait1=nint(vwait1)
      irslp = irsleep(ivwait1)
   elseif(ivar_wait == 1 .and. nw == nwaits) then
      print*,'No future varfiles found: ',trim(varfpfx)
      print*,'    Waited too long:', vwaittot,' seconds.'
      stop ' No future varfile after wait.'
   endif

enddo wait2

! Read future files

do ifm = 1,ngrids
   icm = nxtnest(ifm)
   
   call newgrid(ifm)
   
   ! See if this grid's varfile is created.
   call varf_update(1,ifileok,0)
   
   if (ifileok  ==  1) then
      ! Everything's cool...
      print*,'Future varfile read of grid-',ifm
   else
      ! Using interpolated nudging arrays from parent grid.
      call vfintrpf(ifm,2)
      print*,'Future interpolation of grid-',ifm
   endif

enddo


vtime1=vtime2
vtime2=varf_times(nvarffl)

print*,'New varfile times:',nvarffl,vtime1,vtime2

return
end



subroutine varf_file_inv (varpref,iyear1,imonth1,idate1,itime1)

use mem_varinit

implicit none

character(len=*) :: varpref
integer :: iyear1,imonth1,idate1,itime1

integer :: nc,nf,lnf,nvftot
integer :: inyear,inmonth,indate,inhour


character(len=128), dimension(maxnudfiles) :: fnames
character(len=128) :: vpref
character(len=14)  :: itotdate
real(kind=8) :: secs_init,secs_varf

! Get abs seconds of run start

call date_abs_secs2(iyear1,imonth1,idate1,itime1*100,secs_init)

! Go through history files and make inventory

nc=len_trim(varpref)
nvftot=-1
vpref=varpref

call RAMS_filelist(fnames,trim(vpref)  &
         //'*.tag',nvftot)

if(nvftot > maxnudfiles) then
   print*,'too many varf files'
   stop 'lots_of_varf'
endif

nvarffiles=0
do nf=1,nvftot
   lnf=len_trim(fnames(nf))
   read(fnames(nf)(lnf-20:lnf-4),20) inyear,inmonth,indate,inhour
   20 format(i4,1x,i2,1x,i2,1x,i6)

   call date_make_big(inyear,inmonth,indate,inhour,itotdate)

   nvarffiles=nvarffiles+1
   fnames_varf(nvarffiles)=fnames(nf)
   itotdate_varf(nvarffiles)=itotdate
   
   call date_abs_secs2(inyear,inmonth,indate,inhour,secs_varf)
   varf_times(nvarffiles)=secs_varf - secs_init

enddo

call RAMS_dintsort(nvarffiles,itotdate_varf,fnames_varf)

!  start printing section
!--------------------------------------------------------------

print*,' '
print*,' '
print*,' '
print*,'-------------------------------------------------------------'
print*,'-----------  Varfile Input Inventory -------------'
print*,'-------------------------------------------------------------'
do nf=1,nvarffiles
   print 8,  nf, itotdate_varf(nf),varf_times(nf) ,trim(fnames_varf(nf))
enddo
8 format(i4,1x,a16,1x,f10.0,2x,a)
print*,'------------------------------------------------------'

!--------------------------------------------------------------

return
end
