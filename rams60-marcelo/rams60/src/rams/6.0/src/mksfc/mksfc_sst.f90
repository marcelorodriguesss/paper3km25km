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


subroutine sst_read_dataheader(ifm)

use mem_mksfc
use mem_grid
use io_params

implicit none
integer :: ifm

integer :: itime,nc,nsst,issty,isstm,isstd,issth
character(len=80) :: flnm,line,line2
character(len=1) :: dummy
logical :: there
integer, external :: lastslash
character(len=14) :: totdate_init,totdatem,totdatesst

! Read header file for all sstdata files (all times and locations).  The header
! file contains:
! first line:       geographic block size (degrees), file size, geographic
!                   starting point for the dataset (south pole), and offsets
! second line:      number of data times (NDTIM)
! next NDTIM lines: file prefix, year, month, day, and hour for one data time
! last 3 lines:     comments describing the first, second, and following lines
!                   above

! Construct header file name

flnm=trim(isstfn(ifm))//'HEADER'

print*, 'isstfn(ifm):',trim(flnm)

print*,'------------------------------------------------'
print*,'---> Check grid:',ifm,' sst data. '
print*,'--->   Filename:',trim(flnm)

inquire(file=flnm,exist=there)
if (.not.there) then
   print*,'SSTDATA header file for grid ',ifm,' not there.'
   stop 'sst_read_fileheader-1'
endif

! Read this header file

call rams_f_open(25,flnm,'FORMATTED','OLD','READ',0)
rewind 25

! read number of data times in dataset

read(25,*) dummy
read(25,*) nsst

if (nsst <= 0) then
   print*, 'No SST input files found with specified prefix or incorrect header'
   close(25)
   stop 'sst_read_fileheader-2'
endif

! read prefix list and times 

call date_make_big(iyear1,imonth1,idate1,itime1*100,totdate_init)
call date_add_to_big(totdate_init,timmax,'s',totdatem)
   
nvsstf(ifm)=0
do itime = 1,nsst
   read(25,'(A80)') line
   call char_strip_var(line,flnm,line2)
   read (line2,*) issty,isstm,isstd,issth
   
   call date_make_big(issty,isstm,isstd,issth*100,totdatesst)
   
   ! assumes data in header file is chronologically ordered oldest to newest
   ! if issty=0, assume climo data and do all times
   
   nvsstf(ifm)=nvsstf(ifm)+1
   if(issty /= 0 .and. totdatesst < totdate_init) nvsstf(ifm)=1
   
   vsstfil(nvsstf(ifm),ifm)=trim(isstfn(ifm))//trim(flnm)
   iyearvs(nvsstf(ifm),ifm)=issty
   imonthvs(nvsstf(ifm),ifm)=isstm
   idatevs(nvsstf(ifm),ifm)=isstd
   ihourvs(nvsstf(ifm),ifm)=issth
   
   
   if(issty /= 0 .and. totdatesst > totdatem) exit

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! For testing, print out nsstf, ssttime, and vsstfil.
!
!      print*, 'ifm,itime,nvsstf(ifm)',ifm,itime,nvsstf(ifm)
!      print*, 'vsstfil(itime,ifm)',vsstfil(nvsstf(ifm),ifm)  &
!              ,iyearvs(nvsstf(ifm),ifm),imonthvs(nvsstf(ifm),ifm)  &
!              ,idatevs(nvsstf(ifm),ifm),ihourvs(nvsstf(ifm),ifm)
!      stop
! End of testing code
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

enddo

close(25)

return
end

!******************************************************************************

subroutine sstnest(ifm,ivtime)

use mem_mksfc
use mem_grid
use io_params

implicit none

integer :: ifm,icm,ivtime,mynum

icm = nxtnest(ifm)

! Initialize SEATP and SEATF in subroutine sstinit

call sstinit(nnxp(ifm),nnyp(ifm),ifm, sfcfile_p(ifm)%seatf(1,1))

if (icm >= 1 .and. isstflg(ifm) == 0) then

! Interpolate SEATF from coarser grid

   call fillscr(1,nxpmax,nypmax,1,nnxp(icm),nnyp(icm),1,1  &
      ,scr1,sfcfile_p(icm)%seatf(1,1))
   call eintp(scr1,scr2,1,nxpmax,nypmax  &
      ,1,nnxp(ifm),nnyp(ifm),ifm,2,'t',0,0)
   call fillvar(1,nxpmax,nypmax,1,nnxp(ifm),nnyp(ifm),1,1  &
      ,scr2,sfcfile_p(ifm)%seatf(1,1))
   
   nvsstf(ifm) = nvsstf(icm)
   iyearvs (1:nvsstf(ifm),ifm) = iyearvs (1:nvsstf(ifm),icm)
   imonthvs(1:nvsstf(ifm),ifm) = imonthvs(1:nvsstf(ifm),icm)
   idatevs (1:nvsstf(ifm),ifm) = idatevs (1:nvsstf(ifm),icm)
   ihourvs (1:nvsstf(ifm),ifm) = ihourvs (1:nvsstf(ifm),icm)

elseif (isstflg(ifm) == 1) then

! Interpolate SEATF from standard dataset

   call geodat(nnxp(ifm),nnyp(ifm),sfcfile_p(ifm)%seatf(1,1)  &
      ,isstfn(ifm),vsstfil(ivtime,ifm),vt2da,vt2db,ifm,'SST')

else

   iyearvs (1,ifm) = iyear1 ; imonthvs(1,ifm) = imonth1
   idatevs (1,ifm) = idate1 ; ihourvs (1,ifm) = ihour1        

endif

! If desired, override current values of SEATF with user-defined
! changes to subroutine sstinit_user.

call sstinit_user(nnxp(ifm),nnyp(ifm),ifm ,sfcfile_p(ifm)%seatf(1,1))

return
end

! ****************************************************************************

subroutine sstinit(n2,n3,ifm,seatf)

use mem_leaf

implicit none
integer :: n2,n3,ifm,i,j
real, dimension(n2,n3) :: seatf

! Fill the SEATF array with a default value of seatmp.  This 
! default is used only when a standard RAMS sst dataset is not used and when 
! no overrides to sea temperature are defined in subroutine sstinit_user 
! in the file ruser.f90.

do j = 1,n3
   do i = 1,n2
      seatf(i,j) = seatmp
   enddo
enddo 
return
end


!****************************************************************************

subroutine sst_write(ifm,ivt)

use mem_mksfc
use mem_grid
use io_params

use hdf5_utils

implicit none
integer :: ifm,ivt,i,j

real :: glatr,glonr
character(len=80) :: flnm
character(len=2) :: cgrid
integer :: ndims,idims(4)

! Write sst data to sst file for one grid and one time

write(cgrid,'(a1,i1)') 'g',ifm
call makefnam(flnm,sstfpfx,0.,iyearvs(ivt,ifm),imonthvs(ivt,ifm) &
      ,idatevs(ivt,ifm),ihourvs (ivt,ifm)*10000,'W',cgrid,'h5')

call xy_ll(glatr,glonr,platn(ifm),plonn(ifm),xtn(1,ifm),ytn(1,ifm))

call shdf5_open(flnm,'W',iclobber)
ndims=1 ; idims(1)=1
call shdf5_orec(ndims,idims,'year',ivars=iyearvn(ivt,ifm))
call shdf5_orec(ndims,idims,'month',ivars=imonthvn(ivt,ifm))
call shdf5_orec(ndims,idims,'day',ivars=idatevn(ivt,ifm))
call shdf5_orec(ndims,idims,'hour',ivars=ihourvn(ivt,ifm))
call shdf5_orec(ndims,idims,'nx',ivars=nnxp(ifm))
call shdf5_orec(ndims,idims,'ny',ivars=nnyp(ifm))
call shdf5_orec(ndims,idims,'dx',rvars=deltaxn(ifm))
call shdf5_orec(ndims,idims,'dy',rvars=deltayn(ifm))
call shdf5_orec(ndims,idims,'polelat',rvars=platn(ifm))
call shdf5_orec(ndims,idims,'polelon',rvars=plonn(ifm))
call shdf5_orec(ndims,idims,'sw_lat',rvars=glatr)
call shdf5_orec(ndims,idims,'sw_lon',rvars=glonr)
ndims=2 ; idims(1)=nnxp(ifm) ; idims(2)=nnyp(ifm)
call shdf5_orec(ndims,idims,'SEATF',rvara=sfcfile_p(ifm)%seatf)
call shdf5_close()

return
end
