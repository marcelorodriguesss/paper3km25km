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


subroutine ndvi_read_dataheader(ifm)

use mem_mksfc
use io_params

implicit none
integer :: ifm

integer :: itime,nc
character(len=80) :: flnm,line,line2
character(len=1) :: dummy
logical :: there
integer, external :: lastslash

! Read header file for all ndvidata files (all times and locations).  The header
! file contains:
! first line:       geographic block size (degrees), file size, geographic
!                   starting point for the dataset (south pole), and offsets
! second line:      number of data times (NDTIM)
! next NDTIM lines: file prefix, year, month, day, and hour for one data time
! last 3 lines:     comments describing the first, second, and following lines
!                   above

! Construct header file name

flnm=trim(ndvifn(ifm))//'HEADER'

print*, 'ndvifn(ifm):',trim(flnm)

print*,'------------------------------------------------'
print*,'---> Check grid:',ifm,' ndvi data. '
print*,'--->   Filename:',trim(flnm)

inquire(file=flnm,exist=there)
if (.not.there) then
   print*,'ndvifn data header file for grid ',ifm,' not there.'
   stop 'ndvi_read_fileheader-1'
endif

! Read this header file

call rams_f_open(25,flnm,'FORMATTED','OLD','READ',0)
rewind 25

! read number of data times in dataset

read(25,*) dummy
read(25,*) nvndvif(ifm)

if (nvndvif(ifm) <= 0) then
   print*, 'No ndvi input files found with specified prefix or incorrect header'
   close(25)
   stop 'ndvi_read_fileheader-2'
endif

! read prefix list and times 

do itime = 1,nvndvif(ifm)
   read(25,'(A80)') line
   call char_strip_var(line,flnm,line2)
   read (line2,*) iyearvn(itime,ifm),imonthvn(itime,ifm)  &
      ,idatevn(itime,ifm),ihourvn(itime,ifm)

   nc=lastslash(ndvifn(ifm))
   vndvifil(itime,ifm)=ndvifn(ifm)(1:nc)//trim(flnm)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! For testing, print out nndvif, ndvitime, and vndvifil.

!   print*, 'ifm,itime,nvndvif(ifm)',ifm,itime,nvndvif(ifm)
!      print*, 'vndvifil(itime,ifm)'  &
!              ,vndvifil(itime,ifm),iyearvs(itime,ifm),imonthvs(itime,ifm)  &
!      ,idatevs(itime,ifm),ihourvs(itime,ifm)
!c      stop
! End of testing code
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

enddo

close(25)

return
end

!******************************************************************************

subroutine ndvinest(ifm,ivtime)

use mem_mksfc
use mem_grid
use mem_leaf
use io_params

implicit none

integer :: ifm,icm,ivtime,mynum,i,j,k,ic,jc,ipat

icm = nxtnest(ifm)

! Initialize SEATP and SEATF in subroutine ndviinit

call ndviinit(nnxp(ifm),nnyp(ifm),npatch,ifm  &
   ,sfcfile_p(ifm)%veg_ndvif(1,1,1))

if (icm >= 1 .and. ndviflg(ifm) == 0) then

   ! Assign NDVIF from coarser grid

   do ipat = 2,npatch
      do k = 1,nzg
         do j = 1,nnyp(ifm)
            do i = 1,nnxp(ifm)
               ic = ipm(i,ifm)
               jc = jpm(j,ifm)

               sfcfile_p(ifm)%veg_ndvif(i,j,ipat)  =  &
                  sfcfile_p(icm)%veg_ndvif(ic,jc,ipat)

            enddo
         enddo
      enddo
   enddo
   
   nvndvif(ifm) = nvndvif(icm)
   iyearvn (1:nvndvif(ifm),ifm) = iyearvn (1:nvndvif(ifm),icm)
   imonthvn(1:nvndvif(ifm),ifm) = imonthvn(1:nvndvif(ifm),icm)
   idatevn (1:nvndvif(ifm),ifm) = idatevn (1:nvndvif(ifm),icm)
   ihourvn (1:nvndvif(ifm),ifm) = ihourvn (1:nvndvif(ifm),icm)

elseif (ndviflg(ifm) == 1) then

   ! Assign NDVIF from standard dataset:

   call landuse_opqr(nnxp(ifm),nnyp(ifm),nzg,npatch,nvegpat  &
      ,ivegtflg(ifm),ivegtfn(ifm),isoilflg(ifm),isoilfn(ifm) &
      ,ndviflg(ifm),ndvifn(ifm),vndvifil(ivtime,ifm)  &
      ,'ndvi',platn(ifm),plonn(ifm)        &
      ,sfcfile_p(ifm)%soil_text(1,1,1,1)  &
      ,sfcfile_p(ifm)%patch_area(1,1,1)   &
      ,sfcfile_p(ifm)%leaf_class(1,1,1)   &
      ,sfcfile_p(ifm)%veg_ndvif(1,1,1))
else

   iyearvn (1,ifm) = iyear1 ; imonthvn(1,ifm) = imonth1
   idatevn (1,ifm) = idate1 ; ihourvn (1,ifm) = ihour1        

endif

! If desired, override current values of NDVIP and NDVIF in user-specified
! changes to subroutine ndviinit_user in the file ruser.f.

call ndviinit_user(nnxp(ifm),nnyp(ifm),npatch,ifm  &
   ,sfcfile_p(ifm)%veg_ndvif(1,1,1))

return
end

! ****************************************************************************

subroutine ndviinit(n2,n3,npat,ifm,veg_ndvif)
implicit none
integer :: n2,n3,npat,ifm,i,j,ipat
real, dimension(n2,n3,npat) :: veg_ndvif

! Fill the veg_ndvif array with a default value of .5.  This default is 
! used only when a standard RAMS ndvi dataset is not used and when no 
! overrides to ndvi values are defined in subroutine ndviinit_user in the
! file ruser.f.

do j = 1,n3
   do i = 1,n2
      veg_ndvif(i,j,1) = 0.
      veg_ndvif(i,j,2) = .5

      do ipat = 3,npat
         veg_ndvif(i,j,ipat) = veg_ndvif(i,j,2)
      enddo

   enddo
enddo
return
end

!****************************************************************************

subroutine ndvi_write(ifm,ivt)

use mem_mksfc
use mem_grid
use io_params

use hdf5_utils

implicit none
integer :: ifm,ivt,ip

real :: glatr,glonr
character(len=80) :: flnm
character(len=2) :: cgrid
integer :: ndims,idims(4)

! Write ndvi data to ndvi file for one grid and one time

write(cgrid,'(a1,i1)') 'g',ifm
call makefnam(flnm,ndvifpfx,0.,iyearvn(ivt,ifm),imonthvn(ivt,ifm) &
      ,idatevn(ivt,ifm),ihourvn (ivt,ifm)*10000,'N',cgrid,'h5')

call xy_ll(glatr,glonr,platn(ifm),plonn(ifm),xtn(1,ifm),ytn(1,ifm))


call shdf5_open(flnm,'W',iclobber)
ndims=1 ; idims(1)=1
call shdf5_orec(ndims,idims,'year',ivars=iyearvn(ivt,ifm))
call shdf5_orec(ndims,idims,'month',ivars=imonthvn(ivt,ifm))
call shdf5_orec(ndims,idims,'day',ivars=idatevn(ivt,ifm))
call shdf5_orec(ndims,idims,'hour',ivars=ihourvn(ivt,ifm))
call shdf5_orec(ndims,idims,'nx',ivars=nnxp(ifm))
call shdf5_orec(ndims,idims,'ny',ivars=nnyp(ifm))
call shdf5_orec(ndims,idims,'npatch',ivars=npatch)
call shdf5_orec(ndims,idims,'dx',rvars=deltaxn(ifm))
call shdf5_orec(ndims,idims,'dy',rvars=deltayn(ifm))
call shdf5_orec(ndims,idims,'polelat',rvars=platn(ifm))
call shdf5_orec(ndims,idims,'polelon',rvars=plonn(ifm))
call shdf5_orec(ndims,idims,'sw_lat',rvars=glatr)
call shdf5_orec(ndims,idims,'sw_lon',rvars=glonr)
ndims=3 ; idims(1)=nnxp(ifm) ; idims(2)=nnyp(ifm) ; idims(3)=npatch
call shdf5_orec(ndims,idims,'veg_ndvi',rvara=sfcfile_p(ifm)%veg_ndvif)
call shdf5_close()

return
end
