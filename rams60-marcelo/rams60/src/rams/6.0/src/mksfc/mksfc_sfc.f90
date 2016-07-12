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


subroutine sfc_read(ifm)

use mem_grid
use mem_leaf
use io_params

use hdf5_utils

implicit none

integer :: ifm,ifileok,ipat,k,i,j
logical :: there

character(len=128) :: flnm
character(len=2) :: cgrid
integer :: ndims,idims(4)

! read the "sfc" file

write(cgrid,'(a1,i1)') 'g',ifm
flnm=trim(sfcfiles)//'-S-'//cgrid//'.h5'

inquire(file=flnm,exist=there)

if(.not.there) then
   print*,'------------------------------------------------'
   print*,'SFC_read: file for grid ',ifm,' not there.'
   print*,'SFC_read: file:',trim(flnm)
   print*,'------------------------------------------------'
   stop 'sfc_read: no file'
endif

call shdf5_open(flnm,'R')
ndims=3 ; idims(1)=nnxp(ifm) ; idims(2)=nnyp(ifm) ; idims(3)=npatch
call shdf5_irec('PATCH_AREA',rvara=leaf_g(ifm)%patch_area)
call shdf5_irec('LEAF_CLASS',rvara=leaf_g(ifm)%leaf_class)
ndims=4 ;idims(1)=nzg ; idims(2)=nnxp(ifm) ; idims(3)=nnyp(ifm) ; idims(4)=npatch
call shdf5_irec('SOIL_TEXT',rvara=leaf_g(ifm)%soil_text)
call shdf5_close()

return
end

!******************************************************************

subroutine sfc_check(ifm,ierr)

use mem_grid
use io_params

use hdf5_utils

! This subroutine checks for the existence of a surface file for
! grid number ifm, and if it exists, also checks for agreement of
! grid configuration between the file and the current model run.
! If the file does not exist or does not match grid configuration,
! the flag ifileok is returned with a value of 0.  If the file 
! exists and is ok, ifileok is returned with a value of 1.

implicit none

integer :: ifm,ierr

integer :: lc,isfc_marker,isfc_ver,nsfx,nsfy,nsfzg  &
   ,nsivegtflg,nsisoilflg,nsnofilflg,nspatch
real ::  sfdx,sfdy,sfplat,sfplon,sflat,sflon,glatr,glonr

character(len=128) :: flnm
character(len=2) :: cgrid
logical there
integer :: ndims,idims(4)

lc=len_trim(sfcfiles)
write(cgrid,'(a1,i1)') 'g',ifm
flnm=trim(sfcfiles)//'-S-'//cgrid//'.h5'

print*,'------------------------------------------------'
print*,'---> Check grid:',ifm,' sfc file... '
print*,'--->   Filename:',trim(flnm)

inquire(file=flnm,exist=there)

if(.not.there) then
   ierr = 1
   print*,'SFCfile for grid ',ifm,' not there.'
   print*,'------------------------------------------------'
   return
endif

call xy_ll(glatr,glonr,platn(ifm),plonn(ifm),xtn(1,ifm),ytn(1,ifm))

call shdf5_open(flnm,'R')
ndims=1 ; idims(1)=1
call shdf5_irec('nx',ivars=nsfx)
call shdf5_irec('ny',ivars=nsfy)
call shdf5_irec('nzg',ivars=nsfzg)
call shdf5_irec('npatch',ivars=nspatch)
call shdf5_irec('dx',rvars=sfdx)
call shdf5_irec('dy',rvars=sfdy)
call shdf5_irec('polelat',rvars=sfplat)
call shdf5_irec('polelon',rvars=sfplon)
call shdf5_irec('sw_lat',rvars=sflat)
call shdf5_irec('sw_lon',rvars=sflon)
call shdf5_irec('ivegtflg',ivars=nsivegtflg)
call shdf5_irec('isoilflg',ivars=nsisoilflg)
call shdf5_irec('nofilflg',ivars=nsnofilflg)
call shdf5_close()


if (nsfx                       .ne. nnxp(ifm)     .or.  &
    nsfy                       .ne. nnyp(ifm)     .or.  &
    nsfzg                      .ne. nzg           .or.  &
    nspatch                    .ne. npatch        .or.  &
    abs(sfdx-deltaxn(ifm))     .gt. .001          .or.  &
    abs(sfdy-deltayn(ifm))     .gt. .001          .or.  &
    abs(sfplat-platn(ifm))     .gt. .001          .or.  &
    abs(sfplon-plonn(ifm))     .gt. .001          .or.  &
    abs(sflat-glatr)           .gt. .001          .or.  &
    abs(sflon-glonr)           .gt. .001          .or.  &
    nsivegtflg                 .ne. ivegtflg(ifm) .or.  &
    nsisoilflg                 .ne. isoilflg(ifm) .or.  &
    nsnofilflg                 .ne. nofilflg(ifm) ) then

   ierr = 1

   print*,'SFCfile mismatch on grid:',ifm
   print*,'Values: model, file'
   print*,'-------------------'
   print*,'nnxp:',nnxp(ifm),nsfx
   print*,'nnyp:',nnyp(ifm),nsfy
   print*,'deltax:',deltaxn(ifm),sfdx
   print*,'deltay:',deltayn(ifm),sfdy
   print*,'platn:',platn(ifm),sfplat
   print*,'plonn:',plonn(ifm),sfplon
   print*,'SW lat:',glatr,sflat
   print*,'SW lon:',glonr,sflon
   print*,'ivegtflg:',ivegtflg(ifm),nsivegtflg
   print*,'isoilflg:',isoilflg(ifm),nsisoilflg
   print*,'nofilflg:',nofilflg(ifm),nsnofilflg
   print*,'-------------------'

else

   ierr = 0
   print*,'---> Grid:',ifm,' surface file data okay. '
   print*,'------------------------------------------------'

endif

return
end

!*****************************************************************************

subroutine sfc_write(ifm)

use mem_mksfc
use mem_grid
use io_params

use hdf5_utils

implicit none

integer :: ifm,ip,k,i,j
real :: glatr,glonr
character(len=128) :: flnm
character(len=2) :: cgrid
integer :: ndims,idims(4)

!     write surface characteristics, one file for each grid


write(cgrid,'(a1,i1)') 'g',ifm

flnm=trim(sfcfiles)//'-S-'//cgrid//'.h5'

call xy_ll(glatr,glonr,platn(ifm),plonn(ifm),xtn(1,ifm),ytn(1,ifm))


call shdf5_open(flnm,'W',iclobber)
ndims=1 ; idims(1)=1
call shdf5_orec(ndims,idims,'nx',ivars=nnxp(ifm))
call shdf5_orec(ndims,idims,'ny',ivars=nnyp(ifm))
call shdf5_orec(ndims,idims,'nzg',ivars=nzg)
call shdf5_orec(ndims,idims,'npatch',ivars=npatch)
call shdf5_orec(ndims,idims,'dx',rvars=deltaxn(ifm))
call shdf5_orec(ndims,idims,'dy',rvars=deltayn(ifm))
call shdf5_orec(ndims,idims,'polelat',rvars=platn(ifm))
call shdf5_orec(ndims,idims,'polelon',rvars=plonn(ifm))
call shdf5_orec(ndims,idims,'sw_lat',rvars=glatr)
call shdf5_orec(ndims,idims,'sw_lon',rvars=glonr)
call shdf5_orec(ndims,idims,'ivegtflg',ivars=ivegtflg(ifm))
call shdf5_orec(ndims,idims,'isoilflg',ivars=isoilflg(ifm))
call shdf5_orec(ndims,idims,'nofilflg',ivars=nofilflg(ifm))
ndims=3 ; idims(1)=nnxp(ifm) ; idims(2)=nnyp(ifm) ; idims(3)=npatch
call shdf5_orec(ndims,idims,'PATCH_AREA',rvara=sfcfile_p(ifm)%patch_area)
call shdf5_orec(ndims,idims,'LEAF_CLASS',rvara=sfcfile_p(ifm)%leaf_class)
ndims=4 ;idims(1)=nzg ; idims(2)=nnxp(ifm) ; idims(3)=nnyp(ifm) ;  idims(4)=npatch
call shdf5_orec(ndims,idims,'SOIL_TEXT',rvara=sfcfile_p(ifm)%soil_text)
call shdf5_close()

return
end
