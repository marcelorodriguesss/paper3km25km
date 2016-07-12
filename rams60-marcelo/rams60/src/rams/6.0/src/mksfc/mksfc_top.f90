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

subroutine top_read(ifm)

use mem_grid
use io_params

use hdf5_utils

implicit none

integer :: ifm,i,j

character(len=128) :: flnm
character(len=2) :: cgrid
logical :: there
integer :: ndims,idims(4)

! read the "top" file

write(cgrid,'(a1,i1)') 'g',ifm
flnm=trim(topfiles)//'-S-'//cgrid//'.h5'

inquire(file=flnm,exist=there)

if(.not.there) then
   print*,'------------------------------------------------'
   print*,'TOP_read: file for grid ',ifm,' not there.'
   print*,'TOP_read: file:',trim(flnm)
   print*,'------------------------------------------------'
   stop 'top_read: no file'
endif

call shdf5_open(flnm,'R')
ndims=2 ; idims(1)=nnxp(ifm) ; idims(2)=nnyp(ifm)
call shdf5_irec('topo',rvara=grid_g(ifm)%topta)
call shdf5_irec('topo_z0',rvara=grid_g(ifm)%topzo)
call shdf5_close()

return
end

!******************************************************************

subroutine top_check(ifm,ierr)

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

integer :: lc,isfc_marker,isfc_ver,nsfx,nsfy  &
   ,nsitoptflg,nsitopsflg,nsiz0flg
real ::  sfdx,sfdy,sfplat,sfplon,sflat,sflon,stoptenh,stoptwvl  &
   ,sz0max,sz0fact,glatr,glonr

character(len=128) :: flnm
character(len=2) :: cgrid
logical there
integer :: ndims,idims(4)

lc=len_trim(topfiles)
write(cgrid,'(a1,i1)') 'g',ifm
flnm=trim(topfiles)//'-S-'//cgrid//'.h5'

print*,'------------------------------------------------'
print*,'---> Check grid:',ifm,' top file... '
print*,'--->   Filename:',trim(flnm)

inquire(file=flnm,exist=there)

if(.not.there) then
   ierr = 1
   print*,'TOPfile for grid ',ifm,' not there.'
   print*,'------------------------------------------------'
   return
endif

call xy_ll(glatr,glonr,platn(ifm),plonn(ifm),xtn(1,ifm),ytn(1,ifm))

call shdf5_open(flnm,'R')
ndims=1 ; idims(1)=1
call shdf5_irec('nx',ivars=nsfx)
call shdf5_irec('ny',ivars=nsfy)
call shdf5_irec('dx',rvars=sfdx)
call shdf5_irec('dy',rvars=sfdy)
call shdf5_irec('polelat',rvars=sfplat)
call shdf5_irec('polelon',rvars=sfplon)
call shdf5_irec('sw_lat',rvars=sflat)
call shdf5_irec('sw_lon',rvars=sflon)
call shdf5_irec('itoptflg',ivars=nsitoptflg)
call shdf5_irec('itopsflg',ivars=nsitopsflg)
call shdf5_irec('toptenh',rvars=stoptenh)
call shdf5_irec('toptwvl',rvars=stoptwvl)
call shdf5_irec('iz0flg',ivars=nsiz0flg)
call shdf5_irec('z0max',rvars=sz0max)
call shdf5_irec('z0fact',rvars=sz0fact)
call shdf5_close()


if (nsfx                       .ne. nnxp(ifm)     .or.  &
    nsfy                       .ne. nnyp(ifm)     .or.  &
    abs(sfdx-deltaxn(ifm))     .gt. .001          .or.  &
    abs(sfdy-deltayn(ifm))     .gt. .001          .or.  &
    abs(sfplat-platn(ifm))     .gt. .001          .or.  &
    abs(sfplon-plonn(ifm))     .gt. .001          .or.  &
    abs(sflat-glatr)           .gt. .001          .or.  &
    abs(sflon-glonr)           .gt. .001          .or.  &
    nsitoptflg                 .ne. itoptflg(ifm) .or.  &
    nsitopsflg                 .ne. itopsflg(ifm) .or.  &
    abs(stoptenh-toptenh(ifm)) .gt. .001          .or.  &
    abs(stoptwvl-toptwvl(ifm)) .gt. .001          .or.  &
    nsiz0flg                   .ne. iz0flg(ifm)   .or.  &
    abs(sz0max-z0max(ifm))     .gt. .001          .or.  &
    abs(sz0fact-z0fact)        .gt. .00001) then

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
   print*,'itoptflg:',itoptflg(ifm),nsitoptflg
   print*,'itopsflg:',itopsflg(ifm),nsitopsflg
   print*,'toptenh:',toptenh(ifm),stoptenh
   print*,'toptwvl:',toptwvl(ifm),stoptwvl
   print*,'iz0flg:',iz0flg(ifm),nsiz0flg
   print*,'z0max:',z0max(ifm),sz0max
   print*,'z0fact:',z0fact,sz0fact
   print*,'-------------------'

else

   ierr = 0
   print*,'---> Grid:',ifm,' topography file data okay. '
   print*,'------------------------------------------------'

endif

return
end

! ****************************************************************************

subroutine toptinit(n2,n3,ifm,topt,topzo)
implicit none
integer :: n2,n3,ifm,i,j
real, dimension(n2,n3) :: topt,topzo

! Fill the TOPT array with a default value of 0.  This default is used only
! when a standard RAMS topography dataset is not used and when no overrides
! to topography heights are defined in subroutine toptinit_user in the
! file ruser.f.

do j = 1,n3
   do i = 1,n2
      topt(i,j) = 0.
      topzo(i,j) = .0001
   enddo
enddo
return
end


!*****************************************************************************

subroutine top_write(ifm)

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

flnm=trim(topfiles)//'-S-'//cgrid//'.h5'

call xy_ll(glatr,glonr,platn(ifm),plonn(ifm),xtn(1,ifm),ytn(1,ifm))

call shdf5_open(flnm,'W',iclobber)
ndims=1 ; idims(1)=1
call shdf5_orec(ndims,idims,'nx',ivars=nnxp(ifm))
call shdf5_orec(ndims,idims,'ny',ivars=nnyp(ifm))
call shdf5_orec(ndims,idims,'dx',rvars=deltaxn(ifm))
call shdf5_orec(ndims,idims,'dy',rvars=deltayn(ifm))
call shdf5_orec(ndims,idims,'polelat',rvars=platn(ifm))
call shdf5_orec(ndims,idims,'polelon',rvars=plonn(ifm))
call shdf5_orec(ndims,idims,'sw_lat',rvars=glatr)
call shdf5_orec(ndims,idims,'sw_lon',rvars=glonr)
call shdf5_orec(ndims,idims,'itoptflg',ivars=itoptflg(ifm))
call shdf5_orec(ndims,idims,'itopsflg',ivars=itopsflg(ifm))
call shdf5_orec(ndims,idims,'toptenh',rvars=toptenh(ifm))
call shdf5_orec(ndims,idims,'toptwvl',rvars=toptwvl(ifm))
call shdf5_orec(ndims,idims,'iz0flg',ivars=iz0flg(ifm))
call shdf5_orec(ndims,idims,'z0max',rvars=z0max(ifm))
call shdf5_orec(ndims,idims,'z0fact',rvars=z0fact)
ndims=2 ; idims(1)=nnxp(ifm) ; idims(2)=nnyp(ifm)
call shdf5_orec(ndims,idims,'topo',rvara=sfcfile_p(ifm)%topt)
call shdf5_orec(ndims,idims,'topo_z0',rvara=sfcfile_p(ifm)%topzo)
call shdf5_close()

return
end
