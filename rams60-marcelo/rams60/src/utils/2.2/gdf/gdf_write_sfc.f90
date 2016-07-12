!
! Copyright (C) 1991-2004  ; All Rights Reserved ; ATMET, LLC
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
! 2.2.0
!###########################################################################

subroutine gdf_write_sfc2 (iun,nt,iy,im,id,it,nv,nvr,cformat)

! write Raplh version 2 surface data
 
use grab_coms

implicit none

integer :: iun,nt,iy,im,id,it,nv,nvr(*)
character(len=*) :: cformat

integer :: nloc

do nloc=1,numlocs
   write(iun,cformat) iy,im,id,it  &
                     ,locations(nloc)%id  &
                     ,locations(nloc)%slat  &
                     ,locations(nloc)%slon  &
                     ,locations(nloc)%selev  &
                     ,((ssfc_vals(nt,1,nloc,nv),iflag(nv)),nv=1,numvars)
enddo

return
end


!***************************************************************************

subroutine gdf_write_sfc3 (iun,nt,iy,im,id,it,nv,nvr,cformat)

! write Raplh version 3 surface data

use grab_coms

implicit none

integer :: iun,nt,iy,im,id,it,nv,nvr(*)
character(len=*) :: cformat

integer :: nloc,ihf
real :: sz
do nloc=1,numlocs

   if(locations(nloc)%ihf==1) then
      sz=locations(nloc)%szg
   else
      sz=locations(nloc)%szs
   endif

   write(iun,cformat) iy,im,id,it  &
                     ,locations(nloc)%id  &
                     ,locations(nloc)%slat  &
                     ,locations(nloc)%slon  &
                     ,locations(nloc)%selev  &
                     ,sz  &
                     ,locations(nloc)%ihf  &
                     ,ssfc_vals(nt,nloc,1),iflag(1)  &
                     ,ssfc_vals(nt,nloc,2),iflag(2)  &
                     ,ssfc_vals(nt,nloc,3),iflag(3)  &
                     ,ssfc_vals(nt,nloc,4),iflag(4)
                     
                     
                    ! ,((ssfc_vals(nt,1,nloc,nv),iflag(nv)),nv=1,numvars)
enddo

return
end

