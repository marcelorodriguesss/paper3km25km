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

subroutine gdf_write_upa2 (iun,nt,iy,im,id,it)
 
use grab_coms

implicit none

integer :: iun,nt,iy,im,id,it

integer :: nloc,nfld,k,iflg=0

do nloc=1,numlocs

   write(iun,10) iy,im,id,it  &
                ,locations(nloc)%id  &
                ,locations(nloc)%nlevs  &
                ,locations(nloc)%nlevs  &
                ,locations(nloc)%slat  &
                ,locations(nloc)%slon  &
                ,locations(nloc)%selev
   
   write(iun,11) (supp_vals(nt,nloc,1,1)+supp_vals(nt,nloc,2,1))/2.,iflag(1)  &
                ,locations(nloc)%selev,iflg  &
                ,(supp_vals(nt,nloc,1,2)+supp_vals(nt,nloc,2,2))/2.,iflag(2)  &
                ,(supp_vals(nt,nloc,1,3)+supp_vals(nt,nloc,2,3))/2.,iflag(3)
   do k=2,locations(nloc)%nlevs
      write(iun,11) supp_vals(nt,nloc,k,1),iflag(1)  &
                   ,locations(nloc)%selev+locations(nloc)%slevs(k),iflg  &
                   ,supp_vals(nt,nloc,k,2),iflag(2)  &
                   ,supp_vals(nt,nloc,k,3),iflag(3)
   enddo
   
   write(iun,12) locations(nloc)%selev,iflg  &
                ,(supp_vals(nt,nloc,1,4)+supp_vals(nt,nloc,2,4))/2.,iflag(4)  &
                ,(supp_vals(nt,nloc,1,5)+supp_vals(nt,nloc,2,5))/2.,iflag(5)
   do k=2,locations(nloc)%nlevs
      write(iun,12) locations(nloc)%selev+locations(nloc)%slevs(k),iflg  &
                   ,supp_vals(nt,nloc,k,4),iflag(4)  &
                   ,supp_vals(nt,nloc,k,5),iflag(5)
   enddo
   
enddo

10 format(i4.4,2x,2(i2.2,2x),i4.4,2x,a12,2i5,2f10.4,f7.1)
11 format(f10.1,2x,i4.3,f10.1,2x,i4.3,f7.1,2x,i4.3,f7.3,2x,i4.3)
12 format(f10.1,2x,i4.3,f9.2,2x,i4.3,f7.0,2x,i4.3)

return
end
