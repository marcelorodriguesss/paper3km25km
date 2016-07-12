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

subroutine botset_adap(m1,m2,m3,ia,iz,ja,jz,ibcon,lpx,aa,vnam)

use mem_grid

implicit none

integer :: m1,m2,m3,ia,iz,ja,jz,ibcon,i,j,k,ka
integer, dimension(m2,m3) :: lpx
real :: dzmr
real, dimension(m1,m2,m3) :: aa
character(len=*) :: vnam

if (vnam .eq. 'P') then
   dzmr = dzm(2) / dzm(1)
   do i = 1,m2
      do j = 1,m3
         ka = lpx(i,j)
         do k = ka-1,1,-1
            aa(k,i,j) = aa(k+1,i,j) + (aa(k+1,i,j) - aa(k+2,i,j)) * dzmr
         enddo
      enddo
   enddo
else
   do i = 1,m2
      do j = 1,m3
         ka = lpx(i,j)
         do k = ka-1,1,-1
            aa(k,i,j) = aa(k+1,i,j)
         enddo
      enddo
   enddo
endif

if (vnam == 'U' .or. vnam == 'V' .or. vnam == 'W') then
   do i = 1,m2
      do j = 1,m3
         ka = lpx(i,j)
         do k = ka-1,1,-1
            aa(k,i,j) = 0.
         enddo
      enddo
   enddo
endif

return
end

!******************************************************************************

subroutine rayf_adap(ifrom,m1,m2,m3,ia,iz,ja,jz,ibcon,lpx,var,th0,tht)

use mem_grid
use ref_sounding

implicit none

integer :: ifrom,m1,m2,m3,ia,iz,ja,jz,ibcon
integer, dimension(m2,m3) :: lpx
real, dimension(m1,m2,m3) :: var,th0,tht

real :: zmkf,c1,c2
integer :: kf,i,j,k

!     This routine calculates rayleigh friction terms velocity and theta_il

if (nfpt .eq. 0 .or. distim .le. 0) return
kf = nnz(1) - nfpt
zmkf = zmn(kf,1)
c1 = 1. / (distim * (ztop - zmkf))
c2 = dts * c1
goto(100,200,300,400) ifrom
100   continue

!     u friction

do j = ja,jz
   do i = ia,iz
      do k = m1-1,lpx(i,j),-1
         if (zt(k) .le. zmkf) go to 10

         var(k,i,j) = var(k,i,j) + c2 * (zt(k) - zmkf)  &
            * (u01dn(k,ngrid) - var(k,i,j))

      enddo
10    continue
   enddo
enddo
return
200   continue

!     V friction

if (jdim .eq. 0 .and. icorflg .eq. 0) return
do j = ja,jz
   do i = ia,iz
      do k = m1-1,lpx(i,j),-1
         if (zt(k) .le. zmkf) go to 20
         var(k,i,j) = var(k,i,j) + c2 * (zt(k) - zmkf)  &
            * (v01dn(k,ngrid) - var(k,i,j))
      enddo
20         continue
   enddo
enddo
return
300   continue

!     W friction

do j = ja,jz
   do i = ia,iz
      do k = m1-1,lpx(i,j),-1
         if (zt(k) .le. zmkf) go to 30
         var(k,i,j) = var(k,i,j) - c2 * (zt(k) - zmkf) * var(k,i,j)
      enddo
30         continue
   enddo
enddo
return
400   continue

!     THETA FRICTION

do j = ja,jz
   do i = ia,iz
      do k = m1-1,lpx(i,j),-1
         if (zt(k) .le. zmkf) go to 40
         tht(k,i,j) = tht(k,i,j) + c1 * (zt(k) - zmkf)  &
            * (th0(k,i,j) - var(k,i,j))
      enddo
40         continue
   enddo
enddo
return
end
