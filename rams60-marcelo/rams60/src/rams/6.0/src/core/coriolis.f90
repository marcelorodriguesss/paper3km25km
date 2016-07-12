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

subroutine fcorio(n2,n3,fcoru,fcorv,glat)

use mem_grid
use rconstants

implicit none

integer :: n2,n3
real, dimension(n2,n3) :: fcoru,fcorv,glat

!    +----------------------------------------------------------------+
!    \  This routine calculates the Coriolis parameter.
!    +----------------------------------------------------------------+

real :: omega2
integer :: i,j

omega2 = 2. * omega

do j = 1,max(1,n3-1)
   do i = 1,n2-1
      fcoru(i,j) = omega2*sin((glat(i,j)+glat(i+1,j))  &
           *.5*pi180)
      fcorv(i,j) = omega2*sin((glat(i,j)+glat(i,j+jdim))  &
           *.5*pi180)
   enddo
enddo

return
end

!     ******************************************************************

subroutine corlos(mzp,mxp,myp,i0,j0,ia,iz,ja,jz,izu,jzv)

use mem_basic
use mem_grid
use mem_scratch
use mem_tend

implicit none

integer :: mzp,mxp,myp,i0,j0,ia,iz,ja,jz,izu,jzv

!     This routine is the coriolis driver.  Its purpose is to compute
!     coriolis accelerations for u and v and add them into
!     the accumulated tendency arrays of UT and VT.

if(icorflg.eq.0) return

call corlsu(mzp,mxp,myp,i0,j0,ia,izu,ja,jz  &
     ,basic_g(ngrid)%uc(1,1,1)   &
     ,basic_g(ngrid)%vc(1,1,1)   &
     ,tend%ut(1)                 &
     ,scratch%scr1(1)            &
     ,grid_g(ngrid)%topu(1,1)    &
     ,grid_g(ngrid)%rtgu(1,1)    &
     ,basic_g(ngrid)%fcoru(1,1)  )

call corlsv(mzp,mxp,myp,i0,j0,ia,iz,ja,jzv  &
     ,basic_g(ngrid)%uc(1,1,1)   &
     ,basic_g(ngrid)%vc(1,1,1)   &
     ,tend%vt(1)                 &
     ,scratch%scr1(1)            &
     ,grid_g(ngrid)%topv(1,1)    &
     ,grid_g(ngrid)%rtgv(1,1)    &
     ,basic_g(ngrid)%fcorv(1,1)  )
!
return
end

!     ******************************************************************

subroutine corlsu(m1,m2,m3,i0,j0,ia,iz,ja,jz  &
     ,up,vp,ut,vt3da,top,rtg,fcor)

use mem_grid
use rconstants
use mem_scratch
use ref_sounding

implicit none

integer :: m1,m2,m3,i0,j0,ia,iz,ja,jz
real, dimension(m1,m2,m3) :: up,vp,ut,vt3da
real, dimension(m2,m3) ::    top,rtg,fcor

integer :: i,j,k
real :: c1

do j=ja,jz
   do i=ia,iz
      do k=2,m1-1
         vt3da(k,i,j)=(vp(k,i,j)+vp(k,i,j-jdim)  &
              +vp(k,i+1,j)+vp(k,i+1,j-jdim))*.25
      enddo
   enddo
enddo

c1=1./(erad*erad*2.)
if(ihtran.eq.0) c1=0.
do j=ja,jz
   do i=ia,iz
      do k=2,m1-1
         ut(k,i,j)=ut(k,i,j)-vt3da(k,i,j)*(-fcor(i,j)  &
                  +c1*(vt3da(k,i,j)*xm(i+i0)-up(k,i,j)*yt(j+j0)))
      enddo
   enddo
enddo
!
if (initial /= 1) return

if (if_adap == 0 .and. itopo == 1) then

   do j = ja,jz
      do i = ia,iz
         do k = 1,m1
            vctr2(k) = zt(k) * rtg(i,j) + top(i,j)
         enddo
         call htint(nzp,v01dn(1,ngrid),zt,nz,vctr5,vctr2)
         do k = 2,m1-1
            ut(k,i,j) = ut(k,i,j) - fcor(i,j) * vctr5(k)
         enddo
      enddo
   enddo

else

   do j = ja,jz
      do i = ia,iz
         do k = 2,m1-1
            ut(k,i,j) = ut(k,i,j) - fcor(i,j) * v01dn(k,ngrid)
         enddo
      enddo
   enddo

endif

return
end

! **************************************************************

subroutine corlsv(m1,m2,m3,i0,j0,ia,iz,ja,jz  &
     ,up,vp,vt,vt3da,top,rtg,fcor)

use mem_grid
use rconstants
use mem_scratch
use ref_sounding

implicit none

integer :: m1,m2,m3,i0,j0,ia,iz,ja,jz
real, dimension(m1,m2,m3) :: up,vp,vt3da,vt
real, dimension(m2,m3) ::    top,rtg,fcor

integer :: i,j,k
real :: c1

!       This routine calculates coriolis tendencies to v

do j = ja,jz
   do i = ia,iz
      do k = 2,m1-1
         vt3da(k,i,j) = (up(k,i,j) + up(k,i-1,j)  &
            + up(k,i,j+jdim) + up(k,i-1,j+jdim)) * .25
      enddo
   enddo
enddo

c1 = 1. / (erad * erad * 2.)
if (ihtran .eq. 0) c1 = 0.
do j = ja,jz
   do i = ia,iz
      do k = 2,m1-1
         vt(k,i,j) = vt(k,i,j) - vt3da(k,i,j) * (fcor(i,j)  &
            - c1 * (vp(k,i,j) * xt(i+i0) - vt3da(k,i,j) * ym(j+j0)))
      enddo
   enddo
enddo

if (initial /= 1) return

if (if_adap == 0 .and. itopo == 1) then
   do j = ja,jz
      do i = ia,iz
         do k = 1,m1
            vctr2(k) = zt(k) * rtg(i,j) + top(i,j)
         enddo
         call htint(nzp,u01dn(1,ngrid),zt,nz,vctr5,vctr2)
         do k = 2,m1-1
            vt(k,i,j) = vt(k,i,j) + fcor(i,j) * vctr5(k)
         enddo
      enddo
   enddo

else

   do j = ja,jz
      do i = ia,iz
         do k = 2,m1-1
            vt(k,i,j) = vt(k,i,j) + fcor(i,j) * u01dn(k,ngrid)
         enddo
      enddo
   enddo

endif

return
end





