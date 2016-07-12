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

subroutine fillscr_den(m1,m2,m3,n1,n2,n3,scr,var,i0,j0)
implicit none
integer :: m1,m2,m3,n1,n2,n3,i0,j0
real :: scr(m1,m2,m3),var(n1,n2,n3)

integer :: i,j,k

do j=1,n3
   do i=1,n2
      do k=1,n1
         scr(k,i,j)=var(k,i+i0,j+j0)
      enddo
   enddo
enddo
return
end

!     *****************************************************************

subroutine fillscr(m1,m2,m3,n1,n2,n3,k1,k2,scr,var)
implicit none
integer :: m1,m2,m3,n1,n2,n3,k1,k2
real :: scr(m1,m2,m3),var(n1,n2,n3)

integer :: i,j,k

do j = 1,n3
   do i = 1,n2
      do k = k1,k2
         scr(k,i,j) = var(k,i,j)
      enddo
   enddo
enddo
return
end

!     *****************************************************************

subroutine fillvar(m1,m2,m3,n1,n2,n3,k1,k2,scr,var)
implicit none
integer :: m1,m2,m3,n1,n2,n3,k1,k2
real :: scr(m1,m2,m3),var(n1,n2,n3)

integer :: i,j,k

do j = 1,n3
   do i = 1,n2
      do k = k1,k2
         var(k,i,j) = scr(k,i,j)
      enddo
   enddo
enddo
return
end

!     ****************************************************************

subroutine dnswt(nzmx,nxmx,nymx,n1,n2,n3,var1,dn0,vpnt  &
     ,jdim,idir)
implicit none
integer :: nzmx,nxmx,nymx,n1,n2,n3,jdim,idir
real :: var1(nzmx,nxmx,nymx),dn0(n1,n2,n3)
character(len=*) :: vpnt

integer :: i,j,k

if(idir.eq.1)then
   if(vpnt.eq.'t')then
      do j=1,n3
         do i=1,n2
            do k=1,n1
               var1(k,i,j)=var1(k,i,j)*dn0(k,i,j)
            enddo
         enddo
      enddo
   elseif(vpnt.eq.'u')then
      do j=1,n3
         do i=1,n2
            do k=1,n1
               var1(k,i,j)=var1(k,i,j)  &
                    *.5*(dn0(k,i,j)+dn0(k,i+1,j))
            enddo
         enddo
      enddo
   elseif(vpnt.eq.'v')then
      do j=1,n3
         do i=1,n2
            do k=1,n1
               var1(k,i,j)=var1(k,i,j)  &
                    *.5*(dn0(k,i,j)+dn0(k,i,j+jdim))
            enddo
         enddo
      enddo
   elseif(vpnt.eq.'w')then
      do j=1,n3
         do i=1,n2
            do k=1,n1
               var1(k,i,j)=var1(k,i,j)  &
                    *.5*(dn0(k,i,j)+dn0(k+1,i,j))
            enddo
         enddo
      enddo
   endif
elseif(idir.eq.2)then
   if(vpnt.eq.'t')then
      do j=1,n3
         do i=1,n2
            do k=1,n1
               var1(k,i,j)=var1(k,i,j)/dn0(k,i,j)
            enddo
         enddo
      enddo
   elseif(vpnt.eq.'u')then
      do j=1,n3
         do i=1,n2
            do k=1,n1
               var1(k,i,j)=var1(k,i,j)  &
                    /(.5*(dn0(k,i,j)+dn0(k,i+1,j)))
            enddo
         enddo
      enddo
   elseif(vpnt.eq.'v')then
      do j=1,n3
         do i=1,n2
            do k=1,n1
               var1(k,i,j)=var1(k,i,j)  &
                    /(.5*(dn0(k,i,j)+dn0(k,i,j+jdim)))
            enddo
         enddo
      enddo
   elseif(vpnt.eq.'w')then
      do j=1,n3
         do i=1,n2
            do k=1,n1
               var1(k,i,j)=var1(k,i,j)  &
                    /(.5*(dn0(k,i,j)+dn0(k+1,i,j)))
            enddo
         enddo
      enddo
   endif
endif
return
end

!     ****************************************************************

subroutine dnswt_off(nzmx,nxmx,nymx,n1,n2,n3,i0,j0  &
     ,var1,dn0,vpnt,jdim,idir)
implicit none
integer :: nzmx,nxmx,nymx,n1,n2,n3,i0,j0,jdim,idir
real :: var1(nzmx,nxmx,nymx),dn0(n1,n2,n3)
character(len=*) :: vpnt

integer :: i,j,k

if(idir.eq.1)then
   if(vpnt.eq.'t')then
      do j=1,n3
         do i=1,n2
            do k=1,n1
               var1(k,i,j)=var1(k,i,j)*dn0(k,i+i0,j+j0)
            enddo
         enddo
      enddo
   elseif(vpnt.eq.'u')then
      do j=1,n3
         do i=1,n2
            do k=1,n1
               var1(k,i,j)=var1(k,i,j)  &
                    *.5*(dn0(k,i+i0,j+j0)+dn0(k,i+i0+1,j+j0))
            enddo
         enddo
      enddo
   elseif(vpnt.eq.'v')then
      do j=1,n3
         do i=1,n2
            do k=1,n1
               var1(k,i,j)=var1(k,i,j)  &
                    *.5*(dn0(k,i+i0,j+j0)+dn0(k,i+i0,j+jdim+j0))
            enddo
         enddo
      enddo
   elseif(vpnt.eq.'w')then
      do j=1,n3
         do i=1,n2
            do k=1,n1
               var1(k,i,j)=var1(k,i,j)  &
                    *.5*(dn0(k,i+i0,j+j0)+dn0(k+1,i+i0,j+j0))
            enddo
         enddo
      enddo
   endif
elseif(idir.eq.2)then
   if(vpnt.eq.'t')then
      do j=1,n3
         do i=1,n2
            do k=1,n1
               var1(k,i,j)=var1(k,i,j)/dn0(k,i+i0,j+j0)
            enddo
         enddo
      enddo
   elseif(vpnt.eq.'u')then
      do j=1,n3
         do i=1,n2
            do k=1,n1
               var1(k,i,j)=var1(k,i,j)  &
                    /(.5*(dn0(k,i+i0,j+j0)+dn0(k,i+i0+1,j+j0)))
            enddo
         enddo
      enddo
   elseif(vpnt.eq.'v')then
      do j=1,n3
         do i=1,n2
            do k=1,n1
               var1(k,i,j)=var1(k,i,j)  &
                    /(.5*(dn0(k,i+i0,j+j0)+dn0(k,i+i0,j+jdim+j0)))
            enddo
         enddo
      enddo
   elseif(vpnt.eq.'w')then
      do j=1,n3
         do i=1,n2
            do k=1,n1
               var1(k,i,j)=var1(k,i,j)  &
                    /(.5*(dn0(k,i+i0,j+j0)+dn0(k+1,i+i0,j+j0)))
            enddo
         enddo
      enddo
   endif
endif
return
end

subroutine dnswt2(nzmx,nxmx,nymx,n1,n2,n3,var1,dn0x,vnam,idir)
implicit none
integer :: nzmx,nxmx,nymx,n1,n2,n3,idir
real :: var1(nzmx,nxmx,nymx),dn0x(n1,n2,n3)
character(len=*) :: vnam

integer :: i,j,k

if (idir .eq. 1) then

   if (vnam .eq. 'w') then
      do j = 1,n3
         do i = 1,n2
            do k = 1,n1-1
               var1(k,i,j) = var1(k,i,j)  &
                  * .5 * (dn0x(k,i,j) + dn0x(k+1,i,j))
            enddo
         enddo
      enddo
   else
      do j = 1,n3
         do i = 1,n2
            do k = 1,n1
               var1(k,i,j) = var1(k,i,j) * dn0x(k,i,j)
            enddo
         enddo
      enddo
   endif

elseif (idir .eq. 2) then

   if (vnam .eq. 'w') then
      do j = 1,n3
         do i = 1,n2
            do k = 1,n1-1
               var1(k,i,j) = var1(k,i,j)  &
                  / (.5 * (dn0x(k,i,j) + dn0x(k+1,i,j)))
            enddo
         enddo
      enddo
   else
      do j = 1,n3
         do i = 1,n2
            do k = 1,n1
               var1(k,i,j) = var1(k,i,j) / dn0x(k,i,j)
            enddo
         enddo
      enddo
   endif

endif
return
end
