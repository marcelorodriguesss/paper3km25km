!
! Copyright (C) 1991-2003  ; All Rights Reserved ; Colorado State University
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
!======================================================================================!############################# Change Log ##################################
! 5.0.5

subroutine urban_canopy()

use mem_grid
use mem_turb
use mem_basic
use mem_scratch
use mem_tend

use node_mod

implicit none

! ut = -c abs(spd) u
! vt = -c abs(spd) v


call urb_tend(mzp,mxp,myp,ia,iz,ja,jz,jdim  &
             ,basic_g(ngrid)%up(1,1,1),basic_g(ngrid)%vp(1,1,1)  &
             ,turb_g(ngrid)%cdrag(1,1,1),scratch%scr2(1)  &
             ,tend%ut(1),tend%vt(1),dtlv)
             
return
end

!************************************************************************

subroutine urb_tend(m1,m2,m3,ia,iz,ja,jz,jdim,up,vp,cdrag,spd,ut,vt,dtlv)

implicit none

integer :: m1,m2,m3,ia,iz,ja,jz,jdim 
real, dimension(m1,m2,m3) :: up,vp,cdrag,spd,ut,vt
real :: dtlv

integer :: i,j,k

! Speed is computed at T points, then multiplied by cdrag. This is 
!     averaged to u,v points.
!   Parallel considertion: need proper u at i+1,i-1, v at j+1,j-1

do j=2,m3
   do i=2,m2
      do k=2,m1-1
         spd(k,i,j)= sqrt(  (.5*(up(k,i,j)+up(k,i-1,j)))**2  &
                           +(.5*(vp(k,i,j)+vp(k,i,j-jdim)))**2 )  &
                     *cdrag(k,i,j)
      enddo
   enddo
enddo


do j=ja,jz
   do i=ia,iz
      do k=2,m1-1
         !if(spd(k,i,j) > 0. .or. spd(k,i+1,j) > 0. .or. spd(k,i,j+jdim) > 0.)&
         !     print*,'drag-spd@k,i,j=',k,i,j,spd(k,i,j)

         ut(k,i,j) = ut(k,i,j) - &
            min((spd(k,i,j) + spd(k,i+1,j)) * 0.5, 0.3 / dtlv) * up(k,i,j)
         vt(k,i,j) = vt(k,i,j) - &
            min((spd(k,i,j)+spd(k,i,j+jdim)) * 0.5, 0.3 / dtlv) * vp(k,i,j)

         !if(i==41.and.j==42.and.k==2) &
         !print*,ut(k,i,j),up(k,i,j),spd(k,i,j)/cdrag(k,i,j),cdrag(k,i,j)
      enddo
   enddo
enddo

return
end

!************************************************************************

subroutine urb_drag_init()

use mem_turb
use mem_grid

implicit none

integer :: ng,k
real, allocatable :: plt(:,:)

! Compute drag coefficient from bfrac (building fraction), or whatever
!    else we put in it. Assume this will be zero if not in urban grid cell.

!  c = b*sqrt(A) / (2*(A-a))

! cdrag to be computed at T points, set to zero for non-urban cells


do ng = 1,ngrids

   print*,'calling getdrag for ng=',ng
   call getdrag(nnzp(ng),nnxp(ng),nnyp(ng)  &
               ,xmn(1,ng),ymn(1,ng),zmn(1,ng)  &
               ,turb_g(ng)%cdrag(1,1,1)  &
               ,polelat,polelon  &
               ,grid_g(ng)%glat,grid_g(ng)%glon,grid_g(ng)%rtgm,ng  &
               ,grid_g(ng)%dxm,grid_g(ng)%dym)
   
   allocate (plt(nnxp(ng),nnyp(ng)))
   do k=1,10
      plt(1:nnxp(ng),1:nnyp(ng))=turb_g(ng)%cdrag(k,1:nnxp(ng),1:nnyp(ng))
      call ezcntr(plt,nnxp(ng),nnyp(ng))
   enddo
   deallocate(plt)
   
enddo

!call clsgks
!stop

return
end

!************************************************************************

subroutine getdrag(nzp,nxp,nyp,xm,ym,zm,cdrag,polelat,polelon,glat,glon,rtgm,ng,dxm,dym)

implicit none

integer :: nxp,nyp,nzp,ng
real, dimension(nxp) :: xm
real, dimension(nyp) :: ym
real, dimension(nzp) :: zm
real, dimension(nzp,nxp,nyp) :: cdrag
real, dimension(nxp,nyp) :: glat,glon,rtgm,dxm,dym
real :: polelat,polelon
!real, allocatable, dimension(:,:,:) :: xxdrag
!real, allocatable, dimension(:,:,:) :: xxdrag

integer :: i,j,k,ic,jc,kc,ic1,ic2,jc1,jc2,kc1,kc2
real :: rim1_cd,rim2_cd,rjm1_cd,rjm2_cd,rkm1_cd,rkm2_cd,ric,rjc,rkc
logical :: there

character(len=80) :: fname='./CDRAG_DATA.TXT',ctem
real, allocatable, dimension(:,:,:) :: xxdrag
real, allocatable, dimension(:) :: cz,dx,dy
real, allocatable, dimension(:,:) :: clat,clon
real :: alat,alon,degx,degy,xc,yc,cvol,gvoli
real :: rpole=6356.8e3,req=6378.1e3,pi180=3.141592654/180.
integer :: nxc,nyc,nzc,iun=10,icount,icountx1,icountx2,icounty1,icounty2

! Read in the prepared drag coeff file

inquire(file=fname, exist=there)
if(.not. there) then
   print*,'Urban canopy drag coeff file not found.'
   print*,'File name:',trim(fname)
   stop 'no urban drag file'
endif

open(unit=iun,file=trim(fname),form='formatted',err=1)
read(iun,*) ctem,alat,alon
if(trim(ctem) /= 'Location:')then
   print*,'expecting Location: header'
   stop 'sub getdrag in urban_canopy.f90'
endif
read(iun,*) ctem,nxc,nyc,nzc
if(trim(ctem) /= 'Dimension:')then
   print*,'expecting Dimension: header'
   stop 'sub getdrag in urban_canopy.f90'
endif
read(iun,*) ctem,degx,degy
if(trim(ctem) /= 'Resolution(deg):')then
   print*,'expecting Resolution(deg): header'
   stop 'sub getdrag in urban_canopy.f90'
endif
print*,'alat,alon,nxc,nyc,nzc,degx,degy=',alat,alon,nxc,nyc,nzc,degx,degy

allocate(xxdrag(nxc,nyc,nzc))
allocate(clat(nxc,nyc))
allocate(clon(nxc,nyc))
allocate(dx(nyc))
allocate(dy(nxc))
allocate(cz(nzc))
read(iun,*)ctem,cz
if(trim(ctem) /= 'Levels(m):')then
   print*,'expecting Levels(m): header'
   stop 'sub getdrag in urban_canopy.f90'
endif
read(iun,*)(((xxdrag(i,j,k),i=1,nxc),j=1,nyc),k=1,nzc)

1 continue
close(iun)

! Craig's old way
!!allocate (xxdrag(66,84,11))
!allocate (xxdrag(100,100,11))
!call rams_c_open(trim(fname)//char(0),'r')
!!call rams_c_read(4,66*84*11*4,xxdrag(1,1,1))
!call rams_c_read(4,100*100*11*4,xxdrag(1,1,1))
!call rams_c_close()

!do k=1,11
!call ezcntr(xxdrag(1,1,k),nxc,nyc)
!enddo

cdrag(1:nzp,1:nxp,1:nyp) = 0.

! The cdrag data is on a rectangular grid. Place a temporary rotated PS grid
! w/ the polelat/lon locate at the SW corner of these data so that they can be
! properly projected onto a lat/lon grid.
!yc = -dy
!do jc=1,nyc
!   yc = yc + dy
!   xc = -dx
!   do ic=1,nxc
!      xc = xc + dx
!      call xy_ll(clat(ic,jc),clon(ic,jc),alat,alon,xc,yc)
!   enddo
!enddo
!print*,'clat(50,j)=',clat(50,1:nyc)
!print*,'clon(50,j)=',clon(50,1:nyc)
!print*,'clat(i,50)=',clat(1:nxc,50)
!print*,'clon(i,50)=',clon(1:nxc,50)

clon(1,1:nyc) = alon
clat(1:nxc,1) = alat
dy(1) = degy * pi180 * rpole
dx(1) = degx * pi180 * req * cos(pi180*clat(1,1))
do jc=2,nyc
   do ic=2,nxc
      clat(ic,jc) = clat(ic,jc-1) +  degx
      clon(ic,jc) = clon(ic-1,jc) +  degy
      !if(ic == 1)print*,'ic,jc,clat,lon=',ic,jc,clat(ic,jc),clon(ic,jc)
      !clat(ic,jc) = clat(ic,jc) + yc * pi180 / rpole
      !clon(ic,jc) = clon(ic,jc) + xc * pi180 / req * cos(pi180*clat(ic,jc))
   enddo
enddo
clon(1:nxc,1) = clon(1:nxc,2)
clat(1,1:nyc) = clat(2,1:nyc)
dy(1:nxc) = degy * pi180 * rpole
do jc=1,nyc
   dx(jc) = degx * pi180 * req * cos(pi180*clat(1,jc))
enddo

!print*,'clat(50,j)=',clat(50,1:nyc)
!print*,'clon(50,j)=',clon(50,1:nyc)
!print*,'clat(i,50)=',clat(1:nxc,50)
!print*,'clon(i,50)=',clon(1:nxc,50)
!print*,'dy(1)=',dy(1)
!print*,'dx(1:nyc)=',dx(1:nyc)

! We now have a rectangular grid that has lat/lon coordinates. See if any of
!  these are wholly contained within a RAMS grid cell.

do j = 1,nyp-1
   do i = 1,nxp-1

      icount = 0
      icountx1 = 100000
      icountx2 = -1
      icounty1 = 100000
      icounty2 = -1
      do jc = 1,nyc
         do ic = 1,nxc

            if(glon(i,j) <= clon(ic,jc)   .and.  &
                 glon(i+1,j) >= clon(ic,jc) .and.  &
                 glat(i,j) <= clat(ic,jc) .and.  &
                 glat(i,j+1) >= clat(ic,jc))then

               !print*,'ng,i,j,ic,jc=',ng,i,j,ic,jc

               icount = icount + 1
               icountx1 = min(icountx1,ic)
               icountx2 = max(icountx2,ic)
               icounty1 = min(icounty1,jc)
               icounty2 = max(icounty2,jc)

               do k = 2,nzp

                  !if(zm(k-1)*rtgm(i,j) >= cz(1) .and.  &
                  !     zm(k)*rtgm(i,j) <= cz(nzc))then
                          
                     do kc = 2,nzc
                        
                        if(zm(k)*rtgm(i,j) >= cz(kc) .and.  &
                             zm(k-1)*rtgm(i,j) <= cz(kc))then

                           ! Figure the volume that the cdrag data represents.
                           cvol = dx(jc) * dy(ic) * (cz(kc) - cz(kc-1))
                           gvoli = dxm(i,j) * dym(i,j) &
                                / (zm(k) - zm(k-1)) * rtgm(i,j)

                           !if(i == 80 .and. j == 74)print*,'using ng,k,ic,jc,kc,cvol,gvol=',ng,k,ic,jc,kc,cvol,1./gvoli
                           cdrag(k,i,j) = cdrag(k,i,j) + xxdrag(ic,jc,kc)  &
                                * cvol * gvoli

                        endif

                     enddo
                        
                  !endif
                     
               enddo
               cdrag(1,i,j) = cdrag(2,i,j)

            endif

         enddo
      enddo

      if(icountx2 > 0)write(6,'(a,i2,2(x,i4),5(x,i5))') &
                     'ng,i,j,icount,rangex1,2,y1,2='  &
                     ,ng,i,j,icount,icountx1,icountx2,icounty1,icounty2

   enddo
enddo


! Calculate the x,y position of the SW corner of the input dataset
!call ll_xy (alat,alon,polelat,polelon,xc,yc)
!print*,'polelat,polelon,xc,yc=',polelat,polelon,xc,yc
!print*,'xm=',xm(1:10)
!print*,'ym=',ym(1:10)
                                                                          
! See if point is on this grid.
!if (xxm < xm1(1) .or. xxm > xm1(n2-1) .or. &
!    yym < ym1(1) .or. yym > ym1(n3-1) ) then
!endif
!stop
                                                                                
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
GOTO 100

! Use the following polelat,polelon coordinates!!!

! polelat = 38.889
! polelon = -75.

do j = 2,nyp-1
   do i = 2,nxp-1
      do k = 2,nzp-1
         
         rim1_cd = (xm(i-1) + 500000. - 304527.) / 500. + 1.
         rim2_cd = (xm(i)   + 500000. - 304527.) / 500. + 1.

         rjm1_cd = ym(j-1) / 500. + 32.
         rjm2_cd = ym(j)   / 500. + 32.

         rkm1_cd = zm(k-1) / 5. + 1.
         rkm2_cd = zm(k)   / 5. + 1.
         
         if(i==41.and.j==43.and.k==2) print*,rim1_cd,rim2_cd 
         if(i==41.and.j==43.and.k==2) print*,rjm1_cd,rjm2_cd 
         if(i==41.and.j==43.and.k==2) print*,rkm1_cd,rkm2_cd 
         if(i==41.and.j==43.and.k==2) print*,xm(i),xm(i-1)
         
!         if ((rim1_cd > 1. .and. rim1_cd < 67.) .and.  &
!             (rim2_cd > 1. .and. rim2_cd < 67.) .and.  &
!             (rjm1_cd > 1. .and. rjm1_cd < 85.) .and.  &
!             (rjm2_cd > 1. .and. rjm2_cd < 85.) .and.  &
!             (rkm1_cd > 1. .and. rkm1_cd < 11.) .and.  &
!             (rkm2_cd > 1. .and. rkm2_cd < 11.)  ) then
   
   

            ic1 = max( 1, int(rim1_cd) )
            ic2 = min(66, int(rim2_cd) )
            jc1 = max( 1, int(rjm1_cd) )
            jc2 = min(83, int(rjm2_cd) )
            kc1 = max( 1, int(rkm1_cd) )
            kc2 = min(11, int(rkm2_cd) )
           
            do ic = ic1,ic2 
               do jc = jc1,jc2 
                  do kc = kc1,kc2
                     ric = float(ic)
                     rjc = float(jc)
                     rkc = float(kc)
                     cdrag(k,i,j) = cdrag(k,i,j)  &
                        +  (min(ric+1.,rim2_cd) - max(ric,rim1_cd))  &
                         * (min(rjc+1.,rjm2_cd) - max(rjc,rjm1_cd))  &
                         * (min(rkc+1.,rkm2_cd) - max(rkc,rkm1_cd))  &
                         /  ((rim2_cd - rim1_cd)  &
                           * (rjm2_cd - rjm1_cd)  &
                           * (rkm2_cd - rkm1_cd))  * xxdrag(ic,jc,kc)
                  enddo
               enddo
            enddo
            
!         endif
         
      enddo
   enddo
enddo
100 continue
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

deallocate (xxdrag)
deallocate (clat)
deallocate (clon)
deallocate (cz)

return
end
