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
! 2.5.0
!###########################################################################

subroutine find_ll_grid (nii,njj,nib,nie,njb,nje  &
                        ,xt,yt,platn,plonn  &
                        ,slat,wlon,nlat,elon  &
                        ,dllat,dllon,nllat,nllon  &
                        ,igridll,glldllat,glldllon  &
                        ,gllwlon,gllelon,gllslat,gllnlat)

! finds the largest rectangular lat lon grid that fits within
! the requested REVU selection

real :: wlon,elon,slat,nlat,wlonmax,elonmax,slatmax,nlatmax
real :: xt(*), yt(*)
real, allocatable :: glatn(:),glats(:), glonw(:),glone(:)

if(allocated(glatn)) deallocate(glatn)
if(allocated(glats)) deallocate(glats)
if(allocated(glonw)) deallocate(glonw)
if(allocated(glone)) deallocate(glone)
allocate(glatn(nii),glats(nii),glonw(njj),glone(njj))

do i=nib,nie
   call xy_ll(glats(i),flon,platn,plonn,xt(i),yt(njb))
   call xy_ll(glatn(i),flon,platn,plonn,xt(i),yt(nje))
enddo

do j=njb,nje
   call xy_ll(flat,glonw(j),platn,plonn,xt(nib),yt(j))
   call xy_ll(flat,glone(j),platn,plonn,xt(nie),yt(j))
enddo

if(igridll==0) then

   ! use namelist settings for ll grid

   dllat=glldllat
   dllon=glldllon

   wlon=gllwlon
   elon=gllelon
   slat=gllslat
   nlat=gllnlat

   wlonmax=1000.
   elonmax=-1000.
   do j=njb,nje
      wlonmax=min(wlonmax,glonw(j))
      elonmax=max(elonmax,glone(j))
   enddo
   slatmax=1000.
   nlatmax=-1000.
   do i=nib,nie
      slatmax=min(slatmax,glats(i))
      nlatmax=max(nlatmax,glatn(i))
   enddo
   if(slat > nlat) then
      print*,'in find_ll_grid: specified grid not allowed'
      print*,'  southern lat',slat,' >= northern',nlat
      stop
   endif
   if(wlon >= elon) then
      print*,'in find_ll_grid: specified grid not allowed'
      print*,'  western lon',slon,' >= eastern',nlon
      stop
   endif
   if(wlon < wlonmax .or. elon > elonmax .or.  &
      slat < slatmax .or. nlat > nlatmax) then
      print*,'in find_ll_grid: specified grid outside RAMS grid'
      print*,'  specified: ',wlon,elon,slat,nlat
      print*,'       RAMS: ',wlonmax,elonmax,slatmax,nlatmax
      stop
   endif

elseif(igridll == 1) then

   ! ll grid within RAMS grid
   dxx=xt(3)-xt(2)
   dyy=yt(3)-yt(2)
   dllat=dyy/111120.
   !dllon=dxx/(111120.*cos(platn*3.14159/180.))
   dllon=dllat

   wlon=-1000.
   elon=1000.
   do j=njb,nje
      wlon=max(wlon,glonw(j))
      elon=min(elon,glone(j))
   enddo
   slat=-1000.
   nlat=1000.
   do i=nib,nie
      slat=max(slat,glats(i))
      nlat=min(nlat,glatn(i))
   enddo

   slat=slat+.001
   nlat=nlat-.001
   wlon=wlon+.001
   elon=elon-.001

elseif(igridll == 2) then

   ! RAMS grid within ll grid
   dxx=xt(3)-xt(2)
   dyy=yt(3)-yt(2)
   dllat=dyy/111120.
   !dllon=dxx/(111120.*cos(platn*3.14159/180.))
   dllon=dllat

   wlon=1000.
   elon=-1000.
   do j=njb,nje
      wlon=min(wlon,glonw(j))
      elon=max(elon,glone(j))
   enddo
   slat=1000.
   nlat=-1000.
   do i=nib,nie
      slat=min(slat,glats(i))
      nlat=max(nlat,glatn(i))
   enddo

   slat=slat-dllat
   wlon=wlon-dllon
   nlat=nlat+dllat
   elon=elon+dllon

endif

nllat=int((nlat-slat)/dllat)+1
nllon=int((elon-wlon)/dllon)+1

!print*,'===>llgrid:',nllat,nllon,dllat,dllon
!print*,'===>llgrid:',wlon,elon,slat,nlat

deallocate(glatn,glats,glonw,glone)

return
end

!***************************************************************************

subroutine interp_ll (n1,n2,arr,xt,yt,m1,m2,swlat,swlon  &
                     ,dllat,dllon,platn,plonn,fval)

implicit none

integer, intent(in) :: n1,n2,m1,m2
real, intent(in) :: arr(n1,n2),xt(*),yt(*)
real, intent(in) :: platn,plonn,swlat,swlon,dllat,dllon
real, intent(out) :: fval(m1,m2)
real :: vt2ds(4,4),dxx,dyy,glat,glon,xfx,yfy,xiloc,yiloc,xnni,ynni
integer :: mi,mj,nni,nnj,i,j,im,jm

vt2ds(1:4,1:4)=1.e30

dxx=xt(3)-xt(2)
dyy=yt(3)-yt(2)

do mj=1,m2
   glat=swlat+(mj-1)*dllat
   do mi=1,m1
      glon=swlon+(mi-1)*dllon
      call ll_xy(glat,glon,platn,plonn,xfx,yfy)
      xiloc=1.+(xfx-xt(1))/dxx
      yiloc=1.+(yfy-yt(1))/dyy
      xnni=mod(xiloc,1.)+2.
      ynni=mod(yiloc,1.)+2.
      nni=nint(xiloc)
      nnj=nint(yiloc)
      nni=int(xiloc)
      nnj=int(yiloc)

      do j=1,4
         jm=nnj+j-2
         do i=1,4
           vt2ds(i,j)=1.e30
            im=nni+i-2
            if(im>=1 .and. im<=n1 .and. jm>=1 .and. jm<=n2)  &
               vt2ds(i,j)=arr(im,jm)
         enddo
      enddo
      call gdtost2(vt2ds,4,4,xnni,ynni,fval(mi,mj))

   enddo
enddo

return
end

!***************************************************************************

subroutine interp_ll_v5d (n1,n2,n3,arr,xt,yt,niinc,njinc,nzinc  &
                         ,nii,njj,maxnl,swlat,swlon,dllat,dllon  &
                         ,platn,plonn,nzb,nze,itrans,gg)

implicit none

integer :: n1,n2,n3,niinc,njinc,nzinc,nii,njj,maxnl,nzb,nze,itrans
real :: swlat,swlon,dllat,dllon,platn,plonn
real :: arr(n1,n2,n3),xt(*),yt(*),gg(njj,nii,maxnl)

integer nib,nie,njb,nje,nkb,nke,nki
real vt2ds(4,4),dxx,dyy,glat,glon,xfx,yfy,xiloc,yiloc,xnni,ynni
integer mi,mj,mjv,mk,mii,mjj,mkk,nni,nnj,i,j,k,im,jm

vt2ds(1:4,1:4)=1.e30
      
dxx=xt(3)-xt(2)
dyy=yt(3)-yt(2)

if(itrans.eq.3) then
   nkb=1
   nke=maxnl
   nki=1
else
   nkb=nzb
   nke=nze
   nki=nzinc
endif
!do k=1,n3
!   print*,'var:',k,arr(1,1,k)
!enddo

mk=0
do mkk=nkb,nke,nki
   mk=mk+1
   mjv=njj
   mj=0
   do mjj=1,njj
      mj=mj+1
      glat=swlat+(mj-1)*njinc*dllat
      mi=0
      do mii=1,nii
         mi=mi+1
         glon=swlon+(mi-1)*niinc*dllon
         call ll_xy (glat,glon,platn,plonn,xfx,yfy)
         xiloc=1.+(xfx-xt(1))/dxx
         yiloc=1.+(yfy-yt(1))/dyy
         xnni=mod(xiloc,1.)+2.
         ynni=mod(yiloc,1.)+2.
         nni=int(xiloc)
         nnj=int(yiloc)

         do j=1,4
            jm=nnj+j-2
            do i=1,4
              vt2ds(i,j)=1.e30
               im=nni+i-2
               if(im.ge.1.and.im.le.n1.and.jm.ge.1.and.jm.le.n2)  &
                  vt2ds(i,j)=arr(im,jm,mkk)
            enddo
         enddo
         call gdtost (vt2ds,4,4,xnni,ynni,gg(mjv,mi,mk))

      enddo
      mjv=mjv-1
   enddo
enddo

return
end
