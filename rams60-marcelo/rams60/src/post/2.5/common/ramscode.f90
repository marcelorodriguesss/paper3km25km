!
! Copyright (C) 1991-2003  ; All Rights Reserved ; ATMET, LLC
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

subroutine rams_ref (n1,n2,n3,ng,pi0,dn0,th0,topt)

include 'rcommons.h'
include 'rconstants.h'

dimension pi0(n1,n2,n3),dn0(n1,n2,n3),th0(n1,n2,n3),topt(n2,n3)

! This routine initializes the 3-D reference state arrays from the
! 1-D reference state if topography is used.

ztop=zmn(nnzp(ng)-1,1)
c2=(1.-cp/rgas)
c3=cp**c2
do j=1,nnyp(ng)
   do i=1,nnxp(ng)

      do k=1,nnzp(ng)
         vctr2(k)=ztn(k,ng)*(1.-topt(i,j)/ztop)+topt(i,j)
      enddo
      call htint(nnzp(ng),pi01dn(1,ng),ztn(1,ng)  &
           ,nnzp(ng),vctr3,vctr2)
      call htint(nnzp(ng),th01dn(1,ng),ztn(1,ng)  &
           ,nnzp(ng),vctr4,vctr2)

      do k=1,nnzp(ng)
         pi0(i,j,k)=vctr3(k)
         th0(i,j,k)=vctr4(k)
      enddo

      c1=g*2.*(1.-topt(i,j)/ztop)
      do k=nnzp(ng)-1,1,-1
         pi0(i,j,k)=pi0(i,j,k+1)  &
              +c1*(ztn(k+1,ng)-ztn(k,ng))  &
              /(th0(i,j,k)+th0(i,j,k+1))
      enddo

      do k=1,nzp
         dn0(i,j,k)=(c3*p00)/(rgas*th0(i,j,k)*pi0(i,j,k)**c2)
      enddo

   enddo
enddo

return
end

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine grdcoords ()

implicit none

include 'rcommons.h'
include 'rnest.h'

integer :: ngr,i,j,k,iinc,icnt,jinc,jcnt,kinc,kcnt,nrat,nestza

! Set NRZFLG to the FM which influences the CM vertical spacing
! Fill NRZ with the variable nest ratios.

do ngr=1,ngrids
   do k=1,nzpmax
      nrz(k,ngr)=1
   enddo
enddo

nestza=abs(nestz1)

if(nestza.gt.1.and.nestza.le.ngrids) then
   do k=2,nnzp(1)
      nrz(k,nestza)=max(1,nstratz1(k))
   enddo
   nrz(1,nestza)=nstratz1(2)
endif

! Fill IPM, JPM and KPM arrays with parent grid index values for
! all fine grids.

do ngr=2,ngrids
   ipm(1,ngr)=ninest(ngr)
   iinc=1
   icnt=0
   do i=2,nnxp(ngr)
      ipm(i,ngr)=ipm(i-1,ngr)+iinc
      icnt=icnt+1
      if(icnt.ge.nstratx(ngr)) then
         icnt=0
         iinc=1
      else
         iinc=0
      endif
   enddo

   jpm(1,ngr)=njnest(ngr)
   jinc=1
   jcnt=0
   do j=2,nnyp(ngr)
      jpm(j,ngr)=jpm(j-1,ngr)+jinc
      jcnt=jcnt+1
      if(jcnt.ge.nstraty(ngr)) then
         jcnt=0
         jinc=1
      else
         jinc=0
      endif
   enddo

   if(nknest(ngr).eq.1) then
      kpm(1,ngr)=2
      kinc=0
   else
      kpm(1,ngr)=nknest(ngr)
      kinc=1
   endif
   kcnt=0
   do k=2,nnzp(ngr)
      kpm(k,ngr)=kpm(k-1,ngr)+kinc
      nrat=nrz(kpm(k,ngr),ngr)
      kcnt=kcnt+1
      if(kcnt.ge.nrat.and.(k.lt.nnzp(ngr)-1.or.  &
         kpm(k,ngr).lt.nnxp(nxtnest(ngr))-1)) then
         kcnt=0
         kinc=1
      else
         kinc=0
      endif
   enddo
enddo

return
end
