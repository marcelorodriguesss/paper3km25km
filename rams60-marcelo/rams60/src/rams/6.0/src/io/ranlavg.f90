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

subroutine anlavg(n1,n2,n3,n4)

use var_tables
use mem_scratch
use mem_turb
use mem_basic
use node_mod
use mem_grid
use io_params

implicit none

integer :: n1,n2,n3,n4

integer :: nv,ng
integer :: n3dadd,n3d,n2dadd,n2d,izero,indvar,indavg

integer, save :: ncall=0,nvars(maxgrds,2),numadd,navg
real, save :: avgtim1,frq,avgtim2,timem,timecent

real, pointer :: v_p, vm_p

! This routine averages all of the analysis variables over
! ANLAVG. It also accumulates precipitation for all of the
! microphysics variables since the last analysis write.

if(avgtim == 0.)return
if(frqmean == 0.0.and.frqboth == 0.)return

if(ncall.eq.0) then
   ncall=1
!  Calculate the number of timestep to average over. Note that if
!    AVGTIM>0, then need to have an equal number of timesteps on
!    both sides of the analysis write, else can have an odd number
!    of timesteps if the write represent an average at the end
!    of the averaging period.
   avgtim1=avgtim
   if(avgtim.gt.0)then
     avgtim=max(avgtim,2.*dtlongn(1))
     avgtim=float(nint(avgtim/dtlongn(1)/2.))*dtlongn(1)*2.
   elseif(avgtim.lt.0)then
     avgtim=min(avgtim,-dtlongn(1))
     avgtim=float(nint(avgtim/dtlongn(1)))*dtlongn(1)
   endif
   if(avgtim.ne.avgtim1)then
     print*,' '
     print*,'***************************************************'
     print*,'***Changing AVGTIM so multiple of DTLONG',avgtim
     print*,'***************************************************'
     print*,' '
   endif
   navg=nint(abs(avgtim)/dtlongn(1))
   print*,' '
   print*,'****************************************************'
   print*,'**** AVERAGING OVER THESE # OF TIMESTEPS', navg
   print*,'**** AVERAGING TIME=',avgtim
   print*,'****************************************************'
   print*,' '

!  Choose a output frequency

   if(frqmean.gt.0.0.and.frqboth.gt.0.)then
      frq=min(frqmean,frqboth)
   elseif(frqmean.gt.0.0.and.frqboth.eq.0.)then
      frq=frqmean
   elseif(frqmean.eq.0.0.and.frqboth.gt.0.)then
      frq=frqboth
   endif

endif

izero=0
avgtim2=abs(avgtim)/2.
timem=time+0.01  
timecent=float(nint(timem/frq))*frq

! No need to execute if not in the averaging interval.
if(avgtim.gt.0.0)then
  if(timem.lt.avgtim2)return
  if(timem.lt.timecent-avgtim2.or.timem.gt.timecent+avgtim2) return
elseif(avgtim.lt.0.0)then
  if(mod(timem-avgtim,frq).gt.-avgtim)return
endif

! Zero out the averages before accumulating.
if(avgtim.gt.0.0.and.mod(timem+avgtim2,frq).lt.dtlongn(1)) izero=1
if(avgtim.lt.0.0.and.mod(timem-avgtim,frq).lt.dtlongn(1)) izero=1

!      Implement THERMO call for THETA and RV so matches code in rdriv.f
!         Note that theta is changed, BUT never used on boundary points

 call thermo(n1,n2,n3,1,1,1,n3,'THRM_ONLY')
 call thermo(n1,n2,n3,n2,n2,1,n3,'THRM_ONLY')
 if (jdim .eq. 1) then
    call thermo(n1,n2,n3,1,n2,1,1,'THRM_ONLY')
    call thermo(n1,n2,n3,1,n2,n3,n3,'THRM_ONLY')
 endif


! Loop through the main variable table

do nv = 1,num_var(ngrid)

   if (vtab_r(nv,ngrid)%imean == 1) then
   
      v_p => vtab_r(nv,ngrid)%var_p
      vm_p=> vtab_r(nv,ngrid)%var_m
      
      !print*,'avg:',nv,vtab_r(nv,ngrid)%npts,navg,izero
      if(izero == 1)call azero(vtab_r(nv,ngrid)%npts,vm_p)
      call average(vtab_r(nv,ngrid)%npts,vm_p,v_p,navg)
      
   endif

enddo

return
end

!*******************************************************************************

subroutine average(m,av,v,navg)

implicit none
integer :: m, navg
real :: av(m),v(m)

integer :: i

do i=1,m
  av(i)=av(i)+v(i)/float(navg)
enddo

return
end

!*******************************************************************************

!subroutine zeromeangrad(a,ifm,n1,n2,n3,ia,ib,ja,jb)

!implicit none
!real :: a(*)
!integer :: ifm,n1,n2,n3,ia,ib,ja,jb

!include 'rcommons.h'

!integer :: n3d,ithflg,irvflg

!if(frqmean.eq.0.0.and.frqboth.eq.0.) return

! Check to make sure space allocated for this variable

!ithflg=0
!irvflg=0
!do n3d=1,nm3d(ifm)
!   if(imchr3d(n3d,ifm).eq.'THETAM')ithflg=1
!   if(imchr3d(n3d,ifm).eq.'RVM')irvflg=1
!enddo
!do n3d=1,nb3d(ifm)
!   if(ibchr3d(n3d,ifm).eq.'THETAM')ithflg=1
!   if(ibchr3d(n3d,ifm).eq.'RVM')irvflg=1
!enddo

!if(ithflg.eq.1)call rowcolmn(n1,n2,n3,ia,ib,ja,jb,a(ithetam))
!if(irvflg.eq.1)call rowcolmn(n1,n2,n3,ia,ib,ja,jb,a(irvm))

!return
!end

!*******************************************************************************

subroutine rowcolmn(n1,n2,n3,ia,ib,ja,jb,var)

implicit none
integer :: n1,n2,n3,ia,ib,ja,jb
real :: var(n1,n2,n3)

integer :: i,j,k

if(ia.eq.ib.and.ia.eq.1)then
   do j=ja,jb
      do i=ia,ib
         do k=1,n1
            var(k,i,j)=var(k,i+1,j)
         enddo
      enddo
   enddo
elseif(ia.eq.ib.and.ia.eq.n2)then
   do j=ja,jb
      do i=ia,ib
         do k=1,n1
            var(k,i,j)=var(k,i-1,j)
         enddo
      enddo
   enddo
elseif(ja.eq.jb.and.ja.eq.1)then
   do j=ja,jb
      do i=ia,ib
         do k=1,n1
            var(k,i,j)=var(k,i,j+1)
         enddo
      enddo
   enddo
elseif(ja.eq.jb.and.ja.eq.n3)then
   do j=ja,jb
      do i=ia,ib
         do k=1,n1
            var(k,i,j)=var(k,i,j-1)
         enddo
      enddo
   enddo
endif

return
end
