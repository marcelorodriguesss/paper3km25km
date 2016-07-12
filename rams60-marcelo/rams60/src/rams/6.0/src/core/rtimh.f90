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

subroutine timestep()

use mem_basic
use node_mod

use mem_radiate
use mem_cuparm
use mem_varinit
use mem_turb
use mem_oda,   only:if_oda
use micphys,   only:level
use mem_grid

implicit none

integer :: iter
real :: t1,w1
real, external :: cputime

integer, save :: ncall=0


call acctimes('init',0,'INIT',t1,w1)

!        +-------------------------------------------------------------+
!        |   Timestep driver for the hybrid non-hydrostatic time-split |
!        |      model.                                                 |
!        +-------------------------------------------------------------+

!  Zero out all tendency arrays.   
!--------------------------------
t1=cputime(w1)
CALL TEND0()          
call acctimes('accu',1,'TEND0',t1,w1)

!  Thermodynamic diagnosis   
!--------------------------------
t1=cputime(w1)
if (level  /=  3) then
   CALL THERMO(mzp,mxp,myp,ia,iz,ja,jz,'SUPSAT') 
endif
call acctimes('accu',2,'THERMO',t1,w1)

!  Radiation parameterization
!--------------------------------
t1=cputime(w1)
CALL RADIATE(mzp,mxp,myp,ia,iz,ja,jz,mynum) 
call acctimes('accu',3,'RADIATE',t1,w1)
   
   
!  Surface layer, soil, veggie, urban models
!--------------------------------------------
t1=cputime(w1)
CALL SFC_DRIVER(mzp,mxp,myp,ia,iz,ja,jz,ibcon) 
call acctimes('accu',4,'SFC_DRIVER',t1,w1)

!  Send boundaries to adjoining nodes
!-------------------------------------------
t1=cputime(w1)
if (ipara  ==  1) then
   call node_sendlbc()
   if (ngrid  ==  1) call node_sendcyclic(1)
endif
call acctimes('accu',26,'SendLBC',t1,w1)

!  Coriolis terms
!  ----------------------------------------
t1=cputime(w1)
CALL CORLOS(mzp,mxp,myp,i0,j0,ia,iz,ja,jz,izu,jzv) 
call acctimes('accu',5,'CORLOS',t1,w1)

!  Velocity advection
!----------------------------------------
t1=cputime(w1)
call ADVECTc('V',mzp,mxp,myp,ia,iz,ja,jz,izu,jzv,mynum)
call acctimes('accu',6,'ADVECTv',t1,w1)

!  Cumulus parameterization
!----------------------------------------
t1=cputime(w1)
IF(NNQPARM(ngrid) == 1 .or. IF_CUINV == 1) call cuparm()      
IF(NNQPARM(ngrid) == 2 ) call kf_main()      
call acctimes('accu',7,'CUPARM',t1,w1)

!  Analysis nudging and boundary condition
!------------------------------------------
t1=cputime(w1)
IF(NUD_TYPE > 0) CALL DATASSIM()  
call acctimes('accu',8,'DATASSIM',t1,w1)

!  Observation data assimilation 
!----------------------------------------
t1=cputime(w1)
IF(IF_ODA == 1) CALL oda_nudge()  
call acctimes('accu',8,'DATASSIM',t1,w1)

!  Nested grid boundaries
!----------------------------------------
t1=cputime(w1)
if(nxtnest(ngrid) >= 1) call nstbdriv()  
call acctimes('accu',9,'NSTBDRIV',t1,w1)

!  Rayleigh friction for theta
!----------------------------------------
t1=cputime(w1)
CALL RAYFT()           
call acctimes('accu',11,'RAYFT',t1,w1)

!  Get the overlap region between parallel nodes
!---------------------------------------------------
t1=cputime(w1)
if(ipara == 1) then      
   call node_getlbc()  
   if (ngrid  ==  1) call node_getcyclic(1)
endif
call acctimes('accu',13,'GETlbc',t1,w1)

!  Sub-grid diffusion terms
!----------------------------------------
t1=cputime(w1)
call diffuse ()
call acctimes('accu',12,'DIFFUSE',t1,w1)

!  Velocity advection
!----------------------------------------
t1=cputime(w1)
call ADVECTc('T',mzp,mxp,myp,ia,iz,ja,jz,izu,jzv,mynum)
call acctimes('accu',15,'ADVECTs',t1,w1)

!  Update scalars
!----------------------------------------
t1=cputime(w1)
CALL PREDTR()          
call acctimes('accu',16,'PREDTR',t1,w1)

!  Moisture variables positive definite
!----------------------------------------
t1=cputime(w1)
call negadj1(mzp,mxp,myp) 
call acctimes('accu',17,'NEGADJ1',t1,w1)

!  Microphysics
!----------------------------------------
t1=cputime(w1)
if (level == 3) then
   call micro()
endif
call acctimes('accu',19,'MICRO',t1,w1)

!  Thermodynamic diagnosis
!----------------------------------------
t1=cputime(w1)
if (level /= 3) then
   CALL THERMO(mzp,mxp,myp,1,mxp,1,myp,'MICRO') 
endif
call acctimes('accu',20,'THERMO',t1,w1)

!  Apply scalar b.c.'s
!----------------------------------------
t1=cputime(w1)
CALL TRSETS()          
call acctimes('accu',21,'TRSETS',t1,w1)

!  Lateral velocity boundaries - radiative
!-------------------------------------------
t1=cputime(w1)
CALL LATBND()         
call acctimes('accu',10,'LATBND',t1,w1)

!  First stage Asselin filter
!----------------------------------------
t1=cputime(w1)
CALL HADVANCE(1)     
call acctimes('accu',22,'HADVANCE',t1,w1)

!  Buoyancy term for w equation
!----------------------------------------
t1=cputime(w1)
CALL BUOYANCY()
call acctimes('accu',23,'BUOYANCY',t1,w1)

!  Acoustic small timesteps
!----------------------------------------
t1=cputime(w1)
CALL ACOUSTIC()
call acctimes('accu',24,'ACOUSTIC',t1,w1)

!  Last stage of Asselin filter
!----------------------------------------
t1=cputime(w1)
CALL HADVANCE(2)      

!  Velocity/pressure boundary conditions
!----------------------------------------
CALL VPSETS()          
call acctimes('accu',25,'HADVANCE',t1,w1)

!if(mod(istp,4) == 0) then
!   call acctimes('prin',-1,' ',t1,w1)
!endif

!      call mass_flux(nzp,nxp,nyp,mzp,mxp,myp,a(iup),a(ivp),a(iwp)
!     +     ,a(idn0),a(irtgu),a(irtgv),a(idyu),a(idxv),a(ipp),a(ipi0))

return
end




subroutine acctimes(action,num,string,t1,w1)

use mem_all
use node_mod

implicit none
integer :: num
real :: t1,w1
character(len=*) :: string,action

integer, parameter :: num_times=100
real, save :: rtimes(num_times),wtimes(num_times)
character(len=8),save :: crtimes(num_times)

real, external :: cputime,walltime,valugp
real :: sumtime,pcpu,fsecs,cpuinc,ww
integer :: i,j,npts,ip,jp,kp

if(action(1:4)=='init') then
   crtimes(1:num_times)=' '
   rtimes(1:num_times)=0.
   wtimes(1:num_times)=0.
   basic_g(ngrid)%cputime(1:mxp,1:myp)=0.
elseif(action(1:4)=='prin') then
   if(mynum/=2.and.mynum/=0) return
   sumtime=0.
   do i=1,num_times
      sumtime=sumtime+rtimes(i)
   enddo

   write(6,*) '======= total CPU =====',sumtime,'=========='
   do i=1,26
      write(6,'(a10,i4,''-'',a12,f10.3,f7.2,2f9.3)') &
           'Timings-',mynum,crtimes(i)  &
           ,rtimes(i),rtimes(i)/sumtime*100.,wtimes(i)  &
           ,wtimes(i)-rtimes(i)
   enddo
   sumtime=0.
   do j=1,myp
      do i=1,mxp
         sumtime=sumtime+basic_g(ngrid)%cputime(i,j)
      enddo
   enddo
   write(6,'(a,2i5,f10.5)') 'Total CPU secs -ngrid,mynum,secs:'  &
            ,ngrid,mynum,sumtime
            
elseif(action(1:4)=='accu'.or.action(1:4)=='null') then
   ! Accumulate full times into tables
   pcpu=(cputime(ww)-t1)
   rtimes(num)=rtimes(num) + pcpu
   crtimes(num)=string

   fsecs=72559200.
   wtimes(num)=wtimes(num)+(walltime(fsecs)-w1)
   
   if(action(1:4)=='accu') then
      ! Divide cpu time equally amoung columns and accumulate in
      ! basic_g(ngrid)%cputime(1,1)
      npts=(iz-ia+1)*(jz-ja+1)
      cpuinc=pcpu/float(npts)
      do j=ja,jz
         do i=ia,iz
            basic_g(ngrid)%cputime(i,j)=basic_g(ngrid)%cputime(i,j)+cpuinc
         enddo
      enddo
   endif
endif

!    only here for debugging purposes
   ip=2
   jp=2
   kp=2
      !   if( (mynum == 1.or.mynum == 0).and.ngrid == 2) then
       ! if( (mynum == 1.or.mynum == 0)) then
       !    print '(a10,2i3,f9.1,12e14.7)',string,2,mynum,time
       !       ,radiate_g(ngrid)%rshort(ip,jp) &
       !       ,radiate_g(ngrid)%rlongup(ip,jp) 
       !       ,basic_g(ngrid)%up(kp,ip,jp)  &
       !       ,valugp(mzp,mxp,myp,kp,ip,jp,tend%ut(1))  &
       !       ,basic_g(ngrid)%vp(kp,ip,jp)  &
       !       ,valugp(mzp,mxp,myp,kp,ip,jp,tend%vt(1))  &
       !       ,basic_g(ngrid)%wp(kp,ip,jp)  &
       !       ,valugp(mzp,mxp,myp,kp,ip,jp,tend%wt(1)) &
       !       ,basic_g(ngrid)%pp(kp,ip,jp)  &
       !       ,valugp(mzp,mxp,myp,kp,ip,jp,tend%pt(1))  &
       !       ,basic_g(ngrid)%pi0(kp,ip,jp)  
       !       ,basic_g(ngrid)%theta(kp,ip,jp) &
       !       ,basic_g(ngrid)%thp(kp,ip,jp) 
       !       ,float(grid_g(ngrid)%lpw(ip,jp))  &
!              ,basic_g(2)%rtp(kp,ip,jp)  &
!              ,basic_g(2)%rv(kp,ip,jp)  &
!              ,valugp(mzp,mxp,myp,kp,ip,jp,tend%rtt(1)) 
         
       !  do jp=22,1,-1
       !     print'(i3,20g12.4)',jp,(basic_g(ngrid)%pp(kp,ip,jp)),ip=1,8)
       !  enddo
         
       !  endif
return
end



subroutine mass_flux(n1,n2,n3,m1,m2,m3,up,vp,wp  &
     ,dn0,rtgu,rtgv,dyu,dxv,pp,pi0)

use mem_grid
use rconstants
     
implicit none
integer :: n1,n2,n3,m1,m2,m3
real :: up(m1,m2,m3),vp(m1,m2,m3),wp(m1,m2,m3)  &
     ,dn0(n1,n2,n3),rtgu(n2,n3),dyu(n2,n3),dxv(n2,n3)  &
     ,rtgv(n2,n3),pp(m1,m2,m3),pi0(n1,n2,n3)

real, save :: aintmass=0.

integer :: i,j,k
real :: wmass,emass,smass,nmass,prtot,tmass,ppp,area

!cc      if (mod(time,300.).gt..1) return

!  west/east bound
wmass=0.
emass=0.
do j=2,nyp-1
   do k=2,nzp-1
      i=1
      wmass=wmass +  &
           up(k,i,j)*rtgu(i,j)/(dyu(i,j)*dzt(k))  &
           *(dn0(k,i,j)+dn0(k,i+1,j))*.5
      i=nxp-1
      emass=emass -  &
           up(k,i,j)*rtgu(i,j)/(dyu(i,j)*dzt(k))  &
           *(dn0(k,i,j)+dn0(k,i+1,j))*.5
   enddo
enddo

!  north/south bound
smass=0.
nmass=0.
do i=2,nxp-1
   do k=2,nzp-1
      j=1
      smass=smass +  &
           vp(k,i,j)*rtgv(i,j)/(dxv(i,j)*dzt(k))  &
           *(dn0(k,i,j)+dn0(k,i,j+1))*.5
      j=nyp-1
      nmass=nmass -  &
           vp(k,i,j)*rtgv(i,j)/(dxv(i,j)*dzt(k))  &
           *(dn0(k,i,j)+dn0(k,i,j+1))*.5
   enddo
enddo

k=2
prtot=0.
do j=2,nyp-1
   do i=2,nxp-1
      ppp= ( (pp(k,i,j)+pi0(k,i,j))/cp )**cpor*p00
      prtot=prtot+ppp/(dyu(i,j)*dxv(i,j))
   enddo
enddo


tmass=wmass+emass+smass+nmass
aintmass=aintmass+tmass*dtlong
area=(nxp-2)*deltax*(nyp-2)*deltay


print*,'==============================='
print*,' Mass flux - W, E, S, N'
print*,  wmass,emass,smass,nmass
print*, 'total (kg/(m2 s):',tmass/area
print*, 'total (kg/m2):',aintmass/area
print*, 'total pr change (pa):',aintmass/area*9.8
print*, 'computed mean press:',prtot/area
print*,'==============================='



return
end

