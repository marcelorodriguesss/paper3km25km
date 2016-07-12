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
! KAIN-FRITSCH CUMULUS PARAMETERIZATION SCHEME 
! RAMS VERSION 4.3.0 
! RCONVKF.F90
! Updated July 2002 
!
! RAMS-KF interface originally written, modified, and tested by:
!
! Christopher L. Castro*
! William Y.Y. Cheng
! Adriana B. Beltran
! 
! Department of Atmospheric Science 
! Colorado State University
! Fort Collins, CO  80523
!
! *E-mail contact: chris@atmos.colostate.edu
!
! References:
!
! Castro, C.L., W.Y.Y. Cheng, A.B. Beltran, R.A. Pielke, Sr., and
! W.R. Cotton, 2002. The Incorporation of the Kain-Fritsch Cumulus
! Parameterization Scheme in RAMS.  In preparation.
!
! Kain, J.S., and J.M. Fritsch, 1993. Convective Parameterization
! for Mesoscale Models, The Kain-Fritsch Scheme.  The Representation
! of Convection in Numerical Models. Meteor. Monogr., No. 24,
! Amer. Meteor. Soc., 165-170.
!
!  
! Note: For a thorough description of variables used in the KF interface
! code, please see module kfdriver.f90
! ********************************************************************

SUBROUTINE KF_MAIN()

use mem_tend
use mem_cuparm
use mem_micro
use mem_basic
use mem_grid
use mem_tend
use mem_turb
use node_mod
use micphys

implicit none

integer :: j_lu,ng

ng=ngrid

if(nint(time) == 0) then
   call azero(mxp*myp*mzp,cuparm_g(ng)%thsrc(1,1,1))
   call azero(mxp*myp*mzp,cuparm_g(ng)%rtsrc(1,1,1))
   call azero(mxp*myp*mzp,cuparm_g(ng)%rcsrc(1,1,1))
   call azero(mxp*myp*mzp,cuparm_g(ng)%rrsrc(1,1,1))
   call azero(mxp*myp*mzp,cuparm_g(ng)%rssrc(1,1,1))
   call azero(mxp*myp*mzp,cuparm_g(ng)%rpsrc(1,1,1))
 
   call azero(mxp*myp,cuparm_g(ng)%conprr(1,1))
 
   call azero(mxp*myp*mzp,cuparm_g(ng)%w0avg(1,1,1))
   call azero(mxp*myp*mzp,cuparm_g(ng)%w0avglt(1,1,1))
   call azero(mxp*myp,cuparm_g(ng)%nca(1,1))
   call azero(mxp*myp,cuparm_g(ng)%convgo(1,1))
 
endif

! Check for convective activity at the specified frequency in
! RAMSIN.  A value of 600 seconds for CONFRQ is recommended

if (mod(time,confrq) == 0. .and. time > 0) then
   print*, 'check for convective activity in K-F at ', time, ' sec'

! Call the KF driver (see module kfdriver.f90)

   j_lu=1
   call kfdrive(mzp,mxp,myp,ia,iz,ja,jz,level,icloud,irain  &
       ,ipris,isnow,idiffk(ng)        &             
       ,basic_g(ng)%up(1,1,1),   basic_g(ng)%vp(1,1,1)  &
       ,basic_g(ng)%theta(1,1,1),basic_g(ng)%thp(1,1,1)  &
       ,basic_g(ng)%pp(1,1,1),   basic_g(ng)%pi0(1,1,1)  &
       ,tend%pt(1)  &
       ,basic_g(ng)%rv(1,1,1) ,   micro_g(ng)%rcp(1,1,1)  &
       ,micro_g(ng)%rrp(1,1,1),   micro_g(ng)%rsp(1,1,1)         &
       ,micro_g(ng)%rhp(1,1,1),   micro_g(ng)%rpp(1,1,1)  &
       ,micro_g(ng)%rap(1,1,1),   micro_g(ng)%rgp(1,1,1)  &
       ,grid_g(ng)%rtgt(1,1),  grid_g(ng)%topta(1,1),dzt,zt   &
       ,grid_g(ng)%dxt(1,1),   grid_g(ng)%dyt(1,1)  &
       ,cuparm_g(ng)%nca(1,1),  cuparm_g(ng)%convgo(1,1)  &
       ,cuparm_g(ng)%w0avg(1,1,1),cuparm_g(ng)%w0avglt(1,1,1) &
       ,turb_g(ng)%tkep(1,1,1),dtlt         &               
       ,cuparm_g(ng)%thsrc(1,1,1),cuparm_g(ng)%rtsrc(1,1,1)  &
       ,cuparm_g(ng)%rcsrc(1,1,1),cuparm_g(ng)%rrsrc(1,1,1) &
       ,cuparm_g(ng)%rssrc(1,1,1),cuparm_g(ng)%rpsrc(1,1,1)  &
       ,cuparm_g(ng)%conprr(1,1),j_lu  &
       ,grid_g(ngrid)%lpw(1,1),if_adap,zm)           

endif

! Update the vertical velocity parameters needed by the KF scheme

call update_w0avgkf(mzp,mxp,myp,ia,iz,ja,jz   &
       ,basic_g(ng)%pp(1,1,1),   basic_g(ng)%pi0(1,1,1)  &
       ,grid_g(ng)%f13t(1,1),grid_g(ng)%f23t(1,1)  &
       ,grid_g(ng)%rtgt(1,1),grid_g(ng)%topt(1,1),ztop,zt  &
       ,basic_g(ng)%up(1,1,1),basic_g(ng)%vp(1,1,1)  &
       ,basic_g(ng)%wp(1,1,1)  &
       ,cuparm_g(ng)%w0avg(1,1,1),cuparm_g(ng)%w0avglt(1,1,1) &
       ,dtlt,deltaxn(ng))
! Update convective tendencies

! Theta_il tendency

call accum_kf(mzp,mxp,myp,tend%tht(1)              &
            ,cuparm_g(ng)%thsrc(1,1,1),cuparm_g(ng)%nca(1,1))  

! Total water mixing ratio tendency

call accum_kf(mzp,mxp,myp,tend%rtt(1)              &
            ,cuparm_g(ng)%rtsrc(1,1,1),cuparm_g(ng)%nca(1,1))  

! Cloud water mixing ratio tendency

if(level >= 3 .and. level <= 5) then

   call accum_kf(mzp,mxp,myp,tend%rct(1)              &
            ,cuparm_g(ng)%rcsrc(1,1,1),cuparm_g(ng)%nca(1,1))  

! Rain water mixing ratio tendency

   call accum_kf(mzp,mxp,myp,tend%rrt(1)              &
            ,cuparm_g(ng)%rrsrc(1,1,1),cuparm_g(ng)%nca(1,1))  

! Snow mixing ratio tendency

   call accum_kf(mzp,mxp,myp,tend%rst(1)              &
            ,cuparm_g(ng)%rssrc(1,1,1),cuparm_g(ng)%nca(1,1))  

! Pristine ice ratio tendency

   call accum_kf(mzp,mxp,myp,tend%rpt(1)              &
            ,cuparm_g(ng)%rpsrc(1,1,1),cuparm_g(ng)%nca(1,1))  


endif

! Convective precipitation (mm or kg m^-2)

call update_kf(mxp*myp,cuparm_g(ng)%aconpr(1,1),      &
             cuparm_g(ng)%conprr(1,1),cuparm_g(ng)%nca(1,1),dtlt)           
                                                
! Updates the NCA array

call nca_update(mxp*myp,cuparm_g(ng)%nca(1,1))                
                                               
return
end

! -------------------------------------------------------------
!
!  Routine to compute the contravariant component of vertical
!  velocity. Two new vertical velocity components created here:
!
!     w0avg: running mean vertical velocity (for t=600s)
!     w0avglt: running mean horizontal components of contravariant vertical
!              velocity, scaled by elevation
!
!  The scheme was originally tuned for a July 2001 simulation of the 
!  North American Monsoon at 33 km grid spacing.  Users may wish to modify
!  the variable topocoef in module_cu_kfeta.f90 if precipitation amounts 
!  in areas of significant terrain are physically unreasonable.
!
!  RAMS USERS SHOULD INCLUDE THIS OR A SIMILAR ROUTINE WHEN INCORPORATING
!  ANY ADDITIONAL CONVECTION SCHEMES IN RAMS GIVEN THE PRESENT VERTICAL 
!  COORDINATE SYSTEM!!


      subroutine update_w0avgkf(mzp,mxp,myp,ia,iz,ja,jz,pi_p,pi_0,   &
                             ter_gradx,ter_grady,rtgt,topot,ztop,zt, &
                             up,vp,wp,w0avg,w0avglt,dt,dx)

      real zt(mzp)  
      real up(mzp,mxp,myp), vp(mzp,mxp,myp)
      real wp(mzp,mxp,myp), w0avglt(mzp,mxp,myp), w0avg(mzp,mxp,myp)
      real pi_p(mzp,mxp,myp),pi_0(mzp,mxp,myp)
      real topot(mxp,myp)
      real ter_gradx(mxp,myp), ter_grady(mxp,myp), rtgt(mxp,myp)
      real ucond, vcond
      real w0_parm, w0_parmlt

      ntst=nint(600./dt)         ! number of time steps in 600 sec


! The horizontal components of the contravariant vertical velocity.  
! Note that here (in contrast to same subroutine in Kuo) the terrain related 
! components must be cubed because the trigger function in KF is proportional 
! to w^(1/3)


      do k=2,mzp-1
       do j=ja,jz
        do i=ia,iz

         ucond=up(k,i,j)*((((1-(zt(k)-topot(i,j))/(ztop))*ter_gradx(i,j))  &
              /(rtgt(i,j))))**3
         vcond=vp(k,i,j)*((((1-(zt(k)-topot(i,j))/(ztop))*ter_grady(i,j))  &
              /(rtgt(i,j))))**3  
       
	 w0_parmlt=(ucond+vcond)
	 
         w0_parm=wp(k,i,j)
         

! Running mean vertical velocity

         w0avg(k,i,j)=(w0avg(k,i,j)*(real(ntst)-1)+w0_parm)/real(ntst)       


! Running mean horizontal components of contravariant velocity

         w0avglt(k,i,j)=(w0avglt(k,i,j)*(real(ntst)-1)+w0_parmlt)/real(ntst)   
                
        enddo
       enddo
      enddo

      return
      end



! ----------- subroutine for updating NCA array --------

      subroutine nca_update(npoints,nca)
      integer nca(npoints)

      do j=1,npoints
       nca(j)=nca(j)-1
      enddo

      return
      end

! -------- subroutine for adding source term to tendency -----


      subroutine accum_kf(mzp,mxp,myp,tht,thsrc,nca)
      real tht(mzp,mxp,myp), thsrc(mzp,mxp,myp)
      integer nca(mxp,myp)

      do j=1,myp
       do i=1,mxp
       if (nca(i,j).gt.0) then
        do k=2,mzp
         tht(k,i,j)=tht(k,i,j)+thsrc(k,i,j)
        enddo
       endif
       enddo
      enddo
      return
      end

! subroutine for updating accumulated precip -----------

      subroutine update_kf(npoints,acccon,conpcp,nca,dt)
      real acccon(npoints), conpcp(npoints)
      integer nca(npoints)

      do j=1,npoints
       if (nca(j).gt.0) then
        if(conpcp(j).gt.0) then
          acccon(j)=acccon(j)+conpcp(j)*dt
        endif
        if(acccon(j).lt.0) then
          acccon(j)=0
        endif
       endif
      enddo

      return
      end

