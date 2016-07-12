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

subroutine leaf3(m1,m2,m3,mzg,mzs,np,ia,iz,ja,jz  &
   ,leaf,basic,turb,radiate,grid,cuparm,micro     &
   ,ths2,rvs2,pis2,dens2,ups2,vps2,zts2           )

use mem_all
use leaf_coms
use rconstants
implicit none

integer :: m1,m2,m3,mzg,mzs,np,ia,iz,ja,jz

type (leaf_vars)    leaf
type (basic_vars)   basic
type (turb_vars)    turb
type (radiate_vars) radiate
type (grid_vars)    grid
type (cuparm_vars)  cuparm
type (micro_vars)   micro

real, dimension(m2,m3) :: ths2,rvs2,pis2,dens2,ups2,vps2,zts2

integer :: i,j,ip,iter_leaf

real :: rslif

integer :: k2

real :: dvelu,dvelv,velnew,sflux_uv,cosine1,sine1

! Time interpolation factor for updating SST

if (iupdsst == 0) then
   timefac_sst = 0.
else
   timefac_sst = (time - ssttime1(ngrid)) / (ssttime2(ngrid) - ssttime1(ngrid))
endif

! Define leaf3 and canopy time-split timesteps here.  This ensures that leaf3
! will not use a timestep longer than about 40 seconds, and canopy will not
! use a timestep longer than about 15 seconds.  This allows values of 
! hcapcan = 2.e4, wcapcan = 2.e1, and hcapveg = 3.e4 as are now defined below.

niter_leaf  = max(1,nint(dtlt/40.+.4))
niter_can   = max(1,nint(dtll/15.+.4))

dtll_factor = 1. / float(niter_leaf)
dtll        = dtlt * dtll_factor
dtlc_factor = 1. / float(niter_can)
dtlc        = dtll * dtlc_factor

hcapcan = 2.0e4
wcapcan = 2.0e1
hcapveg = 3.e4

dtllohcc = dtll / hcapcan
dtllowcc = dtll / wcapcan
dtlcohcc = dtlc / hcapcan
dtlcowcc = dtlc / wcapcan
dtlcohcv = dtlc / hcapveg

z0fac_water = .016 / g
snowrough = .001

! Copy surface atmospheric variables into 2d arrays for input to leaf

if (if_adap == 1) then
   call sfc_fields_adap(m1,m2,m3,ia,iz,ja,jz,jdim             &
      ,grid%lpu   (1,1)   ,grid%lpv (1,1)   ,grid%lpw(1,1)    &
      ,grid%topma (1,1)   ,grid%aru (1,1,1) ,grid%arv(1,1,1)  &
      ,basic%theta(1,1,1) ,basic%rv (1,1,1) ,basic%up(1,1,1)  &
      ,basic%vp   (1,1,1) ,basic%dn0(1,1,1) ,basic%pp(1,1,1)  &
      ,basic%pi0  (1,1,1) ,zt,zm,dzt                          &
      ,ths2,rvs2,ups2,vps2,pis2,dens2,zts2                    )
else
   call sfc_fields(m1,m2,m3,ia,iz,ja,jz,jdim                  &
      ,basic%theta(1,1,1) ,basic%rv (1,1,1) ,basic%up(1,1,1)  &
      ,basic%vp   (1,1,1) ,basic%dn0(1,1,1) ,basic%pp(1,1,1)  &
      ,basic%pi0  (1,1,1) ,grid%rtgt(1,1)   ,zt               &
      ,ths2,rvs2,ups2,vps2,pis2,dens2,zts2                    )
endif

do j = ja,jz
   do i = ia,iz
   
! Copy surface variables to single-column values
         
      ups = ups2(i,j)
      vps = vps2(i,j)
      ths = ths2(i,j)
      rvs = rvs2(i,j)
      zts = zts2(i,j)
      pis = pis2(i,j)
      dens = dens2(i,j)

      prss = pis ** cpor * p00
      vels = sqrt(ups ** 2 + vps ** 2)
      gzotheta = g * zts / ths

! Update water internal energy from time-dependent SST
      leaf%soil_energy(mzg,i,j,1) = 334000.  &
         + 4186. * (leaf%seatp(i,j) + (leaf%seatf(i,j) - leaf%seatp(i,j))  &
         * timefac_sst - 273.15)

! Fill surface precipitation arrays for input to leaf

      call sfc_pcp(nnqparm(ngrid),level,i,j,cuparm,micro)

! Zero out albedo, upward surface longwave, and momentum, heat, and moisture
! flux arrays before summing over patches

      if (ilwrtyp > 0 .or. iswrtyp > 0) then
         radiate%albedt(i,j) = 0.
         radiate%rlongup(i,j) = 0.
      endif

      turb%sflux_u(i,j) = 0.
      turb%sflux_v(i,j) = 0.
      turb%sflux_w(i,j) = 0.
      turb%sflux_t(i,j) = 0.
      turb%sflux_r(i,j) = 0.

! For no soil model (patch 2) fill "canopy" temperature and moisture

      if (isfcl == 0) then
         leaf%can_temp  (i,j,2) = (ths - dthcon) * pis
         leaf%can_rvap  (i,j,2) = rvs - drtcon
         leaf%patch_area(i,j,1) = 1. - pctlcon
         leaf%patch_area(i,j,2) = pctlcon
      endif

! Begin patch loop
   
      do ip = 1,np

! Update time-dependent vegetation LAI and fractional coverage

         if (ip >= 2 .and. leaf%patch_area(i,j,ip) >= .009) then

            if (ip >= 2 .and. isfcl >= 1) call vegndvi(ngrid        &
               ,leaf%patch_area  (i,j,ip) ,leaf%leaf_class(i,j,ip)  &
               ,leaf%veg_fracarea(i,j,ip) ,leaf%veg_lai   (i,j,ip)  &
               ,leaf%veg_tai     (i,j,ip) ,leaf%veg_rough (i,j,ip)  &
               ,leaf%veg_height  (i,j,ip) ,leaf%veg_albedo(i,j,ip)  &
               ,leaf%veg_ndvip   (i,j,ip) ,leaf%veg_ndvic (i,j,ip)  &
               ,leaf%veg_ndvif   (i,j,ip)                           )

         endif

! Begin leaf small timestep here.

         do iter_leaf = 1,niter_leaf

! Calculate radiative fluxes between atmosphere, vegetation, and ground/snow
! based on already-computed downward shortwave and longwave fluxes from
! the atmosphere.  Fill tempk array with soil and snow temperature (C) and
! fracliq array with liquid fraction of water content in soil and snow.
! Other snowcover properties are also computed here.

            if (iswrtyp > 0 .or. ilwrtyp > 0) then

               if (ip == 1 .or. leaf%patch_area(i,j,ip) >= .009) then

                  call sfcrad(mzg,mzs,ip           &
         ,leaf%soil_energy    (1,i,j,ip) ,leaf%soil_water      (1,i,j,ip)  &
         ,leaf%soil_text      (1,i,j,ip) ,leaf%sfcwater_energy (1,i,j,ip)  &
         ,leaf%sfcwater_depth (1,i,j,ip) ,leaf%patch_area      (i,j,ip)    &
         ,leaf%can_temp       (i,j,ip)   ,leaf%veg_temp        (i,j,ip)    &
         ,leaf%leaf_class     (i,j,ip)   ,leaf%veg_height      (i,j,ip)    &
         ,leaf%veg_fracarea   (i,j,ip)   ,leaf%veg_albedo      (i,j,ip)    &   
         ,leaf%sfcwater_nlev  (i,j,ip)                                     &
         ,radiate%rshort      (i,j)      ,radiate%rlong        (i,j)       &
         ,radiate%albedt      (i,j)      ,radiate%rlongup      (i,j)       &
         ,radiate%cosz        (i,j)    )

               endif

            endif

! For water surface (patch 1), compute surface saturation mixing ratio
! and roughness length based on previous ustar.
! For soil patches, compute roughness length based on vegetation and snow.

            if (ip == 1) then

               leaf%ground_rsat(i,j,ip) = rslif(prss,tempk(mzg))   
               leaf%patch_rough(i,j,ip)  &
                  = max(z0fac_water * leaf%ustar(i,j,ip) ** 2,.0001)

            elseif (isfcl >= 1) then
            
               if (leaf%patch_area(i,j,ip) >= .009) then
                  leaf%patch_rough(i,j,ip)   &
                     = max(grid%topzo(i,j),leaf%soil_rough(i,j,ip)  &
                        ,leaf%veg_rough(i,j,ip)) * (1. - snowfac)   &
                        + snowrough * snowfac

               endif

            endif

! Calculate turbulent fluxes between atmosphere and canopy (or "canopy")

            if (leaf%patch_area(i,j,ip) < .009 .and. isfcl == 1 .and. ip >= 2) then
               thetacan = ths
            else
               thetacan = leaf%can_temp(i,j,ip) / pis
            endif
         
            call stars(leaf%ustar(i,j,ip),leaf%tstar(i,j,ip)  &
               ,leaf%rstar(i,j,ip),ths,rvs,thetacan,leaf%can_rvap(i,j,ip)  &
               ,zts,leaf%patch_rough(i,j,ip),leaf%patch_area(i,j,ip)  &
               ,vels,vels_pat,vonk,dtllohcc,dens,dtll)
!            call stars(time,ngrid,mynum,i0,j0,i,j,ip  &
!               ,leaf%ustar(i,j,ip),leaf%tstar(i,j,ip),leaf%rstar(i,j,ip)  &
!               ,ths,rvs,thetacan,leaf%can_rvap(i,j,ip)  &
!               ,zts,leaf%patch_rough(i,j,ip),leaf%patch_area(i,j,ip)  &
!               ,vels,vels_pat,vonk,dtllohcc,dens,dtll)

            call sfclmcv(leaf%ustar(i,j,ip),leaf%tstar(i,j,ip)     &
               ,leaf%rstar(i,j,ip),vels,vels_pat,ups,vps,gzotheta  &
               ,leaf%patch_area(i,j,ip),turb%sflux_u(i,j)  &
               ,turb%sflux_v(i,j),turb%sflux_w(i,j)  &
               ,turb%sflux_t(i,j),turb%sflux_r(i,j))

! For water patches, update temperature and moisture of "canopy" from
! divergence of fluxes with water surface and atmosphere.  rdi = ustar/5
! is the viscous sublayer conductivity from Garratt (1992).

           if (ip == 1) then

               rdi = .2 * leaf%ustar(i,j,1)
            
               leaf%can_temp(i,j,1) = leaf%can_temp(i,j,1)        &
                  + dtllohcc * dens * cp                          &
                  * ((tempk(mzg) - leaf%can_temp(i,j,1)) * rdi    &
                  + leaf%ustar(i,j,1) * leaf%tstar(i,j,1) * pis)

               leaf%can_rvap(i,j,1) = leaf%can_rvap(i,j,1) + dtllowcc * dens       &
                  * ((leaf%ground_rsat(i,j,1) - leaf%can_rvap(i,j,1)) * rdi  &
                  + leaf%ustar(i,j,1) * leaf%rstar(i,j,1))
               
            endif

! For soil model patches, update temperature and moisture of soil,
! vegetation, and canopy

            if (isfcl >= 1 .and. ip >= 2) then
   
               if (leaf%patch_area(i,j,ip) >= .009) then

                 call leaftw(mzg,mzs,np  &
            ,leaf%soil_water     (1,i,j,ip) ,leaf%soil_energy    (1,i,j,ip)  &
            ,leaf%soil_text      (1,i,j,ip) ,leaf%sfcwater_mass  (1,i,j,ip)  &
            ,leaf%sfcwater_energy(1,i,j,ip) ,leaf%sfcwater_depth (1,i,j,ip)  &
            ,leaf%ustar            (i,j,ip) ,leaf%tstar            (i,j,ip)  &
            ,leaf%rstar            (i,j,ip) ,leaf%veg_albedo       (i,j,ip)  &
            ,leaf%veg_fracarea     (i,j,ip) ,leaf%veg_lai          (i,j,ip)  &
            ,leaf%veg_tai          (i,j,ip)                                  &
            ,leaf%veg_rough        (i,j,ip) ,leaf%veg_height       (i,j,ip)  &
            ,leaf%patch_area       (i,j,ip) ,leaf%patch_rough      (i,j,ip)  &
            ,leaf%patch_wetind     (i,j,ip) ,leaf%leaf_class       (i,j,ip)  &
            ,leaf%soil_rough       (i,j,ip) ,leaf%sfcwater_nlev    (i,j,ip)  &
            ,leaf%stom_resist      (i,j,ip) ,leaf%ground_rsat      (i,j,ip)  &
            ,leaf%ground_rvap      (i,j,ip) ,leaf%veg_water        (i,j,ip)  &
            ,leaf%veg_temp         (i,j,ip) ,leaf%can_rvap         (i,j,ip)  &
            ,leaf%can_temp         (i,j,ip) ,leaf%veg_ndvip        (i,j,ip)  &
            ,leaf%veg_ndvic        (i,j,ip) ,leaf%veg_ndvif        (i,j,ip)  &
            ,radiate%rshort        (i,j)    ,radiate%cosz          (i,j) ,ip  &
            ,i,j )
            
              endif
            endif
         enddo
      enddo

   enddo
enddo

! Normalize accumulated fluxes and albedo seen by atmosphere over model
! timestep dtlt.

do j = ja,jz
   do i = ia,iz
      turb%sflux_u(i,j) = turb%sflux_u(i,j) * dtll_factor * dens2(i,j)
      turb%sflux_v(i,j) = turb%sflux_v(i,j) * dtll_factor * dens2(i,j)
      turb%sflux_w(i,j) = turb%sflux_w(i,j) * dtll_factor * dens2(i,j)
      turb%sflux_t(i,j) = turb%sflux_t(i,j) * dtll_factor * dens2(i,j)
      turb%sflux_r(i,j) = turb%sflux_r(i,j) * dtll_factor * dens2(i,j)
   enddo
enddo

if (ilwrtyp > 0 .or. iswrtyp > 0) then
   do j = ja,jz
      do i = ia,iz
         radiate%albedt (i,j) = radiate%albedt (i,j) * dtll_factor
         radiate%rlongup(i,j) = radiate%rlongup(i,j) * dtll_factor
      enddo
   enddo
endif


return
end


!*****************************************************************************

subroutine leaf_bcond(m2,m3,mzg,mzs,npat,jdim  &

   ,soil_water       ,sfcwater_mass  ,soil_energy     &
   ,sfcwater_energy  ,soil_text      ,sfcwater_depth  &
   ,ustar            ,tstar          ,rstar           &
   ,veg_albedo       ,veg_fracarea   ,veg_lai         &
   ,veg_tai                                           &
   ,veg_rough        ,veg_height     ,patch_area      &
   ,patch_rough      ,patch_wetind   ,leaf_class      &
   ,soil_rough       ,sfcwater_nlev  ,stom_resist     &
   ,ground_rsat      ,ground_rvap    ,veg_water       &
   ,veg_temp         ,can_rvap       ,can_temp        & 
   ,veg_ndvip        ,veg_ndvic      ,veg_ndvif)

implicit none

integer :: m2,m3,mzg,mzs,npat,jdim

real, dimension(mzg,m2,m3,npat) :: soil_water,soil_energy,soil_text
real, dimension(mzs,m2,m3,npat) :: sfcwater_mass,sfcwater_energy            &
                                  ,sfcwater_depth
real, dimension(m2,m3,npat)     :: ustar        ,tstar         ,rstar        &
                                  ,veg_albedo   ,veg_fracarea  ,veg_lai      &
                                  ,veg_tai                                   &
                                  ,veg_rough    ,veg_height    ,patch_area   &
                                  ,patch_rough  ,patch_wetind  ,leaf_class   &
                                  ,soil_rough   ,sfcwater_nlev ,stom_resist  &
                                  ,ground_rsat  ,ground_rvap   ,veg_water    &
                                  ,veg_temp     ,can_rvap      ,can_temp     &
                                  ,veg_ndvip    ,veg_ndvic     ,veg_ndvif

integer :: i,j,k,ipat

do ipat = 1,npat
   do j = 1,m3

      ustar          (1,j,ipat) = ustar            (2,j,ipat)
      tstar          (1,j,ipat) = tstar            (2,j,ipat)
      rstar          (1,j,ipat) = rstar            (2,j,ipat)
      veg_fracarea   (1,j,ipat) = veg_fracarea     (2,j,ipat)
      veg_lai        (1,j,ipat) = veg_lai          (2,j,ipat)
      veg_tai        (1,j,ipat) = veg_tai          (2,j,ipat)
      veg_rough      (1,j,ipat) = veg_rough        (2,j,ipat)
      veg_height     (1,j,ipat) = veg_height       (2,j,ipat)
      patch_area     (1,j,ipat) = patch_area       (2,j,ipat)
      patch_rough    (1,j,ipat) = patch_rough      (2,j,ipat)
      patch_wetind   (1,j,ipat) = patch_wetind     (2,j,ipat)
      leaf_class     (1,j,ipat) = leaf_class       (2,j,ipat)       
      soil_rough     (1,j,ipat) = soil_rough       (2,j,ipat)
      sfcwater_nlev  (1,j,ipat) = sfcwater_nlev    (2,j,ipat)
      stom_resist    (1,j,ipat) = stom_resist      (2,j,ipat)
      ground_rsat    (1,j,ipat) = ground_rsat      (2,j,ipat)
      ground_rvap    (1,j,ipat) = ground_rvap      (2,j,ipat)
      veg_water      (1,j,ipat) = veg_water        (2,j,ipat)
      veg_temp       (1,j,ipat) = veg_temp         (2,j,ipat)
      can_rvap       (1,j,ipat) = can_rvap         (2,j,ipat)
      can_temp       (1,j,ipat) = can_temp         (2,j,ipat)
      veg_ndvip      (1,j,ipat) = veg_ndvip        (2,j,ipat)
      veg_ndvic      (1,j,ipat) = veg_ndvic        (2,j,ipat)
      veg_ndvif      (1,j,ipat) = veg_ndvif        (2,j,ipat)
   
      ustar         (m2,j,ipat) = ustar         (m2-1,j,ipat)
      tstar         (m2,j,ipat) = tstar         (m2-1,j,ipat)
      rstar         (m2,j,ipat) = rstar         (m2-1,j,ipat)
      veg_albedo    (m2,j,ipat) = veg_albedo    (m2-1,j,ipat)
      veg_fracarea  (m2,j,ipat) = veg_fracarea  (m2-1,j,ipat)
      veg_lai       (m2,j,ipat) = veg_lai       (m2-1,j,ipat)
      veg_tai       (m2,j,ipat) = veg_tai       (m2-1,j,ipat)
      veg_rough     (m2,j,ipat) = veg_rough     (m2-1,j,ipat)
      veg_height    (m2,j,ipat) = veg_height    (m2-1,j,ipat)
      patch_area    (m2,j,ipat) = patch_area    (m2-1,j,ipat)
      patch_rough   (m2,j,ipat) = patch_rough   (m2-1,j,ipat)
      patch_wetind  (m2,j,ipat) = patch_wetind  (m2-1,j,ipat)
      leaf_class    (m2,j,ipat) = leaf_class    (m2-1,j,ipat)       
      soil_rough    (m2,j,ipat) = soil_rough    (m2-1,j,ipat)
      sfcwater_nlev (m2,j,ipat) = sfcwater_nlev (m2-1,j,ipat)
      stom_resist   (m2,j,ipat) = stom_resist   (m2-1,j,ipat)
      ground_rsat   (m2,j,ipat) = ground_rsat   (m2-1,j,ipat)
      ground_rvap   (m2,j,ipat) = ground_rvap   (m2-1,j,ipat)
      veg_water     (m2,j,ipat) = veg_water     (m2-1,j,ipat)
      veg_temp      (m2,j,ipat) = veg_temp      (m2-1,j,ipat)
      can_rvap      (m2,j,ipat) = can_rvap      (m2-1,j,ipat)
      can_temp      (m2,j,ipat) = can_temp      (m2-1,j,ipat)
      veg_ndvip     (m2,j,ipat) = veg_ndvip     (m2-1,j,ipat)
      veg_ndvic     (m2,j,ipat) = veg_ndvic     (m2-1,j,ipat)
      veg_ndvif     (m2,j,ipat) = veg_ndvif     (m2-1,j,ipat)
   
      do k = 1,mzg
         soil_water       (k,1,j,ipat) = soil_water         (k,2,j,ipat)
         soil_energy      (k,1,j,ipat) = soil_energy        (k,2,j,ipat)
         soil_text        (k,1,j,ipat) = soil_text          (k,2,j,ipat)

         soil_water      (k,m2,j,ipat) = soil_water      (k,m2-1,j,ipat)
         soil_energy     (k,m2,j,ipat) = soil_energy     (k,m2-1,j,ipat)
         soil_text       (k,m2,j,ipat) = soil_text       (k,m2-1,j,ipat)
      enddo

      do k = 1,mzs
         sfcwater_mass    (k,1,j,ipat) = sfcwater_mass      (k,2,j,ipat)
         sfcwater_energy  (k,1,j,ipat) = sfcwater_energy    (k,2,j,ipat)
         sfcwater_depth   (k,1,j,ipat) = sfcwater_depth     (k,2,j,ipat)

         sfcwater_mass   (k,m2,j,ipat) = sfcwater_mass   (k,m2-1,j,ipat)
         sfcwater_energy (k,m2,j,ipat) = sfcwater_energy (k,m2-1,j,ipat)
         sfcwater_depth  (k,m2,j,ipat) = sfcwater_depth  (k,m2-1,j,ipat)
      enddo

   enddo   

   if (jdim == 1) then

      do i = 1,m2
         ustar          (i,1,ipat) = ustar            (i,2,ipat)
         tstar          (i,1,ipat) = tstar            (i,2,ipat)
         rstar          (i,1,ipat) = rstar            (i,2,ipat)
         veg_albedo     (i,1,ipat) = veg_albedo       (i,2,ipat)
         veg_fracarea   (i,1,ipat) = veg_fracarea     (i,2,ipat)
         veg_lai        (i,1,ipat) = veg_lai          (i,2,ipat)
         veg_tai        (i,1,ipat) = veg_tai          (i,2,ipat)
         veg_rough      (i,1,ipat) = veg_rough        (i,2,ipat)
         veg_height     (i,1,ipat) = veg_height       (i,2,ipat)
         patch_area     (i,1,ipat) = patch_area       (i,2,ipat)
         patch_rough    (i,1,ipat) = patch_rough      (i,2,ipat)
         patch_wetind   (i,1,ipat) = patch_wetind     (i,2,ipat)
         leaf_class     (i,1,ipat) = leaf_class       (i,2,ipat)       
         soil_rough     (i,1,ipat) = soil_rough       (i,2,ipat)
         sfcwater_nlev  (i,1,ipat) = sfcwater_nlev    (i,2,ipat)
         stom_resist    (i,1,ipat) = stom_resist      (i,2,ipat)
         ground_rsat    (i,1,ipat) = ground_rsat      (i,2,ipat)
         ground_rvap    (i,1,ipat) = ground_rvap      (i,2,ipat)
         veg_water      (i,1,ipat) = veg_water        (i,2,ipat)
         veg_temp       (i,1,ipat) = veg_temp         (i,2,ipat)
         can_rvap       (i,1,ipat) = can_rvap         (i,2,ipat)
         can_temp       (i,1,ipat) = can_temp         (i,2,ipat)
         veg_ndvip      (i,1,ipat) = veg_ndvip        (i,2,ipat)
         veg_ndvic      (i,1,ipat) = veg_ndvic        (i,2,ipat)
         veg_ndvif      (i,1,ipat) = veg_ndvif        (i,2,ipat)
   
         ustar         (i,m3,ipat) = ustar         (i,m3-1,ipat)
         tstar         (i,m3,ipat) = tstar         (i,m3-1,ipat)
         rstar         (i,m3,ipat) = rstar         (i,m3-1,ipat)
         veg_albedo    (i,m3,ipat) = veg_albedo    (i,m3-1,ipat)
         veg_fracarea  (i,m3,ipat) = veg_fracarea  (i,m3-1,ipat)
         veg_lai       (i,m3,ipat) = veg_lai       (i,m3-1,ipat)
         veg_tai       (i,m3,ipat) = veg_tai       (i,m3-1,ipat)
         veg_rough     (i,m3,ipat) = veg_rough     (i,m3-1,ipat)
         veg_height    (i,m3,ipat) = veg_height    (i,m3-1,ipat)
         patch_area    (i,m3,ipat) = patch_area    (i,m3-1,ipat)
         patch_rough   (i,m3,ipat) = patch_rough   (i,m3-1,ipat)
         patch_wetind  (i,m3,ipat) = patch_wetind  (i,m3-1,ipat)
         leaf_class    (i,m3,ipat) = leaf_class    (i,m3-1,ipat)       
         soil_rough    (i,m3,ipat) = soil_rough    (i,m3-1,ipat)
         sfcwater_nlev (i,m3,ipat) = sfcwater_nlev (i,m3-1,ipat)
         stom_resist   (i,m3,ipat) = stom_resist   (i,m3-1,ipat)
         ground_rsat   (i,m3,ipat) = ground_rsat   (i,m3-1,ipat)
         ground_rvap   (i,m3,ipat) = ground_rvap   (i,m3-1,ipat)
         veg_water     (i,m3,ipat) = veg_water     (i,m3-1,ipat)
         veg_temp      (i,m3,ipat) = veg_temp      (i,m3-1,ipat)
         can_rvap      (i,m3,ipat) = can_rvap      (i,m3-1,ipat)
         can_temp      (i,m3,ipat) = can_temp      (i,m3-1,ipat)
         veg_ndvip     (i,m3,ipat) = veg_ndvip     (i,m3-1,ipat)
         veg_ndvic     (i,m3,ipat) = veg_ndvic     (i,m3-1,ipat)
         veg_ndvif     (i,m3,ipat) = veg_ndvif     (i,m3-1,ipat)
   
         do k = 1,mzg
            soil_water       (k,i,1,ipat) = soil_water         (k,i,2,ipat)
            soil_energy      (k,i,1,ipat) = soil_energy        (k,i,2,ipat)
            soil_text        (k,i,1,ipat) = soil_text          (k,i,2,ipat)

            soil_water      (k,i,m3,ipat) = soil_water      (k,i,m3-1,ipat)
            soil_energy     (k,i,m3,ipat) = soil_energy     (k,i,m3-1,ipat)
            soil_text       (k,i,m3,ipat) = soil_text       (k,i,m3-1,ipat)
         enddo

         do k = 1,mzs
            sfcwater_mass    (k,i,1,ipat) = sfcwater_mass      (k,i,2,ipat)
            sfcwater_energy  (k,i,1,ipat) = sfcwater_energy    (k,i,2,ipat)
            sfcwater_depth   (k,i,1,ipat) = sfcwater_depth     (k,i,2,ipat)

            sfcwater_mass   (k,i,m3,ipat) = sfcwater_mass   (k,i,m3-1,ipat)
            sfcwater_energy (k,i,m3,ipat) = sfcwater_energy (k,i,m3-1,ipat)
            sfcwater_depth  (k,i,m3,ipat) = sfcwater_depth  (k,i,m3-1,ipat)
         enddo

      enddo   

   endif

enddo
return
end


