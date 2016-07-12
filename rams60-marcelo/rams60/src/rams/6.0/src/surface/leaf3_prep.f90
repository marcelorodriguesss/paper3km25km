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

subroutine sfc_fields(m1,m2,m3,ia,iz,ja,jz,jd  &
   ,theta,rv,up,vp,dn0,pp,pi0,rtgt,zt,ths2,rvs2,ups2,vps2,pis2,dens2,zts2)

use leaf_coms
use rconstants

implicit none

integer :: m1,m2,m3,ia,iz,ja,jz,jd
real, dimension(m1,m2,m3) :: theta,rv,up,vp,dn0,pp,pi0
real, dimension(m2,m3) :: rtgt,ths2,rvs2,ups2,vps2,pis2,dens2,zts2
real, dimension(m1) :: zt

integer :: i,j
real :: hcpi

! Compute surface atmospheric conditions

hcpi = .5 * cpi

do j = ja,jz
   do i = ia,iz
      ths2(i,j) = theta(2,i,j)
      rvs2(i,j) = rv(2,i,j)
      ups2(i,j) = (up(2,i-1,j) + up(2,i,j)) * .5
      vps2(i,j) = (vp(2,i,j-jd) + vp(2,i,j)) * .5
      zts2(i,j) = zt(2) * rtgt(i,j)
      pis2(i,j) = (pp(1,i,j) + pi0(1,i,j) + pp(2,i,j) + pi0(2,i,j)) * hcpi
      dens2(i,j) = (dn0(1,i,j) + dn0(2,i,j)) * .5
   enddo
enddo

return
end

!****************************************************************************

subroutine sfc_fields_adap(m1,m2,m3,ia,iz,ja,jz,jd,lpu,lpv,lpw  &
   ,topma,aru,arv,theta,rv,up,vp,dn0,pp,pi0,zt,zm,dzt       &
   ,ths2,rvs2,ups2,vps2,pis2,dens2,zts2)

use leaf_coms
use rconstants

implicit none

integer :: m1,m2,m3,ia,iz,ja,jz,jd
integer, dimension(m2,m3) :: lpu,lpv,lpw
real, dimension(m1,m2,m3) :: aru,arv,theta,rv,up,vp,dn0,pp,pi0
real, dimension(m2,m3) :: topma,ths2,rvs2,ups2,vps2,pis2,dens2,zts2
real, dimension(m1) :: zt,zm,dzt

integer :: i,j,k1,k2,k3
real :: topma_t,wtw,wtu1,wtu2,wtv1,wtv2

! Compute surface atmospheric conditions

do j = ja,jz
   do i = ia,iz
      k2 = lpw(i,j)
      k1 = k2 - 1
      k3 = k2 + 1
      
      topma_t = .25 * (topma(i,j) + topma(i-1,j)  &
              + topma(i,j-jd) + topma(i-1,j-jd))

! weights for lowest predicted points, relative to points above them

      wtw = (zm(k2) - topma_t) * dzt(k2)
      wtu1 = aru(lpu(i-1,j),i-1,j)   / aru(lpu(i-1,j)+1,i-1,j)
      wtu2 = aru(lpu(i,j),i,j)       / aru(lpu(i,j)+1,i,j)
      wtv1 = arv(lpv(i,j-jd),i,j-jd) / arv(lpv(i,j-jd)+1,i,j-jd)
      wtv2 = arv(lpv(i,j),i,j)       / arv(lpv(i,j)+1,i,j)

      ths2(i,j) =  wtw * theta(k2,i,j) + (1. - wtw)  * theta(k3,i,j)

      rvs2(i,j) =  wtw * rv(k2,i,j)    + (1. - wtw)  * rv(k3,i,j)

      ups2(i,j) = (wtu1        * up(lpu(i-1,j),i-1,j)    &
                +  (1. - wtu1) * up(lpu(i-1,j)+1,i-1,j)  &
                +  wtu2        * up(lpu(i,j),i,j)        &
                +  (1. - wtu2) * up(lpu(i,j)+1,i,j)) * .5

      vps2(i,j) = (wtv1        * vp(lpv(i,j-jd),i,j-jd)    &
                +  (1. - wtv1) * vp(lpv(i,j-jd)+1,i,j-jd)  &
                +  wtv2        * vp(lpv(i,j),i,j)          &
                +  (1. - wtv2) * vp(lpv(i,j)+1,i,j)) * .5

      zts2(i,j) = (wtw * (zt(k2) - zm(k1))  &
                + (1. - wtw) * (zt(k3) - zm(k2)))

      if (wtw >= .5) then
         pis2(i,j)  = ((wtw - .5) * (pp(k1,i,j) + pi0(k1,i,j))  &
                    + (1.5 - wtw) * (pp(k2,i,j) + pi0(k2,i,j))) * cpi
         dens2(i,j) = (wtw - .5)  * dn0(k1,i,j)  &
                    + (1.5 - wtw) * dn0(k2,i,j)                   
      else
         pis2(i,j)  = ((wtw + .5) * (pp(k2,i,j) + pi0(k2,i,j))  &
                    + (.5 - wtw) * (pp(k3,i,j) + pi0(k3,i,j))) * cpi
         dens2(i,j) = (wtw + .5) * dn0(k2,i,j)  &
                    + (.5 - wtw) * dn0(k3,i,j)         
      endif

   enddo
enddo

return
end

!****************************************************************************

subroutine sfc_pcp(nqparm,level,i,j,cuparm,micro)

use mem_basic
use mem_micro
use mem_cuparm
use leaf_coms

implicit none

integer :: nqparm,level,i,j
type (cuparm_vars)  cuparm
type (micro_vars)   micro

if (nqparm > 0) then

   pcpgl  = cuparm%conprr(i,j) * dtll
   qpcpgl = pcpgl  * 4186. * (ths * pis - 193.36)
   dpcpgl = pcpgl  * .001
   pcpgc  = dtlc_factor * pcpgl
   qpcpgc = dtlc_factor * qpcpgl
   
else

   pcpgl  = 0.
   qpcpgl = 0.
   dpcpgl = 0.
   pcpgc  = 0.
   qpcpgc = 0.
   
endif

if (level >= 3) then

   pcpgl  = pcpgl  + dtll_factor * micro%pcpg(i,j)
   qpcpgl = qpcpgl + dtll_factor * micro%qpcpg(i,j) 
   dpcpgl = dpcpgl + dtll_factor * micro%dpcpg(i,j)
   pcpgc  = pcpgc  + dtlc_factor * dtll_factor * micro%pcpg(i,j)
   qpcpgc = qpcpgc + dtlc_factor * dtll_factor * micro%qpcpg(i,j) 
   
endif
      
return
end

!****************************************************************************

subroutine vegndvi(ifm    &
   ,patch_area,leaf_class,veg_fracarea,veg_lai,veg_tai,veg_rough  &
   ,veg_height,veg_albedo,veg_ndvip,veg_ndvic,veg_ndvif)

use leaf_coms
use rconstants
use io_params
use mem_grid

implicit none

integer :: ifm
real :: patch_area,leaf_class,veg_fracarea,veg_lai,veg_tai,veg_rough  &
   ,veg_height,veg_albedo,veg_ndvip,veg_ndvic,veg_ndvif

integer :: nveg
integer, save :: nvcall = 0

real :: timefac_ndvi,sr,fpar,dead_lai,green_frac
real, save :: sr_min=1.081,fpar_min=.001,fpar_max=.950,fpcon=-.3338082
real, save :: ccc=-2.9657
real, save :: bz=.91,hz=.0075,extinc_veg=.5

real, dimension(nvtyp), save :: dfpardsr

!  Initialize dfpardsr array

if (nvcall == 0) then
   nvcall = 1
   do nveg = 1,nvtyp
      dfpardsr(nveg) = (fpar_max - fpar_min) / (sr_max(nveg) - sr_min)
   enddo
endif

!  Compute LAI, vegetation roughness, albedo, vegfrac from time-dependent NDVI

nveg = nint(leaf_class)

if (tai_max(nveg) < .1) then

   veg_lai = 0.
   veg_tai = 0.
   veg_rough = 0.
   veg_albedo = 0.
   veg_fracarea = 0.

else

   if (iupdndvi == 0) then
      timefac_ndvi = 0.
   else
      timefac_ndvi = (time - ndvitime1(ifm)) / (ndvitime2(ifm) - ndvitime1(ifm))
   endif

!  Time-interpolate ndvi to get current value veg_ndvic(i,j) for this patch

   veg_ndvic = veg_ndvip + (veg_ndvif - veg_ndvip) * timefac_ndvi

!  Limit ndvi to prevent values > .99 to prevent division by zero.

   if (veg_ndvic > .99) veg_ndvic = .99

! Compute "simple ratio" and limit between sr_min and sr_max(nveg).

   sr = (1. + veg_ndvic) / (1. - veg_ndvic)

   if (sr < sr_min) then
      sr = sr_min
   elseif (sr > sr_max(nveg)) then
      sr = sr_max(nveg)
   endif

! Compute fpar

   fpar = fpar_min + (sr - sr_min) * dfpardsr(nveg)

! Compute green leaf area index (veg_lai), dead leaf area index (dead_lai),
! total area index (tai), and green fraction   

   veg_lai = glai_max(nveg) * (veg_clump(nveg) * fpar / fpar_max  &
           + (1. - veg_clump(nveg)) * alog(1. - fpar) * fpcon)

   dead_lai = (glai_max(nveg) - veg_lai) * dead_frac(nveg)
   veg_tai = veg_lai + sai(nveg) + dead_lai
   green_frac = veg_lai / veg_tai
     
! Compute vegetation roughness height, albedo, and fractional area

   veg_rough = veg_height * (1. - bz * exp(-hz * veg_tai))
   veg_albedo = albv_green(nveg) * green_frac  &
              + albv_brown(nveg) * (1. - green_frac)
   veg_fracarea = veg_frac(nveg) * (1. - exp(-extinc_veg * veg_tai))

endif         
return
end

! Remaining issues:
! 
! 1. Relationship between clumping, V, vegfrac
! 2. Impact of V on radiation
! 3. Put in 900 second response time for rc?
! 4. Build lookup tables, especially for things with exponentials?

!****************************************************************************

subroutine sfcrad(mzg,mzs,ip  &
   ,soil_energy,soil_water,soil_text,sfcwater_energy,sfcwater_depth  &
   ,patch_area,can_temp,veg_temp,leaf_class,veg_height,veg_fracarea  &
   ,veg_albedo,sfcwater_nlev,rshort,rlong,albedt,rlongup,cosz)

use mem_leaf
use leaf_coms
use rconstants
use mem_scratch

implicit none

integer :: mzg,mzs,ip
real, dimension(mzg) :: soil_energy,soil_water,soil_text
real, dimension(mzs) :: sfcwater_energy,sfcwater_depth
real :: patch_area,can_temp,veg_temp,leaf_class,veg_height,veg_fracarea  &
       ,veg_albedo,sfcwater_nlev,rshort,rlong,albedt,rlongup,cosz

integer :: k,nsoil,nveg,ksn
real :: alb,vfc,fcpct,alg,rad,als,fractrans,absg,algs,emv,emgs,gslong,vlong,alv

! This routine is called by the radiation parameterization and by leaf.
! It computes net surface albedo plus radiative exchange between the
! atmosphere, vegetation, and the snow/ground given previously computed
! downward longwave and shortwave fluxes from the atmosphere.
! Also computed are functions of snowcover that are required for the above
! radiation calculations as well as other calculations in leaf.

! The shortwave parameterizations are only valid if the cosine of the zenith 
! angle is greater than .03 .  Water albedo from Atwater and Bell (1981)

! alg, als, and alv are the albedos of the ground, snow, and vegetation
! (als needs a better formula based on age of the surface snow).

! absg and vctr32 are the actual fractions of shortwave incident on snow 
! plus ground that get absorbed by the ground and each snow layer, 
! respectively.  They currently use the variable fractrans, which is the 
! fraction of light transmitted through each layer based on mass per square 
! meter.  algs is the resultant albedo from snow plus ground.

if (ip == 1) then
   if (cosz > .03) then
      alb = min(max(-.0139 + .0467 * tan(acos(cosz)),.03),.999)
      albedt = albedt + patch_area * alb
   endif
   call qtk(soil_energy(mzg),tempk(mzg),fracliq(mzg))
   rlongup = rlongup + patch_area * stefan * tempk(mzg) ** 4
         
elseif (isfcl == 0) then

   albedt = albedt + patch_area * albedo
   rlongup = rlongup + patch_area * stefan * can_temp ** 4

else

! Diagnose soil temperature and liquid fraction

   do k = 1,mzg
      nsoil = nint(soil_text(k))
      call qwtk(soil_energy(k),soil_water(k)*1.e3  &
         ,slcpd(nsoil),tempk(k),fracliq(k))
   enddo

! Diagnose snow temperature and the influence of snow covering veg.

   nveg = nint(leaf_class)
   ksn = nint(sfcwater_nlev)
   snowfac = 0.
   do k = 1,ksn
      snowfac = snowfac + sfcwater_depth(k)
      call qtk(sfcwater_energy(k),tempk(k+mzg),fracliq(k+mzg))
   enddo
   snowfac = min(.99, snowfac / max(.001,veg_height))

   vf = veg_fracarea * (1. - snowfac)
   vfc = 1. - vf

! Shortwave radiation calculations

   nsoil = nint(soil_text(mzg))
   fcpct = soil_water(mzg) / slmsts(nsoil)
   if (fcpct > .5) then
      alg = .14
   else
      alg = .31 - .34 * fcpct
   endif
   alv = veg_albedo

   rad = 1.
   if (ksn > 0) then

! als = .14 (the wet soil value) for all-liquid

      als = .5 - .36 * fracliq(ksn+mzg)
      rad = 1. - als
   endif
   do k = ksn,1,-1
      fractrans = exp(-20. * sfcwater_depth(k))
      vctr32(k) = rad * (1. - fractrans)
      rad = rad * fractrans
   enddo
   absg = (1. - alg) * rad
   algs = 1. - absg
   do k = ksn,1,-1
      algs = algs - vctr32(k)
      rshort_s(k) = rshort * vfc * vctr32(k)
   enddo
   rshort_g = rshort * vfc * absg
   rshort_v = rshort * vf * (1. - alv + vfc * algs)
!  rshort_a = rshort * (vf * alv + vfc * vfc * algs)

   alb = vf * alv + vfc * vfc * algs
   albedt = albedt + patch_area * alb

! Longwave radiation calculations

   emv = emisv(nveg)
   emgs = emisg(nsoil)
   if (ksn > 0) emgs = 1.0
   gslong = emgs * stefan * tempk(ksn+mzg) ** 4
   vlong = emv * stefan * veg_temp ** 4

   rlonga_v  = rlong * vf * (emv + vfc * (1. - emgs))
   rlonga_gs = rlong * vfc * emgs
   rlongv_gs = vlong * vf * emgs
   rlongv_a  = vlong * vf * (2. - emgs - vf + emgs * vf)
   rlonggs_v = gslong * vf * emv
   rlonggs_a = gslong * vfc
   rlonga_a  = rlong * (vf * (1. - emv) + vfc * vfc * (1. - emgs))

   rlongup = rlongup + patch_area * (rlongv_a + rlonggs_a + rlonga_a)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! In case rlong is not computed, zero out all longwave fluxes other
! than rlongup.  [On the first timestep, radiative fluxes may not
! be available until microphysics is called, and zeroing these fluxes
! avoids the imbalance of having upward longwave without downward longwave
! in LEAF-2.  Also, this allows LEAF-2 to run without radiation for all
! timesteps, if desired for testing.]

   if (rlong < .1) then
      rlonga_v  = 0.
      rlonga_gs = 0.
      rlongv_gs = 0.
      rlongv_a  = 0.
      rlonggs_v = 0.
      rlonggs_a = 0.
      rlonga_a  = 0.
   endif
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

endif
return
end
