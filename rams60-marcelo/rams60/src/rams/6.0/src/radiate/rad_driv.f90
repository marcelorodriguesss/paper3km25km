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

subroutine radiate(mzp,mxp,myp,ia,iz,ja,jz,mynum)

use mem_tend
use mem_grid
use mem_leaf
use mem_radiate
use mem_basic
use mem_scratch
use mem_micro
use rconstants
use rrad3
use micphys
use ref_sounding

implicit none

integer :: mzp,mxp,myp,ia,iz,ja,jz,koff,mynum

real, pointer :: rc_ptr

real :: solc
real, save :: prsnz,prsnzp

integer, save :: ncall=0

if (ilwrtyp + iswrtyp .eq. 0) return

call tend_accum(mzp,mxp,myp,ia,iz,ja,jz,tend%tht(1)  &
   ,radiate_g(ngrid)%fthrd(1,1,1))

if (mod(time + .001,radfrq) .lt. dtlt .or. time .lt. 0.001) then

   print 90,time,time/3600.+(itime1/100+mod(itime1,100)/60.)
90      format('   radiation tendencies updated    time =',f10.1,  &
        '  UTC TIME (HRS) =',F6.1)


! Compute solar zenith angle, multiplier for solar constant, sfc albedo,
! and surface upward longwave radiation.

!print*, 'at a1 ',ngrid

   call radprep(mxp,myp,nzg,nzs,npatch,ia,iz,ja,jz,jday   &

      ,leaf_g(ngrid)%soil_water      (1,1,1,1)  &
      ,leaf_g(ngrid)%soil_energy     (1,1,1,1)  &
      ,leaf_g(ngrid)%soil_text       (1,1,1,1)  &
      ,leaf_g(ngrid)%sfcwater_energy (1,1,1,1)  &
      ,leaf_g(ngrid)%sfcwater_depth  (1,1,1,1)  &
      ,leaf_g(ngrid)%leaf_class      (1,1,1)    &
      ,leaf_g(ngrid)%veg_fracarea    (1,1,1)    &
      ,leaf_g(ngrid)%veg_height      (1,1,1)    &
      ,leaf_g(ngrid)%veg_albedo      (1,1,1)    &
      ,leaf_g(ngrid)%patch_area      (1,1,1)    &
      ,leaf_g(ngrid)%sfcwater_nlev   (1,1,1)    &
      ,leaf_g(ngrid)%veg_temp        (1,1,1)    &
      ,leaf_g(ngrid)%can_temp        (1,1,1)    &

      ,solfac  &
      ,grid_g(ngrid)%glat       (1,1)  &
      ,grid_g(ngrid)%glon       (1,1)  &
      ,radiate_g(ngrid)%rshort  (1,1)  &
      ,radiate_g(ngrid)%rlong   (1,1)  &
      ,radiate_g(ngrid)%rlongup (1,1)  &
      ,radiate_g(ngrid)%albedt  (1,1)  &
      ,radiate_g(ngrid)%cosz    (1,1)  )


!print*, 'at a2 ',ngrid

   call azero(mzp*mxp*myp,radiate_g(ngrid)%fthrd(1,1,1))


!print*, 'at a3 ',ngrid

   if (ilwrtyp .le. 2 .or. iswrtyp .le. 2) then
   
! If using Mahrer-Pielke and/or Chen-Cotton radiation, call radcomp.

      call radcomp(mzp,mxp,myp,ia,iz,ja,jz,solfac  &
         ,basic_g(ngrid)%theta     (1,1,1)  &
         ,basic_g(ngrid)%pi0       (1,1,1)  &
         ,basic_g(ngrid)%pp        (1,1,1)  &
         ,basic_g(ngrid)%rv        (1,1,1)  &
         ,basic_g(ngrid)%dn0       (1,1,1)  &
         ,basic_g(ngrid)%rtp       (1,1,1)  &
         ,radiate_g(ngrid)%fthrd   (1,1,1)  &
         ,grid_g(ngrid)%rtgt       (1,1)    &
         ,grid_g(ngrid)%f13t       (1,1)    &
         ,grid_g(ngrid)%f23t       (1,1)    &
         ,grid_g(ngrid)%glat       (1,1)    &
         ,grid_g(ngrid)%glon       (1,1)    &
         ,radiate_g(ngrid)%rshort  (1,1)    &
         ,radiate_g(ngrid)%rlong   (1,1)    &
         ,radiate_g(ngrid)%albedt  (1,1)    &
         ,radiate_g(ngrid)%cosz    (1,1)    &
         ,radiate_g(ngrid)%rlongup (1,1)    &
         ,mynum)

   endif

   if (iswrtyp .eq. 3 .or. ilwrtyp .eq. 3) then

! Using Harrington radiation


!print*, 'at a4 ',ngrid

      if (ncall .eq. 0) then

! If first call for this node, initialize several quantities & Mclatchy
! sounding data.


!print*, 'at a5 ',ngrid

         call radinit(ng,nb,nsolb,npsb,nuum,prf,alpha,trf,beta  &
            ,xp,wght,wlenlo,wlenhi,solar0,ralcs,a0,a1,a2,a3,solc  &
            ,exptabc,ulim,npartob,npartg,ncog,ncb  &
            ,ocoef,bcoef,gcoef,gnu)

         prsnz  = (pi01dn(nnzp(1)-1,1) / cp) ** cpor * p00
         prsnzp = (pi01dn(nnzp(1)  ,1) / cp) ** cpor * p00


!print*, 'at a6 ',ngrid

        call mclatchy(1,mzp,if_adap,koff  &
            ,prsnz,prsnzp  &
            ,grid_g(ngrid)%glat       (1,1)  &
            ,grid_g(ngrid)%rtgt       (1,1)  &
            ,grid_g(ngrid)%topt       (1,1)  &
            ,radiate_g(ngrid)%rlongup (1,1)  &
            ,zm,zt,vctr1,vctr2,vctr3,vctr4,vctr5,vctr6,vctr7  &
            ,vctr8,vctr9,vctr10,vctr11,vctr12)

         ncall = ncall + 1
      endif

! For any call, interpolate the mclatchy sounding data by latitude and
! season.


!print*, 'at a7 ',ngrid

     call mclatchy(2,mzp,if_adap,koff  &
         ,prsnz,prsnzp  &
         ,grid_g(ngrid)%glat       (1,1)  &
         ,grid_g(ngrid)%rtgt       (1,1)  &
         ,grid_g(ngrid)%topt       (1,1)  &
         ,radiate_g(ngrid)%rlongup (1,1)  &
         ,zm,zt,vctr1,vctr2,vctr3,vctr4,vctr5,vctr6,vctr7  &
         ,vctr8,vctr9,vctr10,vctr11,vctr12)

! If using Harrington radiation with moisture complexity LEVEL < 3,
! call radcomp3 which is a substitute driving structure to the bulk
! microphysics.

      if (level <= 2) then
	 if (level == 2) then
	    rc_ptr => micro_g(ngrid)%rcp(1,1,1)
	 else
	    call azero(mzp*mxp*myp,scratch%vt3dp(1))
	    rc_ptr => scratch%vt3dp(1)
	 endif


!print*, 'at a8 ',ngrid

         call radcomp3(mzp,mxp,myp,ia,iz,ja,jz  &
            ,grid_g(ngrid)%lpw        (1,1)    &
            ,grid_g(ngrid)%glat       (1,1)    &
            ,grid_g(ngrid)%rtgt       (1,1)    &
            ,grid_g(ngrid)%topt       (1,1)    &
            ,radiate_g(ngrid)%albedt  (1,1)    &
            ,radiate_g(ngrid)%cosz    (1,1)    &
            ,radiate_g(ngrid)%rlongup (1,1)    &
            ,radiate_g(ngrid)%rshort  (1,1)    &
            ,radiate_g(ngrid)%rlong   (1,1)    &
            ,basic_g(ngrid)%rv        (1,1,1)  &
            ,basic_g(ngrid)%dn0       (1,1,1)  &
            ,radiate_g(ngrid)%fthrd   (1,1,1)  &
            ,basic_g(ngrid)%pi0       (1,1,1)  &
            ,basic_g(ngrid)%pp        (1,1,1)  &
            ,basic_g(ngrid)%theta     (1,1,1)  &
            ,rc_ptr  )
      endif

   endif
endif

!print*, 'at a9 ',ngrid


return
end

!*****************************************************************************

subroutine tend_accum(m1,m2,m3,ia,iz,ja,jz,at,at2)

implicit none
integer :: m1,m2,m3,ia,iz,ja,jz,i,j,k
real, dimension(m1,m2,m3) :: at,at2

do j = ja,jz
   do i = ia,iz
      do k = 1,m1
         at(k,i,j) = at(k,i,j) + at2(k,i,j)
      enddo
   enddo
enddo

return
end

!*****************************************************************************

subroutine radprep(m2,m3,mzg,mzs,np,ia,iz,ja,jz,jday   &

   ,soil_water       ,soil_energy      ,soil_text      &
   ,sfcwater_energy  ,sfcwater_depth   ,leaf_class     &
   ,veg_fracarea     ,veg_height       ,veg_albedo     &
   ,patch_area     &
   ,sfcwater_nlev    ,veg_temp         ,can_temp       & 

   ,solfac,glat,glon,rshort,rlong,rlongup,albedt,cosz  )

use rconstants

implicit none

integer :: m2,m3,mzg,mzs,np,ia,iz,ja,jz,jday
real :: solfac
real, dimension(m2,m3) :: glat,glon,rshort,rlong,rlongup,albedt,cosz
real, dimension(mzg,m2,m3,np) :: soil_water,soil_energy,soil_text
real, dimension(mzs,m2,m3,np) :: sfcwater_energy,sfcwater_depth
real, dimension(m2,m3,np) :: leaf_class,veg_fracarea,veg_height,veg_albedo  &
   ,patch_area,sfcwater_nlev,veg_temp,can_temp

integer :: ip,i,j
real :: c1,c2

! Compute solar zenith angle [cosz(i,j)] & solar constant factr [solfac].

call zen(m2,m3,ia,iz,ja,jz,jday,glat,glon,cosz,solfac)

! Compute patch-averaged surface albedo [albedt(i,j)] and up longwave
! radiative flux [rlongup(i,j)].

call azero2(m2*m3,albedt,rlongup)
do ip = 1,np
   do j = 1,jz
      do i = 1,iz

!print*, 'at b1 ',i,j,ip,ngrid

   call sfcrad(mzg,mzs,ip               &
      ,soil_energy    (1,i,j,ip) ,soil_water      (1,i,j,ip)  &
      ,soil_text      (1,i,j,ip) ,sfcwater_energy (1,i,j,ip)  &
      ,sfcwater_depth (1,i,j,ip) ,patch_area      (i,j,ip)    &
      ,can_temp       (i,j,ip)   ,veg_temp        (i,j,ip)    &
      ,leaf_class     (i,j,ip)   ,veg_height      (i,j,ip)    &
      ,veg_fracarea   (i,j,ip)   ,veg_albedo      (i,j,ip)    &
      ,sfcwater_nlev  (i,j,ip)                                &
      ,rshort         (i,j)      ,rlong           (i,j)       &
      ,albedt         (i,j)      ,rlongup         (i,j)       &
      ,cosz           (i,j)                                   )

      enddo
   enddo
enddo
return
end

!******************************************************************************

subroutine radcomp(m1,m2,m3,ia,iz,ja,jz,solfac  &
   ,theta,pi0,pp,rv,dn0,rtp,fthrd  &
   ,rtgt,f13t,f23t,glat,glon,rshort,rlong,albedt,cosz,rlongup  &
   ,mynum)

use mem_grid
use mem_scratch
use mem_radiate
use rconstants

implicit none

integer :: m1,m2,m3,ia,iz,ja,jz,mynum

real :: solfac,tdec,sdec,cdec,declin,dzsdx,dzsdy,dlon,a1,a2,dayhr,gglon  &
   ,dayhrr,hrangl,sinz,sazmut,slazim,slangl,cosi
real, dimension(nzpmax) :: rvr,rtr,dn0r,pird,prd,fthrl,dzmr,dztr,fthrs
real, dimension(nzpmax+1) :: temprd
real, dimension(m1,m2,m3) :: theta,pi0,pp,rv,dn0,rtp,fthrd
real, dimension(m2,m3) :: rtgt,f13t,f23t,glat,glon,rshort,rlong,cosz  &
   ,albedt,rlongup

integer :: i,j,k,k1,k2,nlev

common /radcom/ tdec,sdec,cdec,declin,rvr,rtr,dn0r,pird,prd,temprd  &
               ,fthrl,dzmr,dztr,fthrs

do j = ja,jz
   do i = ia,iz
   
      k2=grid_g(ngrid)%lpw(i,j)
      k1=k2-1
      nlev=m1-k1+1

      do k = k1,m1
         pird(k-k1+1) = (pp(k,i,j) + pi0(k,i,j)) / cp
         temprd(k-k1+1) = theta(k,i,j) * pird(k-k1+1)
         rvr(k-k1+1) = max(0.,rv(k,i,j))
         rtr(k-k1+1) = max(rvr(k-k1+1),rtp(k,i,j))
! Convert the next 7 variables to cgs for now.
         prd(k-k1+1) = pird(k-k1+1) ** cpor * p00 * 10.
         dn0r(k-k1+1) = dn0(k,i,j) * 1.e-3
         dzmr(k-k1+1) = dzm(k) / rtgt(i,j) * 1.e-2
         dztr(k-k1+1) = dzt(k) / rtgt(i,j) * 1.e-2

         if(rvr(k-k1+1)    <   0. .or.  &
            rtr(k-k1+1)    <   0. .or.  &
            prd(k-k1+1)    <   0. .or.  &
            dn0r(k-k1+1)   <   0. .or.  &
            temprd(k-k1+1) < 173.) then
            print*, 'Temperature too low or negative value of'
            print*, 'density, vapor, pressure, or ozone'
            print*, 'when calling Chen-Cotton/Mahrer-Pielke radiation'
            print*, 'at (k,i,j),ngrid = ',k,i,j,ngrid
            print*, 'rvr(k)  rtr(k)  prd(k)  dn0r(k)  temprd(k)'
            print*, rvr(k-k1+1), rtr(k-k1+1), prd(k-k1+1), dn0r(k-k1+1), temprd(k-k1+1)
            print*, 'stopping model'
            stop 'radiation call'
         endif

      enddo
      temprd(1) = (rlongup(i,j) / stefan) ** 0.25
      temprd(m1+1-k1+1) = temprd(m1-k1+1)

! Call the longwave parameterizations.

      if (ilwrtyp .eq. 2) then
         call lwradp(nlev,temprd,rvr,dn0r,dztr,pird,vctr1,fthrl,rlong(i,j))
      elseif (ilwrtyp .eq. 1) then
         call lwradc(nlev+1,rvr,rtr,dn0r,temprd,prd,dztr,fthrl,rlong(i,j)  &
            ,vctr1,vctr2,vctr3,vctr4,vctr5,vctr6,vctr7,vctr8,vctr9,vctr10  &
            ,vctr11,vctr12,vctr13,vctr14,vctr15)
      endif

! The shortwave parameterizations are only valid if the cosine
!    of the zenith angle is greater than .03 .

      if (cosz(i,j) .gt. .03) then

         if (iswrtyp .eq. 2) then
            call shradp(nlev,rvr,dn0r,dzmr,vctr1,pird,cosz(i,j)  &
               ,albedt(i,j),solar*1e3*solfac,fthrs,rshort(i,j))
         elseif (iswrtyp .eq. 1) then
            call shradc(nlev+1,rvr,rtr,dn0r,dztr,prd,vctr1  &
              ,albedt(i,j),solar*1.e3*solfac,cosz(i,j),fthrs,rshort(i,j))
         endif

! Modify the downward surface shortwave flux by considering
!    the slope of the topography.

         if (itopo .eq. 1) then
            dzsdx = f13t(i,j) * rtgt(i,j)
            dzsdy = f23t(i,j) * rtgt(i,j)

! The y- and x-directions must be true north and east for
! this correction. the following rotates the model y/x
! to the true north/east.

! The following rotation seems to be incorrect, so call this instead:
!      subroutine uvtoueve(u,v,ue,ve,qlat,qlon,platn(ngrid),plonn(ngrid))

            dlon = (plonn(ngrid) - glon(i,j)) * pi180
            a1 = dzsdx*cos(dlon) + dzsdy * sin(dlon)
            a2 = -dzsdx*sin(dlon) + dzsdy * cos(dlon)
            dzsdx = a1
            dzsdy = a2

            dayhr = time / 3600. + float(itime1/100)  &
               + float(mod(itime1,100)) / 60.
            gglon = glon(i,j)
            if (lonrad .eq. 0) gglon = centlon(1)
            dayhrr = mod(dayhr+gglon/15.+24.,24.)
            hrangl = 15. * (dayhrr - 12.) * pi180
            sinz = sqrt(1. - cosz(i,j) ** 2)
            sazmut = asin(max(-1.,min(1.,cdec*sin(hrangl)/sinz)))
            if (abs(dzsdx) .lt. 1e-15) dzsdx = 1.e-15
            if (abs(dzsdy) .lt. 1e-15) dzsdy = 1.e-15
            slazim = 1.571 - atan2(dzsdy,dzsdx)
            slangl = atan(sqrt(dzsdx*dzsdx+dzsdy*dzsdy))
            cosi = cos(slangl) * cosz(i,j) + sin(slangl) * sinz  &
               * cos(sazmut-slazim)
            if (cosi > 0.) then
               rshort(i,j) = rshort(i,j) * cosi / cosz(i,j)
            else
               rshort(i,j) =  0.
            endif
         endif

      else
         do k = 1,nzp
            fthrs(k) = 0.
         enddo
         rshort(i,j) = 0.
      endif
      
      ! Add fluxes, adjusting for adap if necessary.
      do k = k2,m1-1
         fthrd(k,i,j) = fthrl(k-k1+1) + fthrs(k-k1+1)
      enddo

! Convert the downward flux at the ground to SI.

      rshort(i,j) = rshort(i,j) * 1.e-3 / (1. - albedt(i,j))
      rlong(i,j) = rlong(i,j) * 1.e-3
      fthrd(k1,i,j) = fthrd(k2,i,j)

   enddo
enddo
return
end

!******************************************************************************

subroutine radcomp3(m1,m2,m3,ia,iz,ja,jz,lpw  &
   ,glat,rtgt,topt,albedt,cosz,rlongup,rshort,rlong  &
   ,rv,dn0,fthrd,pi0,pp,theta,rcp)

use mem_grid
use mem_radiate
use rconstants
use rrad3
use micphys

implicit none

integer :: m1,m2,m3,ia,iz,ja,jz,mcat,i,j,k
integer, dimension(m2,m3) :: lpw

real :: cfmasi,cparmi,glg,glgm,gammln,picpi
real, dimension(m2,m3) :: glat,rtgt,topt,cosz,albedt,rlongup,rshort,rlong
real, dimension(m1,m2,m3) :: dn0,rv,fthrd,pi0,pp,theta,rcp

if (level <= 1) then
   mcat = 0
else
   mcat = 1
   pwmas(1) = 3.
   pwmasi(1) = 1. / pwmas(1)
   cfmasi = 1. / 524.
   cparmi = 1. / cparm
   emb0(1) = 5.24e-16
   emb1(1) = 3.35e-11
   glg = gammln(gnu(1))
   glgm = gammln(gnu(1) + pwmas(1))
   dnfac(1) = (cfmasi * exp(glg - glgm)) ** pwmasi(1)
   do k = 2,m1-1
      jhcat(k,1) = 1
   enddo
endif

do j = ja,jz
   do i = ia,iz

      do k = 2,m1-1
         picpi = (pi0(k,i,j) + pp(k,i,j)) * cpi
         press(k) = p00 * picpi ** cpor
         tair(k) = theta(k,i,j) * picpi
      enddo

      if (level >= 2) then
         do k = 2,m1-1
            emb(k,1) = max(emb0(1),min(emb1(1),rcp(k,i,j) * cparmi))
            cx(k,1) = rcp(k,i,j) / emb(k,1)
         enddo
      endif

      call radcalc3(m1,maxnzp,mcat,iswrtyp,ilwrtyp,if_adap,lpw(i,j)  &
         ,glat(i,j),rtgt(i,j),topt(i,j),albedt(i,j),cosz(i,j)  &
         ,rlongup(i,j),rshort(i,j),rlong(i,j)  &
         ,zm,zt,rv(1,i,j),dn0(1,i,j),fthrd(1,i,j),i,j,time,ngrid)

   enddo
enddo
return
end

!******************************************************************************

subroutine zen(m2,m3,ia,iz,ja,jz,jday,glat,glon,cosz,solfac)

use mem_grid
use mem_radiate
use rconstants

implicit none

integer :: m2,m3,ia,iz,ja,jz,jday,i,j,julday

real :: solfac,tdec,sdec,cdec,declin,d0,d02,dayhr,radlat,cslcsd,snlsnd  &
   ,gglon,dayhrr,hrangl
real, dimension(nzpmax) :: rvr,rtr,dn0r,pird,prd,fthrl,dzmr,dztr,fthrs
real, dimension(nzpmax+1) :: temprd
real, dimension(m2,m3) :: glat,glon,cosz

common /radcom/ tdec,sdec,cdec,declin,rvr,rtr,dn0r,pird,prd,temprd  &
               ,fthrl,dzmr,dztr,fthrs

jday = julday(imonth1,idate1,iyear1)
jday = jday + nint(time/86400.)
!      sdec - sine of declination, cdec - cosine of declination
declin = -23.5 * cos(6.283 / 365. * (jday + 9)) * pi180
sdec = sin(declin)
cdec = cos(declin)

! Find the factor, solfac, to multiply the solar constant to correct
! for Earth's varying distance to the sun.

d0 = 6.2831853 * float(jday-1) / 365.
d02 = d0 * 2.
solfac = 1.000110 + 0.034221 * cos (d0) + 0.001280 * sin(d0)  &
   + 0.000719 * cos(d02) + 0.000077 * sin(d02)

! Find the hour angle, then get cosine of zenith angle.

dayhr = time / 3600. + float(itime1/100) + float(mod(itime1,100)) / 60.

do j = ja,jz
   do i = ia,iz
      radlat = glat(i,j) * pi180
      if (lonrad .eq. 0) radlat = centlat(1) * pi180
      if (radlat .eq. declin) radlat = radlat + 1.e-5
      cslcsd = cos(radlat) * cdec
      snlsnd = sin(radlat) * sdec
      gglon = glon(i,j)
      if (lonrad .eq. 0) gglon = centlon(1)
      dayhrr = mod(dayhr+gglon/15.+24.,24.)
      hrangl = 15. * (dayhrr - 12.) * pi180
      cosz(i,j) = snlsnd + cslcsd * cos(hrangl)
   enddo
enddo
return
end

!****************************************************************************

! subroutine radcalc3: column driver for twostream radiation code

! variables used within subroutine radcalc3:
! ==================================================================

! Variables in rrad3 module parameter statement

!  mb               : maximum allowed number of bands [=8]
!  mg                  : maximum allowed number of gases [=3]
!  mk               : maximum number of pseudobands allowed for any gas [=7]
!  ncog             : number of fit coefficients (omega and asym) [=5]
!  ncb              : number of fit coefficients (extinction) [=2]
!  npartob          : number of hydrometeor categories (including different habits)
!  npartg           : number of hydrometeor categories used for gc coefficients [=7]
!  nrad                  : total number of radiation levels used (m1 - 1 + narad)
!  narad            : number of radiation levels added above model
!  nsolb            : active number of solar bands
!  nb               : active number of bands
!  ng                : active number of gases
!  jday             : julian day
!  solfac           : solar constant multiplier for variable E-S distance
!  ralcs (mb)       : rayleigh scattering integration constants
!  solar1 (mb)      : solar fluxes at top of atmosphere - corrected for ES distance
!  solar0 (mb)      : solar fluxes at top of atmosphere - uncorrected for ES distance
!  nuum (mb)        :    continuum flags
!  a0,a1,a2,a3 (mb) : Planck function fit coefficients
!  npsb (mg,mb)     : number of pseudo bands
!  trf (mg,mb)      : reference temperature for xp and wght coefficients
!  prf (mg,mb)      : reference pressure for xp and wght coefficients
!  ulim (mg,mb)     : upper bound on pathlength for gases
!  xp (mg,mk,mb)    : coefficient used in computing gaseous optical depth
!  alpha (mg,mk,mb) : pressure correction factor exponent
!  beta (mg,mk,mb)  : temperature correction factor exponent
!  wght (mg,mk,mb)  : pseudo band weight
!  exptabc (150)    : table of exponential function values
!  ocoef(ncog,mb,npartob)  : fit coefficients for hyd. single scatter.
!  bcoef(ncb,mb ,npartob)  : fit coefficients for hyd. extinction coefficient.
!  gcoef(ncog,mb,npartg)   : fit coefficients for hyd. asymmetry parameter.

! Input variables from model

!  m1               : number of vertical levels in model grid
!  ncat             : max number of hydrometeor categories [=7]
!  mcat             : actual number of hydrometeor categories [= 0, 1, or 7]
!  nhcat            : number of hydrometeor categories including ice habits [=15]
!  iswrtyp          : shortwave radiation parameterization selection flag
!  ilwrtyp          : longwave radiation parameterization selection flag
!  glat             : latitude
!  rtgt             : terrain-following coordinate metric factor
!  topt             : topography height
!  albedt          : surface albedo
!  cosz             : solar zenith angle
!  rlongup          : upward longwave radiation at surface (W/m^2)
!  rshort           : downward shortwave radiation at surface (W/m^2)
!  rlong            : downward longwave radiation at surface (W/m^2)
!  jnmb (ncat)      : microphysics category flag
!  dnfac (nhcat)    : factor for computing dn from emb
!  pwmasi (nhcat)   : inverse of mass power law exponent for hydrometeors
!  zm (m1)          : model physical heights of W points (m)
!  zt (m1)          : model physical heights of T points (m)
!  press (nzpmax)   : model pressure (Pa)
!  tair (nzpmax)    : model temperature (K)
!  rv (m1)          : model vapor mixing ratio (kg/kg)
!  dn0 (m1)         : model air density (kg/m^3)
!  fthrd (m1)       : theta_il tendency from radiation
!  jhcat (nzpmax,ncat)  : microphysics category array
!  cx (nzpmax,ncat) : hydrometeor number concentration (#/kg)
!  emb (nzpmax,ncat): hydrometeor mean mass (kg)

! Variables input from model scratch space (redefined internally on each call)

!  zml (nrad)       : physical heights of W points of all radiation levels (m)
!  ztl (nrad)       : physical heights of T points of all radiation levels (m)
!  dzl (nrad)       : delta-z (m) of all radiation levels
!  pl (nrad)        : pressure (Pa)
!  tl (nrad)        : temperature (K)
!  dl (nrad)        : air density of all radiation levels (kg/m^3)
!  rl (nrad)        : vapor density of all radiation levels (kg/m^3)
!  vp (nrad)        : vapor pressure (Pa)
!  o3l (nrad)       : stores the calculated ozone profile (g/m^3)
!  flxu (nrad)      : Total upwelling flux (W/m^2)
!  flxd (nrad)      : Total downwelling flux (W/m^2)
!  t (nrad)         : layer transmission function
!  r (nrad)         : layer reflection function
!  tc (nrad)        : cumulative optical depth
!  sigu (nrad)      : upwelling layer source function
!  sigd (nrad)      : downwelling layer source function
!  re (nrad)        : cumulative reflection function
!  vd (nrad)        : multi-scat. diffuse downwelling contributions
!                         from source functions
!  td (nrad)        : inverse of cumulative transmission fnct
!  vu (nrad)        : multi-scat. diffuse upwelling contributions
!                         from source functions
!  tg (nrad)        : gaseous optical depth
!  tcr (nrad)       : continuum/Rayleigh optical depth
!  src (nrad)       : Planck function source for each band
!  fu (nrad*6)      : upwelling fluxes for pseudo-bands (W/m^2)
!  fd (nrad*6)      : downwelling fluxes for pseudo-bands (W/m^2)
!  u (nrad*mg)      : path-length for gases (H_2O, CO_2, O_3)  (Pa)
!  tp (nrad*mb)     : optical depth of hydrometeors (m^-1)
!  omgp (nrad*mb)   : Single scatter albedo of hydrometeors
!  gp (nrad*mb)     : Asymmetry factor of hydrometeors

! Locally-defined variables

!  ngass (mg)       : flags indicating if H20, CO2, O3 are active for solar wavelengths
!  ngast (mg)       : flags indicating if H20, CO2, O3 are active for long wavelengths
!  prsnz,prsnzp     : pressure in top two model reference state levels

! Additional variables used only within subroutine mclatchy:
! ==================================================================

!  namax            : maximum allowed number of added rad levels above model top[=10]
!                       used for oc and bc coefficients [=13]
! mcdat (33,9,6)    : Mclatchy sounding data (33 levels, 9 soundings, 6 vars)
! mclat (33,9,6)    : mcdat interpolated by season to latitude bands
! mcol (33,6)       : mclat interpolated to lat-lon of grid column

! Additional variables used only within subroutine cloud_opt:
! ==================================================================

!  ib .......... band number
!  dn .......... characteristic diameter (m)
!  oc .......... scattering albedo fit coefficients
!  bc .......... extinction fit coefficients
!  gc .......... asymmetery fit coefficients
!  kradcat ..... cross-reference table giving Jerry's 13 hydrometeor category
!                   numbers as a function of 15 microphysics category numbers

! Particle Numbers describe the following particles:

!     Harrington radiation code             RAMS microphysics
! ----------------------------------------------------------------
!  1:   cloud drops                 1.  cloud drops
!  2:   rain                        2.  rain
!  3:   pristine ice columns        3.  pristine ice columns
!  4:   pristine ice rosettes       4.  snow columns
!  5:   pristine ice plates         5.  aggregates
!  6:   snow columns                6.  graupel
!  7:   snow rosettes               7.  hail
!  8:   snow plates                 8.  pristine ice hexagonal plates
!  9:   aggregates columns          9.  pristine ice dendrites
!  10:  aggregates rosettes        10.  pristine ice needles
!  11:  aggregates plates          11.  pristine ice rosettes
!  12:  graupel                    12.  snow hexagonal plates
!  13:  hail                       13.  snow dendrites
!                                  14.  snow needles
!                                  15.  snow rosettes

! for the asymmetery parameter, since we only have spherical
! particles, there are only 7 particle types...

!  1:   cloud drops
!  2:   rain
!  3:   pristine ice
!  4:   snow
!  5:   aggregates
!  6:   graupel
!  7:   hail

!******************************************************************************

subroutine radcalc3(m1,maxnzp,mcat,iswrtyp,ilwrtyp,if_adap,lpw  &
   ,glat,rtgt,topt,albedt,cosz,rlongup,rshort,rlong  &
   ,zm,zt,rv,dn0,fthrd,i,j,time,ngrid)

use rconstants
use rrad3
use micphys

implicit none

integer m1,maxnzp,mcat,ilwrtyp,iswrtyp,if_adap,lpw,ngrid
integer i,j,k,ib,ig,kk,ik,koff
integer, save :: ncall = 0,nradmax

integer, save :: ngass(mg)=(/1, 1, 1/),ngast(mg)=(/1, 1, 1/)
real prsnz,prsnzp

real glat,rtgt,topt,cosz,albedt,rlongup,rshort,rlong,rmix,slr,time
real dzl9,rvk0,rvk1

real zm(m1),zt(m1),dn0(m1),rv(m1),fthrd(m1)

real, allocatable, save, dimension(:)     :: zml,ztl,dzl,pl,tl,dl,rl,o3l  &
                                      ,vp,flxu,flxd,tg,tcr,src,t,r,tc  &
                                      ,sigu,sigd,re,vd,td,vu  &
                                      ,u,fu,fd,tp,omgp,gp

real, save :: eps=1.e-15

!     one can choose the gases of importance here,
!       ngas = 1    gas active
!            = 0    gas not active
!
!       ngas(1) = H2O
!       ngas(2) = CO2
!       ngas(3) =  O3

!data ngass/1, 1, 1/,ngast/1, 1, 1/
!save

if (ncall == 0) then
   ncall = 1
   nradmax = maxnzp + namax
   allocate(zml(nradmax),ztl (nradmax),dzl (nradmax),pl (nradmax)  &
           ,tl (nradmax),dl  (nradmax),rl  (nradmax),o3l(nradmax)  &
           ,vp (nradmax),flxu(nradmax),flxd(nradmax),tg (nradmax)  &
           ,tcr(nradmax),src (nradmax),t   (nradmax),r  (nradmax)  &
           ,tc (nradmax),sigu(nradmax),sigd(nradmax),re (nradmax)  &
           ,vd (nradmax),td  (nradmax),vu  (nradmax)               )
   allocate(u(nradmax*mg),fu(nradmax*6),fd(nradmax*6))
   allocate(tp(nradmax*mb),omgp(nradmax*mb),gp(nradmax*mb))
endif


koff = lpw - 2
nrad = m1 - 1 + narad - koff

call mclatchy(3,m1,if_adap,koff  &
   ,prsnz,prsnzp,glat,rtgt,topt,rlongup  &
   ,zm,zt,press,tair,dn0,rv,zml,ztl,pl,tl,dl,rl,o3l,dzl)

! zero out scratch arrays

call azero(nrad*mg,u)
call azero(nrad*6,fu)
call azero(nrad*6,fd)
call azero(nrad*mb,tp)
call azero(nrad*mb,omgp)
call azero(nrad*mb,gp)

! Compute hydrometeor optical properties

call cloud_opt(mb,nb,nrad,m1,koff,mcat,dzl  &
   ,dn0,tp,omgp,gp &
   ,ocoef,bcoef,gcoef,ncog,ncb,npartob,npartg,i,j,time)

! Get the path lengths for the various gases...

call path_lengths(nrad,u,rl,dzl,dl,o3l,vp,pl,eps)

do k = 1,nrad
   if (rl(k) <   0. .or.  &
       dl(k) <   0. .or.  &
       pl(k) <   0. .or.  &
      o3l(k) <   0. .or.  &
       tl(k) < 183.) then

      print*, 'Temperature too low or negative value of'
      print*, 'density, vapor, pressure, or ozone'
      print*, 'when calling Harrington radiation'
      print*, 'at k,i,j = ',k,i,j,'   ngrid=',ngrid
      print*, 'stopping model'
      print*, 'rad: rl(k), dl(k), pl(k), o3l(k), tl(k)'
      do kk=1,nrad
      print'(5g15.6)', rl(kk), dl(kk), pl(kk), o3l(kk), tl(kk)
      enddo
      stop 'stop: radiation call'
   endif
enddo

! call shortwave and longwave schemes...

if (iswrtyp == 3 .and. cosz > 0.03) then
   call azero2(nrad,flxu,flxd)

   call swrad(nrad,ng,nb,nsolb,npsb,   &         !  counters
      u,pl,tl,dzl,vp,                  &      !  model variables
      xp,alpha,beta,wght,prf,trf,ralcs,  &   !  band specifics
      solar1,ngass,                      &    !        "
      albedt,slr,cosz,               & !  boundaries
      tg,tcr,tp,omgp,gp,   &!  locally defined
      t,r,tc,                              & !  for fluxes
      sigu,sigd,re,vd,td,vu,fu,fd,         & !       "
      flxu,flxd,ulim,i,time)                     !  sw fluxes

   rshort = flxd(1)
   do k = lpw,m1-1
      kk = k - koff
      fthrd(k) = fthrd(k)  &
         + (flxd(kk) - flxd(kk-1) + flxu(kk-1) - flxu(kk)) &
            / (dl(kk) * dzl(kk) * cp)
   enddo

else

   rshort = 0.

endif

if (ilwrtyp == 3) then
   call azero2(nrad,flxu,flxd)

   call lwrad(nrad,ng,nb,nsolb,npsb,nuum,   &    !  counters
      u,pl,tl,dzl,vp,                       & !  model variables
      xp,alpha,beta,wght,prf,trf,ralcs,     &!  band specifics
      a0,a1,a2,a3,                          &!        "
      exptabc,ngast,                        &!  boundaries
      tg,tcr,tp,omgp,gp,src,   &  !  locally defined
      t,r,tc,                               &!  for fluxes
      sigu,sigd,re,vd,td,vu,fu,fd,          &!       "
      flxu,flxd,ulim)                          !  fluxes, output

   rlong = flxd(1)

   do k = lpw,m1-1
      kk = k - koff
      fthrd(k) = fthrd(k)  &
         + (flxd(kk) - flxd(kk-1) + flxu(kk-1) - flxu(kk)) &
            / (dl(kk) * dzl(kk) * cp)
   enddo

endif

return
end

!******************************************************************************

subroutine cloud_opt(mb,nb,nrad,m1,koff,mcat,dzl  &
   ,dn0,tp,omgp,gp,oc,bc,gc,ncog,ncb,npartob,npartg,i,j,time)

! computing properties of spherical liquid water and irregular ice
! using fits to adt theory
!
! ib .......... band number
! mb .......... maximum number of bands
! nb .......... total number of bands
! m1 .......... number of vertical levels
! dzl .......... delta z in each level (m)
! dn .......... characteristic diameter (m)
! emb ......... mean hydrometeor mass (kg)
! cx .......... hydrometeor concentration (#/kg)
! tp .......... optical depth
! omgp ........ scattering albedo
! gp .......... asymmetry parameter
! oc .......... scattering albedo fit coefficients
! bc .......... extinction fit coefficients
! gc .......... asymmetry fit coefficients
! ncog ........ number of fit coefficients (omega and asym)
! ncb ......... number of fit coefficients (extinction)
! kradcat ..... cross-reference table giving Jerry's 13 hydrometeor category
!                 numbers as a function of 15 microphysics category numbers
! dn0 ......... model air density (kg/m^3)
! dnfac ....... factor for computing dn from emb
! pwmasi ...... inverse of power used in mass power law
! npartob ..... number of hydrometeor categories (including different habits)
!                 used for oc and bc coefficients
! npartg ...... number of hydrometeor categories used for gc coefficients
!

use micphys

implicit none

integer mb,nb,ib,nrad,m1,ncog,ncb,iz,krc,npartob,npartg,koff
integer icat,mcat,k,i,j,ihcat

integer kradcat(15)
real dzl(nrad),tp(nrad,mb),omgp(nrad,mb),gp(nrad,mb),dn0(m1)
real oc(ncog,mb,npartob),bc(ncb,mb,npartob),gc(ncog,mb,npartg)
real ext,om,gg,dn,time

real dnmin(7),dnmax(7)
data dnmin /   1.,   10.,   1.,  125.,   10.,   10.,   10./
data dnmax /1000.,10000., 125.,10000.,10000.,10000.,10000./

data kradcat/1,2,3,6,10,12,13,5,5,3,4,8,8,6,7/

do icat = 1,mcat
   if (jnmb(icat) .gt. 0) then

      do k = 2,m1-1-koff

         if (cx(k+koff,icat) .gt. 1.e-9) then
            ihcat = jhcat(k+koff,icat)
            krc = kradcat(ihcat)
            dn = dnfac(ihcat) * emb(k+koff,icat) ** pwmasi(ihcat) * 1.e6
            dn = max(dnmin(icat),min(dnmax(icat),dn))

            do ib = 1,nb

               ext = cx(k+koff,icat) * dn0(k+koff) * dzl(k)  &
                  * bc(1,ib,krc) * dn ** bc(2,ib,krc)
               om = oc(1,ib,krc)  &
                  + oc(2,ib,krc) * exp(oc(3,ib,krc) * dn)  &
                  + oc(4,ib,krc) * exp(oc(5,ib,krc) * dn)
               gg = gc(1,ib,icat)  &
                  + gc(2,ib,icat) * exp(gc(3,ib,icat) * dn)  &
                  + gc(4,ib,icat) * exp(gc(5,ib,icat) * dn)


               tp(k,ib) = tp(k,ib) + ext

               omgp(k,ib) = omgp(k,ib) + om * ext
               gp(k,ib) = gp(k,ib) + gg * om * ext

            enddo

         endif
      enddo

   endif
enddo

! Combine the optical properties....

do ib = 1,nb
   do k = 2,m1-1-koff
      if (tp(k,ib) .gt. 0.0) then
         gp(k,ib) = gp(k,ib) / omgp(k,ib)
         omgp(k,ib) = omgp(k,ib) / tp(k,ib)
      else
         omgp(k,ib) = 0.0
         gp(k,ib) = 0.0
      endif

! Check for validity of opt values before calling radiation
!      if (tp(k,ib) .lt. 0) then
!         print*, 'tp(k,ib) less than zero for k,ib = ',k,ib
!         print*, 'tp(k,ib) = ',tp(k,ib)
!         stop 'opt1'
!      endif
!      if (omgp(k,ib) .lt. 0. .or. omgp(k,ib) .gt. 1.) then
!         print*, 'omgp(k,ib) out of range [0,1] for k,ib = ',k,ib
!         print*, 'omgp(k,ib) = ',omgp(k,ib)
!         stop 'opt2'
!      endif
!      if (gp(k,ib) .lt. 0. .or. gp(k,ib) .gt. 1.) then
!         print*, 'gp(k,ib) out of range [0,1] for k,ib = ',k,ib
!         print*, 'gp(k,ib) = ',gp(k,ib)
!         stop 'opt3'
!      endif

   enddo
enddo

return
end

!------------------------------------------------------------------

subroutine path_lengths(nrad,u,rl,dzl,dl,o3l,vp,pl,eps)

! Get the path lengths for the various gases...

implicit none
integer :: nrad
real, dimension(nrad) :: rl,dzl,dl,o3l,vp,pl
real, dimension(nrad,3) :: u
real :: rvk0,rvk1,dzl9,rmix,eps
integer :: k

u(1,1) = .5 * (rl(2) + rl(1)) * 9.81 * dzl(1)
u(1,2) = .5 * (dl(2) + dl(1)) * .45575e-3 * 9.81 * dzl(1)
u(1,3) = o3l(1) * 9.81 * dzl(1)

rvk0 = rl(1)
do k = 2,nrad
   rvk1 = (rl(k) + 1.e-6)
   dzl9 = 9.81 * dzl(k)
   rmix = rvk1 / dl(k)
   vp(k) = pl(k) * rmix / (.622 + rmix)
   u(k,1) = (rvk1 - rvk0) / (log(rvk1 / rvk0) + eps) * dzl9
   u(k,2) = (dl(k) - dl(k-1)) / (log(dl(k) / dl(k-1)) + eps)  &
       * dzl9 * 0.45575e-3
   u(k,3) = 0.5 * dzl9 * (o3l(k) + o3l(k-1))
   rvk0 = rvk1
enddo

return
end




