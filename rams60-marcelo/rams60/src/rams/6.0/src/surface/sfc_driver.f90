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


subroutine sfc_driver(mzp,mxp,myp,ia,iz,ja,jz,ibcon)

use mem_all

implicit none

integer :: mzp,mxp,myp,ia,iz,ja,jz,ibcon

real :: rslif
integer :: ng
!integer, save :: ncall=0

if (nstbot == 0) return

!print*,'ncall=',ncall
!if(ncall == 0) then
!   ncall=1
!   open(11,file='leaf.lis', status='unknown')
!   rewind 11

!print*,'calling alloc=',ncall
!   call alloc_leafcol(nzg,nzs)

!endif


ng=ngrid

call leaf3(mzp,mxp,myp,nzg,nzs,npatch,ia,iz,ja,jz           &
   ,leaf_g (ng), basic_g (ng), turb_g (ng), radiate_g(ng)   &
   ,grid_g (ng), cuparm_g(ng), micro_g(ng)                  &
   ,scratch%vt2da (1) ,scratch%vt2db(1) ,scratch%vt2dc (1)  &
   ,scratch%vt2dd(1)  ,scratch%vt2de (1) ,scratch%vt2df(1)  &
   ,scratch%vt3da (1)                                       )

if (isfcl == 2) then
   call hydro(mxp,myp,nzg,nzs,npatch         &
      ,leaf_g(ng)%soil_water      (1,1,1,1)  &
      ,leaf_g(ng)%soil_energy     (1,1,1,1)  &
      ,leaf_g(ng)%soil_text       (1,1,1,1)  &
      ,leaf_g(ng)%sfcwater_mass   (1,1,1,1)  &
      ,leaf_g(ng)%sfcwater_energy (1,1,1,1)  &
      ,leaf_g(ng)%patch_area      (1,1,1)    &
      ,leaf_g(ng)%patch_wetind    (1,1,1)    )
endif

! Apply lateral boundary conditions to leaf3 arrays

call leaf_bcond(mxp,myp,nzg,nzs,npatch,jdim     &

   ,leaf_g(ng)%soil_water (1,1,1,1) ,leaf_g(ng)%sfcwater_mass  (1,1,1,1)  &
   ,leaf_g(ng)%soil_energy(1,1,1,1) ,leaf_g(ng)%sfcwater_energy(1,1,1,1)  &
   ,leaf_g(ng)%soil_text  (1,1,1,1) ,leaf_g(ng)%sfcwater_depth (1,1,1,1)  &
   ,leaf_g(ng)%ustar        (1,1,1) ,leaf_g(ng)%tstar            (1,1,1)  &
   ,leaf_g(ng)%rstar        (1,1,1) ,leaf_g(ng)%veg_albedo       (1,1,1)  &
   ,leaf_g(ng)%veg_fracarea (1,1,1) ,leaf_g(ng)%veg_lai          (1,1,1)  &
   ,leaf_g(ng)%veg_tai      (1,1,1)                                       &
   ,leaf_g(ng)%veg_rough    (1,1,1) ,leaf_g(ng)%veg_height       (1,1,1)  &
   ,leaf_g(ng)%patch_area   (1,1,1) ,leaf_g(ng)%patch_rough      (1,1,1)  &
   ,leaf_g(ng)%patch_wetind (1,1,1) ,leaf_g(ng)%leaf_class       (1,1,1)  &
   ,leaf_g(ng)%soil_rough   (1,1,1) ,leaf_g(ng)%sfcwater_nlev    (1,1,1)  &
   ,leaf_g(ng)%stom_resist  (1,1,1) ,leaf_g(ng)%ground_rsat      (1,1,1)  &
   ,leaf_g(ng)%ground_rvap  (1,1,1) ,leaf_g(ng)%veg_water        (1,1,1)  &
   ,leaf_g(ng)%veg_temp     (1,1,1) ,leaf_g(ng)%can_rvap         (1,1,1)  &
   ,leaf_g(ng)%can_temp     (1,1,1) ,leaf_g(ng)%veg_ndvip        (1,1,1)  &
   ,leaf_g(ng)%veg_ndvic    (1,1,1) ,leaf_g(ng)%veg_ndvif        (1,1,1)  )


!  Urban canopy parameterization
!----------------------------------------
IF( IF_URBAN_CANOPY == 1) call urban_canopy()      




return
end
