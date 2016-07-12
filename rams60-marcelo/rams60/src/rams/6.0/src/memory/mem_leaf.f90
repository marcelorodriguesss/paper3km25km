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


Module mem_leaf

   use grid_dims

   Type leaf_vars
   
      ! Variables to be dimensioned by (nxp,nyp,nzg,npatch)
   real, pointer, dimension(:,:,:,:) :: &
                  soil_water, soil_energy, soil_text

      ! Variables to be dimensioned by (nxp,nyp,nzs,npatch)
   real, pointer, dimension(:,:,:,:) :: &
                  sfcwater_mass, sfcwater_energy, sfcwater_depth

      ! Variables to be dimensioned by (nxp,nyp,npatch)
   real, pointer, dimension(:,:,:) :: &
                  ustar,tstar,rstar  &  
                 ,veg_fracarea,veg_lai,veg_rough,veg_height  &
                 ,veg_albedo,veg_tai  &
                 ,patch_area,patch_rough,patch_wetind,leaf_class  &
                 ,soil_rough,sfcwater_nlev,stom_resist  &
                 ,ground_rsat,ground_rvap  &
                 ,veg_water,veg_temp,can_rvap,can_temp &
                 ,veg_ndvip,veg_ndvic,veg_ndvif

      ! Variables to be dimensioned by (nxp,nyp)
   real, pointer, dimension(:,:) :: &
                  snow_mass,snow_depth,seatp,seatf
   End Type
   
   type (leaf_vars), allocatable :: leaf_g(:), leafm_g(:)

   !-------------------------------------------------------------------------------
   integer                 :: nslcon,nvgcon,nvegpat,isfcl
   real                    :: zrough,pctlcon,ubmin,albedo,drtcon,dthcon,seatmp
   real, dimension(nzgmax) :: stgoff,slmstr
   real, dimension(nzgmax+1)  :: slz

  
Contains

   subroutine alloc_leaf(leaf,nz,nx,ny,nzg,nzs,np,ng)

   implicit none
   type (leaf_vars) :: leaf
   integer, intent(in) :: nz,nx,ny,nzg,nzs,np,ng


! Allocate arrays based on options (if necessary)

   allocate (leaf%soil_water     (nzg,nx,ny,np))
   allocate (leaf%soil_energy    (nzg,nx,ny,np))
   allocate (leaf%soil_text      (nzg,nx,ny,np))

   allocate (leaf%sfcwater_mass  (nzs,nx,ny,np))
   allocate (leaf%sfcwater_energy(nzs,nx,ny,np))
   allocate (leaf%sfcwater_depth (nzs,nx,ny,np))

   allocate (leaf%ustar        (nx,ny,np))
   allocate (leaf%tstar        (nx,ny,np))
   allocate (leaf%rstar        (nx,ny,np))
   
   allocate (leaf%veg_fracarea (nx,ny,np))
   allocate (leaf%veg_lai      (nx,ny,np))
   allocate (leaf%veg_rough    (nx,ny,np))
   allocate (leaf%veg_height   (nx,ny,np))
   allocate (leaf%veg_albedo   (nx,ny,np))
   allocate (leaf%veg_tai      (nx,ny,np))

   allocate (leaf%patch_area   (nx,ny,np))
   allocate (leaf%patch_rough  (nx,ny,np))
   allocate (leaf%patch_wetind (nx,ny,np))
   allocate (leaf%leaf_class   (nx,ny,np))
   
   allocate (leaf%soil_rough   (nx,ny,np))
   allocate (leaf%sfcwater_nlev(nx,ny,np))
   allocate (leaf%stom_resist  (nx,ny,np))

   allocate (leaf%ground_rsat  (nx,ny,np))
   allocate (leaf%ground_rvap  (nx,ny,np))
   
   allocate (leaf%veg_water    (nx,ny,np))
   allocate (leaf%veg_temp     (nx,ny,np))

   allocate (leaf%can_rvap     (nx,ny,np))
   allocate (leaf%can_temp     (nx,ny,np))

   allocate (leaf%veg_ndvip    (nx,ny,np))
   allocate (leaf%veg_ndvic    (nx,ny,np))
   allocate (leaf%veg_ndvif    (nx,ny,np))
   
   allocate (leaf%snow_mass    (nx,ny))
   allocate (leaf%snow_depth   (nx,ny))
   allocate (leaf%seatp        (nx,ny))
   allocate (leaf%seatf        (nx,ny))


   return
   end subroutine


   subroutine nullify_leaf(leaf)

   implicit none
   type (leaf_vars) :: leaf


  if(associated(leaf%soil_water))      nullify (leaf%soil_water)
  if(associated(leaf%soil_energy))     nullify (leaf%soil_energy)
  if(associated(leaf%soil_text))       nullify (leaf%soil_text)

  if(associated(leaf%sfcwater_mass))   nullify (leaf%sfcwater_mass)
  if(associated(leaf%sfcwater_energy)) nullify (leaf%sfcwater_energy)
  if(associated(leaf%sfcwater_depth))  nullify (leaf%sfcwater_depth)

  if(associated(leaf%ustar))           nullify (leaf%ustar)
  if(associated(leaf%tstar))           nullify (leaf%tstar)
  if(associated(leaf%rstar))           nullify (leaf%rstar)
 
  if(associated(leaf%veg_fracarea))    nullify (leaf%veg_fracarea)
  if(associated(leaf%veg_lai))         nullify (leaf%veg_lai)
  if(associated(leaf%veg_rough))       nullify (leaf%veg_rough)
  if(associated(leaf%veg_height))      nullify (leaf%veg_height)
  if(associated(leaf%veg_albedo))      nullify (leaf%veg_albedo)
  if(associated(leaf%veg_tai))         nullify (leaf%veg_tai)

  if(associated(leaf%patch_area))      nullify (leaf%patch_area)
  if(associated(leaf%patch_rough))     nullify (leaf%patch_rough)
  if(associated(leaf%patch_wetind))    nullify (leaf%patch_wetind)
  if(associated(leaf%leaf_class))      nullify (leaf%leaf_class)
 
  if(associated(leaf%soil_rough))      nullify (leaf%soil_rough)
  if(associated(leaf%sfcwater_nlev))   nullify (leaf%sfcwater_nlev)
  if(associated(leaf%stom_resist))     nullify (leaf%stom_resist)

  if(associated(leaf%ground_rsat))     nullify (leaf%ground_rsat)
  if(associated(leaf%ground_rvap))     nullify (leaf%ground_rvap)
 
  if(associated(leaf%veg_water))       nullify (leaf%veg_water)
  if(associated(leaf%veg_temp))        nullify (leaf%veg_temp)

  if(associated(leaf%can_rvap))        nullify (leaf%can_rvap)
  if(associated(leaf%can_temp))        nullify (leaf%can_temp)

  if(associated(leaf%veg_ndvip))       nullify (leaf%veg_ndvip)
  if(associated(leaf%veg_ndvic))       nullify (leaf%veg_ndvic)
  if(associated(leaf%veg_ndvif))       nullify (leaf%veg_ndvif)
   
  if(associated(leaf%snow_mass))       nullify (leaf%snow_mass)
  if(associated(leaf%snow_depth))      nullify (leaf%snow_depth)
  if(associated(leaf%seatp))           nullify (leaf%seatp)
  if(associated(leaf%seatf))           nullify (leaf%seatf)

   return
   end subroutine

   subroutine dealloc_leaf(leaf)

   implicit none
   type (leaf_vars) :: leaf


  if(associated(leaf%soil_water))      deallocate (leaf%soil_water)
  if(associated(leaf%soil_energy))     deallocate (leaf%soil_energy)
  if(associated(leaf%soil_text))       deallocate (leaf%soil_text)

  if(associated(leaf%sfcwater_mass))   deallocate (leaf%sfcwater_mass)
  if(associated(leaf%sfcwater_energy)) deallocate (leaf%sfcwater_energy)
  if(associated(leaf%sfcwater_depth))  deallocate (leaf%sfcwater_depth)

  if(associated(leaf%ustar))           deallocate (leaf%ustar)
  if(associated(leaf%tstar))           deallocate (leaf%tstar)
  if(associated(leaf%rstar))           deallocate (leaf%rstar)
 
  if(associated(leaf%veg_fracarea))    deallocate (leaf%veg_fracarea)
  if(associated(leaf%veg_lai))         deallocate (leaf%veg_lai)
  if(associated(leaf%veg_rough))       deallocate (leaf%veg_rough)
  if(associated(leaf%veg_height))      deallocate (leaf%veg_height)
  if(associated(leaf%veg_albedo))      deallocate (leaf%veg_albedo)
  if(associated(leaf%veg_tai))         deallocate (leaf%veg_tai)

  if(associated(leaf%patch_area))      deallocate (leaf%patch_area)
  if(associated(leaf%patch_rough))     deallocate (leaf%patch_rough)
  if(associated(leaf%patch_wetind))    deallocate (leaf%patch_wetind)
  if(associated(leaf%leaf_class))      deallocate (leaf%leaf_class)
 
  if(associated(leaf%soil_rough))      deallocate (leaf%soil_rough)
  if(associated(leaf%sfcwater_nlev))   deallocate (leaf%sfcwater_nlev)
  if(associated(leaf%stom_resist))     deallocate (leaf%stom_resist)

  if(associated(leaf%ground_rsat))     deallocate (leaf%ground_rsat)
  if(associated(leaf%ground_rvap))     deallocate (leaf%ground_rvap)
 
  if(associated(leaf%veg_water))       deallocate (leaf%veg_water)
  if(associated(leaf%veg_temp))        deallocate (leaf%veg_temp)

  if(associated(leaf%can_rvap))        deallocate (leaf%can_rvap)
  if(associated(leaf%can_temp))        deallocate (leaf%can_temp)

  if(associated(leaf%veg_ndvip))       deallocate (leaf%veg_ndvip)
  if(associated(leaf%veg_ndvic))       deallocate (leaf%veg_ndvic)
  if(associated(leaf%veg_ndvif))       deallocate (leaf%veg_ndvif)
   
  if(associated(leaf%snow_mass))       deallocate (leaf%snow_mass)
  if(associated(leaf%snow_depth))      deallocate (leaf%snow_depth)
  if(associated(leaf%seatp))           deallocate (leaf%seatp)
  if(associated(leaf%seatf))           deallocate (leaf%seatf)

   return
   end subroutine


subroutine filltab_leaf(leaf,leafm,imean,nz,nx,ny,nzg,nzs,np,ng)

use var_tables

   implicit none
   type (leaf_vars) :: leaf,leafm
   integer, intent(in) :: imean,nz,nx,ny,nzg,nzs,np,ng
   integer :: npts
   real, pointer :: var,varm

! Fill pointers to arrays into variable tables


   npts=nzg*nx*ny*np
   call vtables2 (leaf%soil_water(1,1,1,1),leafm%soil_water(1,1,1,1)  &
                 ,ng, npts, imean,  &
                 'SOIL_WATER :4:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%soil_energy(1,1,1,1),leafm%soil_energy(1,1,1,1)  &
                 ,ng, npts, imean,  &
                 'SOIL_ENERGY :4:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%soil_text(1,1,1,1),leafm%soil_text(1,1,1,1)  &
                 ,ng, npts, imean,  &
                 'SOIL_TEXT :4:hist:anal:mpti:mpt3:recycle')

   npts=nzs*nx*ny*np
   call vtables2 (leaf%sfcwater_mass(1,1,1,1),leafm%sfcwater_mass(1,1,1,1)  &
                 ,ng, npts, imean,  &
                 'SFCWATER_MASS :5:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%sfcwater_energy(1,1,1,1), leafm%sfcwater_energy(1,1,1,1) &
                 ,ng, npts, imean,  &
                 'SFCWATER_ENERGY :5:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%sfcwater_depth(1,1,1,1),leafm%sfcwater_depth(1,1,1,1)  &
                 ,ng, npts, imean,  &
                 'SFCWATER_DEPTH :5:hist:anal:mpti:mpt3:recycle')

   npts=nx*ny*np
   call vtables2 (leaf%ustar(1,1,1),leafm%ustar(1,1,1)  &
                 ,ng, npts, imean,  &
                 'USTAR :6:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%tstar(1,1,1),leafm%tstar(1,1,1)  &
                 ,ng, npts, imean,  &
                 'TSTAR :6:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%rstar(1,1,1),leafm%rstar(1,1,1)  &
                 ,ng, npts, imean,  &
                 'RSTAR :6:hist:anal:mpti:mpt3:recycle')
            
   call vtables2 (leaf%veg_fracarea(1,1,1),leafm%veg_fracarea(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VEG_FRACAREA :6:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%veg_lai(1,1,1),leafm%veg_lai(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VEG_LAI :6:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%veg_rough(1,1,1),leafm%veg_rough(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VEG_ROUGH :6:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%veg_height(1,1,1),leafm%veg_height(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VEG_HEIGHT :6:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%veg_albedo(1,1,1),leafm%veg_albedo(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VEG_ALBEDO :6:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%veg_tai(1,1,1),leafm%veg_tai(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VEG_TAI :6:hist:anal:mpti:mpt3:recycle')

   call vtables2 (leaf%patch_area(1,1,1),leafm%patch_area(1,1,1)  &
                 ,ng, npts, imean,  &
                 'PATCH_AREA :6:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%patch_rough(1,1,1),leafm%patch_rough(1,1,1)  &
                 ,ng, npts, imean,  &
                 'PATCH_ROUGH :6:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%patch_wetind(1,1,1),leafm%patch_wetind(1,1,1)  &
                 ,ng, npts, imean,  &
                 'PATCH_WETIND :6:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%leaf_class(1,1,1),leafm%leaf_class(1,1,1)  &
                 ,ng, npts, imean,  &
                 'LEAF_CLASS :6:hist:anal:mpti:mpt3:recycle')

   call vtables2 (leaf%soil_rough(1,1,1),leafm%soil_rough(1,1,1)  &
                 ,ng, npts, imean,  &
                 'SOIL_ROUGH :6:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%sfcwater_nlev(1,1,1),leafm%sfcwater_nlev(1,1,1)  &
                 ,ng, npts, imean,  &
                 'SFCWATER_NLEV :6:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%stom_resist(1,1,1),leafm%stom_resist(1,1,1)  &
                 ,ng, npts, imean,  &
                 'STOM_RESIST :6:hist:anal:mpti:mpt3:recycle')

   call vtables2 (leaf%ground_rsat(1,1,1),leafm%ground_rsat(1,1,1)  &
                 ,ng, npts, imean,  &
                 'GROUND_RSAT :6:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%ground_rvap(1,1,1),leafm%ground_rvap(1,1,1)  &
                 ,ng, npts, imean,  &
                 'GROUND_RVAP :6:hist:anal:mpti:mpt3:recycle')

   call vtables2 (leaf%veg_water(1,1,1),leafm%veg_water(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VEG_WATER :6:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%veg_temp(1,1,1),leafm%veg_temp(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VEG_TEMP :6:hist:anal:mpti:mpt3:recycle')

   call vtables2 (leaf%can_rvap(1,1,1),leafm%can_rvap(1,1,1)  &
                 ,ng, npts, imean,  &
                 'CAN_RVAP :6:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%can_temp(1,1,1),leafm%can_temp(1,1,1)  &
                 ,ng, npts, imean,  &
                 'CAN_TEMP :6:hist:anal:mpti:mpt3:recycle')

   call vtables2 (leaf%veg_ndvip(1,1,1),leafm%veg_ndvip(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VEG_NDVIP :6:hist:mpti')
   call vtables2 (leaf%veg_ndvic(1,1,1),leafm%veg_ndvic(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VEG_NDVIC :6:hist:anal:mpti:mpt3:recycle')
   call vtables2 (leaf%veg_ndvif(1,1,1),leafm%veg_ndvif(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VEG_NDVIF :6:hist:mpti')

   npts=nx*ny
   call vtables2 (leaf%snow_mass(1,1),leafm%snow_mass(1,1)  &
                 ,ng, npts, imean,  &
                 'SNOW_MASS :2:mpti')
   call vtables2 (leaf%snow_depth(1,1),leafm%snow_depth(1,1)  &
                 ,ng, npts, imean,  &
                 'SNOW_DEPTH :2:mpti')
   call vtables2 (leaf%seatp(1,1),leafm%seatp(1,1)  &
                 ,ng, npts, imean,  &
                 'SEATP :2:mpti')
   call vtables2 (leaf%seatf(1,1),leafm%seatf(1,1)  &
                 ,ng, npts, imean,  &
                 'SEATF :2:mpti')

   return
   end subroutine

End Module mem_leaf
