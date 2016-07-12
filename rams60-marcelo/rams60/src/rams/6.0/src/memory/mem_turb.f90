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


Module mem_turb

   use grid_dims

   Type turb_vars
   
      ! Variables to be dimensioned by (nzp,nxp,nyp)
   real, pointer, dimension(:,:,:) :: &
                          tkep,epsp,hkm,vkm,vkh,cdrag

      ! Variables to be dimensioned by (nxp,nyp)
   real, pointer, dimension(:,:) :: &
                          sflux_u,sflux_v,sflux_w,sflux_t,sflux_r

   End Type
   
   type (turb_vars), allocatable :: turb_g(:), turbm_g(:)
   
   integer :: if_urban_canopy,ihorgrad
   
   integer, dimension(maxgrds) :: idiffk
   
   real                     :: brunt ,rmax ,rmin
   real, dimension(maxgrds) :: zkhkm,xkhkm,csz,csx,akmin

Contains

   subroutine alloc_turb(turb,n1,n2,n3,ng)

   implicit none
   type (turb_vars) :: turb
   integer, intent(in) :: n1,n2,n3,ng

! Allocate arrays based on options (if necessary)
      
      if(idiffk(ng) == 1 .or. idiffk(ng) == 4 .or.  &
         idiffk(ng) == 5 .or. idiffk(ng) == 6)  &
                         allocate (turb%tkep(n1,n2,n3))
      if(idiffk(ng) == 5 .or. idiffk(ng) == 6)  &
                         allocate (turb%epsp(n1,n2,n3))

                         allocate (turb%hkm(n1,n2,n3))
                         allocate (turb%vkm(n1,n2,n3))
                         allocate (turb%vkh(n1,n2,n3))

      if(if_urban_canopy > 0) allocate (turb%cdrag(n1,n2,n3)) 

                         allocate (turb%sflux_u(n2,n3))
                         allocate (turb%sflux_v(n2,n3))
                         allocate (turb%sflux_w(n2,n3))
                         allocate (turb%sflux_t(n2,n3))
                         allocate (turb%sflux_r(n2,n3))
                         
   return
   end subroutine


   subroutine nullify_turb(turb)

   implicit none
   type (turb_vars) :: turb
   

   if (associated(turb%tkep))    nullify (turb%tkep)
   if (associated(turb%epsp))    nullify (turb%epsp)
   if (associated(turb%hkm))     nullify (turb%hkm)
   if (associated(turb%vkm))     nullify (turb%vkm)
   if (associated(turb%vkh))     nullify (turb%vkh)
   if (associated(turb%cdrag))   nullify (turb%cdrag)
   if (associated(turb%sflux_u)) nullify (turb%sflux_u)
   if (associated(turb%sflux_v)) nullify (turb%sflux_v)
   if (associated(turb%sflux_w)) nullify (turb%sflux_w)
   if (associated(turb%sflux_t)) nullify (turb%sflux_t)
   if (associated(turb%sflux_r)) nullify (turb%sflux_r)

   return
   end subroutine

   subroutine dealloc_turb(turb)

   implicit none
   type (turb_vars) :: turb
   

   if (associated(turb%tkep))    deallocate (turb%tkep)
   if (associated(turb%epsp))    deallocate (turb%epsp)
   if (associated(turb%hkm))     deallocate (turb%hkm)
   if (associated(turb%vkm))     deallocate (turb%vkm)
   if (associated(turb%vkh))     deallocate (turb%vkh)
   if (associated(turb%cdrag))   deallocate (turb%cdrag)
   if (associated(turb%sflux_u)) deallocate (turb%sflux_u)
   if (associated(turb%sflux_v)) deallocate (turb%sflux_v)
   if (associated(turb%sflux_w)) deallocate (turb%sflux_w)
   if (associated(turb%sflux_t)) deallocate (turb%sflux_t)
   if (associated(turb%sflux_r)) deallocate (turb%sflux_r)

   return
   end subroutine


subroutine filltab_turb(turb,turbm,imean,n1,n2,n3,ng)

use var_tables

   implicit none
   type (turb_vars) :: turb,turbm
   integer, intent(in) :: imean,n1,n2,n3,ng
   integer :: npts
   real, pointer :: var,varm

! Fill pointers to arrays into variable tables

   npts=n1*n2*n3

   if (associated(turb%tkep))  &
      call vtables2 (turb%tkep(1,1,1),turbm%tkep(1,1,1)  &
                 ,ng, npts, imean,  &
                 'TKEP :3:hist:anal:mpti:mpt3:mpt1')
   if (associated(turb%epsp))  &
      call vtables2 (turb%epsp(1,1,1),turbm%epsp(1,1,1)  &
                 ,ng, npts, imean,  &
                 'EPSP :3:hist:anal:mpti:mpt3:mpt1')

   if (associated(turb%hkm))  &
      call vtables2 (turb%hkm(1,1,1),turbm%hkm(1,1,1)  &
                 ,ng, npts, imean,  &
                 'RHKM :3:hist:anal:mpti:mpt3:mpt1')
   if (associated(turb%vkm))  &
      call vtables2 (turb%vkm(1,1,1),turbm%vkm(1,1,1)  &
                 ,ng, npts, imean,  &
                 'RVKM :3:hist:mpti:mpt3:mpt1')
   if (associated(turb%vkh))  &
      call vtables2 (turb%vkh(1,1,1),turbm%vkh(1,1,1)  &
                 ,ng, npts, imean,  &
                 'RVKH :3:hist:anal:mpti:mpt3:mpt1')
   if (associated(turb%cdrag))  &
      call vtables2 (turb%cdrag(1,1,1),turbm%cdrag(1,1,1)  &
                 ,ng, npts, imean,  &
                 'CDRAG :3:hist:anal:mpti')

   npts=n2*n3
   if (associated(turb%sflux_u))  &
      call vtables2 (turb%sflux_u(1,1),turbm%sflux_u(1,1)  &
                 ,ng, npts, imean,  &
                 'SFLUX_U :2:hist:anal:mpt3:mpt1')
   if (associated(turb%sflux_v))  &
      call vtables2 (turb%sflux_v(1,1),turbm%sflux_v(1,1)  &
                 ,ng, npts, imean,  &
                 'SFLUX_V :2:hist:anal:mpt3:mpt1')
   if (associated(turb%sflux_w))  &
      call vtables2 (turb%sflux_w(1,1),turbm%sflux_w(1,1)  &
                 ,ng, npts, imean,  &
                 'SFLUX_W :2:hist:anal:mpt3')
   if (associated(turb%sflux_t))  &
      call vtables2 (turb%sflux_t(1,1),turbm%sflux_t(1,1)  &
                 ,ng, npts, imean,  &
                 'SFLUX_T :2:hist:anal:mpt3')
   if (associated(turb%sflux_r))  &
      call vtables2 (turb%sflux_r(1,1),turbm%sflux_r(1,1)  &
                 ,ng, npts, imean,  &
                 'SFLUX_R :2:hist:anal:mpt3')

   return
   end subroutine

End Module mem_turb
