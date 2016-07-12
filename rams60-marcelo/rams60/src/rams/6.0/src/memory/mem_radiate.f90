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


Module mem_radiate

   Type radiate_vars
   
      ! Variables to be dimensioned by (nzp,nxp,nyp)
   real, pointer, dimension(:,:,:) :: &
                          fthrd
                          
      ! Variables to be dimensioned by (nxp,nyp)
   real, pointer, dimension(:,:) :: &
                          rshort,rlong,rlongup,albedt,cosz

   End Type
   
   type (radiate_vars), allocatable :: radiate_g(:), radiatem_g(:)
   
   integer :: lonrad,ilwrtyp,iswrtyp
   real    :: radfrq
  
Contains

   subroutine alloc_radiate(radiate,n1,n2,n3,ng)

   implicit none
   type (radiate_vars) :: radiate
   integer, intent(in) :: n1,n2,n3,ng

! Allocate arrays based on options (if necessary)
      
      if( ilwrtyp+iswrtyp > 0)  then
                         allocate (radiate%fthrd(n1,n2,n3))
                         allocate (radiate%rshort(n2,n3))
                         allocate (radiate%rlong(n2,n3))
                         allocate (radiate%rlongup(n2,n3))
                         allocate (radiate%albedt(n2,n3))
                         allocate (radiate%cosz(n2,n3))
      endif
                         
   return
   end subroutine


   subroutine nullify_radiate(radiate)

   implicit none
   type (radiate_vars) :: radiate
   

   if (associated(radiate%fthrd))    nullify (radiate%fthrd)
   if (associated(radiate%rshort))   nullify (radiate%rshort)
   if (associated(radiate%rlong))    nullify (radiate%rlong)
   if (associated(radiate%rlongup))  nullify (radiate%rlongup)
   if (associated(radiate%albedt))   nullify (radiate%albedt)
   if (associated(radiate%cosz))     nullify (radiate%cosz)

   return
   end subroutine

   subroutine dealloc_radiate(radiate)

   implicit none
   type (radiate_vars) :: radiate
   

   if (associated(radiate%fthrd))    deallocate (radiate%fthrd)
   if (associated(radiate%rshort))   deallocate (radiate%rshort)
   if (associated(radiate%rlong))    deallocate (radiate%rlong)
   if (associated(radiate%rlongup))  deallocate (radiate%rlongup)
   if (associated(radiate%albedt))   deallocate (radiate%albedt)
   if (associated(radiate%cosz))     deallocate (radiate%cosz)

   return
   end subroutine


subroutine filltab_radiate(radiate,radiatem,imean,n1,n2,n3,ng)

use var_tables

   implicit none
   type (radiate_vars) :: radiate,radiatem
   integer, intent(in) :: imean,n1,n2,n3,ng
   integer :: npts
   real, pointer :: var,varm

! Fill pointers to arrays into variable tables

   npts=n1*n2*n3

   if (associated(radiate%fthrd))  &
      call vtables2 (radiate%fthrd(1,1,1),radiatem%fthrd(1,1,1)  &
                 ,ng, npts, imean,  &
                 'FTHRD :3:hist:anal:mpti:mpt3')

   npts=n2*n3
   if (associated(radiate%rshort))  &
      call vtables2 (radiate%rshort(1,1),radiatem%rshort(1,1)  &
                 ,ng, npts, imean,  &
                 'RSHORT :2:hist:anal:mpti:mpt3')
   if (associated(radiate%rlong))  &
      call vtables2 (radiate%rlong(1,1),radiatem%rlong(1,1)  &
                 ,ng, npts, imean,  &
                 'RLONG :2:hist:anal:mpti:mpt3')
   if (associated(radiate%rlongup))  &
      call vtables2 (radiate%rlongup(1,1),radiatem%rlongup(1,1)  &
                 ,ng, npts, imean,  &
                 'RLONGUP :2:hist:anal:mpti:mpt3')
   if (associated(radiate%albedt))  &
      call vtables2 (radiate%albedt(1,1),radiatem%albedt(1,1)  &
                 ,ng, npts, imean,  &
                 'ALBEDT :2:hist:anal:mpti:mpt3')
   if (associated(radiate%cosz))  &
      call vtables2 (radiate%cosz(1,1),radiatem%cosz(1,1)  &
                 ,ng, npts, imean,  &
                 'COSZ :2:hist:anal:mpt3')

   return
   end subroutine

End Module mem_radiate
