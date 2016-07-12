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


Module mem_scalar
   
   ! Added scalar variables and tendencies

   Type scalar_vars   
      real, pointer, dimension(:,:,:) :: sclp
      real, pointer, dimension(:,:) :: drydep
      real, pointer, dimension(:) :: sclt
   End Type
   
   !    scal_p allocated by (maxsclr,ngrids)
   type (scalar_vars), allocatable :: scalar_g(:,:),scalarm_g(:,:)
  
Contains
!---------------------------------------------------------------

   subroutine alloc_scalar(scal,n1,n2,n3,naddsc)

   implicit none

   type (scalar_vars) :: scal(*)
   integer :: n1,n2,n3
   integer :: naddsc
   
   integer :: nsc

! Allocate arrays based on options (if necessary).

   do nsc=1,naddsc
      allocate (scal(nsc)%sclp(n1,n2,n3))
      allocate (scal(nsc)%drydep(n2,n3))
   enddo

          
   return
   end subroutine

!---------------------------------------------------------------

   subroutine dealloc_scalar(scal,naddsc)

   implicit none

   type (scalar_vars) :: scal(*)

   integer :: naddsc
   integer :: nsc

!  Deallocate arrays

   do nsc=1,naddsc
      if (associated(scal(nsc)%sclp))   deallocate (scal(nsc)%sclp)
      if (associated(scal(nsc)%drydep)) deallocate (scal(nsc)%drydep)
   enddo
           
   return
   end subroutine

!---------------------------------------------------------------

   subroutine nullify_scalar(scal,naddsc)

   implicit none

   type (scalar_vars) :: scal(*)

   integer :: naddsc
   integer :: nsc

!  Deallocate arrays

   do nsc=1,naddsc
      if (associated(scal(nsc)%sclp))   nullify (scal(nsc)%sclp)
      if (associated(scal(nsc)%drydep)) nullify (scal(nsc)%drydep)
   enddo
           
   return
   end subroutine

!---------------------------------------------------------------
               
   subroutine filltab_scalar(scal,scalm,imean,n1,n2,n3,ng,naddsc)

   use var_tables

   implicit none

   type (scalar_vars) :: scal(*),scalm(*)
   integer, intent(in) :: imean,n1,n2,n3,ng,naddsc

   integer :: nsc,npts
   character (len=7) :: sname

! Fill pointers to arrays into variable tables

   do nsc=1,naddsc
      if (associated(scal(nsc)%sclp)) then
         npts=n1*n2*n3
         write(sname,'(a4,i3.3)') 'SCLP',nsc
         call vtables2 (scal(nsc)%sclp(1,1,1),scalm(nsc)%sclp(1,1,1)  &
                 ,ng, npts, imean,  &
                 sname//' :3:hist:anal:mpti:mpt3:mpt1')
         npts=n2*n3
         write(sname,'(a4,i3.3)') 'SCDD',nsc
         call vtables2 (scal(nsc)%drydep(1,1),scalm(nsc)%drydep(1,1)  &
                 ,ng, npts, imean,  &
                 sname//' :2:hist:anal:mpti:mpt3:mpt1')
      endif
   enddo
    
   return
   end subroutine

End Module mem_scalar
