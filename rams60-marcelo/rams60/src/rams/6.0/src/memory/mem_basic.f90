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


Module mem_basic

   Type basic_vars
   
      ! Variables to be dimensioned by (nzp,nxp,nyp)
   real, pointer, dimension(:,:,:) :: &
                          up,uc,vp,vc,wp,wc,pp,pc  &
                         ,rv,theta,thp,rtp &
                         ,pi0,th0,dn0,dn0u,dn0v
                         
      ! Variables to be dimensioned by (nxp,nyp)
   real, pointer, dimension(:,:) :: &
                          fcoru,fcorv,cputime

   End Type
   
   type (basic_vars), allocatable :: basic_g(:), basicm_g(:)
  
Contains

   subroutine alloc_basic(basic,n1,n2,n3,ng)

   implicit none
   type (basic_vars) :: basic
   integer, intent(in) :: n1,n2,n3,ng

! Allocate arrays based on options (if necessary)

      allocate (basic%up(n1,n2,n3))
      allocate (basic%uc(n1,n2,n3))
      allocate (basic%vp(n1,n2,n3))
      allocate (basic%vc(n1,n2,n3))
      allocate (basic%wp(n1,n2,n3))
      allocate (basic%wc(n1,n2,n3))
      allocate (basic%pp(n1,n2,n3))
      allocate (basic%pc(n1,n2,n3))
      allocate (basic%thp(n1,n2,n3))
      allocate (basic%rtp(n1,n2,n3))
      allocate (basic%rv(n1,n2,n3))
      allocate (basic%theta(n1,n2,n3))
      allocate (basic%pi0(n1,n2,n3))
      allocate (basic%th0(n1,n2,n3))
      allocate (basic%dn0(n1,n2,n3))
      allocate (basic%dn0u(n1,n2,n3))
      allocate (basic%dn0v(n1,n2,n3))
      
      allocate (basic%fcoru(n2,n3))
      allocate (basic%fcorv(n2,n3))
      allocate (basic%cputime(n2,n3))

   return
   end subroutine


   subroutine nullify_basic(basic)

   implicit none
   type (basic_vars) :: basic
   
   if (associated(basic%up   ))    nullify (basic%up   )
   if (associated(basic%uc   ))    nullify (basic%uc   )
   if (associated(basic%vp   ))    nullify (basic%vp   )
   if (associated(basic%vc   ))    nullify (basic%vc   )
   if (associated(basic%wp   ))    nullify (basic%wp   )
   if (associated(basic%wc   ))    nullify (basic%wc   )
   if (associated(basic%pp   ))    nullify (basic%pp   )
   if (associated(basic%pc   ))    nullify (basic%pc   )
   if (associated(basic%thp  ))    nullify (basic%thp  )
   if (associated(basic%rtp  ))    nullify (basic%rtp  )
   if (associated(basic%rv   ))    nullify (basic%rv   )
   if (associated(basic%theta))    nullify (basic%theta)
   if (associated(basic%pi0  ))    nullify (basic%pi0  )
   if (associated(basic%th0  ))    nullify (basic%th0  )
   if (associated(basic%dn0  ))    nullify (basic%dn0  )
   if (associated(basic%dn0u ))    nullify (basic%dn0u )
   if (associated(basic%dn0v ))    nullify (basic%dn0v )
   if (associated(basic%fcoru ))   nullify (basic%fcoru )
   if (associated(basic%fcorv ))   nullify (basic%fcorv )
   if (associated(basic%cputime )) nullify (basic%cputime )

   return
   end subroutine

   subroutine dealloc_basic(basic)

   implicit none
   type (basic_vars) :: basic
   
   if (associated(basic%up   ))    deallocate (basic%up   )
   if (associated(basic%uc   ))    deallocate (basic%uc   )
   if (associated(basic%vp   ))    deallocate (basic%vp   )
   if (associated(basic%vc   ))    deallocate (basic%vc   )
   if (associated(basic%wp   ))    deallocate (basic%wp   )
   if (associated(basic%wc   ))    deallocate (basic%wc   )
   if (associated(basic%pp   ))    deallocate (basic%pp   )
   if (associated(basic%pc   ))    deallocate (basic%pc   )
   if (associated(basic%thp  ))    deallocate (basic%thp  )
   if (associated(basic%rtp  ))    deallocate (basic%rtp  )
   if (associated(basic%rv   ))    deallocate (basic%rv   )
   if (associated(basic%theta))    deallocate (basic%theta)
   if (associated(basic%pi0  ))    deallocate (basic%pi0  )
   if (associated(basic%th0  ))    deallocate (basic%th0  )
   if (associated(basic%dn0  ))    deallocate (basic%dn0  )
   if (associated(basic%dn0u ))    deallocate (basic%dn0u )
   if (associated(basic%dn0v ))    deallocate (basic%dn0v )
   if (associated(basic%fcoru ))   deallocate (basic%fcoru )
   if (associated(basic%fcorv ))   deallocate (basic%fcorv )
   if (associated(basic%cputime )) deallocate (basic%cputime )

   return
   end subroutine


subroutine filltab_basic(basic,basicm,imean,n1,n2,n3,ng)

use var_tables

   implicit none
   type (basic_vars) :: basic,basicm
   integer, intent(in) :: imean,n1,n2,n3,ng
   integer :: npts
   real, pointer :: var,varm

! Fill pointers to arrays into variable tables

   npts=n1*n2*n3
   
   if (associated(basic%up))  &
      call vtables2 (basic%up(1,1,1),basicm%up(1,1,1)  &
                 ,ng, npts, imean,  &
                 'UP :3:hist:anal:mpti:mpt3:mpt2')
   if (associated(basic%vp))  &
      call vtables2 (basic%vp(1,1,1),basicm%vp(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VP :3:hist:anal:mpti:mpt3:mpt2')
   if (associated(basic%wp))  &
      call vtables2 (basic%wp(1,1,1),basicm%wp(1,1,1)  &
                 ,ng, npts, imean,  &
                 'WP :3:hist:anal:mpti:mpt3:mpt2')
   if (associated(basic%pp))  &
      call vtables2 (basic%pp(1,1,1),basicm%pp(1,1,1)  &
                 ,ng, npts, imean,  &
                 'PP :3:hist:anal:mpti:mpt3:mpt2')
   if (associated(basic%uc))  &
      call vtables2 (basic%uc(1,1,1),basicm%uc(1,1,1)  &
                 ,ng, npts, imean,  &
                 'UC :3:hist:mpti:mpt3:mpt2')
   if (associated(basic%vc))  &
      call vtables2 (basic%vc(1,1,1),basicm%vc(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VC :3:hist:mpti:mpt3:mpt2')
   if (associated(basic%wc))  &
      call vtables2 (basic%wc(1,1,1),basicm%wc(1,1,1)  &
                 ,ng, npts, imean,  &
                 'WC :3:hist:mpti:mpt3:mpt2')
   if (associated(basic%pc))  &
      call vtables2 (basic%pc(1,1,1),basicm%pc(1,1,1)  &
                 ,ng, npts, imean,  &
                 'PC :3:hist:mpti:mpt3:mpt2')


   if (associated(basic%thp)) &
      call vtables2 (basic%thp(1,1,1),basicm%thp(1,1,1)  &
                 ,ng, npts, imean,  &
                 'THP :3:hist:mpti:mpt3:mpt1')
   if (associated(basic%rtp)) &
      call vtables2 (basic%rtp(1,1,1),basicm%rtp(1,1,1)  &
                 ,ng, npts, imean,  &
                 'RTP :3:hist:mpti:mpt3:mpt1')

   if (associated(basic%theta)) &
      call vtables2 (basic%theta(1,1,1),basicm%theta(1,1,1)  &
                 ,ng, npts, imean,  &
                 'THETA :3:hist:anal:mpti:mpt3')
   if (associated(basic%rv)) &
      call vtables2 (basic%rv(1,1,1),basicm%rv(1,1,1)  &
                 ,ng, npts, imean,  &
                 'RV :3:hist:anal:mpti:mpt3')
                 
   if (associated(basic%pi0)) &
      call vtables2 (basic%pi0(1,1,1),basicm%pi0(1,1,1)  &
                 ,ng, npts, imean,  &
                 'PI0 :3:mpti')
   if (associated(basic%th0)) &
      call vtables2 (basic%th0(1,1,1),basicm%th0(1,1,1)  &
                 ,ng, npts, imean,  &
                 'TH0 :3:mpti')
   if (associated(basic%dn0)) &
      call vtables2 (basic%dn0(1,1,1),basicm%dn0(1,1,1)  &
                 ,ng, npts, imean,  &
                 'DN0 :3:mpti')
   if (associated(basic%dn0u)) &
      call vtables2 (basic%dn0u(1,1,1),basicm%dn0u(1,1,1)  &
                 ,ng, npts, imean,  &
                 'DN0U :3:mpti')
   if (associated(basic%dn0v)) &
      call vtables2 (basic%dn0v(1,1,1),basicm%dn0v(1,1,1)  &
                 ,ng, npts, imean,  &
                 'DN0V :3:mpti')
                 
   npts=n2*n3
   if (associated(basic%fcoru)) &
      call vtables2 (basic%fcoru(1,1),basicm%fcoru(1,1)  &
                 ,ng, npts, imean,  &
                 'FCORU :2:mpti')      
   if (associated(basic%fcorv)) &
      call vtables2 (basic%fcorv(1,1),basicm%fcorv(1,1)  &
                 ,ng, npts, imean,  &
                 'FCORV :2:mpti')
   if (associated(basic%cputime)) &
      call vtables2 (basic%cputime(1,1),basicm%cputime(1,1)  &
                 ,ng, npts, imean,  &
                 'CPUTIME :2:hist:anal:mpti:mpt3')
 
   return
   end subroutine

End Module mem_basic
