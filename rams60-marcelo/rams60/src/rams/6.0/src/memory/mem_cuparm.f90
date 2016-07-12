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


Module mem_cuparm

use grid_dims

   Type cuparm_vars
   
      ! Variables to be dimensioned by (nzp,nxp,nyp)
   real, pointer, dimension(:,:,:) :: &
                          thsrc,rtsrc,thsrcf,rtsrcf,thsrcp,rtsrcp &
                         ,rcsrc,rrsrc,rssrc,rpsrc,w0avg,w0avglt

      ! Variables to be dimensioned by (nxp,nyp)
   real, pointer, dimension(:,:) :: &
                          aconpr,conprr,conprrp,conprrf &
                         ,nca,convgo
   

   End Type

   type (cuparm_vars), allocatable :: cuparm_g(:), cuparmm_g(:)


   integer, parameter :: maxcufiles=100, maxcugrids=10

   integer :: if_cuinv
   real :: tcu_beg, tcu_end, cu_til, cu_tel, tnudcu, wt_cu_grid(maxcugrids)
   character(len=128) :: cu_prefix
   
   character(len=128), dimension(maxcufiles) :: fnames_cu
   character(len=14) , dimension(maxcufiles) :: itotdate_cu
   real, dimension(maxcufiles) :: cu_times

   integer :: ncufiles, ncufl
   real :: cutime1,cutime2
   
   integer, dimension(maxgrds) :: nnqparm
   real :: wcldbs,confrq
  
Contains

   subroutine alloc_cuparm(cuparm,n1,n2,n3,ng)

   implicit none
   type (cuparm_vars) :: cuparm
   integer, intent(in) :: n1,n2,n3,ng

! Allocate arrays based on options (if necessary)
      
      if( nnqparm(ng)>= 1 .or. if_cuinv == 1)  then
                         allocate (cuparm%thsrc(n1,n2,n3))
                         allocate (cuparm%rtsrc(n1,n2,n3))
                         allocate (cuparm%aconpr(n2,n3))
                         allocate (cuparm%conprr(n2,n3))
                         if (nnqparm(ng) == 2) then
                            allocate (cuparm%rcsrc(n1,n2,n3))
                            allocate (cuparm%rrsrc(n1,n2,n3))
                            allocate (cuparm%rssrc(n1,n2,n3))
                            allocate (cuparm%rpsrc(n1,n2,n3))
                            allocate (cuparm%w0avg(n1,n2,n3))
                            allocate (cuparm%w0avglt(n1,n2,n3))
                            allocate (cuparm%nca(n2,n3))
                            allocate (cuparm%convgo(n2,n3))
                         endif
                           
                         
                         if (if_cuinv == 1) then
                            allocate (cuparm%thsrcp(n1,n2,n3))
                            allocate (cuparm%rtsrcp(n1,n2,n3))
                            allocate (cuparm%thsrcf(n1,n2,n3))
                            allocate (cuparm%rtsrcf(n1,n2,n3))
                            allocate (cuparm%conprrp(n2,n3))
                            allocate (cuparm%conprrf(n2,n3))
                         endif
      endif
                         
   return
   end subroutine


   subroutine nullify_cuparm(cuparm)

   implicit none
   type (cuparm_vars) :: cuparm
   

   if (associated(cuparm%thsrc))    nullify (cuparm%thsrc)
   if (associated(cuparm%rtsrc))    nullify (cuparm%rtsrc)
   if (associated(cuparm%thsrcp))    nullify (cuparm%thsrcp)
   if (associated(cuparm%rtsrcp))    nullify (cuparm%rtsrcp)
   if (associated(cuparm%thsrcf))    nullify (cuparm%thsrcf)
   if (associated(cuparm%rtsrcf))    nullify (cuparm%rtsrcf)
   if (associated(cuparm%aconpr))   nullify (cuparm%aconpr)
   if (associated(cuparm%conprr))   nullify (cuparm%conprr)
   if (associated(cuparm%conprrp))   nullify (cuparm%conprrp)
   if (associated(cuparm%conprrf))   nullify (cuparm%conprrf)

   if (associated(cuparm%rcsrc)  )   nullify (cuparm%rcsrc)
   if (associated(cuparm%rrsrc)  )   nullify (cuparm%rrsrc)
   if (associated(cuparm%rssrc)  )   nullify (cuparm%rssrc)
   if (associated(cuparm%rpsrc)  )   nullify (cuparm%rpsrc)
   if (associated(cuparm%w0avg)  )   nullify (cuparm%w0avg)
   if (associated(cuparm%w0avglt))   nullify (cuparm%w0avglt)
   if (associated(cuparm%nca)    )   nullify (cuparm%nca)
   if (associated(cuparm%convgo) )   nullify (cuparm%convgo)
   
   return
   end subroutine

   subroutine dealloc_cuparm(cuparm)

   implicit none
   type (cuparm_vars) :: cuparm
   

   if (associated(cuparm%thsrc))    deallocate (cuparm%thsrc)
   if (associated(cuparm%rtsrc))    deallocate (cuparm%rtsrc)
   if (associated(cuparm%thsrcp))    deallocate (cuparm%thsrcp)
   if (associated(cuparm%rtsrcp))    deallocate (cuparm%rtsrcp)
   if (associated(cuparm%thsrcf))    deallocate (cuparm%thsrcf)
   if (associated(cuparm%rtsrcf))    deallocate (cuparm%rtsrcf)
   if (associated(cuparm%aconpr))   deallocate (cuparm%aconpr)
   if (associated(cuparm%conprr))   deallocate (cuparm%conprr)
   if (associated(cuparm%conprrp))   deallocate (cuparm%conprrp)
   if (associated(cuparm%conprrf))   deallocate (cuparm%conprrf)

   if (associated(cuparm%rcsrc)  )   deallocate (cuparm%rcsrc)
   if (associated(cuparm%rrsrc)  )   deallocate (cuparm%rrsrc)
   if (associated(cuparm%rssrc)  )   deallocate (cuparm%rssrc)
   if (associated(cuparm%rpsrc)  )   deallocate (cuparm%rpsrc)
   if (associated(cuparm%w0avg)  )   deallocate (cuparm%w0avg)
   if (associated(cuparm%w0avglt))   deallocate (cuparm%w0avglt)
   if (associated(cuparm%nca)    )   deallocate (cuparm%nca)
   if (associated(cuparm%convgo) )   deallocate (cuparm%convgo)

   return
   end subroutine


subroutine filltab_cuparm(cuparm,cuparmm,imean,n1,n2,n3,ng)

use var_tables

   implicit none
   type (cuparm_vars) :: cuparm,cuparmm
   integer, intent(in) :: imean,n1,n2,n3,ng
   integer :: npts
   real, pointer :: var,varm

! Fill pointers to arrays into variable tables

   npts=n1*n2*n3

   if (associated(cuparm%thsrc))  &
      call vtables2 (cuparm%thsrc(1,1,1),cuparmm%thsrc(1,1,1)  &
                 ,ng, npts, imean,  &
                 'THSRC :3:hist:anal:mpti:mpt3')
   if (associated(cuparm%rtsrc))  &
      call vtables2 (cuparm%rtsrc(1,1,1),cuparmm%rtsrc(1,1,1)  &
                 ,ng, npts, imean,  &
                 'RTSRC :3:hist:anal:mpti:mpt3')
   if (associated(cuparm%thsrcp))  &
      call vtables2 (cuparm%thsrcp(1,1,1),cuparmm%thsrcp(1,1,1)  &
                 ,ng, npts, imean,  &
                 'THSRCP :3:mpti:')
   if (associated(cuparm%rtsrcp))  &
      call vtables2 (cuparm%rtsrcp(1,1,1),cuparmm%rtsrcp(1,1,1)  &
                 ,ng, npts, imean,  &
                 'RTSRCP :3:mpti:')
   if (associated(cuparm%thsrcf))  &
      call vtables2 (cuparm%thsrcf(1,1,1),cuparmm%thsrcf(1,1,1)  &
                 ,ng, npts, imean,  &
                 'THSRCF :3:mpti:')
   if (associated(cuparm%rtsrcf))  &
      call vtables2 (cuparm%rtsrcf(1,1,1),cuparmm%rtsrcf(1,1,1)  &
                 ,ng, npts, imean,  &
                 'RTSRCF :3:mpti:')

   npts=n2*n3
   if (associated(cuparm%aconpr))  &
      call vtables2 (cuparm%aconpr(1,1),cuparmm%aconpr(1,1)  &
                 ,ng, npts, imean,  &
                 'ACONPR :2:hist:anal:mpti:mpt3')
   if (associated(cuparm%conprr))  &
      call vtables2 (cuparm%conprr(1,1),cuparmm%conprr(1,1)  &
                 ,ng, npts, imean,  &
                 'CONPRR :2:hist:anal:mpt3')
   if (associated(cuparm%conprrp))  &
      call vtables2 (cuparm%conprrp(1,1),cuparmm%conprrp(1,1)  &
                 ,ng, npts, imean,  &
                 'CONPRRP :2:mpti')
   if (associated(cuparm%conprrf))  &
      call vtables2 (cuparm%conprrf(1,1),cuparmm%conprrf(1,1)  &
                 ,ng, npts, imean,  &
                 'CONPRRF :2:mpti')

   npts=n1*n2*n3
   if (associated(cuparm%rcsrc)  )  &   
      call vtables2 (cuparm%rcsrc(1,1,1),cuparmm%rcsrc(1,1,1)  &
                 ,ng, npts, imean,  &
                 'RCSRC :3:hist:anal:mpti:mpt3')
   if (associated(cuparm%rrsrc)  )  &   
      call vtables2 (cuparm%rrsrc(1,1,1),cuparmm%rrsrc(1,1,1)  &
                 ,ng, npts, imean,  &
                 'RRSRC :3:hist:anal:mpti:mpt3')
   if (associated(cuparm%rssrc)  )  &   
      call vtables2 (cuparm%rssrc(1,1,1),cuparmm%rssrc(1,1,1)  &
                 ,ng, npts, imean,  &
                 'RSSRC :3:hist:anal:mpti:mpt3')
   if (associated(cuparm%rpsrc)  )  &   
      call vtables2 (cuparm%rpsrc(1,1,1),cuparmm%rpsrc(1,1,1)  &
                 ,ng, npts, imean,  &
                 'RPSRC :3:hist:anal:mpti:mpt3')
   if (associated(cuparm%w0avg)  )  &   
      call vtables2 (cuparm%w0avg(1,1,1),cuparmm%w0avg(1,1,1)  &
                 ,ng, npts, imean,  &
                 'W0AVG :3:hist:anal:mpti:mpt3')
   if (associated(cuparm%w0avglt))  &   
      call vtables2 (cuparm%w0avglt(1,1,1),cuparmm%w0avglt(1,1,1)  &
                 ,ng, npts, imean,  &
                 'W0AVGLT :3:hist:anal:mpti:mpt3')
   npts=n2*n3
   if (associated(cuparm%nca)    )   &  
      call vtables2 (cuparm%nca(1,1),cuparmm%nca(1,1)  &
                 ,ng, npts, imean,  &
                 'NCA :2:hist:anal:mpti:mpt3')
   if (associated(cuparm%convgo) )  &   
      call vtables2 (cuparm%convgo(1,1),cuparmm%convgo(1,1)  &
                 ,ng, npts, imean,  &
                 'CONVGO :2:hist:anal:mpti:mpt3')
                 
   return
   end subroutine

End Module mem_cuparm
