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


Module mem_varinit

   use grid_dims

   ! Memory for varfile, history, and condensate nudging

   Type varinit_vars
   
      ! Variables to be dimensioned by (nzp,nxp,nyp)
   real, pointer, dimension(:,:,:) :: &
                            varup,varvp,varpp,vartp,varrp  &
                           ,varuf,varvf,varpf,vartf,varrf  &
                           ,varwts &
                           ,varrph,varrfh,varcph,varcfh
                           
   End Type
   
   type (varinit_vars), allocatable :: varinit_g(:), varinitm_g(:)


   integer, parameter :: maxnudfiles=500

   integer :: nud_type, nnudfiles, nnudfl, nudlat

   character(len=128), dimension(maxnudfiles) :: fnames_nud
   character(len=14) , dimension(maxnudfiles) :: itotdate_nud
   real, dimension(maxnudfiles) :: nud_times
   character(len=128) :: nud_hfile
   real :: tnudlat,tnudcent,tnudtop,znudtop
   real :: wt_nudge_uv,wt_nudge_th,wt_nudge_pi,wt_nudge_rt
   real :: wt_nudge_grid(maxgrds)
   real :: htime1, htime2
   
   integer :: igrid_match(maxgrds)

   !-------------------------------------------------------------------------------
   integer :: nud_cond, ncondfiles, ncondfl
   character(len=128), dimension(maxnudfiles) :: fnames_cond
   character(len=14) , dimension(maxnudfiles) :: itotdate_cond
   real, dimension(maxnudfiles) :: cond_times
   character(len=128) :: cond_hfile 
   real :: tcond_beg, tcond_end, wt_nudgec_grid(maxgrds),t_nudge_rc
   real :: condtime1, condtime2

   !-------------------------------------------------------------------------------
   character(len=128), dimension(maxnudfiles) :: fnames_varf
   character(len=14) , dimension(maxnudfiles) :: itotdate_varf
   real, dimension(maxnudfiles) :: varf_times
   character(len=128)       :: varfpfx
   real                    :: vtime1,vtime2,vwait1,vwaittot
   integer                 :: nvarffiles,nvarffl
   
   !-------------------------------------------------------------------------------
  
Contains

   subroutine alloc_varinit(varinit,n1,n2,n3,ng)
   
   use mem_grid

   implicit none
   type (varinit_vars) :: varinit
   integer, intent(in) :: n1,n2,n3,ng

! Allocate arrays based on options (if necessary)
      
      if( nud_type >= 1 .or. initial == 2) then
                         allocate (varinit%varup(n1,n2,n3))
                         allocate (varinit%varvp(n1,n2,n3))
                         allocate (varinit%varpp(n1,n2,n3))
                         allocate (varinit%vartp(n1,n2,n3))
                         allocate (varinit%varrp(n1,n2,n3))
                         allocate (varinit%varuf(n1,n2,n3))
                         allocate (varinit%varvf(n1,n2,n3))
                         allocate (varinit%varpf(n1,n2,n3))
                         allocate (varinit%vartf(n1,n2,n3))
                         allocate (varinit%varrf(n1,n2,n3))                      
                         allocate (varinit%varwts(n1,n2,n3))
      endif
      
      if (nud_cond == 1) then
                         allocate (varinit%varcph(n1,n2,n3))
                         allocate (varinit%varcfh(n1,n2,n3))                      
                         allocate (varinit%varrph(n1,n2,n3))
                         allocate (varinit%varrfh(n1,n2,n3))                      
      endif
      
   return
   end subroutine


   subroutine nullify_varinit(varinit)

   implicit none
   type (varinit_vars) :: varinit
   

   if (associated(varinit%varup))     nullify (varinit%varup)
   if (associated(varinit%varvp))     nullify (varinit%varvp)
   if (associated(varinit%varpp))     nullify (varinit%varpp)
   if (associated(varinit%vartp))     nullify (varinit%vartp)
   if (associated(varinit%varrp))     nullify (varinit%varrp)
   if (associated(varinit%varuf))     nullify (varinit%varuf)
   if (associated(varinit%varvf))     nullify (varinit%varvf)
   if (associated(varinit%varpf))     nullify (varinit%varpf)
   if (associated(varinit%vartf))     nullify (varinit%vartf)
   if (associated(varinit%varrf))     nullify (varinit%varrf)
   if (associated(varinit%varwts))    nullify (varinit%varwts)

   if (associated(varinit%varcph))     nullify (varinit%varcph)
   if (associated(varinit%varcfh))     nullify (varinit%varcfh)
   if (associated(varinit%varrph))     nullify (varinit%varrph)
   if (associated(varinit%varrfh))     nullify (varinit%varrfh)

   return
   end subroutine

   subroutine dealloc_varinit(varinit)

   implicit none
   type (varinit_vars) :: varinit
   

   if (associated(varinit%varup))     deallocate (varinit%varup)
   if (associated(varinit%varvp))     deallocate (varinit%varvp)
   if (associated(varinit%varpp))     deallocate (varinit%varpp)
   if (associated(varinit%vartp))     deallocate (varinit%vartp)
   if (associated(varinit%varrp))     deallocate (varinit%varrp)
   if (associated(varinit%varuf))     deallocate (varinit%varuf)
   if (associated(varinit%varvf))     deallocate (varinit%varvf)
   if (associated(varinit%varpf))     deallocate (varinit%varpf)
   if (associated(varinit%vartf))     deallocate (varinit%vartf)
   if (associated(varinit%varrf))     deallocate (varinit%varrf)
   if (associated(varinit%varwts))    deallocate (varinit%varwts)

   if (associated(varinit%varcph))     deallocate (varinit%varcph)
   if (associated(varinit%varcfh))     deallocate (varinit%varcfh)
   if (associated(varinit%varrph))     deallocate (varinit%varrph)
   if (associated(varinit%varrfh))     deallocate (varinit%varrfh)

   return
   end subroutine


subroutine filltab_varinit(varinit,varinitm,imean,n1,n2,n3,ng)

use var_tables

   implicit none
   type (varinit_vars) :: varinit,varinitm
   integer, intent(in) :: imean,n1,n2,n3,ng
   integer :: npts
   real, pointer :: var,varm

! Fill pointers to arrays into variable tables

   npts=n1*n2*n3

   if (associated(varinit%varup))  &
      call vtables2 (varinit%varup(1,1,1),varinitm%varup(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VARUP :3:mpti')
   if (associated(varinit%varvp))  &
      call vtables2 (varinit%varvp(1,1,1),varinitm%varvp(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VARVP :3:mpti')
   if (associated(varinit%varpp))  &
      call vtables2 (varinit%varpp(1,1,1),varinitm%varpp(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VARPP :3:mpti')
   if (associated(varinit%vartp))  &
      call vtables2 (varinit%vartp(1,1,1),varinitm%vartp(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VARTP :3:mpti')
   if (associated(varinit%varrp))  &
      call vtables2 (varinit%varrp(1,1,1),varinitm%varrp(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VARRP :3:mpti')
   if (associated(varinit%varuf))  &
      call vtables2 (varinit%varuf(1,1,1),varinitm%varuf(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VARUF :3:mpti')
   if (associated(varinit%varvf))  &
      call vtables2 (varinit%varvf(1,1,1),varinitm%varvf(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VARVF :3:mpti')
   if (associated(varinit%varpf))  &
      call vtables2 (varinit%varpf(1,1,1),varinitm%varpf(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VARPF :3:mpti')
   if (associated(varinit%vartf))  &
      call vtables2 (varinit%vartf(1,1,1),varinitm%vartf(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VARTF :3:mpti')
   if (associated(varinit%varrf))  &
      call vtables2 (varinit%varrf(1,1,1),varinitm%varrf(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VARRF :3:mpti')
   if (associated(varinit%varwts))  &
      call vtables2 (varinit%varwts(1,1,1),varinitm%varwts(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VARWTS :3:mpti')

   if (associated(varinit%varcph))  &
      call vtables2 (varinit%varcph(1,1,1),varinitm%varcph(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VARCPH :3:mpti')
   if (associated(varinit%varcfh))  &
      call vtables2 (varinit%varcfh(1,1,1),varinitm%varcfh(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VARCFH :3:mpti')
   if (associated(varinit%varrph))  &
      call vtables2 (varinit%varrph(1,1,1),varinitm%varrph(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VARRPH :3:mpti')
   if (associated(varinit%varrfh))  &
      call vtables2 (varinit%varrfh(1,1,1),varinitm%varrfh(1,1,1)  &
                 ,ng, npts, imean,  &
                 'VARRFH :3:mpti')
                 
   return
   end subroutine

End Module mem_varinit
