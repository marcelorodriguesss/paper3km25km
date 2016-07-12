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


subroutine rams_mem_alloc(proc_type)

use mem_all
use node_mod

implicit none 

integer :: proc_type

integer, pointer :: nmzp(:),nmxp(:),nmyp(:)
integer :: ng,nv,imean,ntpts

! First, depending on type of process, define grid point pointers correctly...

print*,' %% mem_alloc %%:',proc_type,mynum

if (proc_type == 0 .or. proc_type == 1) then
   !  This is the call for either a single processor run or
   !    for the master process
   nmzp => nnzp
   nmxp => nnxp
   nmyp => nnyp
elseif (proc_type == 2) then
   !  This is the call for an initial compute node process
   nmzp => mmzp
   nmxp => mmxp
   nmyp => mmyp
elseif (proc_type == 3) then
   !  This is the call for a dynamic balance compute node process
   nmzp => mmzp
   nmxp => mmxp
   nmyp => mmyp

   call dealloc_all()

endif

!  If we are doing time-averaging for output, set flag ...
imean=0
if (avgtim /= 0.) imean=1

! Allocate universal variable tables

allocate (num_var(maxgrds))
allocate (vtab_r(maxvars,maxgrds))

num_var(1:maxgrds)=0
nvgrids=ngrids

! Allocate scalar table  

allocate(num_scalar(maxgrds))
allocate(scalar_tab(maxsclr,maxgrds))
num_scalar(1:maxgrds)=0

! Allocate Basic variables data type
print*,'start basic alloc'
allocate(basic_g(ngrids),basicm_g(ngrids))
do ng=1,ngrids
   call nullify_basic(basic_g(ng)) ; call nullify_basic(basicm_g(ng))
   call alloc_basic(basic_g(ng),nmzp(ng),nmxp(ng),nmyp(ng),ng) 
   if (imean == 1) then  
      call alloc_basic(basicm_g(ng),nmzp(ng),nmxp(ng),nmyp(ng),ng)
   elseif (imean == 0) then
      call alloc_basic(basicm_g(ng),1,1,1,ng)
   endif
   
   call filltab_basic(basic_g(ng),basicm_g(ng),imean  &
          ,nmzp(ng),nmxp(ng),nmyp(ng),ng) 
enddo

! Allocate Cuparm variables data type
print*,'start cuparm alloc'
allocate(cuparm_g(ngrids),cuparmm_g(ngrids))
do ng=1,ngrids
   call nullify_cuparm(cuparm_g(ng)) ; call nullify_cuparm(cuparmm_g(ng))
   call alloc_cuparm(cuparm_g(ng),nmzp(ng),nmxp(ng),nmyp(ng),ng) 
   if (imean == 1) then  
      call alloc_cuparm(cuparmm_g(ng),nmzp(ng),nmxp(ng),nmyp(ng),ng)
   elseif (imean == 0) then
      call alloc_cuparm(cuparmm_g(ng),1,1,1,ng)
   endif
   
   call filltab_cuparm(cuparm_g(ng),cuparmm_g(ng),imean  &
          ,nmzp(ng),nmxp(ng),nmyp(ng),ng) 
enddo

! Allocate Leaf type

print*,'start leaf alloc'
allocate(leaf_g(ngrids),leafm_g(ngrids))
do ng=1,ngrids
   call nullify_leaf(leaf_g(ng)) ; call nullify_leaf(leafm_g(ng))
   call alloc_leaf(leaf_g(ng),nmzp(ng),nmxp(ng),nmyp(ng)  &
       ,nzg,nzs,npatch,ng) 
   if (imean == 1) then  
      call alloc_leaf(leafm_g(ng),nmzp(ng),nmxp(ng),nmyp(ng)  &
          ,nzg,nzs,npatch,ng)
   elseif (imean == 0) then
      call alloc_leaf(leafm_g(ng),1,1,1,1,1,1,1)
   endif
   
   call filltab_leaf(leaf_g(ng),leafm_g(ng),imean  &
          ,nmzp(ng),nmxp(ng),nmyp(ng),nzg,nzs,npatch,ng) 
enddo
! Bob (1/10/2002) added the following line.  Is this the right place for
! the long term??
call alloc_leafcol(nzg,nzs)

! Allocate Micro variables data type
print*,'start micro alloc'
allocate(micro_g(ngrids),microm_g(ngrids))
do ng=1,ngrids
   call nullify_micro(micro_g(ng)) ; call nullify_micro(microm_g(ng))
   call alloc_micro(micro_g(ng),nmzp(ng),nmxp(ng),nmyp(ng),ng) 
   if (imean == 1) then  
      call alloc_micro(microm_g(ng),nmzp(ng),nmxp(ng),nmyp(ng),ng)
   elseif (imean == 0) then
      call alloc_micro(microm_g(ng),1,1,1,ng)
   endif
   
   call filltab_micro(micro_g(ng),microm_g(ng),imean  &
          ,nmzp(ng),nmxp(ng),nmyp(ng),ng) 
enddo

! Allocate radiate variables data type
print*,'start radiate alloc'
allocate(radiate_g(ngrids),radiatem_g(ngrids))
do ng=1,ngrids
   call nullify_radiate(radiate_g(ng)) ; call nullify_radiate(radiatem_g(ng))
   call alloc_radiate(radiate_g(ng),nmzp(ng),nmxp(ng),nmyp(ng),ng) 
   if (imean == 1) then  
      call alloc_radiate(radiatem_g(ng),nmzp(ng),nmxp(ng),nmyp(ng),ng)
   elseif (imean == 0) then
      call alloc_radiate(radiatem_g(ng),1,1,1,ng)
   endif
   
   call filltab_radiate(radiate_g(ng),radiatem_g(ng),imean  &
          ,nmzp(ng),nmxp(ng),nmyp(ng),ng) 
enddo

! Allocate turb variables data type
print*,'start turb alloc'
allocate(turb_g(ngrids),turbm_g(ngrids))
do ng=1,ngrids
   call nullify_turb(turb_g(ng)) ; call nullify_turb(turbm_g(ng))
   call alloc_turb(turb_g(ng),nmzp(ng),nmxp(ng),nmyp(ng),ng) 
   if (imean == 1) then  
      call alloc_turb(turbm_g(ng),nmzp(ng),nmxp(ng),nmyp(ng),ng)
   elseif (imean == 0) then
      call alloc_turb(turbm_g(ng),1,1,1,ng)
   endif
   
   call filltab_turb(turb_g(ng),turbm_g(ng),imean  &
          ,nmzp(ng),nmxp(ng),nmyp(ng),ng) 
enddo

! Allocate varinit variables data type. 
!    These do not need "mean" type ever.
print*,'start varinit alloc'
allocate(varinit_g(ngrids),varinitm_g(ngrids))
do ng=1,ngrids
   call nullify_varinit(varinit_g(ng)) ; call nullify_varinit(varinitm_g(ng))
   call alloc_varinit(varinit_g(ng),nmzp(ng),nmxp(ng),nmyp(ng),ng) 
   call alloc_varinit(varinitm_g(ng),1,1,1,ng)
   
   call filltab_varinit(varinit_g(ng),varinitm_g(ng),0  &
          ,nmzp(ng),nmxp(ng),nmyp(ng),ng) 
enddo

! Allocate oda variables data type. 
!    These do not need "mean" type ever.
print*,'start oda alloc'
allocate(oda_g(ngrids),odam_g(ngrids))
do ng=1,ngrids
   call nullify_oda(oda_g(ng)) ; call nullify_oda(odam_g(ng))
   call alloc_oda(oda_g(ng),nmzp(ng),nmxp(ng),nmyp(ng),ng,proc_type) 
   call alloc_oda(odam_g(ng),1,1,1,ng,proc_type)
   
   call filltab_oda(oda_g(ng),odam_g(ng),0  &
          ,nmzp(ng),nmxp(ng),nmyp(ng),ng) 
enddo

! Allocate grid variables data type. 

print*,'start grid alloc'
allocate(grid_g(ngrids),gridm_g(ngrids))
do ng=1,ngrids
   call nullify_grid(grid_g(ng)) ; call nullify_grid(gridm_g(ng))
   call alloc_grid(grid_g(ng),nmzp(ng),nmxp(ng),nmyp(ng),ng,if_adap) 
   call alloc_grid(gridm_g(ng),1,1,1,ng,if_adap)
   
   call filltab_grid(grid_g(ng),gridm_g(ng),0  &
          ,nmzp(ng),nmxp(ng),nmyp(ng),ng) 
enddo

! Allocate any added Scalar types 
! NOT ALLOWING DIFFERENT NUMBERS OF SCALARS ON DIFFERENT NESTS

print*,'start scalar alloc'
!   Allocate length 1 of these datatypes by default
allocate(scalar_g(1,ngrids),scalarm_g(1,ngrids))
if (naddsc > 0) then
   ! deallocate datatypes, then re-alloc to correct length
   deallocate(scalar_g,scalarm_g)
   allocate(scalar_g(naddsc,ngrids),scalarm_g(naddsc,ngrids))
   do ng=1,ngrids
      call nullify_scalar(scalar_g(:,ng),naddsc)
      call nullify_scalar(scalarm_g(:,ng),naddsc)
      call alloc_scalar(scalar_g(:,ng),nmzp(ng),nmxp(ng),nmyp(ng)  &
                       ,naddsc)
      if (imean == 1) then  
         call alloc_scalar(scalarm_g(:,ng),nmzp(ng),nmxp(ng),nmyp(ng)  &
                       ,naddsc)
      elseif (imean == 0) then
         call alloc_scalar(scalarm_g(:,ng),1,1,1,naddsc)
      endif
      
      call filltab_scalar(scalar_g(:,ng),scalarm_g(:,ng),imean  &
          ,nmzp(ng),nmxp(ng),nmyp(ng),ng,naddsc)
      
   enddo
endif

! Allocate Tendency data type,  filltab_tendency is responsible 
!   for filling the main atmospheric model variables in the scalar table,
!   so make sure to call any routines that define scalar variables first.

! Assuming same scalars on all grids!!!!!

print*,'start tendency alloc'
call nullify_tend(naddsc)
call alloc_tend(nmzp,nmxp,nmyp,ngrids,naddsc,proc_type)
do ng=1,ngrids
   call filltab_tend(basic_g(ng),micro_g(ng),turb_g(ng)  &
                    ,scalar_g(:,ng),naddsc,ng) 
enddo

! Allocate Scratch data type, This also fills the max's that are needed
!    by nesting stuff.

print*,'start scratch alloc'
call nullify_scratch()
call alloc_scratch(nmzp,nmxp,nmyp,nnzp,nnxp,nnyp,ngrids  &
                  ,nzg,nzs,npatch,proc_type,maxnxp,maxnyp,maxnzp)
call filltab_scratch() 

! Allocate nested boundary interpolation arrays. All grids will be allocated.

print*,'start nestb alloc'
!!if (proc_type == 0 .or. proc_type == 2) then
!! We'll allocate this all the time for now, even though the master process in 
!!   a parallel run doesn't really need this. The arrays appear in a call statement
!!   during the initialization, but are never used.
   do ng=1,ngrids
      if(nxtnest(ng) == 0 ) then
         call alloc_nestb(ng,1,1,1)
      else
         call alloc_nestb(ng,nnxp(ng),nnyp(ng),nnzp(ng))
      endif
   enddo
!!endif

! Set "Lite" variable flags according to namelist input LITE_VARS.

call lite_varset()


! Set ALL variables in the vtab_r variable table to zero by default. These
!  are variables processed in the filltab_* routines with a call to vtables2.
!  This does NOT include scratch arrays, tendencies, or mean arrays.

do ng = 1, ngrids
   do nv = 1,num_var(ng)
      call azero( vtab_r(nv,ng)%npts, vtab_r(nv,ng)%var_p)
   enddo
enddo

print*,'end alloc'

return
end subroutine


!---------------------------------------------------------------------

subroutine dealloc_all()

use mem_all

implicit none

! deallocate all model memory.  Used on dynamic balance

integer :: ng


deallocate(num_var,vtab_r,scalar_tab,num_scalar)

call dealloc_tend(naddsc)
call dealloc_scratch()

do ng=1,ngrids
   call dealloc_basic(basic_g(ng)) 
   call dealloc_basic(basicm_g(ng))
   call dealloc_cuparm(cuparm_g(ng))  
   call dealloc_cuparm(cuparmm_g(ng))
   call dealloc_grid(grid_g(ng))    
   call dealloc_grid(gridm_g(ng))
   call dealloc_leaf(leaf_g(ng))    
   call dealloc_leaf(leafm_g(ng))
   call dealloc_micro(micro_g(ng)) 
   call dealloc_micro(microm_g(ng))
   call dealloc_radiate(radiate_g(ng)) 
   call dealloc_radiate(radiatem_g(ng))
   call dealloc_turb(turb_g(ng)) 
   call dealloc_turb(turbm_g(ng))
   call dealloc_varinit(varinit_g(ng)) 
   call dealloc_varinit(varinitm_g(ng))
   call dealloc_oda(oda_g(ng)) 
   call dealloc_oda(odam_g(ng))
 enddo
deallocate(basic_g,basicm_g)
deallocate(cuparm_g,cuparmm_g)
deallocate(grid_g,gridm_g)
deallocate(leaf_g,leafm_g)
deallocate(micro_g,microm_g)
deallocate(radiate_g,radiatem_g)
deallocate(turb_g,turbm_g)
deallocate(varinit_g,varinitm_g)
deallocate(oda_g,odam_g)

if(allocated(scalar_g)) then
   do ng=1,ngrids
      call dealloc_scalar(scalar_g(:,ng),naddsc)
   enddo   
   deallocate(scalar_g,scalarm_g)
endif

return
end
