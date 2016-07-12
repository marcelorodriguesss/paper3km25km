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


Module mem_mksfc


   Type sfcfile_vars

!(nxp,nyp,nzg,npatch)
   real, pointer, dimension(:,:,:,:) :: soil_text

!(nxp,nyp,npatch)
   real, pointer, dimension(:,:,:) :: patch_area,leaf_class,veg_ndvif

!(nxp,nyp)
   real, pointer, dimension(:,:) :: topt,seatf,topzo

   End Type
   


   type (sfcfile_vars), allocatable :: sfcfile_p(:)
  
!(nxpmax,nypmax)
   real, allocatable, dimension(:,:) :: scr1,scr2,vt2da,vt2db

!(max(nxp*nyp)*nzg)
   real, allocatable, dimension(:) :: scrx

!(np,np,nxp,nyp)
   real, allocatable, dimension(:,:,:,:) :: glatp,glonp,datp

!(np,np,nxp,nyp)
   integer, allocatable, dimension(:,:,:,:) :: datq_patch

!(np*np*nxp*nyp)
   integer, allocatable, dimension(:) :: ptable

!(iblksizo,iblksizo)
   real, allocatable, dimension(:,:) :: dato

!(iblksizo,iblksizo)
   character(len=1), allocatable, dimension(:,:) :: cdato
   integer, allocatable, dimension(:,:) :: idato

!(ifile_max,jfile_max)
   integer, allocatable, dimension(:,:) :: nump,numpind,numpind1,numpind2

   integer :: npq
   
   integer, parameter :: maxsfcgrids=10
   
   ! SST file creation variables
   integer, parameter :: maxsstdata=100
   integer, dimension(maxsstdata,maxsfcgrids):: iyearvs,imonthvs,idatevs,ihourvs
   integer,dimension(maxsfcgrids)         :: nvsstf
   character(len=128), dimension(maxsstdata,maxsfcgrids)     :: vsstfil
   
   ! NDVI file creation variables
   integer, parameter :: maxndvidata=100
   integer, dimension(maxndvidata,maxsfcgrids):: iyearvn,imonthvn,idatevn,ihourvn
   integer,dimension(maxsfcgrids)         :: nvndvif
   character(len=128), dimension(maxndvidata,maxsfcgrids)     :: vndvifil


Contains

   subroutine alloc_sfcfile(sfcfile,nx,ny,nzg,npat)

   implicit none
   type (sfcfile_vars) :: sfcfile
   integer, intent(in) :: nx,ny,nzg,npat

   print*,'alloc_sfc:',nx,ny,nzg,npat
   allocate (sfcfile%soil_text    (nzg,nx,ny,npat))

   allocate (sfcfile%patch_area   (nx,ny,npat))
   allocate (sfcfile%leaf_class  (nx,ny,npat))
   allocate (sfcfile%veg_ndvif    (nx,ny,npat))

   allocate (sfcfile%topt         (nx,ny))
   allocate (sfcfile%seatf        (nx,ny))
   allocate (sfcfile%topzo        (nx,ny))
      
   return
   end subroutine


   subroutine dealloc_sfcfile(sfcfile)

   implicit none
   type (sfcfile_vars) :: sfcfile

   deallocate (sfcfile%soil_text)

   deallocate (sfcfile%patch_area)
   deallocate (sfcfile%leaf_class)
   deallocate (sfcfile%veg_ndvif)
   
   deallocate (sfcfile%topt)
   deallocate (sfcfile%seatf)
   deallocate (sfcfile%topzo)
   
   return
   end subroutine


End Module mem_mksfc
