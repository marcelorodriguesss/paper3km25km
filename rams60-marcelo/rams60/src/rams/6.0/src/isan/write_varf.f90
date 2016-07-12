!
! Copyright (C) 1991-2003  ; All Rights Reserved ; Colorado State University
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

subroutine write_varf()

use isan_coms
use mem_grid
use io_params

use hdf5_utils

implicit none

integer :: ng,ndims,idims(4),isan_file_ver
character(len=256) :: locfn
character(len=3) :: csuff


do ng=1,nigrids
   nxyzp=nnxp(ng)*nnyp(ng)*nnzp(ng)
   nxyp =nnxp(ng)*nnyp(ng)
   write(csuff,'(a1,i1)') 'g',ng

   call makefnam (locfn,varpfx,0,iyear,imonth,idate  &
                 ,ihour*100,'V',csuff,'h5 ')
 
   call shdf5_open(locfn,'W',iclobber)

   isan_file_ver=3
   ndims=1 ; idims(1)=1
   call shdf5_orec(ndims,idims,'version',ivars=isan_file_ver)
   call shdf5_orec(ndims,idims,'year'   ,ivars=iyear)     
   call shdf5_orec(ndims,idims,'month'  ,ivars=imonth)    
   call shdf5_orec(ndims,idims,'day'    ,ivars=idate)     
   call shdf5_orec(ndims,idims,'hour'   ,ivars=ihour)     
   call shdf5_orec(ndims,idims,'nx'     ,ivars=nnxp(ng))  
   call shdf5_orec(ndims,idims,'ny'     ,ivars=nnyp(ng))  
   call shdf5_orec(ndims,idims,'nz'     ,ivars=nnzp(ng))  
   call shdf5_orec(ndims,idims,'polelat',rvars=platn(ng))        
   call shdf5_orec(ndims,idims,'polelon',rvars=plonn(ng))       
   call shdf5_orec(ndims,idims,'dx'     ,rvars=deltaxn(ng))    
   call shdf5_orec(ndims,idims,'dy'     ,rvars=deltayn(ng))     
   call shdf5_orec(ndims,idims,'dz'     ,rvars=deltazn(ng))     
   call shdf5_orec(ndims,idims,'dzrat'  ,rvars=dzrat)     
   call shdf5_orec(ndims,idims,'dzmax'  ,rvars=dzmax)

   ndims=3 ; idims(1)=nnxp(ng); idims(2)=nnyp(ng); idims(3)=nnzp(ng)
   call rearrange(nnzp(ng),nnxp(ng),nnyp(ng),is_grids(ng)%rr_u,rr_scr3)
   call shdf5_orec(ndims,idims,'UP',rvara=rr_scr3)
   call rearrange(nnzp(ng),nnxp(ng),nnyp(ng),is_grids(ng)%rr_v,rr_scr3)
   call shdf5_orec(ndims,idims,'VP',rvara=rr_scr3)
   call rearrange(nnzp(ng),nnxp(ng),nnyp(ng),is_grids(ng)%rr_p,rr_scr3)
   call shdf5_orec(ndims,idims,'PI',rvara=rr_scr3)
   call rearrange(nnzp(ng),nnxp(ng),nnyp(ng),is_grids(ng)%rr_t,rr_scr3)
   call shdf5_orec(ndims,idims,'THETA',rvara=rr_scr3)
   call rearrange(nnzp(ng),nnxp(ng),nnyp(ng),is_grids(ng)%rr_r,rr_scr3)
   call shdf5_orec(ndims,idims,'RV',rvara=rr_scr3)

   ndims=2 ; idims(1)=nnxp(ng); idims(2)=nnyp(ng)
   call vmissw(is_grids(ng)%rr_slp(1,1),nxyp,rr_vt2da(1),1E30,-9999.)
   call shdf5_orec(ndims,idims,'SL_PRESS',rvara=rr_vt2da)
   call vmissw(is_grids(ng)%rr_sfp(1,1),nxyp,rr_vt2da(1),1E30,-9999.)
   call shdf5_orec(ndims,idims,'SFC_PRESS',rvara=rr_vt2da)
   call vmissw(is_grids(ng)%rr_sft(1,1),nxyp,rr_vt2da(1),1E30,-9999.)
   call shdf5_orec(ndims,idims,'SFC_TEMP',rvara=rr_vt2da)
   call vmissw(is_grids(ng)%rr_snow(1,1),nxyp,rr_vt2da(1),1E30,-9999.)
   call shdf5_orec(ndims,idims,'SNOW_MASS',rvara=rr_vt2da)
   call vmissw(is_grids(ng)%rr_sst(1,1),nxyp,rr_vt2da(1),1E30,-9999.)
   call shdf5_orec(ndims,idims,'SEATP',rvara=rr_vt2da)

   call shdf5_close()

enddo
call makefnam (locfn,varpfx,0,iyear,imonth,idate  &
              ,ihour*100,'V','$','tag')
call rams_f_open (2,locfn,'FORMATTED','REPLACE','WRITE',iclobber)
write(2,*) nigrids
close(2)

return
end
