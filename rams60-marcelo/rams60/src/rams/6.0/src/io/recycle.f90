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

subroutine recycle()

use mem_grid
use mem_leaf
use mem_scratch
use var_tables
use io_params
use an_header

implicit none

character(len=128) :: flnm
integer :: idtype,ng,nvars,lenf,ierr
integer, external :: rams_getvar

flnm=pastfn(1:len_trim(pastfn)-9)

print*,'Reading assimilation fields from analysis file ',pastfn

call rams_read_header(trim(flnm),action='read')

! read the requested analysis file variables

do ng=1,ngrids


   DO nvars=1,num_var(ng)
   
      if(vtab_r(nvars,ng)%irecycle == 1) then

         print*,'Reading assimilation field:',vtab_r(nvars,ng)%name  &
                                ,' for grid:',ng
      
         ierr = RAMS_getvar(vtab_r(nvars,ng)%name,ng  &
             ,scratch%scr1(1),scratch%scr2(1),flnm)
         if(vtab_r(nvars,ng)%idim_type == 4) then
            call unarrange_p(nnxp(ng),nnyp(ng),nzg,npatch  &
                         ,scratch%scr1(1),vtab_r(nvars,ng)%var_p)
         elseif(vtab_r(nvars,ng)%idim_type == 5) then
            call unarrange_p(nnxp(ng),nnyp(ng),nzs,npatch  &
                         ,scratch%scr1(1),vtab_r(nvars,ng)%var_p)
         else
            call atob(vtab_r(nvars,ng)%npts  &
                     ,scratch%scr1(1),vtab_r(nvars,ng)%var_p)
         endif
             
      endif

   enddo

enddo

return
end
