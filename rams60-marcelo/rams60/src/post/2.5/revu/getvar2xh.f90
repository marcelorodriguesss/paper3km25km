!
! Copyright (C) 1991-2004  ; All Rights Reserved ; ATMET, LLC
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
! 2.5.0
!###########################################################################

integer function RAMS_getvar (string,itype,ngrd,a,b,flnm)

use an_header
use hdf5_utils

implicit none

include 'interface.h'

real :: a(*),b(*)
integer :: itype,ngrd,rams_c_pos
character(len=*) :: flnm,string
character :: cgrid*1,flng*128,errmsg*120
logical :: there,h5
integer :: ni,npts,npts_chk,iword,n
integer :: ndims,idims(4)
integer :: ierr_getvar,ifound
common /getvar/ierr_getvar,ifound

!print*,'getvar:hhhhhhh:',string

! First see if data file for this grid/time exists...

write(cgrid,'(i1)') ngrd
flng=trim(flnm)//'-g'//cgrid//'.vfm'

inquire(file=flng,exist=there)
h5=.false.
if(.not.there) then
  flng=trim(flnm)//'-g'//cgrid//'.h5'
  inquire(file=flng,exist=there)
  h5=.true.
endif

!print*,'+++++there:',there,trim(flng)
if(.not.there) then
   errmsg='File not found - '//flng
   call error_mess(errmsg)
   RAMS_getvar=2
   return
endif

! Now search table for actual variable

do ni=1,nvbtab
   !print*,ni,anal_table(ni)%string,anal_table(ni)%ngrid
   if(string == anal_table(ni)%string.and.ngrd == anal_table(ni)%ngrid) then
   
      npts=anal_table(ni)%nvalues
      itype=anal_table(ni)%idim_type
      iword=anal_table(ni)%npointer

      if (h5) then
         call shdf5_open(trim(flng),'R')
         !call shdf5_info(string,ndims,idims)
         !npts_chk=product(idims(1:ndims))
 
         !if (npts /= npts_chk) then
         !   print*,'No. of points in anal table and in hdf5 file do not match.'
         !   print*,'   anal field:',string
         !   print*,'   anal table:',npts
         !   print*,'   hdf5 file :',idims(1:ndims)
         !   print*,'   hdf5 file :',npts_chk
         !   stop
         !endif
         call shdf5_irec(trim(string),rvara=a)
         call shdf5_close()
      else
         !print*,'gv:opening'
         call RAMS_c_open(trim(flng)//char(0),'r'//char(0))
         !print*,'gv:opened'
         call vfirecr(10,a,npts,'LIN',b,iword)
         !print*,'gv:vfirecr'
         call RAMS_c_close()
         !print*,'gv:closed'
      endif

      RAMS_getvar=0
print*,'getvar good:',string
      ifound=ifound+1
      return

   endif
enddo

errmsg='Variable not available in this run - '//string
call error_mess(errmsg)
RAMS_getvar=1
ierr_getvar=1

return
end
