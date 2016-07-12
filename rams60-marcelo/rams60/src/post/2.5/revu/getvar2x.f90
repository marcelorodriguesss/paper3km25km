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

implicit none
include 'interface.h'
real :: a(*),b(*)
integer :: itype,ngrd
character*(*) flnm,cgrid*1,flng*128,errmsg*120,string
logical there
integer :: ierr_getvar,ifound,ni,npts,iword
common /getvar/ierr_getvar,ifound

do ni=1,nvbtab

   if(string.eq.anal_table(ni)%string.and.ngrd.eq.anal_table(ni)%ngrid) then
   
      write(cgrid,'(i1)') ngrd
     ! print*,flnm(1:len_trim(flnm))
     ! print*,cgrid
      flng=flnm(1:len_trim(flnm))//'-g'//cgrid//'.vfm'
     ! print*,flng
      print*,' C_open - ',trim(flng),' ',string
      flng=flng(1:len_trim(flng))//char(0)

      inquire(file=flng,exist=there)
      if(.not.there) then
         errmsg='File not found - '//flng
         call error_mess(errmsg)
         return
      endif

      npts=anal_table(ni)%nvalues
      itype=anal_table(ni)%idim_type
      iword=anal_table(ni)%npointer

      call RAMS_c_open(flng,'r'//char(0))
      call vfirecr(10,a,npts,'LIN',b,iword)
      call RAMS_c_close()

      RAMS_getvar=0
      ifound=ifound+1
      return

   endif
enddo

errmsg=string//' not available on grid '//cgrid
call error_mess(errmsg)
RAMS_getvar=1
ierr_getvar=1

return
end
