!
! Copyright (C) 1991-2005  ; All Rights Reserved ; ATMET, LLC
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

subroutine rams_read_header (flnm,cread)

use an_header

implicit none

character(len=*) :: flnm
character(len=*), optional :: cread

integer :: lenf,nv,ng

if(allocated(anal_table)) deallocate(anal_table)

! open analysis file and read in commons

open(10,file=trim(flnm)//'-head.txt')
read(10,*) nvbtab
allocate (anal_table(nvbtab))
do nv=1,nvbtab
   read(10,*)  anal_table(nv)%string   &
              ,anal_table(nv)%npointer  &
              ,anal_table(nv)%idim_type  &
              ,anal_table(nv)%ngrid  &
              ,anal_table(nv)%nvalues
enddo


if(present(cread)) then
   if(cread == 'y') then
      call commio('ANAL','READ',10)
   endif
else
   call commio('ANAL','READ',10)
endif

close(10)

return
end
