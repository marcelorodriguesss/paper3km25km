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
! 2.2.0
!###########################################################################

module an_header

type head_table
   character(len=16) :: string
   integer :: npointer,idim_type,ngrid,nvalues
end type

type (head_table), allocatable :: anal_table(:)
integer :: nvbtab

Contains


subroutine rams_read_header(flnm,action,ldata)

implicit none

character(len=*), optional :: flnm
character(len=*), optional :: action
logical, dimension(*), optional :: ldata

character(len=128) :: flnm2
integer :: lenf,nv


! open analysis file and read in commons
if(action == 'read') then
   if(.not.present(flnm)) then
      print*,'rams_read_header: read action specified, flnm not passed'
      stop 'rams_read_header: no flnm'
   endif

   if(allocated(anal_table)) deallocate(anal_table)
   
   flnm2=flnm
   if(index(flnm2,'-head.txt') == 0) flnm2=trim(flnm)//'-head.txt'
   
   open(10,file=trim(flnm2),status='old')
   read(10,*) nvbtab
   
   allocate (anal_table(nvbtab))
   
   do nv=1,nvbtab
      read(10,*)   anal_table(nv)%string   &
                  ,anal_table(nv)%npointer  &
                  ,anal_table(nv)%idim_type  &
                  ,anal_table(nv)%ngrid  &
                  ,anal_table(nv)%nvalues
   enddo

   close(10)

! Do other optional activities. All based on current file headers

elseif(action == 'grid_inv') then
  
   ! grid_inventory: set logical flags if a grid exists at this time...
   !     ldata array should be initialized to .f. before call
   
   if(.not.present(ldata)) then
      print*,'rams_read_header: grid_inv action specified, ldata not passed'
      stop 'rams_read_header: no ldata'
   endif
   
   do nv=1,nvbtab
      ldata( anal_table(nv)%ngrid ) = .true.
   enddo

else
   print*,'rams_read_header: unknown action:',action
   stop 'rams_read_header: unknow action'
endif


return
end subroutine


end module an_header
