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

#if defined (PROGRAM)

program envget

implicit none

character(len=128) :: var,value
character(len=256) :: fname
character(len=128), external :: fenvget

integer :: numarg
integer, external :: iargc

numarg=iargc()

if(numarg >= 1 .and. numarg <= 2) then
   call getarg(1,var)
else
   print*,'envget usage:  envget env_variable [optional_file_name]'
   stop 'envget: bad args'
endif
   
if(numarg == 2) call getarg(2,fname)
print*,'var:',numarg,trim(var)
print*,'fname:',trim(fname)

if(numarg == 1) value = fenvget(var)
if(numarg == 2) value = fenvget(var,fname)
print*,'value:',trim(value)
write(*,'(a)') trim(adjustl(value))

end

#endif

!-----------------------------------------------------------------------

character(len=128) function fenvget(var,fname)

implicit none

character(len=*) :: var
character(len=*), optional :: fname

character(len=128) :: value
character(len=256) :: efn,line,line2
logical :: there
integer :: ic

fenvget='NULL'

! First check for an environmental variable 
call getenv(trim(var),value)
print*,'var:',trim(var)
print*,'value:',trim(value)

print*,'fname:',trim(fname)
if (value /= ' ') then
   fenvget=value
   return
endif

! See if a file of env variables is specified. If so, look in there.
if(present(fname)) then
   efn=fname
   inquire(file=efn, exist=there)
   if(.not.there) then
      ! Last check: see if an env variable pointing to a file of env variables 
      !              is specified. 
      call getenv(trim(fname),efn)
print*,'fname2:',trim(efn)
      inquire(file=efn, exist=there)
      if(.not.there) then
         print*,'fenvget: fname:',trim(fname)
         print*,'        can''t find file or path file'
         stop 'fenvget: bad fname'
      endif
   endif
   
   ! Found a file, now look for var
   open(11,file=efn,status='old')
   
   do while(.true.)
      read(11,'(a)',end=100) line2
      line=adjustl(line2)
      ! check for comment
      if(line(1:1) == '#') cycle
      
      if (index(line,trim(var)) > 0) then
         ! found one
         line2=line(index(line,trim(var)):)
         ! value will be after = sign. If no =, then after space.
         ic=index(line2,'=')
         if(ic == 0) ic=index(line2,' ')
         ic=ic+1
         value=line2(ic:)
         fenvget=adjustl(value)
         return
      endif
   enddo
   100 continue
endif

return
end




















