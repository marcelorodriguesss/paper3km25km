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

program main

implicit none

character(len=128) :: name_name
data name_name/'RAMSIN'/
character(len=2) :: cargv
character(len=1024) :: cargs(0:20),cargx

integer :: bad=0, machsize=0,machnum=0
integer :: taskids(512),numarg,nn,icall,i,n,nproc,ipara,hdferr
integer, external :: iargc

print*,'in main',len(cargs)
numarg=iargc()
print*,'numarg:',numarg
do n=0,numarg
   call ugetarg(n,cargx)
   cargs(n)=trim(cargx)//char(0)
   !!print*,'cargs:',n,trim(cargs(n))
enddo

numarg=numarg+1
call par_init_fortran(numarg,cargs,len(cargs),machnum,machsize)
print*,'par size:',machnum,machsize
icall=0
if (machnum .ne. 0) icall=1
nproc=machsize-1
do nn=1,nproc
   taskids(nn)=nn
enddo
!      stop 'test'

!       Parse the command line arguments

i=1
do while (i .le. numarg)
   call getarg(i,cargv)
   print*,'args: ',i,cargv

   if (cargv(1:1) == '-') then
      if(cargv(2:2) == 'f') then

! /*       The name of the input namlist file is:   */
       call getarg(i+1,name_name)
         i=i+2

      else
         print*,'RAMS unknown option: ', cargv
!              bad=bad+1
         i=i+1
      endif

   else
      print*,'RAMS unknown option: ', cargv
!            bad=bad+1
      i=i+1
   endif


enddo


if (bad > 0) then
   print*,'RAMS usage: ''exec name'' '
   print*,'  [-f ''Namelist file''] '
   stop 'bad command line arguments'
endif

ipara=0
if (machsize > 0 ) then
   ipara = 1
   nproc=machsize-1
   if(nproc == 0) ipara=0
endif


print*
print*
print*,'+-----------------------------------------------------'
print*,'! RAMS input namelist file :'  &
         ,trim(name_name)
print*,'! RAMS call (master=0,node=1):', icall
print*,'! Parallel info,machnum,machsize,ipara:', machnum,machsize,ipara
print*,'+-----------------------------------------------------'
print*
print*

if (icall == 0) then

call rams_master (ipara,nproc,taskids,machnum,name_name)

else if (icall == 1) then

   call rams_node (name_name)

endif

end

