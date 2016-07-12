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

! Newer version that just uses ls and C to form unique filenames

subroutine RAMS_filelist (fnames,file_prefix,nfile)
implicit none
integer :: nfile
character(len=*) :: fnames(*),file_prefix

character(len=128) :: file,command,cdir
character(len=30) :: tmpname

integer :: iflag,iprelen,nc,nf,iun,lndir
      
! this version uses nfile as flag for whether to stop if no files exist
! if nfile.ge.0, then stop

iflag=nfile

nfile = 0
print*,'RAMS_filelist: Checking prefix: ',trim(file_prefix)

iprelen=len_trim(file_prefix)
if(iprelen == 0) iprelen=len(file_prefix)
      
#if defined (PC_NT1)

   ! First change all "/" to "\" so same namelist can be used 
   !   for Unix/Linux/Windows
   
   do nc=1,iprelen
      if(file_prefix(nc:nc) == '/') file_prefix(nc:nc)='\'
   enddo

   command=  &
     'dir /b '//file_prefix(1:len_trim(file_prefix))//' >c:\temp\RAMS_filelist'
   call system(command)
 
   ! open the directory list
 
   iun=98
   open(unit=iun,file='c:\temp\RAMS_filelist',status='old',err=15)
 
   ! read through the files
   ! windows doesn't put directory names on "dir", so...

   do nc=len_trim(file_prefix),1,-1
      if(file_prefix(nc:nc).eq.'\') then
         lndir=nc
         cdir=file_prefix(1:lndir)
         goto 25
      endif
   enddo
   lndir=2
   cdir='.\'
   25 continue
 
   do nf=1,1000000
      read(iun,'(a128)',end=30,err=30) file
      fnames(nf) = cdir(1:lndir)//file
   enddo
 
   30 continue

   close(iun)

   command= 'del c:\temp\RAMS_filelist'
   call system(command)
      
#else

   ! Let C determine a unique filename
   tmpname='/tmp/XXXXXX'//char(0)
   call form_tmpname(tmpname)
 
   if(iflag >= 0) then
      command = '/bin/ls -1 '//file_prefix(1:iprelen)//' > '//tmpname
   else
      command = '/bin/ls -1 '//file_prefix(1:iprelen)//' > '//tmpname//' 2>/dev/null'
   endif
   call system(command)
   command = 'chmod 777 '//tmpname
   call system(command)
 
   ! open the directory list and read through the files
 
   iun=98
   open(unit=iun,file=tmpname,status='unknown',err=15)
   rewind iun

   do nf=1,1000000
      read(iun,'(a128)',end=30,err=30) file
      fnames(nf) = file
   enddo
      
   30 continue

   close(iun)

   command= '/bin/rm -f '//tmpname
   call system(command)

#endif

nfile=nf-1

if (nfile == 0) then
   print *, 'No RAMS files for prefix:',file_prefix
   if(iflag >= 0) stop 'RAMS_filelist-no_files'
endif

return 
      
15 print *, 'RAMS_filelist: Error opening temporary RAMS_filelist'
stop 'RAMS_filelist-/tmp file error : run again'
return
      
100 continue

return
end

