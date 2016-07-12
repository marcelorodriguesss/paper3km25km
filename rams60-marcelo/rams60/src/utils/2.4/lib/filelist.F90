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

! Newer version that just uses ls and C to form unique filenames

subroutine RAMS_filelist (fnames,file_prefix,nfile)
implicit none
integer :: nfile
character(len=*) :: fnames(*),file_prefix

character(len=256) :: file,command,cdir,file_pref,chome
character(len=30) :: tmpname
character(len=1000000) :: fstring

integer :: iflag,iprelen,nc,nf,iun,lndir
      
! this version uses nfile as flag for whether to stop if no files exist
! if nfile.ge.0, then stop

iflag=nfile

nfile = 0
print*,'RAMS_filelist: Checking prefix: ',trim(file_prefix)

iprelen=len_trim(file_prefix)
if(iprelen == 0) iprelen=len(file_prefix)

! Process leading shell variables
if (file_prefix(1:5) == '$HOME') then
   call getenv('HOME',chome)
   file_pref=trim(chome)//trim(file_prefix(6:))
else
   file_pref=trim(file_prefix)
endif
 
      
#if defined (PC_NT1)

   ! First change all "/" to "\" so same namelist can be used 
   !   for Unix/Linux/Windows
   
   do nc=1,iprelen
      if(file_prefix(nc:nc) == '/') file_prefix(nc:nc)='\'
   enddo

   command=  &
     'dir /b '//trim(file_pref)//' >c:\temp\RAMS_filelist'
   call system(command)
 
   ! open the directory list
 
   iun=98
   open(unit=iun,file='c:\temp\RAMS_filelist',status='old',err=15)
 
   ! read through the files
   ! windows doesn't put directory names on "dir", so...

   do nc=len_trim(file_pref),1,-1
      if(file_pref(nc:nc).eq.'\') then
         lndir=nc
         cdir=file_pref(1:lndir)
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

   nfile=nf-1
      
#elif defined (SGI)

   ! Let C determine a unique filename
   tmpname='/tmp/XXXXXX'//char(0)
   call form_tmpname(tmpname)
 
   if(iflag >= 0) then
      command = '/bin/ls -1 '//trim(file_pref)//' > '//trim(tmpname)//char(0)
   else
      command = '/bin/ls -1 '//trim(file_pref)//' > ' &
                            //trim(tmpname)//' 2>/dev/null'//char(0)
   endif
   print*,'comm:',trim(command)
   call usystem(trim(command))
 
   ! open the directory list and read through the files
 
   iun=98
   open(unit=iun,file=tmpname,status='unknown',err=15)
   rewind iun

   do nf=1,1000000
   print*,'nf:',nf
      read(iun,'(a128)',end=30,err=30) file
   print*,'file:',trim(file)
      fnames(nf) = file
   enddo
      
   30 continue

   close(iun)
   
   print*,'nf end:',nf
   nfile = nf-1
   
   command= '/bin/rm -f '//trim(tmpname)
   print*,'comm:',trim(command)
   !call usystem(trim(command)//char(0))

#else

 
   ! Call glob'bing routine
   fstring = ' '
   call c_listfile (trim(file_pref)//char(0),fstring)
  ! print*,'ffffs:',nfile,trim(fstring)
   
   ! Parse file name string 
   call tokenize2(fstring,fnames,nfile,':')
   
   !do nc=1,nfile
   !   print*,'flist:',nc,trim(fnames(nc))
   !enddo

#endif


if (nfile == 0) then
   print *, 'No RAMS files for prefix:',trim(file_pref)
   if(iflag >= 0) stop 'RAMS_filelist-no_files'
endif

return 

15 print *, 'RAMS_filelist: Error opening temporary ls file!'
   print *, 'RAMS_filelist: filename:', trim(tmpname)
   command = '/bin/pwd  > /home/tremback/aaa'
   call usystem(command)
   command = '/bin/ls -l '//trim(file_pref)//' 2> /home/tremback/tmp_error.lis'
   call usystem(command)
   print *, 'RAMS_filelist: /tmp list saved as local file: tmp_error.lis'
   print *, 'RAMS_filelist: command:',trim(command)
   stop 'RAMS_filelist-/tmp file error : run again'
return
           
100 continue

return
end

