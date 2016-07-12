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

subroutine ISAN_file_inv (iyear1,imonth1,idate1,itime1,timmax)

use isan_coms

implicit none

integer :: iyear1,imonth1,idate1,itime1
real :: timmax

integer :: nfgfiles,nc,nf,lnf,nn,ndates,nupfiles,nsffiles,isan_err_flag
integer :: inyear,inmonth,indate,inhour
integer :: iyearf,imonthf,idatef,ihourf
integer :: iyear2,imonth2,idate2,ihour2,ihour1
real :: tinc

character(len=14) :: itotdate_fg(maxisfiles),itotdate_up(maxisfiles)  &
            ,itotdate_sf(maxisfiles),itotdates(4*maxisfiles),idate_end
character(len=128), dimension(maxisfiles) :: fnames_fg,fnames_up,fnames_sf

!          Go through first guess, upper air, surface input files
!            and make inventory

if(igridfl.ne.0) then
   if(iapr(1:1).ne.' '.and.iapr(1:1).ne.char(0) ) then

    nfgfiles=-1
      nc=len_trim(iapr)
      if(guess1st.eq.'PRESS')  &
           call RAMS_filelist(fnames_fg,iapr(1:nc)//'????-??-??-????',nfgfiles)
      if(guess1st.eq.'RAMS')  &
           call RAMS_filelist(fnames_fg,iapr(1:nc)//'*-head.txt',nfgfiles)

     ! do nf=1,nfgfiles
     !    print*,'reading fg:',nc,nfgfiles,igridfl,fnames_fg(nf)
     ! enddo

      if(nfgfiles.gt.maxisfiles) then
         print*,'too many first guess files'
         stop 'lots_of_first_guess'
      endif

      do nf=1,nfgfiles

         lnf=len_trim(fnames_fg(nf))

         if(guess1st.eq.'PRESS')  &
            read(fnames_fg(nf)(lnf-14:lnf),20) inyear,inmonth,indate,inhour
      
         ! form of a-A-2000-07-01-060000-head.txt
         if(guess1st.eq.'RAMS')  &
            read(fnames_fg(nf)(lnf-25:lnf-10),20) inyear,inmonth,indate,inhour

         20 format(i4,1x,i2,1x,i2,1x,i4)

         call date_make_big(inyear,inmonth,indate,inhour*100,itotdate_fg(nf))

      enddo

      call RAMS_dintsort(nfgfiles,itotdate_fg,fnames_fg)

!      do nf=1,nfgfiles
!         print*,'fg files:',nf,itotdate_fg(nf),fnames_fg(nf)
!      enddo

   endif
endif

    nupfiles=0
    nsffiles=0

if(igridfl.ne.3) then
   if(iarawi(1:1).ne.' '.and.iarawi(1:1).ne.char(0) ) then

    nupfiles=-1
      nc=len_trim(iarawi)
    call RAMS_filelist(fnames_up,iarawi(1:nc)//'????-??-??-????',nupfiles)

      if(nupfiles.gt.maxisfiles) then
         print*,'too many upper air files'
         stop 'lots_of_upper_air'
      endif

      do nf=1,nupfiles
         lnf=len_trim(fnames_up(nf))
         read(fnames_up(nf)(lnf-14:lnf),20) inyear,inmonth,indate,inhour

         call date_make_big(inyear,inmonth,indate,inhour*100,itotdate_up(nf))

      enddo

      call RAMS_dintsort(nupfiles,itotdate_up,fnames_up)

!      do nf=1,nupfiles
!         print*,'up files:',nf,itotdate_up(nf),fnames_up(nf)
!      enddo

   endif


   if(iasrfce(1:1).ne.' '.and.iasrfce(1:1).ne.char(0) ) then

    nsffiles=-1
      nc=len_trim(iasrfce)
      call RAMS_filelist(fnames_sf,iasrfce(1:nc)//'????-??-??-????',nsffiles)

      if(nsffiles.gt.maxisfiles) then
         print*,'too many surface air files'
         stop 'lots_of_surface'
      endif

      do nf=1,nsffiles
         lnf=len_trim(fnames_sf(nf))
         read(fnames_sf(nf)(lnf-14:lnf),20) inyear,inmonth,indate,inhour

         call date_make_big(inyear,inmonth,indate,inhour*100,itotdate_sf(nf))
      enddo

      call RAMS_dintsort(nsffiles,itotdate_sf,fnames_sf)

!      do nf=1,nsffiles
!         print*,'sf files:',nf,itotdate_sf(nf),fnames_sf(nf)
!      enddo

   endif

endif
print*,'here:'

! put dates in order, removing duplicates

call RAMS_sort_dint3(nfgfiles,itotdate_fg  &
                    ,nupfiles,itotdate_up,nsffiles,itotdate_sf  &
                    ,ndates,itotdates)

print*,'here1:'

call RAMS_unique_dint(ndates,itotdates)

!print*,'dates:',ndates
!do nn=1,ndates
!   print*,'dates:',itotdates(nn)
!enddo

print*,'here2:'

!  start printing section
!--------------------------------------------------------------

print*,' '
print*,' '
print*,' '
print*,'---------------------------------------------------------'
print*,'-----------  ISAN Input File Date Inventory -------------'
print*,'---------------------------------------------------------'
do nn=1,ndates
   print*,'---- Date:',itotdates(nn)
   do nf=1,nfgfiles
      if(itotdates(nn).eq.itotdate_fg(nf)) then
         print*,'---- First guess file:'  &
              ,fnames_fg(nf)(1:len_trim(fnames_fg(nf)))
      endif
   enddo
   do nf=1,nupfiles
      if(itotdates(nn).eq.itotdate_up(nf)) then
         print*,'---- Upper air   file:'  &
              ,fnames_up(nf)(1:len_trim(fnames_up(nf)))
      endif
   enddo
   do nf=1,nsffiles
      if(itotdates(nn).eq.itotdate_sf(nf)) then
         print*,'---- Surface obs file:'  &
              ,fnames_sf(nf)(1:len_trim(fnames_sf(nf)))
      endif
   enddo
   print*,'------------------------------------------------------'
enddo

!--------------------------------------------------------------

! Find dates we are going to process

!print*,'start dates:',timmax,iyear1,imonth1,idate1,itime1,isan_inc

!Find end date
call date_add_to(iyear1,imonth1,idate1,itime1*100  &
                ,timmax,'s',iyearf,imonthf,idatef,ihourf)
call date_make_big(iyearf,imonthf,idatef,ihourf,idate_end)
!print*,'end dates:',iyearf,imonthf,idatef,ihourf,idate_end

ihour1=itime1*100
tinc= (isan_inc/100) * 60.  + mod(isan_inc,100)
npdates = 1
call date_add_to (iyear1,imonth1,idate1,ihour1  &
     ,tinc*(npdates-1),'m',iyear2,imonth2,idate2,ihour2)
call date_make_big(iyear2,imonth2,idate2,ihour2  &
     ,iproc_dates(npdates))
do while (iproc_dates(npdates) .lt. idate_end)
   npdates = npdates + 1
   call date_add_to (iyear1,imonth1,idate1,ihour1  &
        ,tinc*(npdates-1),'m',iyear2,imonth2,idate2,ihour2)
   call date_make_big(iyear2,imonth2,idate2,ihour2  &
        ,iproc_dates(npdates))
end do


!   We have the dates to process. Find if files exist and set overall
!     "go ahead" flag. Put filenames in iproc_names array if they will be used.

!      iproc_flag array info:
!      1) Process?     0=no, 1=yes
!      2) First guess? 0=no file, 1=exists, 2=exists, don't use, 3=interpolate
!      3) Upper air?   0=no file, 1=exists, 2=exists, don't use
!      4) Surface?     0=no file, 1=exists, 2=exists, don't use

isan_err_flag=0

print*,' '
print*,' '
print*,' '
print*,'---------------------------------------------------------'
print*,'-----------  ISAN Processing Information    -------------'
print*,'---------------------------------------------------------'
print*,'----    Flags:  IGRIDFL =',igridfl,'  I1ST_FLG=',i1st_flg
print*,'----    Flags:  IUPA_FLG=',iupa_flg,'  ISFC_FLG=',isfc_flg
print*,'---------------------------------------------------------'

do nn=1,npdates
   iproc_flag(nn,1)=0
   iproc_flag(nn,2)=0
   iproc_flag(nn,3)=0
   iproc_flag(nn,4)=0

   print*,'------------------------------------------------------'
   print*,'---- Date:', iproc_dates(nn)

   iproc_flag(nn,1)=1

   ! first guess
   iproc_flag(nn,2)=0
   do nf=1,nfgfiles
      if(iproc_dates(nn).eq.itotdate_fg(nf)) then
         print*,'---- First guess file exists.'
         if(igridfl.eq.0) then
            iproc_flag(nn,2)=2
         else
            iproc_flag(nn,2)=1
         endif
         iproc_names(nn,1)=fnames_fg(nf)
         goto 71
      endif
   enddo
   
   ! file doesn't exist
   if(igridfl == 0) then
      print*,'---- First guess file not needed.'
      iproc_flag(nn,1)=1
   elseif(i1st_flg.eq.1) then
      print*,'---- First guess file does not exist.'  &
           ,' Will not process this time.'
      iproc_flag(nn,1)=0
   elseif(i1st_flg.eq.2) then
      isan_err_flag=1
      print*,'---- First guess file does not exist.'  &
           ,' Will STOP run.'
      iproc_flag(nn,1)=0
   elseif(i1st_flg.eq.3) then
      print*,'---- First guess file does not exist.'  &
           ,' Will attempt interpolation.'
      iproc_flag(nn,2)=3
   endif
   71 continue

   ! upper air
   iproc_flag(nn,3)=0
   do nf=1,nupfiles
      if(iproc_dates(nn).eq.itotdate_up(nf)) then
         print*,'---- Upper air file exists.'
         if(igridfl.eq.3) then
            iproc_flag(nn,3)=2
         else
            iproc_flag(nn,3)=1
         endif
         iproc_names(nn,2)=fnames_up(nf)
         goto 72
      endif
   enddo
   
   ! file doesn't exist
   if(iupa_flg.eq.1) then
      print*,'---- Upper air file does not exist.'  &
            ,' Will not process this time.'
      iproc_flag(nn,1)=0
   elseif(iupa_flg.eq.2) then
      isan_err_flag=1
      print*,'---- Upper air file does not exist.',' Will STOP run.'
      iproc_flag(nn,1)=0
   elseif(iupa_flg.eq.3) then
      print*,'---- Upper air file does not exist.',' Will try running without.'
      iproc_flag(nn,3)=3
   endif
   72 continue

   ! surface
   iproc_flag(nn,4)=0
   do nf=1,nsffiles
      if(iproc_dates(nn).eq.itotdate_sf(nf)) then
         print*,'---- Surface obs file exists.'
         if(igridfl.eq.3) then
            iproc_flag(nn,4)=2
         else
            iproc_flag(nn,4)=1
         endif
         iproc_names(nn,3)=fnames_sf(nf)
         goto 73
      endif
   enddo
   
   ! file doesn't exist
   if(isfc_flg.eq.1) then
      print*,'---- Surface file does not exist.',' Will not process this time.'
      iproc_flag(nn,1)=0
   elseif(isfc_flg.eq.2) then
      isan_err_flag=1
      print*,'---- Surface file does not exist.',' Will STOP run.'
      iproc_flag(nn,1)=0
   elseif(isfc_flg.eq.3) then
      print*,'---- Surface file does not exist.',' Will try running without.'
      iproc_flag(nn,4)=3
   endif
   73 continue

enddo
print*,'---------------------------------------------------------'

if (isan_err_flag.ne.0) then
   print*,'ISAN run stopping because of errors!'
   print*,'See previous output listing.'
   stop 'isan_file_errors'
endif

return
end
