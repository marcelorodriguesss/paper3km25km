!
! Copyright (C) 1991-2003  ; All Rights Reserved ; ATMET, LLC
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

subroutine RAMS_dump (a,coor,itrans,ivtype,nngd,n1,n2,n3  &
                     ,zlevs,ns,slz,iyear1,imonth1,idate1,itime1  &
                     ,fcstsec,dx,dy,x1,y1,alat1,alon1  &
                     ,cdname,cdunits,head1,revpref)

!-----------------------------------------------------------------
! Routine to dump (write) RAMS fields as chosen through
! REVU_IN. This is intended to be modified at will to produce the
! desired output. Basically, one 3 or 2-dimensional variable at a
! time will be sent here.

! Arguments:
! ----------
! cdname - name of variable
! cdunits - units of variable
! a - data
! coor - coordinates of RAMS grid
! n1,n2,n3 - actual dimensions of field
! itrans - type of vertical transformation 1-sigma-z, 2-Cartesian, 3-pressure
! ivtype - type of variable 2-2d surface, 3-3d atmospheric
! islab - 1 = x-z, 2 = y-z, 3 = x-y slab
! nib,nie,njb,nje - horizontal or vertical "window" as chosen in namelist.
! icoord - slab value as chosen in namelist.
! nii,njj - number of windowed points
! nplevs - number of atmospheric coordinate levels (if pressure transformation)
! iplevs(nplevs)- atmospheric coordinate levels (if pressure transformation)
! n3 - number of atmospheric height levels (if sigma_z or Cartesian)
! zlevs(n3) - atmospheric coordinate levels (if sigma_z or Cartesian)
! ns - number of soil coordinate levels
! slz(ns) - soil coordinate levels
! iyear1 - year of model start
! imonth1 - month of model start
! idate1 - date of model start
! itime1 - time of model start (hhmm)
! fcstsec - seconds into run
!-----------------------------------------------------------------

use an_header

implicit none

integer :: itrans,ivtype,n1,n2,n3,ns,iyear1,imonth1,idate1,itime1,nngd
real :: fcstsec,dx,dy,x1,y1,alat1,alon1
real :: a(n1,n2,n3),coor(n1,n2,n3,*),zlevs(*),slz(*)
character(len=*) :: cdname,cdunits,head1,revpref

include 'plevs.h'
include 'window.h'

integer :: i,j,k,lastslash,nval
character(len=256) :: flnm,flnm1
character(len=1) :: ftran
integer, save :: ncall(20),iun

data ncall/20*0/

!print*,'===> dumpout =>',fcstsec
!print*,iyear1,imonth1,idate1,itime1,nngd
!print*,cdname(1:len_trim(cdname)-1),n1,n2,n3
!print*,dx,dy,x1,y1,alat1,alon1
!print*,itrans,ivtype,icoord,islab
!print*,nib,nie,njb,nje,nii,njj,islab

if(ncall(nngd).eq.0) then

   ! If it is the first time into this routine, make and open a file.
   
   if(itrans.eq.3) then
      ftran='P'
   elseif(itrans.eq.2) then
      ftran='C'
   elseif(itrans.eq.1) then
      ftran='S'
   endif
   call RAMS_get_cdata(0,1,flnm1,nval)
   write(flnm,'(2a,2a1,i4.4,a1,i2.2,a1,i2.2,a1,i6.6,a2,i1,a4)' )  &
       revpref(1:len_trim(revpref))  &
      ,flnm1(lastslash(flnm1)+1:len_trim(flnm1)-27),ftran,'-'  &
      ,iyear1,'-',imonth1,'-',idate1,'-',itime1*100,'-g',nngd,'.dmp'
   print*
   print*,'===='
   print*,'Dump file: ',flnm(1:len_trim(flnm))
   
   iun=79
   open(iun,file=flnm,status='unknown')
   rewind iun
   
   ncall(nngd)=1
endif

!----------------------------------------------------------------
! Sample set of output statements.
! Modify them to produce the output you want.
!----------------------------------------------------------------

if(itrans.eq.3) then

  ! If this is pressure level data . . .

  if(ivtype.eq.3) then
  
     ! If this is a 3-dimensional variable

     do k=nnb,nne,nninc
        write (iun,'(6a,i5,f10.0,2i3)')  &
           head1(1:len_trim(head1)),' '  &
           ,cdname(1:len_trim(cdname)-1),' ['  &
           ,cdunits(1:len_trim(cdunits)-1),'] '  &
           ,iplevs(k),fcstsec,nii,njj
        write (iun,'(5e14.6)') ((a(i,j,k),i=nib,nie,niinc),j=njb,nje,njinc)
     enddo

   else
      print*,'unknown ivtype',ivtype
      stop 'RAMS_dump'
   endif

elseif(itrans.eq.1.or.itrans.eq.2) then

   ! If this is terrain following or Cartesian data . . .
   
   if(ivtype.eq.3) then
   
      ! If this is a 3-dimensional atmospheric variable
      
      do k=nnb,nne,nninc
         write(iun,'(6a,f10.2,f10.0,2i3)')  &
            head1(1:len_trim(head1)),' '  &
            ,cdname(1:len_trim(cdname)-1),' ['  &
            ,cdunits(1:len_trim(cdunits)-1),'] '  &
            ,zlevs(k),fcstsec,nii,njj
         write(iun,'(5e14.6)') ((a(i,j,k),i=nib,nie,niinc),j=njb,nje,njinc)
      enddo

   
   elseif(ivtype.eq.5) then
   
      ! If this is a 3-dimensional soil variable

      do k=nnb,nne,nninc
         write(iun,'(6a,f10.4,f10.0,2i3)')  &
            head1(1:len_trim(head1)),' '  &
            ,cdname(1:len_trim(cdname)-1),' ['  &
            ,cdunits(1:len_trim(cdunits)-1),'] '  &
            ,(slz(k)+slz(k+1))/2.,fcstsec,nii,njj
         write(iun,'(5e14.6)') ((a(i,j,k),i=nib,nie,niinc),j=njb,nje,njinc)
      enddo

   elseif (ivtype.eq.2) then
   
      ! If this is a 2-dimensional variable

      write(iun,'(6a,2f10.2,2i3)')  &
         head1(1:len_trim(head1)),' '  &
         ,cdname(1:len_trim(cdname)-1),' ['  &
         ,cdunits(1:len_trim(cdunits)-1),'] '  &
         ,0.,fcstsec,nii,njj
      
      write(iun,'(5e14.6)') ((a(i,j,1),i=nib,nie,niinc),j=njb,nje,njinc)
   
   else
      print*,'unknown ivtype',ivtype
      stop 'RAMS_dump'
   endif

endif

return
end
