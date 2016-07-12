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

subroutine RAMS_anal_init (nfile,file_prefix)

use an_header

implicit none

integer :: nfile,nopt,nfl,ngr,nval
character(len=*) :: file_prefix,cdata(*)
real :: fdata(*)
integer :: idata(*)

include 'rcommons.h'

integer, parameter :: maxfiles=1000
integer :: nv,ng,k,nfn,ln,ihr1,imin1,iitime,iiday,iimonth,iiyear
character(len=4)   :: ctime1
character(len=256) :: fpref

real, save :: ftimes(maxfiles),flevels(nzpmax,maxgrds,maxfiles),startutc  &
             ,fdelx(maxgrds,maxfiles),fdely(maxgrds,maxfiles),httop  &
             ,polelatn(maxgrds,maxfiles),polelonn(maxgrds,maxfiles)  &
             ,swx(maxgrds,maxfiles),swy(maxgrds,maxfiles)
integer, save :: nfiles,nfgrids(maxfiles),nfgpnts(5,maxgrds,maxfiles)  &
                ,ifdates(maxfiles),iftimes(maxfiles)
character(len=256), save :: fnames(maxfiles)
character(len=15), save  :: startdate
character(len=64), save  :: exptname

! get the files
print*
fpref=file_prefix
fpref(len_trim(fpref)+1:)='*-head.txt'
call RAMS_filelist(fnames,fpref,nfile)
nfiles=nfile
print*,' files found: ',nfile
print*

! construct arrays
do nfn=1,nfile
   open(10,file=fnames(nfn),form='formatted')
   read(10,*) nvbtab
   allocate (anal_table(nvbtab))
   do nv=1,nvbtab
      read(10,*) anal_table(nv)%string   &
                ,anal_table(nv)%npointer  &
                ,anal_table(nv)%idim_type  &
                ,anal_table(nv)%ngrid  &
                ,anal_table(nv)%nvalues
   enddo
   call commio('ANAL','READ',10)
   close(10)
   
   if(nfn==1) then
      write(ctime1,'(i4.4)') itime1
      read(ctime1(1:2),*) ihr1
      read(ctime1(3:4),*) imin1
      startutc=float(ihr1)+float(imin1)/60.
      write(startdate,'(i4.4,a1,i2.2,a1,i2.2,a1,i4.4)')  &
          iyear1,'-',imonth1,'-',idate1,'-',itime1
      !print*,'RAMS_anal_init: start utc, date ',startutc,' ',startdate
   endif
   
   ln=len_trim(fnames(nfn))
   read(fnames(nfn)(ln-14:ln-9),*) iitime
   read(fnames(nfn)(ln-17:ln-16),*) iiday
   read(fnames(nfn)(ln-20:ln-19),*) iimonth
   read(fnames(nfn)(ln-25:ln-22),*) iiyear
   ftimes(nfn)=time
   ifdates(nfn)=iiyear*10000+iimonth*100+iiday
   iftimes(nfn)=iitime
   write(*,'(a,i4,f12.2,2i10)') ' files-',nfn,ftimes(nfn),ifdates(nfn)  &
                                         ,iftimes(nfn)

   call grdcoords()
   exptname=expnme
   nfgrids(nfn)=ngrids
   do ng=1,ngrids
      nfgpnts(1,ng,nfn)=nnxp(ng)
      nfgpnts(2,ng,nfn)=nnyp(ng)
      nfgpnts(3,ng,nfn)=nnzp(ng)
      nfgpnts(4,ng,nfn)=nzg
      nfgpnts(5,ng,nfn)=nzs
      swx(ng,nfn)=xtn(1,ng)
      swy(ng,nfn)=ytn(1,ng)
      fdelx(ng,nfn)=deltaxn(ng)
      fdely(ng,nfn)=deltayn(ng)
      polelatn(ng,nfn)=platn(ng)
      polelonn(ng,nfn)=plonn(ng)
      do k=1,nnzp(ng)
         flevels(k,ng,nfn)=ztn(k,ng)
      enddo
   enddo
   httop=zmn(nnzp(1)-1,1)
   
   close(10)
   deallocate (anal_table)
enddo

return

!------------------------------------------------

entry RAMS_get_idata(nopt,nfl,ngr,idata,nval)

if(nopt.eq.0) then
   nval=4
   do k=1,nval
      idata(k)=nfgpnts(k,ngr,nfl)
   enddo
elseif(nopt.eq.1 )then
   nval=1
   idata(1)=nfgrids(nfl)
elseif(nopt.eq.2) then
   nval=1
   idata(1)=ifdates(nfl)
elseif(nopt.eq.3) then
   nval=1
   idata(1)=iftimes(nfl)
elseif(nopt.eq.4) then
   nval=1
   idata(1)=nfiles
endif

return

!------------------------------------------------

entry RAMS_get_fdata(nopt,nfl,ngr,fdata,nval)

if(nopt.eq.0) then
   nval=nfgpnts(3,ngr,nfl)
   do k=1,nval
      fdata(k)=flevels(k,ngr,nfl)
   enddo
elseif(nopt.eq.1) then
   nval=1
   fdata(1)=ftimes(nfl)
elseif(nopt.eq.2) then
   nval=1
   fdata(1)=startutc
elseif(nopt.eq.3) then
   nval=1
   fdata(1)=httop
elseif(nopt.eq.4) then
   nval=1
   fdata(1)=fdelx(ngr,nfl)
elseif(nopt.eq.5) then
   nval=1
   fdata(1)=fdely(ngr,nfl)
elseif(nopt.eq.6) then
   nval=1
   fdata(1)=polelatn(ngr,nfl)
elseif(nopt.eq.7) then
   nval=1
   fdata(1)=polelonn(ngr,nfl)
elseif(nopt.eq.8) then
   nval=1
   fdata(1)=swx(ngr,nfl)
elseif(nopt.eq.9) then
   nval=1
   fdata(1)=swy(ngr,nfl)
endif

return

!------------------------------------------------

entry RAMS_get_cdata(nopt,nfl,cdata,nval)

if(nopt.eq.0) then
   cdata(1)=fnames(nfl)
   nval=1
elseif(nopt.eq.1) then
   cdata(1)=startdate
   nval=1
elseif(nopt.eq.2) then
   cdata(1)=exptname
   nval=1
endif

return
end

