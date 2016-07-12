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

subroutine RAMS_grads (igunit,a,n1,n2,n3,nllat,nllon,dllat,dllon  &
                      ,swllat,swllon,nzb,nze,izstep  &
                      ,ivtime,ivvar,iztran,xt,yt,zt,ivtype,platn,plonn)
                      
! Write the GrADS binary file

dimension a(n1,n2,n3),zt(*),xt(*),yt(*)
real, allocatable :: aa(:,:)
include 'plevs.h'
common/grcom/ngrlevs(100),ngrlat,ngrlon,dgrlat,dgrlon,swgrlat,swgrlon
save nrec

!print*,'in RAMS_GRADS:',ivtype,nzb,nze,izstep
!print*,'in RAMS_GRADS:',nllat,nllon

if(allocated(aa)) deallocate(aa)
allocate(aa(nllon,nllat))

ngrlat=nllat
ngrlon=nllon
dgrlat=dllat
dgrlon=dllon
swgrlat=swllat
swgrlon=swllon

if(ivtime.eq.1.and.ivvar.eq.1)nrec=0

if((iztran.eq.1.or.iztran.eq.2).and.ivtype.ne.2)then
   ! get rid of first ZT level since below ground
   if(nzb.eq.1) nzb=2
elseif(iztran.eq.3) then
   nze=min(nze,nplevs)
endif

if(ivtype.eq.2) then
   nzb=1
   nzee=nzb
   ngrlevs(ivvar)=1
elseif(ivtype.eq.3) then
   nzee=nze
   ngrlevs(ivvar)=(nze-nzb)/izstep+1
endif

!print*,n1,n2,n3,nzee
!print*,((a(i,j,4),i=1,n1),j=1,n2)

!print*
!print*,'nzb,nzee,izstep=',nzb,nzee,izstep
!print*
do k=nzb,nzee,izstep
   call interp_ll (n1,n2,a(1,1,k),xt,yt  &
                  ,nllon,nllat,swllat,swllon,dllat,dllon  &
                  ,platn,plonn,aa)
   nrec=nrec+1
   !print*,'doing levels:',k,nrec,nllon,nllat
   if(k.eq.nzb) nrecb=nrec
   !if(k==4 )print*,((aa(i,j),i=1,nllon),j=1,nllat)
   write(igunit,rec=nrec) ((aa(i,j),i=1,nllon),j=1,nllat)
enddo
!print*,'1st value for ivtime,ivvar=',ivtime,ivvar,aa(1,1)

!print*,'Writing to GrADS file,ivtime,ivvar,recb,rece: '  &
!,       ivtime,ivvar,nrecb,nrec
!print*,'--------------'

deallocate(aa)

return
end

!***************************************************************************

subroutine RAMS_gradsbin (igunit,fileaction,iztran,ngd,nii,njj  &
                         ,iyear1,imonth1,idate1,itime1,revpref)
                         
! Open/Close the GrADS binary file and keep track of write records
                         
implicit none

integer, intent(in) :: igunit
integer, intent(in) :: nii
integer, intent(in) :: njj
integer, intent(in) :: ngd
integer, intent(in) :: iztran
integer, intent(in) :: iyear1
integer, intent(in) :: imonth1
integer, intent(in) :: idate1
integer, intent(in) :: itime1
character(len=*)    :: fileaction,revpref

character(len=1)    :: ftran
character(len=256)  :: flnm,flnm1
integer             :: ireclen,lastslash,iran_recsize,nval

if(fileaction.eq.'OPEN')then

   print*,iztran
   ! binary filename
   if(iztran.eq.3) then
      ftran='P'
   elseif(iztran.eq.2) then
      ftran='C'
   elseif(iztran.eq.1) then
      ftran='S'
   endif
   call RAMS_get_cdata(0,1,flnm1,nval)
   write(flnm,'(2a,2a1,i4.4,a1,i2.2,a1,i2.2,a1,i4.4,a4,i1,a4)' )  &
       revpref(1:len_trim(revpref))  &
      ,flnm1(lastslash(flnm1)+1:len_trim(flnm1)-27),ftran,'-'  &
      ,iyear1,'-',imonth1,'-',idate1,'-',itime1,'00-g',ngd,'.gra'
   print*
   print*,'GrADS binary file: ',flnm(1:len_trim(flnm))
   
   ireclen=nii*njj*iran_recsize()
   !print*,'recl=',ireclen,nii,njj
   
   open(igunit,file=flnm(1:len_trim(flnm)),form='unformatted'  &
       ,access='direct',status='unknown',recl=ireclen)
       
elseif(fileaction.eq.'CLOSE')then

   close(igunit)
   
endif

return
end

!***************************************************************************

subroutine RAMS_gradsctl (igunit,ngd,platn,plonn  &
                         ,deltax,deltay,glats,glatn,glonw,glone  &
                         ,irefg,jrefg,xref,yref,iztran,dategrads  &
                         ,cincgrads,nib,nie,njb,nje,nzb,nze  &
                         ,izstep,zt,ivtime,ivvar,gradsvar  &
                         ,cdname,cdunits,iyear1,imonth1,idate1  &
                         ,itime1,revpref)

! Write the GrADS control file.

dimension zt(*)
character*(*) dategrads,cincgrads,gradsvar(*),cdname(*),cdunits(*),revpref
include 'plevs.h'
common/grcom/ngrlevs(100),ngrlat,ngrlon,dgrlat,dgrlon,swgrlat,swgrlon

character*13 mach_type
character(len=1)    :: ftran
character(len=256)  :: flnm,flnm1
integer :: nval

call ENDIAN(mach_type)

nxp=nie-nib+1
nyp=nje-njb+1
resll=min((glone-glonw)/float(nxp),(glatn-glats)/float(nyp))

! This has already been done in RAMS_grads above, but for clarity...
if(iztran.eq.1.or.iztran.eq.2)then
   ! Get rid of first ZT level since below ground.
   if(nzb.eq.1)nzb=2
elseif(iztran.eq.3) then
   nze=min(nze,nplevs)
endif
nzp=(nze-nzb+1)/izstep

! control filename
if(iztran.eq.3) then
   ftran='P'
elseif(iztran.eq.2) then
   ftran='C'
elseif(iztran.eq.1) then
   ftran='S'
endif
call RAMS_get_cdata(0,1,flnm1,nval)
write(flnm,'(2a,2a1,i4.4,a1,i2.2,a1,i2.2,a1,i4.4,a4,i1)' )  &
    revpref(1:len_trim(revpref))  &
   ,flnm1(lastslash(flnm1)+1:len_trim(flnm1)-27),ftran,'-'  &
   ,iyear1,'-',imonth1,'-',idate1,'-',itime1,'00-g',ngd
print*
print*,'GrADS control file: ',flnm(1:len_trim(flnm))
   
open(igunit,file=flnm(1:len_trim(flnm))//'.ctl',status='unknown')
rewind(igunit)
     
write(igunit,2001) '^'//flnm(1:len_trim(flnm))//'.gra'
write(igunit,2002) 'undef 1.0e30'
write(igunit,2011) mach_type
write(igunit,2002) 'title RAMS Output'
!write(igunit,2010) nxp,nyp,platn,plonn,xref,yref  &
!                  ,irefg,jrefg,deltax,deltay
write(igunit,2003) ngrlon,swgrlon,dgrlon
write(igunit,2004) ngrlat,swgrlat,dgrlat
if(iztran.eq.3) then
   write(igunit,2005) nzp,(float(iplevs(i)),i=1,nzp)
else
   write(igunit,2005) nzp,(zt(k),k=nzb,nze,izstep)
   !print*,'+++++++++++',nzp,nzb,nze,izstep,ngd
   !print*,'+++++++++++',zt(1),zt(2),zt(3)
endif
write(igunit,2006) ivtime,dategrads,cincgrads

write(igunit,2007) ivvar

do i=1,ivvar
   lc1=len_trim(gradsvar(i))+1
   lc2=index(cdname(i),';')-1
   ! Need to make sure that there are no special characters in name
   iunderscore=index(gradsvar(i),'_')
   if(iunderscore.gt.0)then
     lc1=lc1-1
     gradsvar(i)=gradsvar(i)(1:iunderscore-1)//  &
                 gradsvar(i)(iunderscore+1:lc1)
   endif

   lc3=index(cdunits(i),';')-1
   write(igunit,2008) gradsvar(i)(1:lc1),ngrlevs(i),cdname(i)(1:lc2)  &
                     ,cdunits(i)(1:lc3)
enddo

write(igunit,2002) 'endvars'
close(20)

2001  format('dset ',a)
2002  format(a)
2003  format('xdef ',i4,' linear ',2f15.7)
2004  format('ydef ',i4,' linear ',2f15.7)
2005  format('zdef ',i4,' levels ',60f10.3)
2006  format('tdef ',i4,' linear ',2a20)
2007  format('vars ',i4)
2008  format(a,i4,' 99    - RAMS : ',a,' [',a,']')
!2010  format('pdef ',2i4,' ops ',4f16.5,2i5,2f10.2)
2011  format('options ',a)

return
end

!***************************************************************************

subroutine date_mkgrads (iyyyy,imm,idd,startutc,fcstsec,dategrads,cincgrads)

! Get new date and increment into GrADS format

character*(*) dategrads,cincgrads
character cmonth(12)*3
data cmonth/'jan','feb','mar','apr','may','jun'  &
           ,'jul','aug','sep','oct','nov','dec'/
data fcstsec1 /9999999/

! The GrADS time increment format

step=fcstsec-fcstsec1
if(step.gt.0)then
  if(mod(step,86400.).eq.0)then
    write(cincgrads,'(i2.2,a2)')int(step/86400.),'dy'
  elseif(mod(step,3600.).eq.0)then
    write(cincgrads,'(i2.2,a2)')int(step/3600.),'hr'
  elseif(mod(step,60.).eq.0)then
    write(cincgrads,'(i2.2,a2)')int(step/60.),'mn'
  endif
endif

! Need something that checks that all increments are the same
! Maybe wait until 4.2 so can use filenames to get dates
!      print*,'GrADS needs a constant time increment'
!      print*,'Have non-periodic analysis files'
!      print*,'fcstsec,fcstsec1,step=',fcstsec,fcstsec1,step
!      stop'in date_mkgrads in iplt.f in revu'

! Now, the GrADS date string

ihh=int(startutc)
imin=int((startutc-float(ihh))*60.)
isec=int(((startutc-float(ihh))*60.-float(imin))*60.)
ihhmmss=ihh*10000+imin*100+isec

call date_add_to(iyyyy,imm,idd,ihhmmss,fcstsec,'s',jyyyy,jmm,jdd,jhhmmss)
jhh=jhhmmss/10000
jmin=jhhmmss-jhh*10000
jsec=jhhmmss-jhh*100000-jmin*100

write(dategrads(1:2),'(i2.2)')jhh
write(dategrads(3:3),'(a1)')':'
write(dategrads(4:5),'(i2.2)')jmin/100
write(dategrads(6:6),'(a1)')'Z'
write(dategrads(7:8),'(i2.2)')jdd
write(dategrads(9:11),'(a3)')cmonth(jmm)
write(dategrads(12:15),'(i4.4)')jyyyy

fcstsec1=fcstsec

return
end

