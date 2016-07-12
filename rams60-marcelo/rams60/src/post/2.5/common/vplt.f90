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

subroutine RAMS_3to2d (islab,indata,incoords,ni1,ni2,ni3  &
                      ,outdata,outcoords,id1,id2,jd1,jd2,numsl,nii,njj)
     
real indata(ni1,ni2,ni3),incoords(ni1,ni2,ni3,3)
real outdata(nii,njj),outcoords(nii,njj,3)

if(islab.eq.3) then
   do j=jd1,jd2
      do i=id1,id2
         outdata(i-id1+1,j-jd1+1)=indata(i,j,numsl)
         outcoords(i-id1+1,j-jd1+1,1)=incoords(i,j,numsl,1)
         outcoords(i-id1+1,j-jd1+1,2)=incoords(i,j,numsl,2)
         outcoords(i-id1+1,j-jd1+1,3)=incoords(i,j,numsl,3)
      enddo
   enddo
elseif(islab.eq.1) then
   do k=jd1,jd2
      do i=id1,id2
         outdata(i-id1+1,k-jd1+1)=indata(i,numsl,k)
         outcoords(i-id1+1,k-jd1+1,1)=incoords(i,numsl,k,1)
         outcoords(i-id1+1,k-jd1+1,2)=incoords(i,numsl,k,2)
         outcoords(i-id1+1,k-jd1+1,3)=incoords(i,numsl,k,3)
      enddo
   enddo
   ! adjust lowest level outdata values if lowest plotting level is
   ! on the ground (jd1=1)
   if(jd1==1) then
      do i=id1,id2
         outdata(i-id1+1,1)=(outdata(i-id1+1,1)+outdata(i-id1+1,2))/2
      enddo
   endif
elseif(islab.eq.2) then
   do k=jd1,jd2
      do j=id1,id2
         outdata(j-id1+1,k-jd1+1)=indata(numsl,j,k)
         outcoords(j-id1+1,k-jd1+1,1)=incoords(numsl,j,k,1)
         outcoords(j-id1+1,k-jd1+1,2)=incoords(numsl,j,k,2)
         outcoords(j-id1+1,k-jd1+1,3)=incoords(numsl,j,k,3)
      enddo
   enddo
   ! adjust lowest level outdata values if lowest plotting level is
   ! on the ground (jd1=1)
   if(jd1==1) then
      do i=id1,id2
         outdata(j-id1+1,1)=(outdata(j-id1+1,1)+outdata(j-id1+1,2))/2
      enddo
   endif
endif

return
end

!***************************************************************************

subroutine RAMS_3to2d_v (islab,indata,incoords,ni1,ni2,ni3  &
                        ,outdata,outcoords,id1,id2,jd1,jd2,numsl,nii,njj)
     
real indata(ni1,ni2,ni3),incoords(ni1,ni2,ni3,3)
real outdata(0:nii,0:njj),outcoords(0:nii,0:njj,3)

iso = 0
jso = 0
if (id1 .ge. 2) iso = 1
if (jd1 .ge. 2) jso = 1

if(islab.eq.3) then
   do j=jd1-jso,jd2
      do i=id1-iso,id2
         outdata(i-id1+1,j-jd1+1)=indata(i,j,numsl)
         outcoords(i-id1+1,j-jd1+1,1)=incoords(i,j,numsl,1)
         outcoords(i-id1+1,j-jd1+1,2)=incoords(i,j,numsl,2)
         outcoords(i-id1+1,j-jd1+1,3)=incoords(i,j,numsl,3)
      enddo
   enddo
elseif(islab.eq.1) then
   do k=jd1-jso,jd2
      do i=id1-iso,id2
         outdata(i-id1+1,k-jd1+1)=indata(i,numsl,k)
         outcoords(i-id1+1,k-jd1+1,1)=incoords(i,numsl,k,1)
         outcoords(i-id1+1,k-jd1+1,2)=incoords(i,numsl,k,2)
         outcoords(i-id1+1,k-jd1+1,3)=incoords(i,numsl,k,3)
      enddo
   enddo
elseif(islab.eq.2) then
   do k=jd1-jso,jd2
      do j=id1-iso,id2
         outdata(j-id1+1,k-jd1+1)=indata(numsl,j,k)
         outcoords(j-id1+1,k-jd1+1,1)=incoords(numsl,j,k,1)
         outcoords(j-id1+1,k-jd1+1,2)=incoords(numsl,j,k,2)
         outcoords(j-id1+1,k-jd1+1,3)=incoords(numsl,j,k,3)
      enddo
   enddo
endif

return
end

!***************************************************************************

subroutine RAMS_3to2d_vpat (islab,inarea,indata,ni1,ni2,ni3,np  &
                           ,outdata,fracarea,id1,id2,jd1,jd2,numsl  &
                           ,nii,njj,ivtype,atmslab)
   
real indata(ni1,ni2,ni3,np),inarea(ni1,ni2,np)
real outdata(nii,njj,np),fracarea(nii,njj,np),atmslab(nii,njj)

kleaf = jd2
if(ivtype .eq. 10)kleaf = jd2-1

if(islab.eq.1) then

   do k=jd1,kleaf
      do i=id1,id2
         do ip=1,np
            fracarea(i-id1+1,k-jd1+1,ip)=inarea(i,numsl,ip)
            outdata(i-id1+1,k-jd1+1,ip)=indata(i,numsl,k,ip)
         enddo
      enddo
   enddo

   if(ivtype .eq. 10) then
      do i=id1,id2
         do ip=1,np
            outdata(i-id1+1,jd2-jd1+1,ip)=atmslab(i,1)
         enddo
      enddo
   endif

elseif(islab.eq.2) then

   do k=jd1,kleaf
      do j=id1,id2
         do ip=1,np
            fracarea(j-id1+1,k-jd1+1,ip)=inarea(numsl,j,ip)
            outdata(j-id1+1,k-jd1+1,ip)=indata(numsl,j,k,ip)
         enddo
      enddo
   enddo

   if(ivtype .eq. 10) then
      do j=id1,id2
         do ip=1,np
            outdata(j-id1+1,jd2-jd1+1,ip)=atmslab(j,1)
         enddo
      enddo
   endif

elseif(islab.eq.3) then

print*,'SLICING:',np,numsl
   do j=jd1,jd2
      do i=id1,id2
         do ip=1,np
            fracarea(i-id1+1,j-jd1+1,ip)=inarea(i,j,ip)
            outdata(i-id1+1,j-jd1+1,ip)=indata(i,j,numsl,ip)
         enddo
      enddo
   enddo

endif

return
end

!***************************************************************************

subroutine RAMS_3to2d_hpat (indata,ni1,ni2,np  &
                           ,outdata,fracarea,id1,id2,jd1,jd2,nii,njj)
                           
real indata(ni1,ni2,np,2)
real outdata(nii,njj,np),fracarea(nii,njj,np)

do j=jd1,jd2
   do i=id1,id2
      do ip = 1,np
         fracarea(i-id1+1,j-jd1+1,ip)=indata(i,j,ip,1)
         outdata(i-id1+1,j-jd1+1,ip)=indata(i,j,ip,2)
      enddo
   enddo
enddo

return
end

!***************************************************************************

subroutine RAMS_fill_fld (n1,n2,n3,a,b,bb,coords,topo,prefile  &
                         ,ngrd,cvar,itrans,nplevs,iplevs,icfile,icgrid  &
                         ,icvar,cdname,cdunits,ivtype,icoor,topta)
                         
implicit none

integer :: iplevs(*),n1,n2,n3,ngrd,itrans,icfile,icgrid,icvar,ivtype  &
          ,icoor,nplevs
real :: a(*),b(*),bb(*),coords(n1,n2,n3,*),topo(*),topta(*)
character(len=*) :: prefile,cvar,cdname,cdunits
character(len=24) :: fdname,fdunits

include 'rcommons.h'

integer :: i,j,k

call RAMS_varlib(cvar,n1,n2,n3,ngrd,a,b,prefile,cdname,cdunits,icoor)
call RAMS_varinfo(1,ivtype)
call RAMS_varlib('topo',n1,n2,1,ngrd,topo,b,prefile,fdname,fdunits,icoor)
call RAMS_varlib('topta',n1,n2,1,ngrd,topta,b,prefile,fdname,fdunits,icoor)

if(itrans.eq.2.and.ivtype.eq.3) then
   call RAMS_Ctrans(n1,n2,n3,a,b,topo,ztn(1,ngrd),zmn(nnzp(1)-1,1))
elseif(itrans.eq.3.and.ivtype.eq.3) then
   call RAMS_varlib('pi',n1,n2,n3,ngrd,bb,b,prefile,fdname,fdunits,icoor)
   call RAMS_Ptrans(n1,n2,n3,a,bb,b)
endif

! fill coords array with x and y locations
do i = 1,n1
   do j = 1,n2
      do k = 1,n3
         coords(i,j,k,1) = xtn(i,ngrd)
         coords(i,j,k,2) = ytn(j,ngrd)
      enddo
   enddo
enddo

return
end

!***************************************************************************

subroutine RAMS_fill_fld_rgrab (n1,n2,n3,a,b,coords,topo,prefile  &
                               ,ngrd,cvar,itrans,icfile,icgrid,icvar  &
                               ,cdname,cdunits,ivtype,icoor)

include 'rcommons.h'
dimension a(*),b(*),coords(n1,n2,n3,*),topo(*)
character*(*) prefile,cdname,cdunits,cvar
character*24 fdname,fdunits

call RAMS_varlib(cvar,n1,n2,n3,ngrd,a,b,prefile  &
                ,cdname,cdunits,icoor)
call RAMS_varinfo(1,ivtype)

! Added 1/27/97 by MJW from Ian's code to make grabber work.
! Not sure if it will modify other uses of this routine

 if(icfile+icgrid.ge.1) then
    if(ivtype.eq.5) then
       do i=1,nnxp(ngrid)
          do j=1,nnyp(ngrid)
             do k=1,nnzp(ngrid)
                coords(i,j,k,1)=xtn(i,ngrid)
                coords(i,j,k,2)=ytn(j,ngrid)
                coords(i,j,k,3)=slz(k)
             enddo
          enddo
       enddo
    else
       call RAMS_varlib('topo',n1,n2,1,ngrid,topo,b  &
           ,prefile,fdname,fdunits,icoor)

       do i=1,nnxp(ngrid)
          do j=1,nnyp(ngrid)
             do k=1,nnzp(ngrid)
                top=topo(1+(j-1)*n1+i-1)
                rtg=1.-top/zmn(nnzp(ngrid),1)
                coords(i,j,k,1)=xtn(i,ngrid)
                coords(i,j,k,2)=ytn(j,ngrid)
             enddo
          enddo
       enddo
       if(itrans.eq.1) then
          do i=1,nnxp(ngrid)
             do j=1,nnyp(ngrid)
                do k=1,nnzp(ngrid)
                   coords(i,j,k,3)=ztn(k,ngrid)
                enddo
             enddo
          enddo
       elseif(itrans.eq.2) then
          do i=1,nnxp(ngrid)
             do j=1,nnyp(ngrid)
                do k=1,nnzp(ngrid)
                   coords(i,j,k,3)=ztn(k,ngrid)
                enddo
             enddo
          enddo
       endif
    endif
 endif

RETURN
end

!***************************************************************************

subroutine HYP_fill_fld (ilayer,ivar_type,flnm)

! routine to read in HYPACT particle locations for the requested species
! (other particle attributes are ignored) - the species and sources to be
! read are determined from the ':' tokenized field cvar(ilayer).
!   i.e. cvar(ilayer)='part:2:3' will result particles being drawn for 
!        species no 2, source no 3.

use hypparts

implicit none

integer :: ilayer,ivar_type
character(len=*)  :: flnm

include 'frame.h'
include 'interface.h'

integer :: mem,lv,iun,nsrc,ispec,isrc,iver,iy,imn,id,ih,im,ng,l,i  &
          ,ntotparts,ntokfr
real :: hytime
integer, parameter :: maxspec=50,maxem=300
integer, dimension(maxspec) :: pspecies
integer, dimension(maxem)   :: species,psource,nsrcparts
real, dimension(4) :: a
character(len=256) :: flnmp
character(len=16)  :: frtokens(50)
character(len=1)   :: toksep

integer, dimension(maxlayers) :: nparts,nsources
common /cparts/ nparts,nsources

data toksep/':'/

!print*,'HYP_fill_fld- ',cvar(ilayer)

ivar_type=0

lv=len_trim(cvar(ilayer))
call tokenize1 (cvar(ilayer)(1:lv)//':',frtokens,ntokfr,toksep)
read(frtokens(2)(1:len_trim(frtokens(2))),*,err=1) ispec
read(frtokens(3)(1:len_trim(frtokens(3))),*,err=2) isrc
goto 3
1 continue
ispec=0
print*,'warning: species unspecified or incorrectly specified'
print*,'         particles for all species and sources will be plotted'
2 continue
isrc=0
print*,'warning: source unspecified or incorrectly specified'
print*,'         particles for all sources will be plotted'
3 continue

flnmp=flnm(1:len_trim(flnm)-19)//'P'//  &
      flnm(len_trim(flnm)-17:len_trim(flnm))//'-g0.txt'
iun=90
print*,' F_open - ',flnmp(1:len_trim(flnmp)),'   ',cvar(ilayer)
open(iun,file=flnmp,status='old',err=4)
goto 5
4 continue
print*,'Could not find Particle file ',flnmp(1:len_trim(flnmp))
stop 'HYP_fill_fld'
5 continue

! get the number of particles for each species and source, where
!    nsrc is the internal HYPACT source number
!    psource(nsrc) is the source number from the database
!    species(nsrc) is the internal HYPACT species number
!    pspecies(species(nsrc)) is the species number from the database
!    nsrcparts(psource(nsrc)) is the number of particles from this source
! the total number of particles for each species is the sum of the source
! contributions

read(iun,'(i3,e16.8,8i8)') iver,hytime,iy,imn,id,ih,im,ng  &
                          ,nsources(ilayer),ntotparts
mem=0
do nsrc=1,nsources(ilayer)
   read(iun,'(4i8)') psource(nsrc),species(nsrc)  &
                    ,pspecies(nsrc),nsrcparts(nsrc)
   !print*,nsrc,psource(nsrc),species(nsrc),pspecies(nsrc)  &
   !      ,nsrcparts(nsrc)

   if((ispec.eq.pspecies(nsrc).or.ispec.eq.0).and.  &
      (isrc.eq.psource(nsrc).or.isrc.eq.0)) then
      mem=mem+nsrcparts(nsrc)
      ivar_type=1
   endif
enddo

! allocate required memory
if(allocated(atp)) deallocate(atp)
allocate(atp(maxlayers,mem*6))
!print*,'allocated particle memory',maxlayers,mem,mem*6

! read in the particle location attributes for the requested species

nparts(ilayer)=0
do nsrc=1,nsources(ilayer)
   !print*,nsrc,nsrcparts(nsrc)
   do l=1,nsrcparts(nsrc)
      read(iun,'(4e16.8)') (a(i),i=1,4)
      if((ispec.eq.pspecies(nsrc).or.ispec.eq.0).and.  &
         (isrc.eq.psource(nsrc).or.isrc.eq.0)) then
         nparts(ilayer)=nparts(ilayer)+1
         do i=1,4
            atp(ilayer,(nparts(ilayer)-1)*6+i)=a(i)
         enddo
         atp(ilayer,(nparts(ilayer)-1)*6+5)=float(psource(nsrc))
         atp(ilayer,(nparts(ilayer)-1)*6+6)=float(pspecies(nsrc))
      endif
   enddo
enddo

close(iun)

!print*,'species',ispec ,'  source ',isrc ,'  parts ',nparts(ilayer)
call read_SPEC(ispec,isrc,flnmp,cdname(ilayer),cdunits(ilayer))

return
end

!***************************************************************************

subroutine RAMS_Ptrans (n1,n2,n3,a,pi,b)

include 'plevs.h'
dimension a(n1,n2,n3),pi(n1,n2,n3),b(n3,*)

do np=1,nplevs
   b(nplevs-np+1,4)=1004.*(float(iplevs(np))/1000.)**.286
enddo

do j=1,n2
   do i=1,n1
      do k=1,n3
         kk=n3-k+1
         b(kk,1)=a(i,j,k)
         b(kk,2)=pi(i,j,k)
      enddo
      call htint(n3,b(1,1),b(1,2),nplevs,b(1,3),b(1,4))
      do k=1,nplevs
         a(i,j,nplevs-k+1)=b(k,3)
      enddo
   enddo
enddo

return
end

!***************************************************************************

subroutine RAMS_Ctrans (n1,n2,n3,a,b,topo,zt,ztop)

dimension a(n1,n2,n3),b(n3,*),topo(n1,n2),zt(n3)

do j=1,n2
   do i=1,n1
      do k=1,n3
         b(k,1)=a(i,j,k)
         b(k,2)=topo(i,j)+zt(k)*(1.-topo(i,j)/ztop)
      enddo
      call htint(n3,b(1,1),b(1,2),n3,b(1,3),zt)
      do k=1,n3
         a(i,j,k)=b(k,3)
      enddo
   enddo
enddo

return
end

!***************************************************************************

integer function ichvar_changed (cvar,ch)

character*(*) cvar,ch
character*12 ctime,chvar,cgrid,cwinds,ocvar
character*80 cfile
save
data cfile/' '/,ctime/' '/,chvar/' '/,cgrid/' '/  &
    ,cwinds/' '/,ocvar/' '/

ichvar_changed=0
if(cvar.eq.'cfile') then
   if(ch.ne.cfile) then
      ichvar_changed=1
      cfile=ch
   endif
elseif(cvar.eq.'ctime') then
   if(ch.ne.ctime) then
      ichvar_changed=1
      ctime=ch
   endif
elseif(cvar.eq.'cvar') then
   if(ch.ne.chvar) then
      ichvar_changed=1
      chvar=ch
   endif
elseif(cvar.eq.'ocvar') then
   if(ch.ne.ocvar) then
      ichvar_changed=1
      ocvar=ch
   endif
elseif(cvar.eq.'cgrid') then
   if(ch.ne.cgrid) then
      ichvar_changed=1
      cgrid=ch
   endif
elseif(cvar.eq.'cwinds') then
   if(ch.ne.cwinds) then
      ichvar_changed=1
      cwinds=ch
   endif
endif

return
end

!***************************************************************************

integer function intvar_changed (cvar,ival)

character*(*) cvar
save
data iwoffl,iwoffr,iwoffb,iwofft,itrans,icoor,iplotinfo/0,0,0,0,0,0,0/

intvar_changed=0
if(cvar.eq.'iwoffl') then
   if(ival.ne.iwoffl) then
      intvar_changed=1
      iwoffl=ival
   endif
elseif(cvar.eq.'iwoffr') then
   if(ival.ne.iwoffr) then
      intvar_changed=1
      iwoffr=ival
   endif
elseif(cvar.eq.'iwoffb') then
   if(ival.ne.iwoffb) then
      intvar_changed=1
      iwoffb=ival
   endif
elseif(cvar.eq.'iwofft') then
   if(ival.ne.iwofft) then
      intvar_changed=1
      iwofft=ival
   endif
elseif(cvar.eq.'itrans') then
   if(ival.ne.itrans) then
      intvar_changed=1
      itrans=ival
   endif
elseif(cvar.eq.'iplotinfo') then
   if(ival.ne.iplotinfo) then
      intvar_changed=1
      iplotinfo=ival
   endif
elseif(cvar.eq.'icoor') then
   if(ival.ne.icoor) then
      intvar_changed=1
      icoor=ival
   endif
endif

return
end

!***************************************************************************

function iclosest (np,x,p)

dimension x(*)

iclosest=1
dmin=abs(x(1)-p)
do n=2,np
   if(abs(x(n)-p).lt.dmin) then
      iclosest=n
      dmin=abs(x(n)-p)
   endif
enddo
return
end

!***************************************************************************

subroutine max_min( a,n,amx,amn)

dimension a(*)

amx=a(1)
amn=a(1)
do i=2,n
   amx=max(amx,a(i))
   amn=min(amn,a(i))
enddo

return
end

!***************************************************************************

subroutine thelatlon (n1,n2,n3,alatlon,all1,i1,j1)

dimension alatlon(n1,n2,n3)

all1=alatlon(i1,j1,1)

return
end

