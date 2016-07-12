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

subroutine RAMS_v5dh(nfl,ngd)

use an_header

implicit none

integer :: nfl,ngd

include 'vcomm2.h'
include 'v5df.h'
include 'interface.h'

integer, parameter :: maxfiles=1000
integer :: n,nvi,nvv,izz,iiz,iz,ntt,nti,id,j,i,nfle,ng,maxmem  &
          ,maxnl,ierr_getvar,ifound,ivar_type,memsiz4,ntokfr  &
          ,imon2,iday2,iyear2,icent2,nv,ntb,nte,ntinc  &
          ,lenv,nllat,nllon,lastslash,niii,njjj,nval
integer, save :: ncall
real :: swllat,swllon,nellat,nellon,dllat,dllon
integer dmonth(12),dmonthl(12)
integer :: ifdates(maxfiles),iftimes(maxfiles)
integer nl(MAXVARS),vertical,fldates(MAXTIMES),ivtyp(MAXVARS)
integer fltimes(MAXTIMES),compressmode,projection
real proj_args(100),vert_args(MAXLEVELS)
real, allocatable :: a(:),b(:)
character(len=1)   :: toksep,cwind,ftran
character(len=16)  :: frtokens(50)
character(len=256) :: flnm,flnm1
character(len=24)  :: cdname,cdunits
character(len=3)   :: cmon(12)
character(len=10)  :: varname(MAXVARS),varunits(MAXVARS)
character(len=80)  :: fnames(maxfiles)
character(len=256) :: fpref

common /v5dc/nl,ivtyp,maxnl,swllat,swllon,dllat,dllon,nllat,nllon,niii,njjj
common /getvar/ierr_getvar,ifound,ivar_type
common /revu_memsize/memsiz4

data toksep/'/'/
data ncall/0/

data cmon/'jan','feb','mar','apr','may','jun'  &
         ,'jul','aug','sep','oct','nov','dec'/
data dmonth/0,31,28,31,30,31,30,31,31,30,31,30/
data dmonthl/0,31,29,31,30,31,30,31,31,30,31,30/
data (varname(i),i=1,MAXVARS) / MAXVARS*"          " /
data (varunits(i),i=1,MAXVARS) / MAXVARS*"          " /
data vertical / IMISSING /
data nvv / IMISSING /
data ntt / IMISSING /
data (fldates(i),i=1,MAXTIMES) / MAXTIMES*IMISSING /
data (fltimes(i),i=1,MAXTIMES) / MAXTIMES*IMISSING /
data compressmode / 1 /
data projection / IMISSING /
data (proj_args(i),i=1,100) / 100*MISSING /
data (vert_args(i),i=1,100) / 100*MISSING /

!print*,'RAMS_v5dh: ',maxfore
!print*

nvi=0
do nplot=1,maxfore

   ! get cvar and do some checks on validity and increment counter
   if(cframe_a(nplot)(1:1).ne.'/') goto 5
   CALL TOKENIZE1(cframe_a(nplot),frtokens,ntokfr,toksep)
   if(frtokens(1).eq.'none'.or.len_trim(frtokens(1)).eq.0) goto 5
   nvi=nvi+1
   varname(nvi)=frtokens(1)(1:min(10,len_trim(frtokens(1))))

   ! need to get some data for each varaible for the grid setup
   call RAMS_get_cdata(0,itbeg,flnm,nval)
   print*
   print*,'open header file- ',flnm(1:len_trim(flnm))
   open(10,file=flnm)
   read(10,*) nvbtab
   if (allocated(anal_table)) deallocate (anal_table)
   allocate (anal_table(nvbtab))
   do nv=1,nvbtab
      read(10,*)  anal_table(nv)%string   &
                 ,anal_table(nv)%npointer  &
                 ,anal_table(nv)%idim_type  &
                 ,anal_table(nv)%ngrid  &
                 ,anal_table(nv)%nvalues
   enddo
   call commio('ANAL','READ',10)
   close(10)
   call grdcoords

   allocate (a(nnxp(ngd)*nnyp(ngd)*nnzp(ngd)))
   allocate (b(nnxp(ngd)*nnyp(ngd)*nnzp(ngd)))
   
  ! get required memory size for RAMS_varlib allocations
   if(ncall.eq.0) then
      memsiz4=0
      do ng=1,ngrids
         memsiz4=max(memsiz4,(nnxp(ng)+1)*(nnyp(ng)+1)  &
            *max(nnzp(ng),(nzg+nzs+3)*npatch))
      enddo
   endif
   ncall=1

   call RAMS_varlib(varname(nvi),nnxp(ngd),nnyp(ngd)  &
                   ,nnzp(ngd),ngd,a,b  &
                   ,flnm(1:len_trim(flnm)-9),cdname,cdunits  &
                   ,icoor)
   deallocate (a,b)

   call RAMS_varinfo(1,ivtyp(nvi))
   if(ivtyp(nvi).eq.0) then
      print*,'Variable not available in hvlib: ',varname(nvi)
      nvi=nvi-1
      goto 5
   endif

   if(ierr_getvar.eq.1) then
      print*,'Variable not available in anal files for: ',varname(nvi)
      nvi=nvi-1
      goto 5
   endif

   ! reassign some variable names so skewt plots automatically detect vars
   if(varname(nvi)(1:2).eq.'u ') then
      print*,'Warning: should use ue_avg for U but using u anyway'
      varname(nvi)='U'
   endif
   if(varname(nvi)(1:3).eq.'ue ') then
      print*,'Warning: should use ue_avg for U but using ue anyway'
      varname(nvi)='U'
   endif
   if(varname(nvi)(1:3).eq.'ue_avg ') varname(nvi)='U'
   if(varname(nvi)(1:2).eq.'v ') then
      print*,'Warning: should use ve_avg for V but using v anyway'
      varname(nvi)='V'
   endif
   if(varname(nvi)(1:3).eq.'ve ') then
      print*,'Warning: should use ve_avg for V but using ve anyway'
      varname(nvi)='V'
   endif
   if(varname(nvi)(1:3).eq.'ve_avg ') varname(nvi)='V'
   if(varname(nvi)(1:2).eq.'w ') then
      print*,'Warning: should use w_avg for W but using w anyway'
      varname(nvi)='W'
   endif
   if(varname(nvi)(1:2).eq.'w_avg ') varname(nvi)='W'
   if(varname(nvi)(1:6).eq.'tempk ') varname(nvi)='T'
   if(varname(nvi)(1:7).eq.'dewptk ') varname(nvi)='TD'
   lenv=index(cdunits,';')-1
   varunits(nvi)=cdunits(1:lenv)//' '

   if(nplot.eq.1) then

      ! domain size (horizontal) - ensure selection is one in from edges

      if(ixbeg.le.0) then
         nib=1-ixbeg
      else
         nib=ixbeg
      endif
      nib=max(nib,2)
      if(ixend.le.0) then
         nie=nnxp(ngd)+ixend
      else
         nie=ixend
      endif
      nie=min(nie,nnxp(ngd)-1)

      if(iybeg.le.0) then
         njb=1-iybeg
      else
         njb=iybeg
      endif
      njb=max(njb,2)
      if(iyend.le.0) then
         nje=nnyp(ngd)+iyend
      else
         nje=iyend
      endif
      nje=min(nje,nnyp(ngd)-1)
     
      call find_ll_grid (nnxp(ngd),nnyp(ngd),nib,nie,njb,nje  &
                        ,xtn(1,ngd),ytn(1,ngd),platn(ngd),plonn(ngd)  &
                        ,swllat,swllon,nellat,nellon  &
                        ,dllat,dllon,nllat,nllon  &
                        ,igridll,glldllat,glldllon  &
                        ,gllwlon,gllelon,gllslat,gllnlat)
                        
      niii=int(nllon/float(ixstep))+1
      if(niii.le.1) stop 'RAMS_v5dh- V5d grid too small in x dir'

      njjj=int(nllat/float(iystep))+1
      if(njjj.le.1) stop 'RAMS_v5dh- V5d grid too small in y dir'

   endif

   5 continue
enddo
10 continue
if(nvi.eq.0) Stop 'RAMS_v5dh- Nothing to plot'
nvv=nvi

! domain size (vertical)

nnb=max(izbeg,1)
if(izbeg.le.0) nnb=1-izbeg
nne=min(izend,nnzp(ngd))
if(izend.le.0) then
   if(iztran(1).eq.3) then
      nne=nplevs+izend
   elseif (iztran(1).eq.1.or.iztran(1).eq.2) then
      nne=nnzp(ngd)+izend
   endif
endif
nninc=max(1,izstep)
nnn=1+int(float(nne-nnb)/float(nninc))
if(nnn.le.0) stop 'RAMS_v5dh- V5d grid too small in vert dir'

ntb=max(itbeg,1)
nte=min(itend,nfl)
ntinc=max(1,itstep)
ntt=1+int(float(nte-ntb)/float(ntinc))

do nvi=1,nvv
   if(ivtyp(nvi).eq.3) then
      nl(nvi)=nnn
   elseif (ivtyp(nvi).eq.2) then
      nl(nvi)=1
   endif
enddo
maxnl=nl(1)
do nvi=1,nvv
   maxnl=max(maxnl,nl(nvi))
enddo
print*

! y2k fix
if(iyear1.gt.50.and.iyear1.lt.100) iyear1=iyear1+1900
if(iyear1.lt.50) iyear1=iyear1+2000

! file name
if(iztran(1).eq.3) then
   ftran='P'
elseif(iztran(1).eq.2) then
   ftran='C'
elseif(iztran(1).eq.1) then
   ftran='S'
endif
call RAMS_get_cdata(0,1,flnm1,nval)
write(flnm,'(2a,2a1,i4.4,a1,i2.2,a1,i2.2,a1,i6.6,a2,i1,a4)' )  &
    revpref(1:len_trim(revpref))  &
   ,flnm1(lastslash(flnm1)+1:len_trim(flnm1)-27),ftran,'-'  &
   ,iyear1,'-',imonth1,'-',idate1,'-',itime1*100,'-g',ngd,'.v5d'
print*,'Vis5d file: ',flnm(1:len_trim(flnm))

! dates and times

nti=0
do nfle=max(itbeg,1),min(itend,nfl),itstep
   ! Skip time if this grid is not there
      print*,'555555:',nfle,ngd,gridinv(nfle,ngd)
   if(.not.gridinv(nfle,ngd)) cycle
   nti=nti+1
   call RAMS_get_idata(3,nfle,ngd,iftimes(nfle),nval)
   call RAMS_get_idata(2,nfle,ngd,ifdates(nfle),nval)
   fltimes(nti)=float(iftimes(nfle))
   icent2=int(float(ifdates(nfle))/1000000.)
   iyear2=int(float(ifdates(nfle))/10000.)-icent2*100
   imon2=int(float(ifdates(nfle))/100.)-icent2*10000-iyear2*100
   iday2=ifdates(nfle)-icent2*1000000-iyear2*10000-imon2*100
   id=0
   do j=1,imon2
      if(mod(iyear2,4).eq.0) then
         id=id+dmonthl(j)
      else
         id=id+dmonth(j)
      endif
   enddo
   fldates(nti)=iyear2*1000+iday2+id
   print*,nti,fldates(nti),fltimes(nti)
enddo
print*

ntt=nti


print*,'Vis5d grid'
print*,'   no. of times  = ',ntt
print*,'   no. of vars   = ',nvv
print*,'   z grid pts    = ',maxnl
print*,'   x grid pts    = ',niii
print*,'   y grid pts    = ',njjj
print*

print*,'Vis5D times'
do nti=1,ntt
   print*,nti,fltimes(nti),fldates(nti)
enddo
print*
print*,'Vis5D vars'
do nvi=1,nvv
   print*,nvi,'  ',varname(nvi),varunits(nvi),nl(nvi)
enddo
print*

! vertical arguments

if(iztran(1).eq.3) then
   vertical=3
   print*,'Vis5d vertical levels (mb)'
   iiz=0
   do iz=nnb,nne,nninc      ! v5d grid z array
      iiz=iiz+1
      vert_args(iiz)=iplevs(iz)
      print*,iiz,vert_args(iiz)
   enddo
elseif (iztran(1).eq.1.or.iztran(1).eq.2) then
   vertical=2
   print*,'Vis5d vertical levels (km)'
   iiz=0
   do iz=nnb,nne,nninc      ! v5d grid z array
      iiz=iiz+1
      vert_args(iiz)=ZTN(iz,ngd)/1000.
      if(vert_args(iiz).lt.0.) vert_args(iiz)=0.
      print*,iiz,vert_args(iiz)
   enddo
endif
print*

! set projection

projection=1
print*,'Vis5d projection params:',' projection = ',projection
proj_args(1)=nellat
proj_args(2)=-swllon    ! North America is +ve
proj_args(3)=dllat
proj_args(4)=dllon
print*,'   northern edge  = ',proj_args(1)
print*,'   western edge   = ',proj_args(2)
print*,'   del lat (deg)  = ',proj_args(3)
print*,'   del lon (deg)  = ',proj_args(4)
print*

! set compressmode

compressmode=1
print*,'Vis5d compressmode = ',compressmode
print*

! create v5d file

!print*,flnm(1:len_trim(flnm))
!print*,ntt,nvv,niii,njjj
!print*,(nl(i),i=1,nvv)
!print*,(varname(i),i=1,nvv)
!print*,(fltimes(i),i=1,ntt)
!print*,(fldates(i),i=1,ntt)
!print*,compressmode
!print*,projection
!print*,(proj_args(i),i=1,4)
!print*,vertical
!print*,(vert_args(i),i=1,nl(1))

n=v5dCreate (flnm(1:len_trim(flnm)),ntt,nvv,njjj,niii,nl  &
            ,varname,fltimes,fldates,compressmode  &
            ,projection,proj_args,vertical,vert_args)
            
if(n.eq.0) then
   print*,'value returned:',n
   stop 'RAMS_v5dh- v5dCreate'
endif
print*,'Vis5D file created'

do nvi=1,nvv

   n=v5dsetunits(nvi,varunits(nvi))
   
   if(n.eq.0) then
      print*,'value returned:',n
      stop 'RAMS_v5dh- v5dSetUnits'
   endif
enddo
print*,'Vis5D units set'
print*,'--------------'

return
end

!***************************************************************************

subroutine RAMS_v5d (n1,n2,n3,ag,xt,yt,niinc,njinc,nninc,nnb,nne  &
                    ,platn,plonn,ivtime,ivvar,itrans)

implicit none

integer :: n1,n2,n3,ivtime,ivvar,nnb,nne,niinc,njinc,nninc,itrans
real :: ag(n1,n2,n3),xt(*),yt(*),platn,plonn

include 'v5df.h'
include 'interface.h'

real, allocatable :: gg(:,:,:)
integer nl(maxvars),ivtyp(maxvars)
integer :: nllat,nllon,maxnl,n,niii,njjj
real :: swllat,swllon,dllat,dllon

common /v5dc/nl,ivtyp,maxnl,swllat,swllon,dllat,dllon,nllat,nllon,niii,njjj

if(allocated(gg)) deallocate(gg); allocate(gg(njjj,niii,maxnl))

call interp_ll_v5d (n1,n2,n3,ag,xt,yt,niinc,njinc,nninc  &
                   ,niii,njjj,maxnl,swllat,swllon,dllat,dllon  &
                   ,platn,plonn,nnb,nne,itrans,gg)

n=v5dwrite(ivtime,ivvar,gg)

if(n.eq.0) then
   print*,'value returned:',n
   stop 'RAMS_v5dh- v5dWrite'
endif
print*,'Vis5d write complete for',ivtime,ivvar
print*,'--------------'

deallocate(gg)

return
end
