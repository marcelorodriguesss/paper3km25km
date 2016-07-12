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

!      program def
!      character*20 vlabel,vunit,vstring*200

!      call var_defaults_read('contour.defaults')

!      call var_labels('tke',vlabel)
!      call var_units('tke',vunit)
!      call var_cats('tke',icat)
!      print*,'got:',icat,' ',vlabel,vunit
!      call var_contours('tke',clo,chi,cinc)
!      print*,'got:',clo,chi,cinc

!      call var_setlabels('tke','turb stuff')
!      call var_setunits('tke','m/s/r/t/y;')
!      call var_setcats('tke',5)
!      call var_setcontours('tke',35.,47.,-1233336.)

!      call var_contours('tke',clo,chi,cinc)
!      print*,'got:',clo,chi,cinc
!      call var_labels('tke',vlabel)
!      call var_units('tke',vunit)
!      call var_cats('tke',icat)
!      print*,'got:',icat,' ',vlabel,vunit

!      call var_defaults_write('c.def')

!      call var_getcat(2,nvc,vstring)
!      call var_getcat(3,nvc,vstring)
!      call var_getcat(5,nvc,vstring)
!      end

subroutine var_defaults_read(flnm)
character*(*) flnm,vname,vlabel,vunit,vstring
parameter (maxvar=200)
character vnames*16,vlabels*40,vunits*40
common/var_defchars/vnames(maxvar),vlabels(maxvar),vunits(maxvar)
common/var_defparams/nvar,icats(maxvar)  &
     ,conlo(maxvar),conhi(maxvar),coninc(maxvar)
common/var_params/  &
     conlo2(maxvar),conhi2(maxvar),coninc2(maxvar)
parameter (nparams=1)
common/def_params/ioffx1,ioffx2,ioffy1,ioffy2,ioffz1,ioffz2
character line*200
data nofile/0/
save

print*,'in var_defaults_read: ',flnm

nfields=7

open(20,file=flnm,status='old',err=200)

nvar=0

do nl=1,10000
   read(20,'(a)',end=100) line
      if(line(1:1).eq.' '.or.line(1:1).eq.char(0)) goto 10

   if(line(1:1).eq.'#') then
!     This is a param line:
      if(line(1:6).eq.'#param') then
         nsc=index(line(8:),';')-2
         if(line(8:8+nsc).eq.'offsets') then
            read(line(8+nsc+2:),*) ioffx1,ioffx2,ioffy1,ioffy2,ioffz1,ioffz2
         elseif(line(8:8+nsc).eq.'headleng0') then
            read(line(8+nsc+2:),*) headleng0
         elseif(line(8:8+nsc).eq.'headleng') then
            read(line(8+nsc+2:),*) headleng
         elseif(line(8:8+nsc).eq.'headang') then
            read(line(8+nsc+2:),*) headang
         elseif(line(8:8+nsc).eq.'stemleng') then
            read(line(8+nsc+2:),*) stemleng
         endif
      endif
      goto 10
   endif

   nvar=nvar+1

   ic=1
   do nf=1,nfields
      icc=index(line(ic:200),';')
      ice=icc+ic-1
      if(nf.eq.1) then
         vnames(nvar)=line(ic:ice)
      elseif(nf.eq.2) then
         read(line(ic:ice-1),*) icats(nvar)
      elseif(nf.eq.3) then
         vlabels(nvar)=line(ic:ice)
      elseif(nf.eq.4) then
         vunits(nvar)=line(ic:ice)
      elseif(nf.eq.5) then
         read(line(ic:ice-1),*) conlo(nvar)
      elseif(nf.eq.6) then
         read(line(ic:ice-1),*) conhi(nvar)
      elseif(nf.eq.7) then
         read(line(ic:ice-1),*) coninc(nvar)
      endif

      ic=ic+icc
   enddo

!print*,nvar,vnames(nvar)
!print*,vnames(nvar)
!print*,vlabels(nvar)
!print*,vunits(nvar)
!print*,conlo(nvar),conhi(nvar),coninc(nvar)

10 continue
enddo
goto 100

200 continue
print*,'variable defaults file not read- file name:',flnm
nofile=1

100 continue

do nv=1,nvar
   conlo2(nv)=conlo(nv)
   conhi2(nv)=conhi(nv)
   coninc2(nv)=coninc(nv)
enddo

close(20)

return

!*******************************************************************************

entry var_defaults_write(flnm)

if(nofile.eq.1) return
nfields=6

open(20,file=flnm,status='unknown',err=201)
rewind 20

do n=1,nvar
   ic=1
   icc=index(vnames(n),';')
   line(ic:icc)=vnames(n)(1:icc)
   ic=ic+icc

   write(line(ic:),'(i3,a1)') icats(n),';'
   ic=ic+4

   icc=index(vlabels(n),';')
   line(ic:ic+icc)=vlabels(n)(1:icc)
   ic=ic+icc
   icc=index(vunits(n),';')
   line(ic:ic+icc)=vunits(n)(1:icc)
   ic=ic+icc
   write(line(ic:),'(g11.5,a1)') conlo(n),';'
   ic=ic+12
   write(line(ic:),'(g11.5,a1)') conhi(n),';'
   ic=ic+12
   write(line(ic:),'(g11.5,a1)') coninc(n),';'

   write(20,'(a)') line
enddo

goto 101

201 continue
print*,'Error in variable defaults file- file name:',flnm

101 continue
close(20)

return

!*******************************************************************************

entry var_contours(vname,clo,chi,cinc)

if(nofile.eq.1) return
nb=index(vname,' ')-1
if(nb.le.0) nb=lastchar(vname)

do n=1,nvar
   nc=index(vnames(n),';')-1
   if(vname(1:nb).eq.vnames(n)(1:nc)) then
      clo=conlo2(n)
      chi=conhi2(n)
      cinc=coninc2(n)
      return
   endif
enddo
print*,'Var_contours - variable name not found:',vname
return

!*******************************************************************************

entry var_resetcontours(vname,clo,chi,cinc)

if(nofile.eq.1) return
nb=index(vname,' ')-1
if(nb.le.0) nb=lastchar(vname)

do n=1,nvar
   nc=index(vnames(n),';')-1
   if(vname(1:nb).eq.vnames(n)(1:nc)) then
      clo=conlo(n)
      chi=conhi(n)
      cinc=coninc(n)
      conlo2(n)=clo
      conhi2(n)=chi
      coninc2(n)=cinc
      return
   endif
enddo
print*,'Var_resetcontours - variable name not found:',vname
return

!*******************************************************************************

entry var_defcontours(vname,clo,chi,cinc)
print*,'var_defcontours(',vname,clo,chi,cinc,') START'
if(nofile.eq.1) return
nb=index(vname,' ')-1
if(nb.le.0) nb=lastchar(vname)

do n=1,nvar
   nc=index(vnames(n),';')-1
   if(vname(1:nb).eq.vnames(n)(1:nc)) then
      conlo2(n)=0.
      conhi2(n)=0.
      coninc2(n)=0.

!     BPT put the next three lines in so that it would do something nicer
!     in ringi.  This is NOT reseting to the default found in file.  I wasn't
!     quite sure how to do that properly.  Prior to this it was not passing
!     back anything at all to the calling procedure.
      clo=conlo(n)
      chi=conhi(n)
      cinc=coninc(n)
      return
   endif
enddo
print*,'Var_defcontours - variable name not found:',vname
return

!*******************************************************************************

entry var_cats(vname,icat)

if(nofile.eq.1) return
nb=index(vname,' ')-1
if(nb.le.0) nb=lastchar(vname)

do n=1,nvar
   nc=index(vnames(n),';')-1
   if(vname(1:nb).eq.vnames(n)(1:nc)) then
      icat=icats(n)
      return
   endif
enddo
print*,'Var_cats  - variable name not found:',vname
return

!*******************************************************************************

entry var_labels(vname,vlabel)

if(nofile.eq.1) return
nb=index(vname,' ')-1
if(nb.le.0) nb=lastchar(vname)

do n=1,nvar
   nc=index(vnames(n),';')-1
   if(vname(1:nb).eq.vnames(n)(1:nc)) then
      vlabel=vlabels(n)
      return
   endif
enddo
print*,'Var_labels - variable name not found:',vname
return

!*******************************************************************************

entry var_units(vname,vunit)

if(nofile.eq.1) return
nb=index(vname,' ')-1
if(nb.le.0) nb=lastchar(vname)

do n=1,nvar
   nc=index(vnames(n),';')-1
   if(vname(1:nb).eq.vnames(n)(1:nc)) then
      vunit=vunits(n)
      return
   endif
enddo
print*,'Var_units - variable name not found:',vname
return

!*******************************************************************************

entry var_setcontours(vname,clo,chi,cinc)
print*,'Var_setcontours - ',vname,clo,chi,cinc

if(nofile.eq.1) return
nb=index(vname,' ')-1
if(nb.le.0) nb=lastchar(vname)

do n=1,nvar
   nc=index(vnames(n),';')-1
   if(vname(1:nb).eq.vnames(n)(1:nc)) then
      conlo2(n)=clo
      conhi2(n)=chi
      coninc2(n)=cinc
      return
   endif
enddo
print*,'Var_setcontours - variable name not found:',vname
return

!*******************************************************************************

entry var_setcats(vname,icat)

if(nofile.eq.1) return
nb=index(vname,' ')-1
if(nb.le.0) nb=lastchar(vname)

do n=1,nvar
   nc=index(vnames(n),';')-1
   if(vname(1:nb).eq.vnames(n)(1:nc)) then
      icats(n)=icat
      return
   endif
enddo
print*,'Var_setcats - variable name not found:',vname
return

!*******************************************************************************

entry var_setlabels(vname,vlabel)

if(nofile.eq.1) return
nb=index(vname,' ')-1
if(nb.le.0) nb=lastchar(vname)
if(index(vlabel,';').eq.0) then
   print*,'var_setlabels - no ; in string for : ',vname
   return
endif

do n=1,nvar
   nc=index(vnames(n),';')-1
   if(vname(1:nb).eq.vnames(n)(1:nc)) then
      vlabels(n)=vlabel
      return
   endif
enddo
print*,'Var_setlabels - variable name not found:',vname
return

!*******************************************************************************

entry var_setunits(vname,vunit)

if(nofile.eq.1) return
nb=index(vname,' ')-1
if(nb.le.0) nb=lastchar(vname)
if(index(vunit,';').eq.0) then
   print*,'var_setunits - no ; in string for : ',vname
   return
endif
do n=1,nvar
   nc=index(vnames(n),';')-1
   if(vname(1:nb).eq.vnames(n)(1:nc)) then
      vunits(n)=vunit
      return
   endif
enddo
print*,'Var_setunits - variable name not found:',vname
return

!*******************************************************************************

entry var_getcat(icat,nvcat,vstring)

if(nofile.eq.1) return
vstring=' '
ic=1
nvcat=0
do n=1,nvar
   if(icat.eq.icats(n)) then
      nvcat=nvcat+1
      nc=index(vnames(n)(1:),';')
      vstring(ic:ic+nc)=vnames(n)(1:nc)
      vstring(ic+nc-1:ic+nc-1)='/'
      ic=ic+nc
   endif
enddo
if(nvcat.gt.0) vstring(ic-1:ic-1)=' '
print*,'vstring-',icat,nvcat,vstring(1:ic-1),lastchar(vstring)

return

!*******************************************************************************

entry var_getparams(vname,i1,i2,i3,i4,i5,i6)

i1=ioffx1
i2=ioffx2
i3=ioffy1
i4=ioffy2
i5=ioffz1
i6=ioffz2

return
end
