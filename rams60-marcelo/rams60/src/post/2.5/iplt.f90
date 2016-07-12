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
! 2.3.1.12
!
! 010314 MJB read_RAMS ##
!            Stopped map drawing when do not wish to display it. ##
! 010311 MJB read_RAMS ##
!            Added MEDOC gridded output. ##
! 010215 MJB read_RAMS ##
!            Added ability to plot without any plot information - 
!            IPLTINFO=3. ##
! 010215 MJB read_RAMS ##
!            Added ability to plot grid boundaries using a LANDMARKS file
!            entry of 'grid#' where the '#' is 0 (all grids) or the grid
!            number. ##
! 010215 MJB read_RAMS ##
!            Added ability to plot just the map with CFRAME_A(1)='/map/'. ##
! 010206 MJB read_RAMS ##
!            Fixed to allow viewing of 2D runs. ##
! 010126 BPT read_RAMS ##
!            Fixed problem where ringi wouldn't keep the current zoom. ##
! 001209 MJB read_RAMS ##
!            Replaced different transformation STOP with a WARNING. ##
! 001204 MJB read_RAMS ##
!            Added brief plot info option. ##
! 001204 MJB read_RAMS ##
!            Fix to modified memory allocations. ##
! 001114 MJB read_RAMS ##
!            Added ability to draw line between landmarks. ##
! 001114 MJB read_RAMS ##
!            Now using t point averaged u, v and w for vectors and barbs. ##
! 001107 MJB read_RAMS ##
!            Modified memory allocations. ##
! 001019 MJB read_RAMS ##
!            Added tracking of max and min values with chimax and clomin. ##
! 001014 MJB read_RAMS ##
!            Added output filename prefix REVPREF.
!            Standardized output filenames. ##
! 001013 MJB read_RAMS ##
!            Added new color/plotting controls.
!            Added panels options. ##
! 001012 MJB read_RAMS ##
!            Fixed bug in Vis5D pressure level file (IZTRAN=3). ##
! 000927 MJB date_mkgrads ##
!            Moved date_mkgrads to RAMS_grads.f90. ##
! 000925 MJB read_RAMS ##
!            Removed background color call - now done in gks_colors.
!            Getting fillcols and icolmeth in argument list. ##
! 000923 MJB read_RAMS ##
!            Fix for plot pressure surfaces.
!            Fix to f90 memory allocation mods made on 000915.
!            GRIB calls added. ##
! 000921 MJB read_RAMS ##
!            Cleaned up screen output. ##
! 000915 CJT read_RAMS ##
!            Switch to f90 memory allocation and implict none. ##
! 000906 MJB read_RAMS ##
!            Removed pre RAMS 4.2 memory size determination. ##
! 000830 CJT read_RAMS ##
!            Included interface.h and removed anal_table from dumpout call. ##
! 000829 CJT read_RAMS ##
!            Changed from include file to using an_header module. ##
!
!###########################################################################

subroutine read_RAMS (prog,action,ivvar,ivtime  &
                     ,cgrid,ctime,itrans,ccont  &
                     ,niboff,nieoff,njboff,njeoff,nnboff,nneoff  &
                     ,head1,ppx1,ppx2,ppy1,ppy2,fobs,pred,fpos  &
                     ,cframe_a,maxfore,igunit,glats,glatn,glonw,glone  &
                     ,irefg,jrefg,xref,yref,dategrads,cincgrads  &
                     ,igridll,glldllat,glldllon  &
                     ,gllwlon,gllelon,gllslat,gllnlat  &
                     ,cdname1,cdunits1,revpref)
                     
use an_header

implicit none

real :: fobs(*),pred(*),fpos(*)
character(len=*) :: action,cgrid,ctime,ccont,head1,cdname1,cdunits1  &
                   ,revpref,prog,cframe_a(*)
integer :: ivvar,ivtime,itrans,igunit,irefg,jrefg,igridll  &
          ,niboff,nieoff,njboff,njeoff,nnboff,nneoff,maxfore
real :: ppx1,ppx2,ppy1,ppy2  &
       ,glats,glatn,glonw,glone,xref,yref,glldllat,glldllon  &
       ,gllwlon,gllelon,gllslat,gllnlat
             
include 'rcommons.h'
include 'plevs.h'
include 'frame.h'
include 'window.h'
include 'interface.h'

character(len=100) :: listvar,str_grids
character(len=1)   :: horiz,vert
character(len=256) :: flnm
character(len=80)  :: cfile,hfiles(100)
character(len=16)  :: cinfo

real, allocatable, save :: arra(:),arrb(:),arrbb(:),arrover(:),coor(:)  &
                          ,slab(:),sarr2(:),slabover(:),scoor(:)  &
                          ,wind(:),swind(:),windt(:),swindt(:)  &
                          ,topo(:),pctlnd(:),cornert(:,:,:),corner(:,:,:)
real, dimension(1000) :: xmplot,xtplot,ymplot,ytplot
real :: work(2*maxdim**2)
integer :: idims(3)

integer :: memsiz4,maxmem,maxslab,maxnxyg
common /revu_memsize/memsiz4

character(len=24) :: cdscr
integer, parameter :: nztr=1000
real, dimension(nztr) :: ztr,zs
integer ivtran,izstran
real :: zmodtop
common /trans2/ivtran,ztr,izstran,zs,zmodtop

real :: swllat,swllon,nellat,nellon,av5d(1),zuv(nztr)  &
       ,xmin,xmax,ymin,ymax,zmin,zmax
integer :: ibugout
common /mapping/ibugout
common /mapcnr/xmin,xmax,ymin,ymax

real :: alat1(maxgrds),alon1(maxgrds)
data alat1 /maxgrds*0./, alon1 /maxgrds*0./

real :: xloc,yloc,rloc,xxx,yyy,xx1,xx2,yy1,yy2,boxtop,boxhite  &
       ,xdist,ydist,dllat,dllon,glats1,glats2,glatn1,glatn2  &
       ,glatref,glonw1,glonref,glonw2,glone1,glone2,vioff,vjoff
integer :: ipx,ipy,ngr,iu,iuu,iv,ivv,ixx1,ixx2,iyy1,iyy2,izz1,izz2  &
          ,itime,idate,leafdim,ittt,icmp,ng,nllat,nllon  &
          ,ivtype,iovtype,iwvtype,i,j,k

! GrADS arrays
integer, parameter :: nplmax=30
character :: dategrads*15,cincgrads*4

integer :: nf,nval,lenhf,nv,nngd,nnnn
integer :: islab_r,icoor_r    !  Arguments passed from Ringi C code
real :: fcstsec,startutc,ppx1m,ppx2m,ppy1m,ppy2m

integer, external ::  ichvar_changed,iclosest,intvar_changed,lastchar
integer,save :: ncall=0,icfile,icfilen,icgrid,icplotinfo,icvar,iocvar  &
               ,icwinds,ictrans,iccoor,iciwoff,ihtflx=0
character(len=1), save :: cddel=';'

if(ncall.eq.0) then
   do i=1,maxlayers
      do j=1,maxframes
         do k=1,maxgrds
            clomin(i,j,k)=1.e15
            chimax(i,j,k)=-1.e15
            cdnames(i,j,k)=''
            cdunitss(i,j,k)=''
         enddo
      enddo
   enddo
endif

if(cvar(1)(1:8).eq.'part_lag'.and.cvar(1)(13:14).ne.'  ') then
   conttyp(1)=cvar(1)(14:14)
   cvar(1)(13:14)='  '
endif
if(cvar(2)(1:8).eq.'part_lag'.and.cvar(2)(13:14).ne.'  ') then
   conttyp(2)=cvar(2)(14:14)
   cvar(2)(13:14)='  '
endif

print*
!print*,'in read_RAMS ',action,' ',cgrid,' ',ctime
!print*,'cvar(1) ',cvar(1),' ',conttyp(1),conrinc(1),conrlo(1),conrhi(1)
!print*,'ovar ',cvar(2),' ',conttyp(2),conrinc(1),conrlo(2),conrhi(2)
!print*,'wind ',cwinds,' ',intwindi,intwindj,ibscale

icfile=ichvar_changed('ctime',ctime)
read(ctime,*) nf
call RAMS_get_cdata (0,nf,flnm,nval)
lenhf=len_trim(flnm)
write(cfile,*) flnm(1:lenhf-9)
icfilen=ichvar_changed('cfile',cfile)
if(icfilen.eq.1) icfile=1

if(icfile.eq.1.or.ncall.eq.0) then
   open(10,file=flnm)
   read(10,*) nvbtab
   if(allocated(anal_table)) deallocate (anal_table)
   allocate (anal_table(nvbtab))
   do nv=1,nvbtab
      read(10,*)  anal_table(nv)%string   &
                 ,anal_table(nv)%npointer  &
                 ,anal_table(nv)%idim_type  &
                 ,anal_table(nv)%ngrid  &
                 ,anal_table(nv)%nvalues
                 
      if(action(1:5).eq.'MEDOC') then
         ! Check 2D variables to see if surface heat flux is on the tape
         if(anal_table(nv)%idim_type==2.and.  &
            anal_table(nv)%string(1:3)=='TFZ') ihtflx=1
      endif
   enddo
   call commio('ANAL','READ',10)
   close(10)
   
   call RAMS_get_fdata (1,nf,nngd,fcstsec,nnnn)
   call RAMS_get_fdata (2,nf,nngd,startutc,nnnn)

   ! Some GrADS date/time setting
   if(action(1:5).eq.'GRADS') then
      read(cgrid,*) nngd
      cincgrads='01hr'
      call date_mkgrads (iyear1,imonth1,idate1,startutc,fcstsec  &
                        ,dategrads,cincgrads)
   endif

   maxmem=0
   maxslab=0
   maxnxyg=0
   do ng=1,ngrids
      maxmem=max(maxmem,(nnxp(ng)+1)*(nnyp(ng)+1)*  &
               max(nnzp(ng),(nzg+nzs+3)*npatch))
      maxslab=max(maxslab,(nnxp(ng)+1)*(nnyp(ng)+1)*npatch)
      maxslab=max(maxslab,(nnxp(ng)+1)*  &
               max(nnzp(ng),(nzg+nzs+4)*npatch))
      maxslab=max(maxslab,(nnyp(ng)+1)*  &
               max(nnzp(ng),(nzg+nzs+4)*npatch))
      maxnxyg=max(maxnxyg,(nnxp(ng)+1)*(nnyp(ng)+1)*npatch)
      memsiz4=maxmem
   enddo

   if(allocated(arra))    deallocate(arra);    allocate(arra(maxmem))
   if(allocated(arrb))    deallocate(arrb);    allocate(arrb(maxmem))
   if(allocated(arrbb))   deallocate(arrbb);   allocate(arrbb(maxmem))
   if(allocated(arrover)) deallocate(arrover); allocate(arrover(maxmem))
   if(allocated(coor))    deallocate(coor);    allocate(coor(maxmem*3))
  
   if(allocated(slab))    deallocate(slab);    allocate(slab(maxslab))
   if(allocated(sarr2))   deallocate(sarr2);   allocate(sarr2(maxslab))
   if(allocated(slabover))deallocate(slabover);allocate(slabover(maxslab))
   if(allocated(scoor))   deallocate(scoor);   allocate(scoor(maxslab*3))

   if(allocated(wind))    deallocate(wind);    allocate(wind(3*maxmem))
   if(allocated(swind))   deallocate(swind);   allocate(swind(3*maxslab))
   if(allocated(windt))   deallocate(windt);   allocate(windt(3*maxmem))
   if(allocated(swindt))  deallocate(swindt);  allocate(swindt(3*maxslab))

   if(allocated(topo))    deallocate(topo);    allocate(topo(maxslab))
   if(allocated(pctlnd))  deallocate(pctlnd);  allocate(pctlnd(maxslab))
   
   ! fill array with corners of grids
   if(allocated(corner))  deallocate(corner);  allocate(corner(5,2,ngrids))
   do ng=1,ngrids
      corner(1,1,ng)=xtn(1,ng);        corner(1,2,ng)=ytn(1,ng)
      corner(2,1,ng)=xtn(1,ng);        corner(2,2,ng)=ytn(nnyp(ng),ng)
      corner(3,1,ng)=xtn(nnxp(ng),ng); corner(3,2,ng)=ytn(nnyp(ng),ng)
      corner(4,1,ng)=xtn(nnxp(ng),ng); corner(4,2,ng)=ytn(1,ng)
      corner(5,1,ng)=xtn(1,ng);        corner(5,2,ng)=ytn(1,ng)
   enddo
endif

read(cgrid,*) nngd

! Previous window is (px1,px2,py1,py2). We are zooming if (ppx1.ne.0).

if(prog=='ringi') then
   if(ppx1.gt.0.) then
      ppx1m=xmin+(xmax-xmin)/(px2-px1)*max(0.,ppx1-px1)
      ppx2m=xmax-(xmax-xmin)/(px2-px1)*max(0.,px2-ppx2)
      ppy1m=ymin+(ymax-ymin)/(py2-py1)*max(0.,ppy1-py1)
      ppy2m=ymax-(ymax-ymin)/(py2-py1)*max(0.,py2-ppy2)
      if(islab.eq.1) then
         niboff=iclosest(nnxp(nngd),xtn(1,nngd),ppx1m)-1
         nieoff=nnxp(nngd)-iclosest(nnxp(nngd),xtn(1,nngd),ppx2m)
         njboff=iclosest(nnzp(nngd),ztn(1,nngd),ppy1m)-1
         njeoff=nnzp(nngd)-iclosest(nnzp(nngd),ztn(1,nngd),ppy2m)
      elseif(islab.eq.2) then
         niboff=iclosest(nnyp(nngd),ytn(1,nngd),ppx1m)-1
         nieoff=nnyp(nngd)-iclosest(nnyp(nngd),ytn(1,nngd),ppx2m)
         njboff=iclosest(nnzp(nngd),ztn(1,nngd),ppy1m)-1
         njeoff=nnzp(nngd)-iclosest(nnzp(nngd),ztn(1,nngd),ppy2m)
      elseif(islab.eq.3) then
         niboff=iclosest(nnxp(nngd),xtn(1,nngd),ppx1m)-1
         nieoff=nnxp(nngd)-iclosest(nnxp(nngd),xtn(1,nngd),ppx2m)
         njboff=iclosest(nnyp(nngd),ytn(1,nngd),ppy1m)-1
         njeoff=nnyp(nngd)-iclosest(nnyp(nngd),ytn(1,nngd),ppy2m)
      endif
      !print*,'windows:',px1,px2,py1,py2
      !print*,'windows:',ppx1,ppx2,ppy1,ppy2
      !print*,'windows:',xmin,xmax,ymin,ymax
      !print*,'windows:',ppx1m,ppx2m,ppy1m,ppy2m
   elseif(ppx1.lt.0.) then
      niboff=0
      nieoff=0
      njboff=0
      njeoff=0
   else
      niboff=-niboff
      nieoff=-nieoff
      njboff=-njboff
      njeoff=-njeoff
   endif
   call var_getparams('offsets',ixx1,ixx2,iyy1,iyy2,izz1,izz2)
   if(islab.eq.1) then
      niboff=max(niboff,ixx1)
      nieoff=max(nieoff,ixx2)
      njboff=max(njboff,izz1)
      njeoff=max(njeoff,izz2)
   elseif(islab.eq.2) then
      niboff=max(niboff,iyy1)
      nieoff=max(nieoff,iyy2)
      njboff=max(njboff,izz1)
      njeoff=max(njeoff,izz2)
   elseif(islab.eq.3) then
      niboff=max(niboff,ixx1)
      nieoff=max(nieoff,ixx2)
      njboff=max(njboff,iyy1)
      njeoff=max(njeoff,iyy2)
   endif
   niboff=-niboff
   nieoff=-nieoff
   njboff=-njboff
   njeoff=-njeoff
endif

! show grids points plotted
!if(islab.eq.1) then
!   print*,'horiz bounds- ',niboff,nieoff,abs(niboff)+1,nnxp(nngd)-nieoff
!   print*,' vert bounds- ',njboff,njeoff,abs(njboff)+1,nnzp(nngd)-njeoff
!elseif(islab.eq.2) then
!   print*,'horiz bounds- ',niboff,nieoff,abs(niboff)+1,nnyp(nngd)-nieoff
!   print*,' vert bounds- ',njboff,njeoff,abs(njboff)+1,nnzp(nngd)-njeoff
!elseif(islab.eq.3) then
!   print*,'horiz bounds- ',niboff,nieoff,abs(niboff)+1,nnxp(nngd)-nieoff
!   print*,' vert bounds- ',njboff,njeoff,abs(njboff)+1,nnyp(nngd)-njeoff
!endif

icgrid=ichvar_changed('cgrid',cgrid)
icplotinfo=intvar_changed('iplotinfo',ipinfo)
icvar=ichvar_changed('cvar',cvar(1))
iocvar=ichvar_changed('ocvar',cvar(2))
icwinds=ichvar_changed('cwinds',cwinds)
ictrans=intvar_changed('itrans',itrans)
iccoor=intvar_changed('icoor',icoor)
iciwoff=intvar_changed('niboff',niboff)  &
       +intvar_changed('nieoff',nieoff)  &
       +intvar_changed('njboff',njboff)  &
       +intvar_changed('njeoff',njeoff)
!print*,'after changed-',ctime,cgrid,cvar(1),icplotinfo
!print*,'after changed-',icfile,icgrid,icvar,iocvar
!print*,'after changed-',icwinds,ictrans,iccoor,iciwoff

idims(1)=nnxp(nngd)
idims(2)=nnyp(nngd)
idims(3)=nnzp(nngd)

if(alat1(nngd)==0. .and. (action(1:4)=='DUMP' .or.  &
                          action(1:5)=='MEDOC' .or.  &
                          action(1:5)=='GRADS')) then
   if(islab.ne.3) stop 'Horiz slab not defined for gridded output'
   nib=1+niboff
   nie=idims(1)-nieoff
   njb=1+njboff
   nje=idims(2)-njeoff
   !print*,'finding lat/lon corners for grid',nngd
   call RAMS_fill_fld (idims(1),idims(2),idims(3)  &
                      ,arra,arrb,arrbb ,coor,topo  &
                      ,flnm(1:lenhf-9),nngd,'lat',itrans,nplevs,iplevs  &
                      ,icfile,icgrid,icvar  &
                      ,cdname,cdunits,ivtype,icoor)
   if(action(1:4).eq.'DUMP'.or.action(1:5)=='MEDOC') then
      alat1(nngd)=arra(1) ! 1st point
      call thelatlon (idims(1),idims(2),idims(3),arra,alat1(nngd),nib,njb)
   elseif(action(1:5).eq.'GRADS') then
      call thelatlon (idims(1),idims(2),idims(3),arra,glats1,nib,njb)
      call thelatlon (idims(1),idims(2),idims(3),arra,glats2,nie,njb)
      glats=min(glats1,glats2)
      call thelatlon (idims(1),idims(2),idims(3),arra,glatn1,nib,nje)
      call thelatlon (idims(1),idims(2),idims(3),arra,glatn2,nie,nje)
      glatn=max(glatn1,glatn2)
      ! Choose a reference gridpoint near the center of the domain
      ! to maintain precision in the X,Y distances to polepoint
      ! when writing the GrADS control file
      irefg=idims(1)/2
      jrefg=idims(2)/2
      call thelatlon (idims(1),idims(2),idims(3),arra,glatref  &
                     ,irefg,jrefg)
   endif
   call RAMS_fill_fld (idims(1),idims(2),idims(3)  &
                      ,arra,arrb,arrbb,coor,topo  &
                      ,flnm(1:lenhf-9),nngd,'lon',itrans,nplevs,iplevs  &
                      ,icfile,icgrid,icvar  &
                      ,cdname,cdunits,ivtype,icoor)
   if(action(1:4).eq.'DUMP'.or.action(1:5)=='MEDOC') then
      alon1(nngd)=arra(1) ! 1st point
      call thelatlon (idims(1),idims(2),idims(3),arra,alon1(nngd),nib,njb)
   elseif(action(1:5).eq.'GRADS') then
      call thelatlon (idims(1),idims(2),idims(3),arra,glonw1,nib,njb)
      glonref=glonw1
      call thelatlon (idims(1),idims(2),idims(3),arra,glonw2,nie,njb)
      glonw=min(glonw1,glonw2)
      call thelatlon (idims(1),idims(2),idims(3),arra,glone1,nib,nje)
      call thelatlon (idims(1),idims(2),idims(3),arra,glone2,nie,nje)
      glone=max(glone1,glone2)
      ! Finish the reference gridpoints distance to polepoint.
      call thelatlon (idims(1),idims(2),idims(3),arra,glonref,irefg,jrefg)
      call LL_XY (glatref,glonref,platn(nngd),plonn(nngd),xref,yref)
   endif
endif

! only drawing map, reset cvar to none and skip read and fill routines
if(cvar(1)(1:3)=='map'.and.islab==3) then
   cvar(1)='none'
   colorbar(1)='n'
   goto 400
endif

if(icfile.eq.1.or.icgrid.eq.1)  &
   call RAMS_varlib ('land',idims(1),idims(2),1,nngd  &
                    ,pctlnd,arrb,flnm(1:lenhf-9)  &
                    ,cdname,cdunits,icoor)

if((icfile.eq.1.or.icgrid.eq.1.or.icvar.eq.1.or.  &
   ictrans.eq.1.or.iccoor.eq.1).and.cvar(1)(1:4).ne.'none') then

   call RAMS_fill_fld (idims(1),idims(2),idims(3)  &
                      ,arra,arrb,arrbb,coor,topo  &
                      ,flnm(1:lenhf-9),nngd,cvar(1),itrans,nplevs  &
                      ,iplevs,icfile,icgrid,icvar  &
                      ,cdname,cdunits,ivtype,icoor)

   if(cvar(1)(1:8).eq.'part_lag')  &
      call HYP_fill_fld (1,ivtype,flnm(1:lenhf-9))
      
   cdnames(1,ivvar,nngd)=cdname(1)(1:len_trim(cdname(1)))
   cdunitss(1,ivvar,nngd)=cdunits(1)(1:len_trim(cdunits(1)))

   if(ivtype==0.and.cvar(1)(1:4).ne.'none') then
      print*,'Primary variable not found: ',cvar(1)(1:len_trim(cvar(1)))
      cdname(1)=cvar(1)(1:len_trim(cvar(1)))//' not found'
      cdnames(1,ivvar,nngd)=cvar(1)(1:len_trim(cvar(1)))//' not found;'
      cdunits(1)=';'
      cdunitss(1,ivvar,nngd)=';'
      if(action(1:5).eq.'SPACE') then
         goto 300
      else
         goto 310
      endif
   endif

endif

if(action(1:5).eq.'SPACE'.and.cvar(1)(1:3).ne.'map') then

   if((icfile.eq.1.or.icgrid.eq.1.or.iocvar.eq.1.or.  &
      ictrans.eq.1.or.iccoor.eq.1).and.  &
      cvar(2)(1:4).ne.'none') then
      
      call RAMS_fill_fld (idims(1),idims(2),idims(3)  &
                         ,arrover,arrb,arrbb,coor,topo  &
                         ,flnm(1:lenhf-9),nngd,cvar(2),itrans,nplevs  &
                         ,iplevs,icfile,icgrid,iocvar &
                         ,cdname(2),cdunits(2),iovtype,icoor)

      if(cvar(2)(1:8).eq.'part_lag')  &
         call HYP_fill_fld (2,iovtype,flnm(1:lenhf-9))

      cdnames(2,ivvar,nngd)=cdname(2)(1:len_trim(cdname(2)))
      cdunitss(2,ivvar,nngd)=cdunits(2)(1:len_trim(cdunits(2)))
               
      if(iovtype==0.and.cvar(2)(1:4).ne.'none') then
         print*,'Secondary variable not found: ',cvar(2)(1:len_trim(cvar(2)))
         cdname(2)=cvar(2)(1:len_trim(cvar(2)))//' not found;'
         cdnames(2,ivvar,nngd)=cvar(2)(1:len_trim(cvar(2)))//' not found;'
         cdunits(2)=';'
         cdunitss(2,ivvar,nngd)=';'
         goto 300
      endif

   endif
   
   if(ivtype.ne.iovtype.and.islab.ne.3.and.cvar(1)(1:4).ne.'none'.and.  &
      cvar(2)(1:4).ne.'none') then
      print*,'WARNING: cannot plot 2 variables with 2 different types of '  &
            ,'vertical transformations',ivtype,iovtype
   endif

   if((cwinds.ne.'n'.and.cwinds.ne.'r').and.(icfile.eq.1.or.  &
      icgrid.eq.1.or.icwinds.eq.1.or.ictrans.eq.1.or.iccoor.eq.1)) then

      !print*,'filling with winds anyway '
      call RAMS_fill_fld (idims(1),idims(2),idims(3)  &
                         ,wind,arrb,arrbb,coor,topo  &
                         ,flnm(1:lenhf-9),nngd,'u_avg',itrans,nplevs  &
                         ,iplevs,icfile,icgrid,icvar,cdscr,cdscr  &
                         ,iwvtype,icoor)
      call RAMS_fill_fld (idims(1),idims(2),idims(3)  &
                         ,wind(1+maxmem),arrb,arrbb,coor,topo  &
                         ,flnm(1:lenhf-9),nngd,'v_avg',itrans,nplevs  &
                         ,iplevs,icfile,icgrid,icvar,cdscr,cdscr  &
                         ,iwvtype,icoor)
      call RAMS_fill_fld (idims(1),idims(2),idims(3)  &
                         ,wind(1+2*maxmem),arrb,arrbb,coor,topo  &
                         ,flnm(1:lenhf-9),nngd,'w_avg',itrans,nplevs  &
                         ,iplevs,icfile,icgrid,icvar,cdscr,cdscr  &
                         ,iwvtype,icoor)

      ! Add these to carry uu,vv,ww around
      if(cwinds.eq.'t') then
        call RAMS_fill_fld (idims(1),idims(2),idims(3)  &
                           ,windt,arrb,arrbb,coor,topo  &
                           ,flnm(1:lenhf-9),nngd,'uu',itrans,nplevs  &
                           ,iplevs,icfile,icgrid,icvar,cdscr,cdscr  &
                           ,iwvtype,icoor)
        call RAMS_fill_fld (idims(1),idims(2),idims(3)  &
                           ,windt(1+maxmem),arrb,arrbb,coor,topo  &
                           ,flnm(1:lenhf-9),nngd,'vv',itrans,nplevs  &
                           ,iplevs,icfile,icgrid,icvar,cdscr,cdscr  &
                           ,iwvtype,icoor)
        call RAMS_fill_fld (idims(1),idims(2),idims(3)  &
                           ,windt(1+2*maxmem),arrb,arrbb,coor,topo  &
                           ,flnm(1:lenhf-9),nngd,'ww',itrans,nplevs  &
                           ,iplevs,icfile,icgrid,icvar,cdscr,cdscr  &
                           ,iwvtype,icoor)
      endif

   elseif(cwinds.eq.'r'.and.(icfile.eq.1.or.icgrid.eq.1.or.  &
      icwinds.eq.1.or.ictrans.eq.1.or.iccoor.eq.1)) then

      !print*,'entering vort calculations for vectors'
      call RAMS_fill_fld (idims(1),idims(2),idims(3)  &
                         ,wind,arrb,arrbb,coor,topo  &
                         ,flnm(1:lenhf-9),nngd,'relvortx',itrans,nplevs  &
                         ,iplevs,icfile,icgrid,icvar,cdscr,cdscr  &
                         ,iwvtype,icoor)
      call RAMS_fill_fld (idims(1),idims(2),idims(3)  &
                         ,wind(1+maxmem),arrb,arrbb,coor,topo  &
                         ,flnm(1:lenhf-9),nngd,'relvorty',itrans,nplevs  &
                         ,iplevs,icfile,icgrid,icvar,cdscr,cdscr  &
                         ,iwvtype,icoor)
      call RAMS_fill_fld (idims(1),idims(2),idims(3)  &
                         ,wind(1+2*maxmem),arrb,arrbb,coor,topo  &
                         ,flnm(1:lenhf-9),nngd,'relvortz',itrans,nplevs  &
                         ,iplevs,icfile,icgrid,icvar,cdscr,cdscr  &
                         ,iwvtype,icoor)
   endif

endif

400 continue

!print*,'islab,ivtype,icoor',islab,ivtype,icoor
!print*,'niboff,nieoff',niboff,nieoff
!print*,'njboff,njeoff',njboff,njeoff
!print*,'nnboff,nneoff',nnboff,nneoff
if(islab.eq.1) then

   horiz='X'
   vert='Z'
   
   if(niboff.le.0) nib=1-niboff
   if(niboff.gt.0) nib=niboff
   nib=max(0,min(nib,idims(1)))
   if(nieoff.le.0) nie=idims(1)+nieoff
   if(nieoff.gt.0) nie=nieoff
   nie=max(nib,min(nie,idims(1)))
   
   if(njboff.le.0) njb=1-njboff
   if(njboff.gt.0) njb=njboff
   if(ivtype.le.3.and.itrans.eq.3) njb=max(0,min(njb,nplevs))
   if(ivtype.le.3.and.itrans.ne.3) njb=max(0,min(njb,idims(3)))
   if(ivtype==8.or.ivtype==5) njb=max(0,min(njb,nzg))
   if(ivtype==7.or.ivtype==4) njb=max(0,min(njb,nzs))
   if(ivtype==10) njb=max(0,min(njb,nzg+nzs+3))
   if(njeoff.le.0) then
      if(ivtype.le.3.and.itrans.eq.3) nje=nplevs+njeoff
      if(ivtype.le.3.and.itrans.ne.3) nje=idims(3)+njeoff
      if(ivtype==8.or.ivtype==5) nje=nzg+njeoff
      if(ivtype==7.or.ivtype==4) nje=nzs+njeoff
      if(ivtype==10) nje=nzg+nzs+3+njeoff
   endif
   if(njeoff.gt.0) nje=njeoff
   if(ivtype.le.3.and.itrans.eq.3) nje=max(njb,min(nje,nplevs))
   if(ivtype.le.3.and.itrans.ne.3) nje=max(njb,min(nje,idims(3)))
   if(ivtype==8.or.ivtype==5) nje=max(njb,min(nje,nzg))
   if(ivtype==7.or.ivtype==4) nje=max(njb,min(nje,nzs))
   if(ivtype==10) nje=max(njb,min(nje,nzg+nzs+3))
   
   if(nnboff.le.0) nnb=1-nnboff
   if(nnboff.gt.0) nnb=nnboff
   nnb=max(0,min(nnb,idims(2)))
   if(nneoff.le.0) nne=idims(2)+nneoff
   if(nneoff.gt.0) nne=nneoff
   nne=max(nnb,min(nne,idims(2)))
   
   iu=1
   iv=1+2*maxslab
   iuu=1
   ivv=1+2*maxslab
   
elseif(islab.eq.2) then

   horiz='Y'
   vert='Z'
   
   if(niboff.le.0) nib=1-niboff
   if(niboff.gt.0) nib=niboff
   nib=max(0,min(nib,idims(2)))
   if(nieoff.le.0) nie=idims(2)+nieoff
   if(nieoff.gt.0) nie=nieoff
   nie=max(nib,min(nie,idims(2)))
   
   if(njboff.le.0) njb=1-njboff
   if(njboff.gt.0) njb=njboff
   if(ivtype.le.3.and.itrans.eq.3) njb=max(0,min(njb,nplevs))
   if(ivtype.le.3.and.itrans.ne.3) njb=max(0,min(njb,idims(3)))
   if(ivtype==8.or.ivtype==5) njb=max(0,min(njb,nzg))
   if(ivtype==7.or.ivtype==4) njb=max(0,min(njb,nzs))
   if(ivtype==10) njb=max(0,min(njb,nzg+nzs+3))
   if(njeoff.le.0) then
      if(ivtype.le.3.and.itrans.eq.3) nje=nplevs+njeoff
      if(ivtype.le.3.and.itrans.ne.3) nje=idims(3)+njeoff
      if(ivtype==8.or.ivtype==5) nje=nzg+njeoff
      if(ivtype==7.or.ivtype==4) nje=nzs+njeoff
      if(ivtype==10) nje=nzg+nzs+3+njeoff
   endif
   if(njeoff.gt.0) nje=njeoff
   if(ivtype.le.3.and.itrans.eq.3) nje=max(njb,min(nje,nplevs))
   if(ivtype.le.3.and.itrans.ne.3) nje=max(njb,min(nje,idims(3)))
   if(ivtype==8.or.ivtype==5) nje=max(njb,min(nje,nzg))
   if(ivtype==7.or.ivtype==4) nje=max(njb,min(nje,nzs))
   if(ivtype==10) nje=max(njb,min(nje,nzg+nzs+3))
   
   if(nnboff.le.0) nnb=1-nnboff
   if(nnboff.gt.0) nnb=nnboff
   nnb=max(0,min(nnb,idims(1)))
   if(nneoff.le.0) nne=idims(1)+nneoff
   if(nneoff.gt.0) nne=nneoff
   nne=max(nnb,min(nne,idims(1)))
   
   iu=1+maxslab
   iv=1+2*maxslab
   iuu=1+maxslab
   ivv=1+2*maxslab
   
elseif(islab.eq.3) then

   horiz='X'
   vert='Y'
   
   if(niboff.le.0) nib=1-niboff
   if(niboff.gt.0) nib=niboff
   nib=max(0,min(nib,idims(1)))
   if(nieoff.le.0) nie=idims(1)+nieoff
   if(nieoff.gt.0) nie=nieoff
   nie=max(nib,min(nie,idims(1)))
   
   if(njboff.le.0) njb=1-njboff
   if(njboff.gt.0) njb=njboff
   njb=max(0,min(njb,idims(2)))
   if(njeoff.le.0) nje=idims(2)+njeoff
   if(njeoff.gt.0) nje=njeoff
   nje=max(njb,min(nje,idims(2)))
   
   if(nnboff.le.0) nnb=1-nnboff
   if(nnboff.gt.0) nnb=nnboff
   if(ivtype.le.3.and.itrans.eq.3) nnb=max(0,min(nnb,nplevs))
   if(ivtype.le.3.and.itrans.ne.3) nnb=max(0,min(nnb,idims(3)))
   if(ivtype==8.or.ivtype==5) nnb=max(0,min(nnb,nzg))
   if(ivtype==7.or.ivtype==4) nnb=max(0,min(nnb,nzs))
   if(ivtype==10) nnb=max(0,min(nnb,nzg+nzs+3))
   if(nneoff.le.0) then
      if(ivtype.le.3.and.itrans.eq.3) nne=nplevs+nneoff
      if(ivtype.le.3.and.itrans.ne.3) nne=idims(3)+nneoff
      if(ivtype==8.or.ivtype==5) nne=nzg+nneoff
      if(ivtype==7.or.ivtype==4) nne=nzs+nneoff
      if(ivtype==10) nne=nzg+nzs+3+nneoff
   endif
   if(nneoff.gt.0) nne=nneoff
   if(ivtype.le.3.and.itrans.eq.3) nne=max(nnb,min(nne,nplevs))
   if(ivtype.le.3.and.itrans.ne.3) nne=max(nnb,min(nne,idims(3)))
   if(ivtype==8.or.ivtype==5) nne=max(nnb,min(nne,nzg))
   if(ivtype==7.or.ivtype==4) nne=max(nnb,min(nne,nzs))
   if(ivtype==10) nne=max(nnb,min(nne,nzg+nzs+3))
   
   iu=1
   iv=1+maxslab
   iuu=1
   ivv=1+maxslab
   
endif

if(itrans.ne.3) then
   icoor=max(nnb,min(nne,icoor))
endif
!print*,'icoor',icoor

if(action.eq.'SPACE') then
   nnb=icoor
   nne=icoor
endif
nii=nie-nib+1
njj=nje-njb+1
nnn=nne-nnb+1
!print*,'nib,nie,nii,niinc',nib,nie,nii,niinc
!print*,'njb,nje,njj,njinc',njb,nje,njj,njinc
!print*,'nnb,nne,nnn,nninc',nnb,nne,nnn,nninc
!print*,'idims(1),idims(2),idims(3)',idims(1),idims(2),idims(3)

! Exit cleanly if grid too small to view
if((nii <= 1.and.idims(1) > 1).or.(nii < 1.and.idims(1) == 1)) then
   print*,'Warning: Selected grid too small in x direction'
   goto 301
endif
if((njj <= 1.and.idims(2) > 1).or.(njj < 1.and.idims(2) == 1)) then
   print*,'Warning: Selected grid too small in y direction'
   goto 302
endif

! Now open the binary file since now know plotted grid dimensions.
if(icgrid.eq.1.and.action(1:5).eq.'GRADS') then
   call RAMS_get_idata (3,nf,nngd,itime,nnnn)
   call RAMS_get_idata (2,nf,nngd,idate,nnnn)
   call find_ll_grid (idims(1),idims(2),nib,nie,njb,nje  &
                     ,xtn(1,nngd),ytn(1,nngd),platn(nngd),plonn(nngd)  &
                     ,swllat,swllon,nellat,nellon  &
                     ,dllat,dllon,nllat,nllon  &
                     ,igridll,glldllat,glldllon  &
                     ,gllwlon,gllelon,gllslat,gllnlat)
   call RAMS_gradsbin (igunit,'OPEN',itrans,nngd,nllat,nllon  &
                     ,iyear1,imonth1,idate1,itime1,revpref)
endif

! Choose what to do with fields now that they are read and optionally
! interpolated

!print*,'action: ',action
if(action.eq.'SPACE') then

   ! if "action" is 'SPACE', slice field and do normal plotting

   if(cvar(2)(1:4).ne.'none') then
      icoord=icoor
      if(iovtype.eq.2) icoord=1
      if(iovtype.ne.1)  &
         call RAMS_3to2d (islab,arrover  &
                         ,coor,idims(1),idims(2),idims(3)  &
                         ,slabover,scoor  &
                         ,nib,nie,njb,nje,icoord,nii,njj)
   endif

   if(cwinds.ne.'n') then
      icoord=icoor
      call RAMS_3to2d_v (islab,wind  &
                        ,coor,idims(1),idims(2),idims(3)  &
                        ,swind,scoor  &
                        ,nib,nie,njb,nje,icoord,nii,njj)
      call RAMS_3to2d_v (islab,wind(1+maxmem)  &
                        ,coor,idims(1),idims(2),idims(3)  &
                        ,swind(1+maxslab),scoor  &
                        ,nib,nie,njb,nje,icoord,nii,njj)
      call RAMS_3to2d_v (islab,wind(1+2*maxmem)  &
                        ,coor,idims(1),idims(2),idims(3)  &
                        ,swind(1+2*maxslab),scoor  &
                        ,nib,nie,njb,nje,icoord,nii,njj)

      ! Also added to carry around turbulence uu,vv,ww
      if(cwinds.eq.'t') then
         call RAMS_3to2d_v (islab,windt  &
                           ,coor,idims(1),idims(2),idims(3)  &
                           ,swindt,scoor  &
                           ,nib,nie,njb,nje,icoord,nii,njj)
         call RAMS_3to2d_v (islab,windt(1+maxmem)  &
                           ,coor,idims(1),idims(2),idims(3)  &
                           ,swindt(1+maxslab),scoor  &
                           ,nib,nie,njb,nje,icoord,nii,njj)
         call RAMS_3to2d_v (islab,windt(1+2*maxmem)  &
                           ,coor,idims(1),idims(2),idims(3)  &
                           ,swindt(1+2*maxslab),scoor  &
                           ,nib,nie,njb,nje,icoord,nii,njj)
      endif
   endif

   if(cvar(1)(1:4).ne.'none') then
      icoord=icoor

      if(ivtype==2.or.ivtype==3) then
         if(ivtype.eq.2) icoord=1
         call RAMS_3to2d (islab,arra,coor  &
                         ,idims(1),idims(2),idims(3)  &
                         ,slab,scoor  &
                         ,nib,nie,njb,nje,icoord,nii,njj)
      elseif(ivtype==6) then
         call RAMS_3to2d_hpat (arra  &
                              ,idims(1),idims(2),npatch,slab  &
                              ,sarr2,nib,nie,njb,nje,nii,njj)
!*****************SBR**********************************************
      elseif(ivtype==4) then
         call RAMS_3to2d (islab,arra,coor  &
                         ,idims(1),idims(2),nzs  &
                         ,slab,scoor  &
                         ,nib,nie,njb,nje,icoord,nii,njj)
      elseif(ivtype==5) then
         call RAMS_3to2d (islab,arra,coor  &
                         ,idims(1),idims(2),nzg  &
                         ,slab,scoor  &
                         ,nib,nie,njb,nje,icoord,nii,njj)
!*****************SBR**********************************************
      elseif(ivtype.ne.1.and.ivtype.ne.6) then
         if (ivtype==8) then
            leafdim = nzg
         elseif(ivtype==7) then
            leafdim = nzs
         elseif(ivtype==10) then
            leafdim = nzg+nzs+3
         else
            print*,'ivtype',ivtype
            stop 'read_RAMS'
         endif
         call RAMS_3to2d_vpat (islab,arra  &
                              ,arra(1+nnxp(nngd)*nnyp(nngd)*npatch)  &
                              ,idims(1),idims(2),leafdim,npatch  &
                              ,slab,sarr2  &
                              ,nib,nie,njb,nje,icoord,nii,njj  &
                              ,ivtype,slabover)
      endif
   endif

   CALL framset (horiz,vert,xmplot,xtplot,ymplot,ytplot  &
                ,xmn(1,nngd),ymn(1,nngd),zmn(1,nngd)  &
                ,xtn(1,nngd),ytn(1,nngd),ztn(1,nngd)  &
                ,slz,topo,nnxp(nngd),nnyp(nngd)  &
                ,zmn(nnzp(nngd)-1,nngd),nib,nie,njb,nje,nii,njj  &
                ,xmin,xmax,ymin,ymax,zmin,zmax,icoord  &
                ,ivtype,zuv,pctlnd)

   ! Get the set coordinates used in plotting a map or background frame.
   !   added the ability to not display plot info - mjb
   !   added the ability to do up to 4 panels on a frame - mjb

   !print*,'ivtype= ',ivtype,'  iovtype=   ',iovtype
   !print*,'ipanel= ',ipanl,'  iplotinfo= ',ipinfo
   !print*,'ivvar=  ',ivvar ,'  mod+1=     ',mod(ivvar,ipanl)+1

   if(ipanl.eq.1) then
   
      if(ipinfo.eq.2.or.ipinfo.eq.3) then
         px1=.01
      else
         px1=.09
      endif
      
      if((cvar(1)(1:4).ne.'none'.and.colorbar(1).eq.'b').or.  &
         (cvar(2)(1:4).ne.'none'.and.colorbar(2).eq.'b')) then
         px2=.87
      else
         px2=.99
      endif
      
      if(ipinfo.eq.2) then
         py2=.96
      else
         py2=.99
      endif
      
      if(ipinfo.eq.1) then
         py1=.22
      elseif(ipinfo.eq.2) then
         py1=.04
      elseif(ipinfo.eq.3) then
         py1=.01
      else
         py1=.07
      endif
      
   elseif(ipanl.ge.2.and.ipanl.le.4) then
   
      ipinfo=0
      
      if(mod(ivvar+ipanl-1,ipanl).eq.0) then
         px1=0.02
         px2=0.48
         py2=0.972
         py1=0.524
      elseif(mod(ivvar+ipanl-1,ipanl).eq.1) then
         px1=0.52
         px2=0.98
         py2=0.972
         py1=0.524
      elseif(mod(ivvar+ipanl-1,ipanl).eq.2) then
         px1=0.02
         px2=0.48
         py2=0.472
         py1=0.024
      elseif(mod(ivvar+ipanl-1,ipanl).eq.3) then
         px1=0.52
         px2=0.98
         py2=0.472
         py1=0.024
      endif
      
      if((cvar(1)(1:4).ne.'none'.and.colorbar(1).eq.'b').or.  &
         (cvar(2)(1:4).ne.'none'.and.colorbar(2).eq.'b')) px2=px2-.06
         
      if(cvar(1)(1:4).ne.'none'.and.cvar(2)(1:4).ne.'none') py1=py1+0.01 
        
      if (cwinds.eq.'v'.or.cwinds.eq.'t') then
         vioff=.08
      elseif(cwinds.eq.'b') then
         vioff=.07
      else
         vioff=.1
      endif
      
      if(vert.eq.'Z'.and.cwinds.ne.'b') then
         vjoff=.05
      else
         vjoff=.03
      endif
      
   else
      stop 'read_RAMS: unknown panels for frame'
   endif

   call RAMS_get_idata (3,nf,nngd,itime,nnnn)
   call RAMS_get_idata (2,nf,nngd,idate,nnnn)
   
   call setframe ()  ! not sure why we have to do this
   if(islab.eq.3.and.abs(mfill).gt.0) then
      ibugout=0
      call RAMS_mkmap (platn(nngd),plonn(nngd),xtn(nib,nngd)  &
                      ,ytn(njb,nngd),xtn(nie,nngd),ytn(nje,nngd)  &
                      ,mfill,px1,px2,py1,py2,icplotinfo)
      if (ibugout.eq.1) return
      call getset (px1,px2,py1,py2,xx1,xx2,yy1,yy2,ittt)
   else
      mfill=0
      if(horiz.eq.'X'.and.vert.eq.'Y') then
         xdist=xmax-xmin
         ydist=ymax-ymin
         px2=px1+min(px2-px1,(xdist/ydist)*.74)
         py2=py1+(ydist/xdist)*(px2-px1)
      endif
   endif

   ! Plot the filled map
   if(abs(mfill).ge.2) call RAMS_sendmap(1)
   if(abs(mfill).eq.2) then
      if(mfill.lt.0)      call RAMS_sendmap (11)   ! shadow
      if(abs(mfill).gt.0) call RAMS_sendmap (10)   ! outline
      if(abs(mfill).gt.0) call RAMS_sendroads ()   ! other stuff
   endif
   
   !  Plot the background labels
   boxtop=py1-0.06
   boxhite=0.03
   call backlab (px1,px2,py1,py2,horiz,vert,nngd  &
                ,xtn(icoor,nngd),ytn(icoor,nngd),ztn(icoor,nngd)  &
                ,slz(icoor),itime,idate,ihtran,itrans  &
                ,iplevs(icoor),head1,boxtop,boxhite,ivtype,iovtype,nf)

   ! Plot contours, tiles and parts
   if(ccont.ne.'n') then
   
      ! Plot primary contour field - no fill
      if(cvar(1)(1:4).ne.'none'.and.icover(1)==0.and.conttyp(1).eq.'c')  &
         call contour (nngd,ivvar,1,2,px1,px2,py1,py2,slab  &
                      ,nii,njj,vert,ymin,ymax,boxtop,boxhite)
                      
      ! Plot primary contour field - filled
      if(cvar(1)(1:4).ne.'none'.and.conttyp(1).eq.'f')  &
         call contour (nngd,ivvar,1,icover(1),px1,px2,py1,py2,slab  &
                      ,nii,njj,vert,ymin,ymax,boxtop,boxhite)
      
      ! Plot primary tiled field
      if(cvar(1)(1:4).ne.'none'.and.conttyp(1).eq.'t')  &
         call tileplot (nngd,ivvar,1,px1,px2,py1,py2,slab,sarr2  &
                       ,njb,nii,njj,npatch,xmplot,xtplot,ymplot  &
                       ,xmin,xmax,ymin,ymax,zs,zuv,zmodtop,vert  &
                       ,ivtype,boxtop,boxhite)
                       
      ! Plot secondary contour field - no fill
      if(cvar(2)(1:4).ne.'none'.and.icover(2)==0.and.conttyp(2).eq.'c')  &
         CALL contour (nngd,ivvar,2,2,px1,px2,py1,py2,slabover  &
                      ,nii,njj,vert,ymin,ymax,boxtop,boxhite)

      ! Plot secondary contour field - filled
      if(cvar(2)(1:4).ne.'none'.and.conttyp(2).eq.'f')  &
         CALL contour (nngd,ivvar,2,icover(2),px1,px2,py1,py2,slabover  &
                      ,nii,njj,vert,ymin,ymax,boxtop,boxhite)
                      
      ! Plot secondary tiled field
      if(cvar(2)(1:4).ne.'none'.and.conttyp(2).eq.'t')  &
         call tileplot (nngd,ivvar,2,px1,px2,py1,py2,slabover,sarr2  &
                       ,njb,nii,njj,npatch,xmplot,xtplot,ymplot  &
                       ,xmin,xmax,ymin,ymax,zs,zuv,zmodtop,vert  &
                       ,ivtype,boxtop,boxhite)

      ! Plot primary contour field - no fill
      if(cvar(1)(1:4).ne.'none'.and.icover(1)==1.and.  &
         (conttyp(1).eq.'c'.or.conttyp(1).eq.'f'))  &
         call contour (nngd,ivvar,1,2,px1,px2,py1,py2,slab  &
                      ,nii,njj,vert,ymin,ymax,boxtop,boxhite)

      ! Plot secondary contour field - no fill
      if(cvar(2)(1:4).ne.'none'.and.icover(2)==1.and.  &
         (conttyp(2).eq.'c'.or.conttyp(2).eq.'f'))  &
         call contour (nngd,ivvar,2,2,px1,px2,py1,py2,slabover  &
                      ,nii,njj,vert,ymin,ymax,boxtop,boxhite)

      ! Plot primary particles
      if(cvar(1)(1:4).ne.'none'.and.conttyp(1).eq.'a'.or.  &
         conttyp(1).eq.'s'.or.conttyp(1).eq.'l')  &
         call parts (1,px1,px2,py1,py2,xmin,xmax,ymin,ymax,icoord  &
                    ,xmn(1,nngd),ymn(1,nngd),zmn(1,nngd)  &
                    ,nnxp(nngd),nnyp(nngd),nnzp(nngd)  &
                    ,horiz,vert,boxtop,boxhite)
          
      ! Plot secondary particles
      if(cvar(2)(1:4).ne.'none'.and.conttyp(2).eq.'a'.or.  &
         conttyp(2).eq.'s'.or.conttyp(2).eq.'l')  &
         call parts (2,px1,px2,py1,py2,xmin,xmax,ymin,ymax,icoord  &
                    ,xmn(1,nngd),ymn(1,nngd),zmn(1,nngd)  &
                    ,nnxp(nngd),nnyp(nngd),nnzp(nngd)  &
                    ,horiz,vert,boxtop,boxhite)

   endif

   ! Plot wind vectors or streamlines
   if(cwinds.ne.'n')  &
      call vecstrm (swind(iu),swind(iv),nii,njj,work,px1,px2,py1,py2  &
                   ,horiz,vert,xmin,xmax,ymin,ymax,zmin,zmax  &
                   ,slabover,boxtop,boxhite,xtplot,ytplot,zs  &
                   ,zmn(nnzp(nngd)-1,nngd),nib,njb,swindt(iuu)  &
                   ,swindt(ivv),nngd,ivvar,2,vioff,vjoff)

   ! Plot the map outlines
   if(abs(mfill).eq.1.or.abs(mfill).eq.3) then
      if(mfill.lt.0)      call RAMS_sendmap (11)   ! shadow
      if(abs(mfill).gt.0) call RAMS_sendmap (10)   ! outline
      if(abs(mfill).gt.0) call RAMS_sendroads ()   ! other stuff
   endif

   ! Plot the LANDMARKS
   if(islab.eq.3) call landmks (px1,px2,py1,py2,xx1,xx2,yy1,yy2  &
                               ,xmin,xmax,ymin,ymax,nngd,ngrids,corner)

   ! Draw the frame
   call framdrw (horiz,vert,xmplot,xtplot,ymplot,ytplot  &
                ,topo,nnxp(nngd),nnyp(nngd)  &
                ,nib,nie,njb,nje,nii,njj  &
                ,px1,px2,py1,py2,xmin,xmax,ymin,ymax,icoord  &
                ,zuv,zmn(nnzp(nngd)-1,nngd),ivtype,vioff,vjoff)

elseif(action(1:4).eq.'DUMP') then

   ! if action is 'DUMP', call routine to output field

   icoord=icoor
   if(ivtype.eq.2) icoord=1
   call RAMS_dump (arra,coor,itrans,ivtype,nngd,idims(1),idims(2)  &
                  ,idims(3),ztn(1,nngd),nzg,slz  &
                  ,iyear1,imonth1,idate1,itime1  &
                  ,fcstsec,deltaxn(nngd),deltayn(nngd)  &
                  ,xtn(1,nngd),ytn(1,nngd),alat1(nngd),alon1(nngd)  &
                  ,cdname,cdunits,head1,revpref)

elseif(action(1:5).eq.'MEDOC') then

   ! if action is 'MEDOC', call routine to output field

   icoord=icoor
   if(ivtype.eq.2) icoord=1
   call RAMS_medoc (arra,coor,itrans,ivtype,nngd,idims(1),idims(2)  &
                   ,idims(3),ztn(1,nngd),iyear1,imonth1,idate1,itime1  &
                   ,fcstsec,deltaxn(nngd),deltayn(nngd)  &
                   ,xtn(1,nngd),ytn(1,nngd),alat1(nngd),alon1(nngd)  &
                   ,revpref,cframe_a,maxfore,ihtflx)

elseif(action(1:3).eq.'V5D') then

   ! if action is 'V5D', call routine to write to vis5d file
   call RAMS_v5d (idims(1),idims(2),idims(3),arra  &
                 ,xtn(1,nngd),ytn(1,nngd)  &
                 ,niinc,njinc,nninc,nnb,nne,platn(nngd)  &
                 ,plonn(nngd),ivtime,ivvar,itrans)
                    
elseif(action(1:5).eq.'GRADS') then

   ! if action is 'GRADS', call routine to write to GRADS file

   call RAMS_grads (igunit,arra,idims(1),idims(2),idims(3)  &
                   ,nllat,nllon,dllat,dllon,swllat,swllon  &
                   ,nnb,nne,nninc,ivtime,ivvar,itrans,xtn(1,nngd)  &
                   ,ytn(1,nngd),ztn(1,nngd),ivtype,platn(nngd),plonn(nngd))
   cdname1=cdname(1)
   cdunits1=cdunits(1)

elseif(action(1:4).eq.'GRIB') then

   ! if action is 'GRIB', call routine to write to GRIB file

   call find_ll_grid (idims(1),idims(2),nib,nie,njb,nje  &
                     ,xtn(1,nngd),ytn(1,nngd),platn(nngd),plonn(nngd)  &
                     ,swllat,swllon,nellat,nellon,dllat,dllon,nllat,nllon  &
                     ,igridll,glldllat,glldllon  &
                     ,gllwlon,gllelon,gllslat,gllnlat)
   call RAMS_get_fdata (2,nf,nngd,startutc,nnnn)

   call RAMS_grib (igunit,arra,idims(1),idims(2),idims(3)  &
                  ,nllat,nllon,dllat,dllon,swllat,swllon  &
                  ,nnb,nne,nninc,ivtime,ivvar,itrans     &
                  ,xtn(1,nngd),ytn(1,nngd),ztn(1,nngd),ivtype &
                  ,platn(nngd),plonn(nngd),iyear1,imonth1,idate1  &
                  ,startutc,time,cvar)

elseif(action(1:4).eq.'GRAB'.or.action(1:5).eq.'STATS') then

   ! if action is 'GRAB' or action is 'STATS', call routine to return
   ! model data from specified locations

   call RAMS_grab (ivtype,arra,coor,topo,idims(1),idims(2),idims(3)  &
                  ,pred,fpos,nngd)

endif

ncall=1
return

300 continue
call plchhq(.5,.5,'Variable Not Available',-1.,0.,0.)
return

301 continue
call plchhq(.5,.5,'Selected grid too small in x direction',-1.,0.,0.)
return

302 continue
call plchhq(.5,.5,'Selected grid too small in y direction',-1.,0.,0.)
return
   
310 continue
ivvar=ivvar-1
return

!----------------------------------------------------


entry loc_slab (islab_r,ppx1,ppy1,ipx,ipy,ngr)

! Previous window is (px1,px2,py1,py2)
! Find grid point values of (ppx1,ppy1)

ppx1m=xmin+(xmax-xmin)/(px2-px1)*max(0.,ppx1-px1)
ppy1m=ymin+(ymax-ymin)/(py2-py1)*max(0.,ppy1-py1)
if(islab_r.eq.1) then
   ipx=iclosest(nnxp(ngr),xtn(1,ngr),ppx1m)
   ipy=iclosest(nnzp(ngr),ztn(1,ngr),ppy1m)
   xxx=xtn(nnxp(ngr)/2,ngr)
   yyy=ytn(ipy,ngr)
   call xy_ll(yloc,xloc,platn(ngr),plonn(ngr),xxx,yyy)
elseif(islab_r.eq.2) then
   ipx=iclosest(nnyp(ngr),ytn(1,ngr),ppx1m)
   ipy=iclosest(nnzp(ngr),ztn(1,ngr),ppy1m)
elseif(islab_r.eq.3) then
   ipx=iclosest(nnxp(ngr),xtn(1,ngr),ppx1m)
   ipy=iclosest(nnyp(ngr),ytn(1,ngr),ppy1m)
   xxx=xtn(nnxp(ngr)/2,ngr)
   yyy=ytn(ipy,ngr)
   call xy_ll(yloc,xloc,platn(ngr),plonn(ngr),xxx,yyy)
endif

return

!----------------------------------------------------

entry slab_coor (islab_r,icoor_r,rloc,ngr)

! return real world coordinates

if(islab_r.eq.1) then
   xxx=xtn(nnxp(ngr)/2,ngr)
   yyy=ytn(icoor_r,ngr)
   call xy_ll(rloc,xloc,platn(ngr),plonn(ngr),xxx,yyy)
elseif(islab_r.eq.2) then
   xxx=xtn(icoor_r,ngr)
   yyy=ytn(nnyp(ngr)/2,ngr)
   call xy_ll(xloc,rloc,platn(ngr),plonn(ngr),xxx,yyy)
elseif(islab_r.eq.3) then
   rloc=ztn(icoor_r,ngr)
endif

return

end
