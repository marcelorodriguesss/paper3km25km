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

subroutine framset (chhor,chver,xmplot,xtplot,ymplot,ytplot  &
                   ,xmn,ymn,zmn,xtn,ytn,ztn,slz  &
                   ,topt,n2,n3,ztop,nib,nie,njb,nje,nii,njj  &
                   ,xmin,xmax,ymin,ymax,zmin,zmax  &
                   ,icoor,ivtype,zuv,pctlnd,topta)

implicit none

integer :: nib,nie,njb,nje,nii,njj,ivtype,n2,n3,icoor
real :: xmin,xmax,ymin,ymax,zmin,zmax,px1,px2,py1,py2,ztop,vioff,vjoff
real :: xmplot(*),xtplot(*),ymplot(*),ytplot(*),slz(*),topt(*),pctlnd(*)  &
       ,xmn(*),ymn(*),zmn(*),xtn(*),ytn(*),ztn(*),zuv(*),topta(*)  &
       ,voltsl(nii,njj)
character(len=*)  :: chhor,chver

include 'frame.h'

integer, parameter :: nztr=1000
integer :: ic,icw,icl,i,j,k,ih,izstran,ivtran,ixbeg,iybeg,nn,iii,itrans  &
          ,if_adap,nseg,ns
real :: zmodtop,valugp,ztopz,factor,pp1,pp2,pq1,pq2,xxx1,xxx2,yyy1,yyy2  &
       ,pclwc,zfactor,zst,xseg
real :: pcoor(nztr),ztr(nztr),zs(nztr),pclw(nztr),zx(2),zy(2)
character(len=80) :: title
character(len=6)  :: axlabx,axlaby
character(len=1)  :: cpoint,h,v
character(len=16) :: number,cformat,titl,xlabfmt,ylabfmt

real :: topta_slab(1000),xfill(4),yfill(4),dstfill(8)
integer :: indfill(12)

common/trans2/ivtran,ztr,izstran,zs,zmodtop
common/landwat/pclw

save

h=chhor(1:1)
v=chver(1:1)

! Set up defaults and plotting array for horizontal X axis.
! This involves finding the minimum and maximum values and
! setting defaults for the axis label and numeric format.


if(h.eq.'X') then
   do i=nib,nie
      xmplot(i-nib+2)=xmn(i)
      xtplot(i-nib+1)=xtn(i)
   enddo
   if(nib.ge.2) then
      xmplot(1)=xmn(nib-1)
   else
      xmplot(1)=2.*xmn(nib)-xmn(nib+1)
   endif

   axlabx='x'
   xlabfmt='-3pf6.0'

elseif(h.eq.'Y') then
   do i=nib,nie
      xmplot(i-nib+2)=ymn(i)
      xtplot(i-nib+1)=ytn(i)
   enddo
   if(nib.ge.2) then
      xmplot(1)=ymn(nib-1)
   else
      xmplot(1)=2.*ymn(nib)-ymn(nib+1)
   endif

   axlabx='y'
   xlabfmt='-3pf6.0'

elseif(h.eq.'Z') then
   do i=nib,nie
      xmplot(i-nib+2)=zmn(i)
      xtplot(i-nib+1)=ztn(i)
   enddo
   if(nib.ge.2) then
      xmplot(1)=zmn(nib-1)
   else
      xmplot(1)=2.*zmn(nib)-zmn(nib+1)
   endif

   axlabx='z'
   xlabfmt='0pf6.0'

!elseif(h.eq.'T') then
!   do i=nib,nie
!      xtplot(i-nib+1)=times
!   enddo
!   axlabx=' t (s)'
!   xlabfmt='f6.0'
endif

xmin=1.e20
xmax=-1.e20
! plots with tiles only are set to the full area
! others are limited by the t-grid
! would like to have things plotted on the actual stagger
if((cvar(1)(1:4).ne.'none'.and.conttyp(1)(1:1).eq.'t'.and.  &
    cvar(2)(1:4).eq.'none').or.  &
   (cvar(1)(1:4).ne.'none'.and.conttyp(1)(1:1).eq.'t'.and.  &
    cvar(2)(1:4).ne.'none'.and.conttyp(2)(1:1).eq.'t')) then
   do i=1,nii+1
      xmin=min(xmin,xmplot(i))
      xmax=max(xmax,xmplot(i))
   enddo
else
   do i=1,nii
      xmin=min(xmin,xtplot(i))
      xmax=max(xmax,xtplot(i))
   enddo
endif

if(xmax-xmin < 5000.) then
   axlabx=axlabx(1:1)//' (m)'
   xlabfmt='0pf6.0'
else
   axlabx=axlabx(1:1)//' (km)'
   xlabfmt='-3pf6.0'
endif


! Set up the y array for plotting.

ivtran=0
izstran=0

if(v.eq.'X') then
   do j=njb,nje
      ymplot(j-njb+2)=xmn(j)
      ytplot(j-njb+1)=xtn(j)
   enddo
   if(njb.ge.2) then
      ymplot(1)=xmn(njb-1)
   else
      ymplot(1)=2.*xmn(njb)-xmn(njb+1)
   endif

   axlaby='x (km)'
   ylabfmt='-3pf6.0'

elseif(v.eq.'Y') then
   do j=njb,nje
      ymplot(j-njb+2)=ymn(j)
      ytplot(j-njb+1)=ytn(j)
   enddo
   if(njb.ge.2) then
      ymplot(1)=ymn(njb-1)
   else
      ymplot(1)=2.*ymn(njb)-ymn(njb+1)
   endif

   axlaby = 'y (km)'
   ylabfmt = '-3pf6.0'

elseif(v.eq.'Z') then

   ! normal section for atmosphere plots

   if (ivtype.le.3) then

      do j=njb,nje
         ymplot(j-njb+2)=zmn(j)
         ytplot(j-njb+1)=ztn(j)
      enddo
      if(njb.ge.2) then
         ymplot(1)=zmn(njb-1)
      else
         ymplot(1)=2.*zmn(njb)-zmn(njb+1)
         ytplot(1)=ymplot(2)
      endif

   elseif(ivtype==8.or.ivtype==5) then

      ! next section modified for vertical soil (only) plots.  modify
      ! further for vertical leaf plots that combine soil, snow, veg,
      ! canopy, atm.

      do k=njb,nje
         ymplot(k-njb+2)=slz(k+1)
         ytplot(k-njb+1)=0.5*(slz(k)+slz(k+1))
      enddo
      ymplot(1)=slz(njb)

   endif

   ! Set up array with topography as necessary.

   ivtran=1
   izstran=1
   if(ivtype.le.3) then

      ! normal section for atmosphere plots

      if(h.eq.'X') then

         do ih=nib,nie
            zs(ih-nib+1)=valugp(1,n2,n3,1,ih,icoor,topt)
            topta_slab(ih-nib+1)=valugp(1,n2,n3,1,ih,icoor,topta)
            pclw(ih-nib+1)=valugp(1,n2,n3,1,ih,icoor,pctlnd)
            print*,'topta:',ih,ih-nib+1,topta_slab(ih-nib+1)
         enddo
         do ih=nib,nie-1
            zuv(ih-nib+2)=0.5*(zs(ih-nib+1)+zs(ih-nib+2))
         enddo
         zuv(1)=zs(1)
         zuv(nie+1)=zs(nie)
         if(nib.gt.1) zuv(1)=.5*(zs(1)+valugp(1,n2,n3,1,nib-1,icoor,topt))
         if(nie.lt.n2) zuv(nie-nib+2)=.5*(zs(nie-nib+1)  &
                                     +valugp(1,n2,n3,1,nie+1,icoor,topt))

      elseif(h.eq.'Y') then
         do ih=nib,nie
            zs(ih-nib+1)=valugp(1,n2,n3,1,icoor,ih,topt)
            pclw(ih-nib+1)=valugp(1,n2,n3,1,icoor,ih,pctlnd)
            topta_slab(ih-nib+1)=valugp(1,n2,n3,1,ih,icoor,topta)
         enddo
         do ih=nib,nie-1
            zuv(ih-nib+2)=0.5*(zs(ih-nib+1)+zs(ih-nib+2))
         enddo
         zuv(1)=zs(1)
         zuv(nie+1)=zs(nie)
         if (nib.gt.1) zuv(1)=.5*(zs(1)+valugp(1,n2,n3,1,icoor,nib-1,topt))
         if (nie.lt.n3) zuv(nie-nib+2)=.5*(zs(nie-nib+1)  &
                                      +valugp(1,n2,n3,1,icoor,nie+1,topt))

      elseif(h.eq.'T') then
         zst=valugp(1,n2,n3,1,ixbeg,iybeg,topt)
         pclwc=valugp(1,n2,n3,1,ixbeg,iybeg,pctlnd)
         do k=nib,nie
            zs(k-nib+1)=zst
            pclw(k-nib+1)=pclwc
            zuv(k-nib+1)=zst
         enddo
         zuv(nie-nib+2)=zst
      endif

      do k=nib,nie
         zmax=max(zmax,zs(k-nib+1))
      enddo
      zmax=max(zmax,zuv(1))
      zmax=max(zmax,zuv(nie-nib+2))

   elseif(ivtype==8.or.ivtype==5) then

      ! new section for soil (only) plot

      do ih=nib,nie+1
         zuv(ih-nib+1)=0.
      enddo
      zmax=0.

   endif

   if(ivtype.le.3) then
      axlaby='z (m)'
      ylabfmt='0pf6.0'
   else
      axlaby='z (m)'
      ylabfmt='0pf6.2'
   endif

   do nn=1,nztr
      ztr(nn)=ytplot(nn)
   enddo
   zmodtop=ztop

!elseif(v.eq.'T') then
!   do j=1,njj
!      ytplot(j)=times
!   enddo
!   axlaby=' t (s)'
!   ylabfmt='f6.0'
!   ivtran=0

endif

ymin=1.e20
ymax=-1.e20
! plots with tiles only are set to the full area
! others are limited by the t-grid
! would like to have things plotted on the actual stagger
if((cvar(1)(1:4).ne.'none'.and.conttyp(1)(1:1).eq.'t'.and.  &
    cvar(2)(1:4).eq.'none').or.  &
   (cvar(1)(1:4).ne.'none'.and.conttyp(1)(1:1).eq.'t'.and.  &
    cvar(2)(1:4).ne.'none'.and.conttyp(2)(1:1).eq.'t')) then
   do j=1,njj+1
      ymin=min(ymin,ymplot(j))
      ymax=max(ymax,ymplot(j))
   enddo
else
   do j=1,njj
      ymin=min(ymin,ytplot(j))
      ymax=max(ymax,ytplot(j))
   enddo
endif 
if(v=='Z'.and.njb.le.2.and.ivtype.ne.8.and.ivtype.ne.5) ymin=0.
if(v=='Z') ymax=ymax*(1.-zmax/ztop)+zmax

if(ymax-ymin < 5000. .or. v == 'Z') then
   axlaby=axlaby(1:1)//' (m)'
   ylabfmt='0pf6.0'
else
   axlaby=axlaby(1:1)//' (km)'
   ylabfmt='-3pf6.0'
endif

return

!--------------------------------------------

! Plot ticks marks and axis labels

entry framdrw (chhor,chver,xmplot,xtplot,ymplot,ytplot,topt,n2,n3  &
              ,nib,nie,njb,nje,nii,njj,px1,px2,py1,py2,xmin,xmax  &
              ,ymin,ymax,icoor,zuv,ztop,ivtype,vioff,vjoff,if_adap,voltsl)

call colortab_ind ('axis',ic)
call gsplci(ic)
call gstxci(ic)

if(ipinfo.ne.3)  &
   call axislab (px1,px2,py1,py2,nib,nie,njb,nje,nii,njj  &
                ,xtplot,ytplot,xmin,xmax,ymin,ymax  &
                ,axlabx,axlaby,xlabfmt,ylabfmt,zs,zuv,ztop  &
                ,v,ivtype,vioff,vjoff)

! Plot perimeter

call set (px1,px2,py1,py2,0.,1.,0.,1.,1)
call perim (1,1,1,1)

   print*,'@@@@@@@@@@@@@@@start buildgs/topo:',h,v,ivtype,ic
if((h.eq.'X'.or.h.eq.'Y'.or.h.eq.'T').and.  &
   ivtype.le.3.and.v.eq.'Z') then

   ! Draw the topography (not for leaf)
   
   call gslwsc(3.)  ! line width
   call colortab_ind ('axis',ic)
   call gsplci(ic)

   do i=1,nii+1
      pcoor(i)=0.
   enddo

   if((cvar(1)(1:4).ne.'none'.and.conttyp(1)(1:1).eq.'t'.and.  &
       cvar(2)(1:4).eq.'none').or.  &
      (cvar(1)(1:4).ne.'none'.and.conttyp(1)(1:1).eq.'t'.and.  &
       cvar(2)(1:4).ne.'none'.and.conttyp(2)(1:1).eq.'t')) then

      call set (px1,px2,py1,py2,xmplot(1),xmplot(nii+1),ymin,ymax,1)
      call getset (pp1,pp2,pq1,pq2,xxx1,xxx2,yyy1,yyy2,iii)
      zx(1)=xmplot(1)
      zy(1)=zs(1)
      zx(2)=xtplot(1)
      zy(2)=zs(1)
      call gpl (2,zx,zy)
      call gpl(nii,xtplot,zs)
      zx(1)=xtplot(nii)
      zy(1)=zs(nii)
      zx(2)=xmplot(nii+1)
      zy(2)=zs(nii)
      call gpl (2,zx,zy)
      
      ! this does topo steps - keep for 4.4
      !do i=1,nii
      !   zx(1)=xmplot(i)
      !   zy(1)=zs(i)
      !   zx(2)=xmplot(i+1)
      !   zy(2)=zs(i)
      !   call gpl (2,zx,zy)
      !enddo
      
   else

      call set (px1,px2,py1,py2,xtplot(1),xtplot(nii),ymin,ymax,1)
      call getset (pp1,pp2,pq1,pq2,xxx1,xxx2,yyy1,yyy2,iii)
      call gpl (nii,xtplot,zs)
      if(if_adap == 1) call gpl (nii,xtplot,topta_slab)
      

   endif

   ! plot land/water bar under frame
   
   if(ipanl.eq.1) then  ! line width
   
      call gslwsc(5.)  ! line width
         
      call colortab_ind ('land',icl)
      call colortab_ind ('water',icw)

      if((cvar(1)(1:4).ne.'none'.and.conttyp(1)(1:1).eq.'t'.and.  &
          cvar(2)(1:4).eq.'none').or.  &
         (cvar(1)(1:4).ne.'none'.and.conttyp(1)(1:1).eq.'t'.and.  &
          cvar(2)(1:4).ne.'none'.and.conttyp(2)(1:1).eq.'t')) then

         call set (px1,px2,py1-.005,py2,xmplot(1),xmplot(nii+1),0.,1.,1)
 
         call gsplci(icw)
         zy(1)=0.
         zy(2)=0.
         do i=1,nii
            zx(1)=xmplot(i)
            zx(2)=xmplot(i+1)
            if(pclw(i).lt..5) then
               call gsplci(icw)
               call gpl (2,zx,zy)
            else
               call gsplci(icl)
               call gpl (2,zx,zy)
            endif
         enddo

      else
 
         call set (px1,px2,py1-.005,py2,xtplot(1),xtplot(nii),0.,1.,1)

         call gsplci(icl)
         call gpl (nii,xtplot(1),pcoor(1))
         call gsplci(icw)
         do i=1,nii
            if(pclw(i).lt..5) call gpl (2,xtplot(i),pcoor(i))
         enddo

      endif
   
   endif

endif
!============================================================================   
   print*,'@@@@@@@@@@@@@@@start buildgs:',v,if_adap,nii,njj,ymin,ymax

!   if(if_adap == 1 .and. v =='Z') then
   if(if_adap == 1 ) then
   !  Draw buildings with softfill
      call set (px1,px2,py1,py2,xtplot(1),xtplot(nii),ymin,ymax,1)
      call gsplci(ic)
      nseg=5

      call gsfais (1)
      call sfseti ('TYPE OF FILL',0)
      
      do i=1,nii-1
         do k=1,njj-1
            if( voltsl(i,k) < 1.) then
               print '(3i4,10g12.5)'  &
                     ,i,k,ic,voltsl(i,k),ymplot(k+1),ymplot(k),xmplot(i)
               xfill(1) = xmplot(i)   ; yfill(1) = ymplot(k)
               xfill(2) = xmplot(i+1) ; yfill(2) = ymplot(k)
               xfill(3) = xmplot(i+1) ; yfill(3) = ymplot(k+1)
               xfill(4) = xmplot(i)   ; yfill(4) = ymplot(k+1)
               call sfsgfa (xfill,yfill,4,dstfill,8,indfill,12,ic)
            endif
         enddo
      enddo
      call sflush()         
  endif
         
!=============================================================================

call gslwsc(1.)  ! reset line width

! No map will be drawn - set flag correctly
itrans=0

! Set the vertical exaggeration factor used in plotting vectors in
! the FV subroutine
zfactor=(px2-px1)*(ymax-ymin)/((py2-py1)*(xmax-xmin))

call set (0.,1.,0.,1.,0.,1.,0.,1.,1)

call colortab_ind ('title1',ic)
call gsplci(ic)
call gstxci(ic)

return
end

!***************************************************************************

subroutine axislab (px1,px2,py1,py2,nib,nie,njb,nje,nii,njj  &
                   ,xtplot,ytplot,xmin,xmax,ymin,ymax  &
                   ,axlabx,axlaby,xlabfmt,ylabfmt  &
                   ,zs,zuv,ztop,vert,ivtype,vioff,vjoff)

implicit none

integer :: nib,nie,njb,nje,nii,njj,ivtype
real :: px1,px2,py1,py2,xmin,xmax,ymin,ymax,ztop,vioff,vjoff
real :: xtplot(*),ytplot(*),zs(*),zuv(*)
character(len=*) :: axlabx,axlaby,xlabfmt,ylabfmt,vert

include 'frame.h' 

integer :: i,j,ic,nlably,nlablx
real :: yleft,yright,ylabn,zleft,zright,ymin1,ymax1,xmin1,xmax1,ycoord  &
       ,xcoord,xlabn,xlabnp,yinc,ylabst,xinc,xlabst,tickin,tickout
character(len=16) :: cformat,number

call colortab_ind ('axis',ic)
call gsplci(ic)
call gstxci(ic)

! inward and outward tickmark sizes
if(ipanl.eq.1) then
   tickin=.008
   tickout=.01
else
   tickin=.005
endif

! Put inward pointing ticks on the Y-axis
! account for vector box in panel plots

ymin1=ymin-(ymax-ymin)/(py2-py1)*py1
ymax1=ymax+(ymax-ymin)/(py2-py1)*(1.-py2)
call set (0.,1.,0.,1.,0.,1.,ymin1,ymax1,1)
if(vert.eq.'Z'.and.ivtype.le.3) then
   if (conttyp(1)(1:1).eq.'t'.or.conttyp(2)(1:1).eq.'t') then
      zleft=zuv(1)
      zright=zuv(nie+1)
   else
      zleft=zs(1)
      zright=zs(nie)
   endif
else
   zleft=0.
   zright=0.
endif

do j=1,njj
   yleft=zleft+ytplot(j)*(1.-zleft/ztop)
   call frstpt(px1,yleft)
   call vector(px1+tickin,yleft)
   yright=zright+ytplot(j)*(1.-zright/ztop)
   if(vert.eq.'Z') then
      if((ytplot(j).lt.ytplot(njj)-vjoff/(px2-px1)*(ytplot(njj)-ytplot(1))  &
        .and.ipanl.ne.1).or.ipanl.eq.1) then
         call frstpt(px2,yright)
         call vector(px2-tickin,yright)
      endif
   else
      if((ytplot(j).gt.ytplot(1)+vjoff/(px2-px1)*(ytplot(njj)-ytplot(1))  &
        .and.ipanl.ne.1).or.ipanl.eq.1) then
         call frstpt(px2,yright)
         call vector(px2-tickin,yright)
      endif
   endif
enddo

! Draw outward pointing ticks on the Y-axis and label them
      
if(ipanl.eq.1.and.ipinfo.ne.2) then
   write(cformat,'(''('',a8,'')'')') ylabfmt
   nlably=9
   call niceinc_mrc (ymin,ymax,nlably,yinc,ylabst)
   ycoord=ymax-.05*(ymax-ymin)
   do j=1,nlably
      ylabn=ylabst+(j-1)*yinc
      write(number,cformat) ylabn
      if(ylabn.lt.ycoord) call plchlq (px1-.01,ylabn,number(1:6),-.7,0.,1.)
      call frstpt(px1,ylabn)
      call vector(px1-tickout,ylabn)
   enddo
   ycoord=.99*ymax+.01*ymin
   call plchlq (px1-.01,ycoord,axlaby,-0.7,0.,1.)
endif

! Put inward pointing ticks at every grid point on the x-axis
! account for vector box in panel plots
   
xmin1=xmin-(xmax-xmin)/(px2-px1)*px1
xmax1=xmax+(xmax-xmin)/(px2-px1)*(1.-px2)
call set(0.,1.,0.,1.,xmin1,xmax1,0.,1.,1)
do i=1,nii
   if((xtplot(i).lt.xtplot(nii)-vioff/(px2-px1)*(xtplot(nii)-xtplot(1))  &
     .and.ipanl.ne.1).or.ipanl.eq.1) then
      if(vert.eq.'Z') then
         call frstpt(xtplot(i),py2)
         call vector(xtplot(i),py2-tickin)
      else
         call frstpt(xtplot(i),py1)
         call vector(xtplot(i),py1+tickin)
      endif
   endif
   if(vert.eq.'Z') then
      call frstpt(xtplot(i),py1)
      call vector(xtplot(i),py1+tickin)
   else
      call frstpt(xtplot(i),py2)
      call vector(xtplot(i),py2-tickin)
   endif
enddo

! Draw outward pointing ticks on the X-axis and label them

if(ipanl.eq.1.and.ipinfo.ne.2) then
   write(cformat,'(''('',a8,'')'')') xlabfmt
   nlablx=9
   call niceinc_mrc (xmin,xmax,nlablx,xinc,xlabst)
   xcoord=.90*xmax+.10*xmin
   do i=1,nlablx
      xlabn=xlabst+(i-1)*xinc
      write(number,cformat) xlabn
      xlabnp=xlabn-.02*(xmax1-xmin1)
      if(xlabn.lt.xcoord) call plchlq (xlabnp,py1-.03,number(1:6),-.7,0.,0.)
      call frstpt (xlabn,py1)
      call vector (xlabn,py1-tickout)
   enddo
   call plchlq (.9*xmax+.1*xmax1,py1-.03,axlabx,-0.7,0.,1.)
endif

return
end

!***************************************************************************

subroutine setframe ()

implicit none

call set (0.,1.,0.,1.,0.,1.,0.,1.,1)
call gsplci(0)
call frstpt(.0,.0)
call vector(.0,.0)
call sflush
 
return
end

!***************************************************************************

subroutine backlab (px1,px2,py1,py2,axis1,axis2,ngrid  &
                   ,xtn,ytn,ztn,slz,itime,idate,ihtran,itrans  &
                   ,iplev,head1,boxtop,boxhite,ivtype,iovtype,nf)

implicit none

integer :: ngrid,itime,idate,ihtran,itrans,iplev,ivtype,iovtype,nf
real    :: px1,px2,py1,py2,xtn,ytn,ztn,slz,boxtop,boxhite
character(len=*)  :: axis1,axis2,head1

include 'frame.h'

integer :: ic,imon,imonth,iday,iyear,iyr,isec,nch,julday1970,nnn,ittt  &
          ,nfiles
real :: hpos,box5,fcsthr,fcstsec,startutc,xx1,xx2,yy1,yy2,fcstsec1  &
       ,fcstsec2,dfcstsec,fcstday
character(len=60) :: title
character(len=3)  :: dayofwk(0:6),vday,month(12),cfhr,cfday
character(len=2)  :: cmon,cday,chr
character(len=4)  :: cyear
character(len=6)  :: ctim
character(len=8)  :: cyrmoda
character(len=7)  :: ctn,ctn2

data month/'Jan','Feb','Mar','Apr','May','Jun'  &
          ,'Jul','Aug','Sep','Oct','Nov','Dec'/
data dayofwk/'Thu','Fri','Sat','Sun','Mon','Tue','Wed'/    

! Draw box and label for the run title

call set (0.,1.,0.,1.,0.,1.,0.,1.,1)

if(ipinfo.eq.1) then

   ! Draw boxes for the background labels
   
   call colortab_ind ('title0',ic)
   call plotif(0.,0.,2)
   call gsplci(ic)
   call plotif(0.,0.,2)
   call gstxci(ic)
   call plotif(0.,0.,2)
 
   call frstpt(.01,boxtop)
   call vector(.01,boxtop-boxhite)
   call vector(.99,boxtop-boxhite)
   call vector(.99,boxtop)
   call vector(.01,boxtop)
 
   call frstpt(.55,boxtop)
   call vector(.55,boxtop-boxhite)
 
   box5=boxtop-.5*boxhite
   call sflush
   
   call colortab_ind ('title1',ic)
   call plotif(0.,0.,2)
   call gsplci(ic)
   call plotif(0.,0.,2)
   call gstxci(ic)
   call plotif(0.,0.,2)
 
   write(title,'(''grid '',i1)') ngrid
   call plchhq (.60,box5,title,-0.7,0.,-1.)
   write (title,'(a)') head1
   call plchhq (.03,box5,title,-0.7,0.,-1.)
 
   ! Draw boxes and labels for the grid number, time, and slab position
 
   call colortab_ind ('title0',ic)
   call plotif(0.,0.,2)
   call gsplci(ic)
   call plotif(0.,0.,2)
   call gstxci(ic)
   call plotif(0.,0.,2)
 
   boxtop=boxtop-boxhite
 
   call frstpt(.01,boxtop)
   call vector(.01,boxtop-boxhite)
   call vector(.99,boxtop-boxhite)
   call vector(.99,boxtop)
 
   call frstpt(.20,boxtop)
   call vector(.20,boxtop-boxhite)
 
   call frstpt(.55,boxtop)
   call vector(.55,boxtop-boxhite)
 
   call frstpt(.675,boxtop)
   call vector(.675,boxtop-boxhite)
 
   call frstpt(.79,boxtop)
   call vector(.79,boxtop-boxhite)

   call frstpt(.915,boxtop)
   call vector(.915,boxtop-boxhite)
 
   box5=boxtop-.5*boxhite
   call sflush
 
   call colortab_ind ('title2',ic)
   call plotif(0.,0.,2)
   call gsplci(ic)
   call plotif(0.,0.,2)
   call gstxci(ic)
   call plotif(0.,0.,2)
 
   if(axis1.ne.'T'.and.axis2.ne.'T') then
      iyr=idate/10000
      imon=mod(idate,10000)/100
      iday=mod(idate,100)
      isec=mod(itime,100)
      write (title,'(i4.4,a1,i2.2,a1,i2.2,a1,i4.4,a1,i2.2,a4)')  &
        iyr,'-',imon,'-',iday,'-',itime/100,'.',isec,' UTC'
      call plchhq (.21,box5, title(1:index(title,'C')), -.7,0.,-1.)
   endif
 
   hpos=.03
   if(axis1.ne.'X'.and.axis2.ne.'X') then
      write (title,'(a,-3PF7.0,a)') 'x =', xtn,' km'
        call plchhq (hpos, box5, title, -.7,0., -1.)
        hpos=hpos+.19
   endif
 
   IF(axis1.ne.'Y'.and.axis2.ne.'Y') then
      write (title,'(a,-3PF7.0,a)') 'y =', ytn,' km'
        call plchhq (hpos, box5, title, -.7,0., -1.)
        hpos=hpos+.19
   endif
 
   if(axis1.ne.'Z'.and.axis2.ne.'Z') then
      if(itrans.ne.3) then
         if((ivtype.eq.3.and.cvar(1)(1:4).ne.'none').or.  &
           (iovtype.eq.3.and.cvar(2)(1:4).ne.'none').or.  &
             cwinds(1:1).ne.'n') then
            write (title,'(a,F7.1,a)') 'z =', ztn,' m'
         elseif(((ivtype==8.or.ivtype==5).and.cvar(1)(1:4).ne.'none').or.  &
               ((iovtype==8.or.iovtype==5).and.cvar(2)(1:4).ne.'none')) then
            write (title,'(a,F7.2,a)') 'z =', slz,' m'
         else
            write (title,'(a,F7.1,a)') ' '
         endif
         call plchhq (hpos,box5,title,-0.7,0.,-1.)
         hpos=hpos+.19
      else
         write (title,'(a,i5,a)') 'p =', iplev,' mb'
         call plchhq (hpos,box5,title,-0.7,0.,-1.)
         hpos=hpos+.19
      endif
   endif
 
   call plchhq (.58,box5,' min ',-0.6,0.,-1.)
   call plchhq (.705,box5,' max ',-0.6,0.,-1.)
   call plchhq (.82,box5,' inc ',-0.6,0.,-1.)
   call plchhq (.925,box5,' lab* ',-0.6,0.,-1.)

endif

if(ipanl.ne.1.or.ipinfo==2) then
   
   call colortab_ind ('title1',ic)
   call gsplci(ic)
   call gstxci(ic)

   ! construct and write the top title
   
   call RAMS_get_idata (3,nf,ngrid,itime,nnn)
   call RAMS_get_idata (2,nf,ngrid,idate,nnn)
   call RAMS_get_fdata (1,nf,ngrid,fcstsec,nnn)
   call RAMS_get_fdata (2,nf,ngrid,startutc,nnn)
   write(cyrmoda,'(i8)') idate
   cyear=cyrmoda(1:4)
   read(cyear,'(i4)') iyear
   cmon=cyrmoda(5:6)
   read(cmon,'(i2)') imonth      
   cday=cyrmoda(7:8)
   read(cday,'(i2)') iday
   write(ctim,'(i6.6)') itime
   vday=dayofwk(mod(julday1970(imonth,iday,iyear)-1,7))
   fcsthr=fcstsec/3600.
   write(cfhr,'(i3.3)') nint(fcsthr)
   fcstday=fcstsec/86400.
   write(cfday,'(i3)') nint(fcstday)
   
   ! files spaced in days or hours?
   call RAMS_get_idata (4,1,ngrid,nfiles,nnn)
   call RAMS_get_fdata (1,1,ngrid,fcstsec1,nnn)
   fcstsec2=0.
   if(nfiles.gt.1) call RAMS_get_fdata (1,2,ngrid,fcstsec2,nnn)
   dfcstsec=fcstsec2-fcstsec1
   if(dfcstsec.lt.86400.) then
      title=cfhr//' hr RAMS Valid '//ctim(1:4)  &
            //' UTC '//vday//' '//cday//' '//month(imonth)//' '//cyear
   else
      title='day '//cday(1:len_trim(cday))//' RAMS Valid '//ctim(1:4)  &
            //' UTC '//vday//' '//cday//' '//month(imonth)//' '//cyear
   endif
   
   if(ipanl==1) then
      call plchlq (px1,py2+.015,title(1:len_trim(title)),-.8,0.,-1.)
   else
      call plchlq (px1,py2+.01,title(1:len_trim(title)),-.65,0.,-1.)
   endif
   
   ! bottom title
   
   title=''
   if(axis1.ne.'X'.and.axis2.ne.'X') then
      write(ctn,'(-3pf7.0)') xtn
      call deblank (ctn,ctn2,nch)
      title='y='//ctn2(1:len_trim(ctn2))//'km'
   endif
   if(axis1.ne.'Y'.and.axis2.ne.'Y') then
      write(ctn,'(-3pf7.0)') ytn
      call deblank (ctn,ctn2,nch)
      title='y='//ctn2(1:len_trim(ctn2))//'km'
   endif
   if(axis1.ne.'Z'.and.axis2.ne.'Z') then
      if(itrans.ne.3) then
         if((ivtype.eq.3.and.cvar(1)(1:4).ne.'none').or.  &
           (iovtype.eq.3.and.cvar(2)(1:4).ne.'none').or.  &
            (cwinds(1:1).ne.'n')) then
            write(ctn,'(f7.1)') ztn
            call deblank(ctn,ctn2,nch)
            title='z='//ctn2(1:len_trim(ctn2))//'m'
         elseif(((ivtype==8.or.ivtype==5).and.cvar(1)(1:4).ne.'none').or.  &
               ((iovtype==8.or.iovtype==5).and.cvar(2)(1:4).ne.'none')) then
            write(ctn,'(f7.2)') slz
            call deblank (ctn,ctn2,nch)
            title='z='//ctn2(1:len_trim(ctn2))//'m'
         endif
      else
         write(ctn,'(i5)') iplev
         call deblank (ctn,ctn2,nch)
         title='z='//ctn2(1:len_trim(ctn2))//'mb'
      endif
   endif
   
   if(ipanl==1) then
      call plchlq(px2,py2+.015,title(1:len_trim(title)),-.8,.8,1.)
   else
      call plchlq(px2,py1-.01,title(1:len_trim(title)),-.55,0.,1.)
   endif

endif

return
end

!***************************************************************************

subroutine vecstrm (u,v,nii,njj,work,px1,px2,py1,py2  &
                   ,horiz,vert,xmin,xmax,ymin,ymax,zmin,zmax  &
                   ,slab,boxtop,boxhite,xtplot,ytplot,zs  &
                   ,ztop,nib,njb,uu,vv,nngd,ivvar,iolayer,vioff,vjoff)

implicit none

integer :: nii,njj,nib,njb,nngd,ivvar,iolayer
real :: px1,px2,py1,py2,xmin,xmax,ymin,ymax,zmin,zmax,boxtop,boxhite,ztop  &
       ,vioff,vjoff
real :: u(0:nii,0:njj),v(0:nii,0:njj),work(*),slab(*),xtplot(*),ytplot(*)  &
       ,zs(*),uu(0:nii,0:njj),vv(0:nii,0:njj)
character(len=*)  :: horiz,vert

include 'frame.h'
include 'interface.h'

integer :: i,j,ier,lcdname,lcdunits,ic
real :: dir,spd,box5,scle,vexag
real :: rif(6),rjf(6),dst(6),ind(8)
real, allocatable :: un(:),vn(:)
character(len=80) :: title

integer :: inita,initb,iterp,iterc,igflg,imsg,icyc,incxv,incyv
real :: arowl,uvmsg,displ,dispc,cstop,bbbbb,tilelo,tilehi
common /str03/ inita,initb,arowl,iterp,iterc,igflg  &
              ,imsg,uvmsg,icyc,displ,dispc,cstop
common /vec2/ bbbbb,incxv,incyv

incxv=intwindi
incyv=intwindj
inita=intwindi
initb=2

call gsplci(15)
call gstxci(6)

! Plot the vector field.

! the following line has been modified to give the user the option
! of plotting out vorticity vectors

if(cwinds(1:1).eq.'v'.or.cwinds(1:1).eq.'r'.or.cwinds(1:1).eq.'t')  &
   call ramsvect (nii,njj,u,v,slab,boxtop,boxhite,xtplot,ytplot  &
                 ,zs,ztop,vert,px1,px2,py1,py2,xmin,xmax,ymin,ymax  &
                 ,zmax,nib,njb,uu,vv,nngd,ivvar,iolayer,vioff,vjoff)

! Plot the stream lines

if(cwinds(1:1).eq.'s'.or.cwinds(1:1).eq.'b') then
  if(vert(1:1).eq.'Z') then
    if(conttyp(1)(1:1).eq.'t') then
      call set(px1,px2,py1,py2,0.5,float(nii)+.5,ymin,ymax,1)
    else
      call set(px1,px2,py1,py2,1.0,float(nii),ymin,ymax,1)
    endif
  else
    if(conttyp(1)(1:1) .eq. 't') then
      call set(px1,px2,py1,py2,.5,float(nii)+.5,.5,float(njj)+.5,1)
    else
      call set(px1,px2,py1,py2,1.,float(nii),1.,float(njj),1)
    endif
  endif
endif

if(cwinds(1:1).eq.'s') then
   ! getting a nii by njj array from the oversized u and v arrays
   vexag=1.
   if(vert(1:1) == 'Z')  &
           vexag=(py2-py1)/(px2-px1) * (xmax-xmin)/(ymax-ymin)
   allocate(un(nii*njj))
   allocate(vn(nii*njj))
   call strmln_mrc2 (u,v,un,vn,work,nii,nii,njj,1,ier,vexag)   
   deallocate(un,vn)
   print *,'Drawn streamlines'
endif

! Plot the wind barbs

if(cwinds(1:1).eq.'b') then
   call set (px1,px2,py1,py2,1.,float(nii),1.,float(njj),1)
   call gsplci(iwcolor)
   call gstxci(iwcolor)
   if(stemleng.le.0.) then
      scle=.85*float(intwindi)
   else
      scle=.85*float(intwindi)*stemleng
   endif
   tilelo=1.e15
   tilehi=-1.e15
   do j=1,njj,intwindj
      do i=1,nii,intwindi
         call winddf (dir,spd,u(i,j),v(i,j))
         call windbarb (float(i),float(j),1,dir,spd,scle,0.)
         tilelo=min(tilelo,spd)
         tilehi=max(tilehi,spd)
      enddo
   enddo
   !print*,tilelo,tilehi
   print*,'Drawn barbs'
 
   lcdname=len_trim(cdname(iolayer))-1
   lcdunits=len_trim(cdunits(iolayer))-1
      
   if(ipinfo.eq.1) then

      ! Plot label information for color vector field

      call set (0.,1.,0.,1.,0.,1.,0.,1.,1)
      
      ! Draw vector information and label box
 
      call colortab_ind ('title0',ic)
      call gsplci(ic)
      call plotif(0.,0.,2)
      call gstxci(ic)
      call plotif(0.,0.,2)
      
      boxtop=boxtop-boxhite

      !print*, 'vectors',boxtop,boxhite

      call frstpt(.01,boxtop)
      call vector(.01,boxtop-boxhite)
      call vector(.99,boxtop-boxhite)
      call vector(.99,boxtop)

      call frstpt(.20,boxtop)
      call vector(.20,boxtop-boxhite)

      call frstpt(.55,boxtop)
      call vector(.55,boxtop-boxhite)

      call frstpt(.675,boxtop)
      call vector(.675,boxtop-boxhite)

      call frstpt(.79,boxtop)
      call vector(.79,boxtop-boxhite)

      call frstpt(.915,boxtop)
      call vector(.915,boxtop-boxhite)

      box5=boxtop-.5*boxhite
      call sflush

      ! Plot the barb values

      call gsplci(iwcolor)
      call plotif(0.,0.,2)
      call gstxci(iwcolor)
      call plotif(0.,0.,2)

      call plchhq (.03,box5,'barbs  ',-.7,0.,-1.)
      if(ibscale.le.1) then
         title='staff 10 m/s   flag 50 m/s'
      elseif(ibscale.eq.2) then
         title='staff 4 m/s   flag 20 m/s'
      elseif(ibscale.eq.3) then
         title='staff 2 m/s   flag 10 m/s'
      elseif(ibscale.ge.4) then
         title='staff 1 m/s   flag 5 m/s'
      endif
      call plchhq (.21,box5,title,-.7,0.,-1.)

      write (title,'(g10.4)') tilelo
      call plchhq (.56,box5,title,-.6,0.,-1.)
      write (title,'(g10.4)') tilehi
      call plchhq (.685,box5,title,-.6,0.,-1.)
 
   endif
   
   if(ipanl.ne.1) then

      ! Construct the title for multi-panel plots

      call set (0.,1.,0.,1.,0.,1.,0.,1.,1)
 
      ! fill box in lower right corner
 
      rif(1)=px2
      rif(3)=rif(1)-vioff
      rif(2)=rif(1)
      rif(4)=rif(3)
      rif(5)=rif(1)
 
      if(vert.eq.'Z') then
         rjf(1)=py2
         rjf(2)=rjf(1)-vjoff
         rjf(3)=rjf(2)
         rjf(4)=rjf(1)
         rjf(5)=rjf(1)
      else
         rjf(1)=py1
         rjf(2)=rjf(1)+vjoff
         rjf(3)=rjf(2)
         rjf(4)=rjf(1)
         rjf(5)=rjf(1)
      endif
 
      call sfsgfa (rif,rjf,4,dst,6,ind,8,0)
 
      ! put boarder around box
      !call colortab_ind ('axis',ic)
      !call gsplci(ic)
      !call gstxci(ic)
 
      !call frstpt(rif(1),rjf(1))
      !call vector(rif(2),rjf(2))
      !call vector(rif(3),rjf(3))
      !call vector(rif(4),rjf(4))
      !call vector(rif(5),rjf(5))
      !call sflush

      ! Plot the barb values

      call gsplci(iwcolor)
      call gstxci(iwcolor)

      if(ibscale.le.1) then
         title='staff 10m/s'
      elseif(ibscale.eq.2) then
         title='staff 4m/s'
      elseif(ibscale.eq.3) then
         title='staff 2m/s'
      elseif(ibscale.ge.4) then
         title='staff 1m/s'
      endif
      if(vert(1:1).eq.'Z') then
         call plchlq (px2-.002,py2-.022,title(1:(len_trim(title))),-.5,0.,1.)
      else
         call plchlq (px2-.002,py1+.009,title(1:(len_trim(title))),-.5,0.,1.)
      endif
 
      if(ibscale.le.1) then
         title='flag 50m/s'
      elseif(ibscale.eq.2) then
         title='flag 20m/s'
      elseif(ibscale.eq.3) then
         title='flag 10m/s'
      elseif(ibscale.ge.4) then
         title='flag 5m/s'
      endif
      if(vert(1:1).eq.'Z') then
         call plchlq (px2-.002,py2-.009,title(1:(len_trim(title))),-.5,0.,1.)
      else
         call plchlq (px2-.002,py1+.022,title(1:(len_trim(title))),-.5,0.,1.)
      endif
   
   endif
endif

return
end
!***************************************************************************

SUBROUTINE STRMLN_mrc2(Ux,Vx,U,V,WORK,IMAX,IPTSX,JPTSY,NSET,IER,vexag)
DIMENSION       Ux(IMAX+1,JPTSY+1)    ,Vx(IMAX+1,JPTSY+1)      ,  &
                U(IMAX,JPTSY)         ,V(IMAX,JPTSY)           ,  &
                WORK(1)
DIMENSION       WIND(4)               ,VIEW(4)
!
COMMON /STR01/  IS         ,IEND      ,JS        ,JEND  &
             ,  IEND1      ,JEND1     ,I         ,J  &
             ,  X          ,Y         ,DELX      ,DELY  &
             ,  ICYC1      ,IMSG1     ,IGFL1
COMMON /STR02/  EXT , SIDE , XLT , YBT
COMMON /STR03/  INITA , INITB , AROWL , ITERP , ITERC , IGFLG  &
             ,  IMSG , UVMSG , ICYC , DISPL , DISPC , CSTOP

SAVE

! fix to give a nii by njj array going into nii by njj (not nii+1 by njj+1)
do i=1,IMAX
  do j=1,JPTSY
      U(i,j)=Ux(i,j)
      V(i,j)=Vx(i,j)*vexag
   enddo
enddo

EXT       = 0.25
SIDE      = 0.90
XLT       = 0.05
YBT       = 0.05

!INITA     = 2
!INITB     = 2
AROWL     = 0.33
ITERP     = 35
ITERC     = -99
IGFLG     = 0
ICYC      = 0
IMSG      = 0
UVMSG     = 1.E+36
DISPL     = 0.33
DISPC     = 0.67
CSTOP     = 0.50


IER = 0

! LOAD THE COMMUNICATION COMMON BLOCK WITH PARAMETERS

IS = 1
IEND = IPTSX
JS = 1
JEND = JPTSY
IEND1 = IEND-1
JEND1 = JEND-1
IEND2 = IEND-2
JEND2 = JEND-2
XNX = FLOAT(IEND-IS+1)
XNY = FLOAT(JEND-JS+1)
ICYC1 = ICYC
IGFL1 = IGFLG
IMSG1 = 0

! IF ICYC .NE. 0 THEN CHECK TO MAKE SURE THE CYCLIC CONDITION EXISTS.

IF (ICYC1.NE.0) CALL CHKCYC  (U,V,IMAX,JPTSY,IER)

! Save original SET call.

CALL GETSET (VIEW(1),VIEW(2),VIEW(3),VIEW(4),  &
             WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)

IF (NSET.lt.0) GOTO 10
IF (NSET.eq.0) GOTO 20
IF (NSET.gt.0) GOTO 60

10 X1 = VIEW(1)
X2 = VIEW(2)
Y1 = VIEW(3)
Y2 = VIEW(4)
X3 = IS
X4 = IEND
Y3 = JS
Y4 = JEND
GOTO  55

20 ITYPE = 1
X1 = XLT
X2 = (XLT+SIDE)
Y1 = YBT
Y2 = (YBT+SIDE)
X3 = IS
X4 = IEND
Y3 = JS
Y4 = JEND
IF (AMIN1(XNX,XNY)/AMAX1(XNX,XNY).LT.EXT) GO TO  50
IF (XNX.lt.XNY) GOTO 30
IF (XNX.eq.XNY) GOTO 50
IF (XNX.gt.XNY) GOTO 40
30 X2 = (SIDE*(XNX/XNY) + XLT)
GOTO  50
40 Y2 = (SIDE*(XNY/XNX) + YBT)
50 CONTINUE

! CENTER THE PLOT

DX = 0.25*( 1. - (X2-X1) )
DY = 0.25*( 1. - (Y2-Y1) )
X1 = (XLT+DX)
X2 = (X2+DX )
Y1 = (YBT+DY)
Y2 = (Y2+DY )

55 CONTINUE

! DEFINE AND SELECT NORMALIZATION TRANS, SET LOG SCALING

CALL SET(X1,X2,Y1,Y2,X3,X4,Y3,Y4,ITYPE)

IF (NSET.EQ.0) CALL PERIM (1,0,1,0)

60 CONTINUE

! DRAW THE STREAMLINES
! .   BREAK THE WORK ARRAY INTO TWO PARTS.  SEE DRWSTR FOR FURTHER
! .   COMMENTS ON THIS.


CALL DRWSTR (U,V,WORK(1),WORK(IMAX*JPTSY+1),IMAX,JPTSY)

! Restore SET call.

CALL SET (VIEW(1),VIEW(2),VIEW(3),VIEW(4),  &
          WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)

RETURN
END
