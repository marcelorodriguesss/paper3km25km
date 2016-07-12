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

subroutine ramsvect (nii,njj,uplt,vplt,slab,boxtop,boxhite,xtplot,ytplot  &
                    ,zs,ztop,vert,px1,px2,py1,py2,xmin,xmax,ymin,ymax  &
                    ,zmax,nib,njb,uu,vv,nngd,ivvar,iolayer,vioff,vjoff)

implicit none

integer :: nii,njj,nib,njb,nngd,ivvar,iolayer
real :: boxtop,boxhite,ztop,px1,px2,py1,py2,xmin,xmax,ymin,ymax,zmax  &
       ,vioff,vjoff
real :: xtplot(*),ytplot(*),zs(*)
real :: uplt(0:nii,0:njj),vplt(0:nii,0:njj),slab(nii,njj)  &
       ,uu(0:nii,0:njj),vv(0:nii,0:njj)
character(len=*) :: vert

include 'frame.h'

integer :: icolor,nincs,ist,jst,ic,npsize,i,j,lcdname,lcdunits,inc,ivec  &
          ,mv0,mv1,mv2,mv3,mv4,mv5,mv6,mv7,mvt0,mvt1,mvt2,mvt3,mvt4,mvt5  &
          ,mvt6,mvt7,nvec,icmj,ib,ifllbeg,ici,imid,nnn,ln,ndum,labstep
real :: varmax,varmin,adiff,stemfac,hsize1,hsize2,hsize,angstem  &
       ,dxh1,dyh1,dxh2,dyh2,x1,x2,x3,x4,y1,y2,y3,y4,box5,angh,velmax  &
       ,xxt1,xxt2,xxt3,xxt4,yyt1,yyt2,yyt3,yyt4,xa,xz,ya,z,val,vexag  &
       ,oplt,vt,vvt,ut,uut,aplt,scalefach,vel,xt1,yt1,xt2,yt2,xt3,yt3  &
       ,xt4,yt4,yz,xstart,ystart,yinc,tilelo,tilehi,dir,spd,sz,cbargap  &
       ,valuelab,conrlo1,conrhi1,conrlo2,conrinc1,step,slz,voff
real :: rif(6),rjf(6),dst(6),ind(8)

integer, allocatable :: iinc(:)
real, allocatable :: cv(:),cvt(:)
character(len=80) :: title
character(len=8)  :: number,numbr
character(len=10) :: cconinc
         
integer, parameter :: ncolor=256
real :: r(ncolor),g(ncolor),b(ncolor)

! If negative values are specified for stemleng, headleng0, headleng, and/or
! headang, assign default values to them.

if(stemleng .lt.0.) stemleng =float(intwindi)
if(headleng0.lt.0.) headleng0=.1*float(intwindi)
if(headleng .lt.0.) headleng =.2*float(intwindi)
if(headang  .lt.0.) headang  =25.

icmj=iwcolor
icolor=0
nincs=1
ist=2
jst=2
if(nib.ge.2) ist=1
if(njb.ge.2) jst=1

! If vectors are to be colored, find how many levels

if(cvar(iolayer)(1:4).ne.'none'.and.conttyp(iolayer)(1:1).eq.'v') then

   icolor=1

   varmax=-1.e15
   varmin=1.e15
   do j=jst,njj,intwindj
      do i=ist,nii,intwindi
         varmin=min(varmin,slab(i,j))
         varmax=max(varmax,slab(i,j))
      enddo
   enddo
   if(abs(varmin).lt.1.e-20) varmin=0.
   if(abs(varmax).lt.1.e-20) varmax=0.
   clomin(iolayer,ivvar,nngd)=min(clomin(iolayer,ivvar,nngd),varmin)
   chimax(iolayer,ivvar,nngd)=max(chimax(iolayer,ivvar,nngd),varmax)
   !print*,varmin,varmax,clomin(iolayer,ivvar),chimax(iolayer,ivvar)

   if(abs(conrlo(iolayer)).lt.1.e-10 .and.  &
      abs(conrhi(iolayer)).lt.1.e-10) then
      conrlo1=varmin
      conrhi1=varmax
   else
      conrlo1=conrlo(iolayer)
      conrhi1=conrhi(iolayer)
   endif
   !print*,conrlo1,conrhi1

   !print*,iolayer,cgrad(iolayer),icmeth(iolayer),icol(iolayer)  &
   !      ,' ',colorbar(iolayer),ccent(iolayer)

   if(abs(conrinc(iolayer)).lt.1.e-10) then
      call niceinc6_mrc(conrlo1,conrhi1,ndum,conrinc1,conrlo2)
   elseif(conrinc(iolayer).lt.0.) then
      conrinc1=(conrhi1-conrlo1)/abs(conrinc(iolayer))
   else
      conrinc1=conrinc(iolayer)
   endif
   !print*,'c',ccent(iolayer),conrinc1,conrlo1,conrhi1
   
   if(ccent(iolayer).eq.conrlo1) then
      conrlo1=ccent(iolayer)
   elseif(ccent(iolayer).gt.conrlo1) then
      do i=1,20000
         !print*,'a1',i,ccent(iolayer)-conrinc1*i,conrlo1
         if(ccent(iolayer)-conrinc1*i.le.conrlo1) goto 50
      enddo
      50 continue
      conrlo1=ccent(iolayer)-conrinc1*i
   else
      do i=1,20000
         !print*,'a2',i,ccent(iolayer)+conrinc1*i,conrlo1
         if(ccent(iolayer)+conrinc1*i.gt.conrlo1) goto 51
      enddo
      51 continue
      conrlo1=ccent(iolayer)+conrinc1*(i-1)
   endif
   if(ccent(iolayer).eq.conrhi1) then
      conrhi1=ccent(iolayer)
   elseif(ccent(iolayer).lt.conrhi1) then
      do i=1,20000
         !print*,'b1',i,ccent(iolayer)+conrinc1*i,conrhi1
         if(ccent(iolayer)+conrinc1*i.ge.conrhi1) goto 52
      enddo
      52 continue
      conrhi1=ccent(iolayer)+conrinc1*i
   else
      do i=1,20000
         !print*,'b2',i,ccent(iolayer)-conrinc1*i,conrhi1
         if(ccent(iolayer)-conrinc1*i.lt.conrhi1) goto 53
      enddo
      53 continue
      conrhi1=ccent(iolayer)-conrinc1*(i-1)
   endif
   !print*,conrlo1,conrinc1,conrhi1

   ! get number of levels
   nincs=nint((conrhi1-conrlo1)/conrinc1)
   if(icmeth(iolayer)==0) then
      nincs=min(6,max(1,nincs))
      conrinc1=(conrhi1-conrlo1)/float(nincs)
      conrhi1=conrlo1+conrinc1*nincs
   endif
   !print*,'m',icmeth(iolayer),nincs,conrinc1,conrlo1,conrhi1

   if(nincs.gt.40) then
      print*,'WARNING: too many vector color levels in ramsvect for '  &
            ,cvar(iolayer)(1:len_trim(cvar(iolayer))),' - max=40: ',nincs
      return
   endif
   
   ! get the mid point
   !print*,conrlo1,conrinc1,ccent(iolayer)
   do imid=0,nincs
      !print*,imid,conrlo1+imid*conrinc1,ccent(iolayer)
      if(conrlo1+imid*conrinc1.ge.ccent(iolayer)) goto 1
   enddo
   fillcols(1,iolayer)=fillcols(2,iolayer)
   1 continue
   !print*,nincs,imid
                    
   ! set the initial color index
   if(icmeth(iolayer)==0) then
      ifllbeg=89
   else
      ! compute fill colors based on number of lines
      if(iolayer==1) ifllbeg=95
      if(iolayer==2) ifllbeg=175
      !print*,ifllbeg+1,ifllbeg+imid,ifllbeg+nincs
      call gks_fillcolors (iolayer,ifllbeg+1,ifllbeg+imid,ifllbeg+nincs)
   endif
 
   ! color indices for tile title text
   if(nint(cgrad(iolayer)).lt.0) then
      icmj=iwcolor                   
   else
      if(abs(icmeth(iolayer)).ge.1.and.abs(icmeth(iolayer)).le.3) then
         call colortab_ind (fillcols(1,iolayer),icmj)
      else
         if(imid.gt.nincs-imid) then
            call colortab_ind (fillcols(2,iolayer),icmj)
         else
            call colortab_ind (fillcols(1,iolayer),icmj)
         endif
      endif
   endif
   !print*,icmj

endif

npsize=max(nii,njj)+1
npsize=npsize**2/(intwindi*intwindj)

allocate (cv(npsize*8),cvt(npsize*8),iinc(npsize))

mv0=0  
mv1=npsize     
mv2=npsize*2
mv3=npsize*3
mv4=npsize*4
mv5=npsize*5
mv6=npsize*6
mv7=npsize*7

mvt0=0
mvt1=npsize  
mvt2=npsize*2
mvt3=npsize*3
mvt4=npsize*4
mvt5=npsize*5
mvt6=npsize*6
mvt7=npsize*7
nvec=0

call set (px1,px2,py1,py2,px1,px2,py1,py2,1)

! compute vertical exageration factor

vexag=(py2-py1)*(xmax-xmin)/((px2-px1)*(ymax-ymin))

! get max and min speeds

tilelo=1.e15
tilehi=-1.e15
do j=1,njj
   do i=1,nii
      call winddf (dir,spd,uplt(i,j),vplt(i,j))
      tilelo=min(tilelo,spd)
      tilehi=max(tilehi,spd)
   enddo
enddo
!print*,'tilelo,tilehi',tilelo,tilehi

! Compute head angle in radians.  Find maximum velocity in field.

angh=headang*3.14159/180.
if(cwinds.eq.'v'.or.cwinds.eq.'t') then
    !velmax=10.
    velmax=tilehi
    if(velomax.gt.0.) velmax=velomax
endif

! ut and vt are velocity components averaged to T points.  vel is velocity
! magnitude at T points.  hsize is the vector head length.  angstem is
! the plot angle of the vector stem.
   
do j=jst,njj,intwindj
   do i=ist,nii,intwindi

      if(vert.eq.'Z') then

         oplt=py1+(py2-py1)*(zs(i)+ytplot(j)*(1.-zs(i)/ztop)-ymin)/(ymax-ymin)
         if(cwinds.eq.'v'.or.cwinds.eq.'t') then
            vt =.5*(vplt(i,j)+vplt(i,j-1))*vexag
            vvt=.5*(vv(i,j)+vv(i,j-1))*vexag
         else
            vt = vplt(i,j)*vexag
         endif

      else

         oplt=py1+(py2-py1)*(ytplot(j)-ymin)/(ymax-ymin)
         if(cwinds.eq.'v'.or.cwinds.eq.'t') then
            vt=.5*(vplt(i,j)+vplt(i,j-1))
            vvt=.5*(vv(i,j)+vv(i,j-1))
         else
            vt=vplt(i,j)
         endif

      endif

      aplt=px1+(px2-px1)*(xtplot(i)-xmin)/(xmax - xmin)
      if(cwinds.eq.'v'.or.cwinds.eq.'t') then
         ut=.5*(uplt(i,j)+uplt(i-1,j))
         uut=.5*(uu(i,j)+uu(i-1,j))
      else
         ut=uplt(i,j)
      endif

      vel=sqrt(ut**2+vt**2)
      
      ! recompute vmax to scale vort vectors on 'log' scale (i.e. a
      ! vector 2x longer than ref. vector has 2x magnitude of ref. vector)
      ! Vorticity--log scale (i.e. a vector 2x longer than ref. vector has
      ! 10x magnitude of ref. vector)

      if(cwinds.eq.'r') then
         velmax=.001
         if(velomax.gt.0.) velmax=velomax
         scalefach=2.**(log10(vel/velmax))
         velmax=vel/scalefach
      endif

      ! Compute size factor for vector stem, and min and max vector head lengths

      adiff=(px2-px1)*(xtplot(3)-xtplot(2))/(xmax-xmin)
      stemfac=stemleng*adiff/velmax
      hsize1=headleng0*adiff
      hsize2=headleng*adiff
      hsize=hsize1+(hsize2-hsize1)*vel/velmax
      angstem=atan2(vt,ut)

      ! Compute the x and y displacements of the vector head line segments

      dxh1=hsize*cos(angstem+angh)
      dyh1=hsize*sin(angstem+angh)
      dxh2=hsize*cos(angstem-angh)
      dyh2=hsize*sin(angstem-angh)

      ! Find plot coordinates of the 4 endpoints of the vector parts

      x1=aplt
      x2=aplt-ut*stemfac
      x3=aplt-dxh1
      x4=aplt-dxh2
      y1=oplt
      y2=oplt-vt*stemfac
      y3=oplt-dyh1
      y4=oplt-dyh2

      ! Define coordinates of turbulence box around head of arrow
      
      xxt1=-2.*sqrt(uut)*stemfac
      xxt2=-2.*sqrt(uut)*stemfac
      xxt3=+2.*sqrt(uut)*stemfac
      xxt4=+2.*sqrt(uut)*stemfac

      yyt1=-2.*sqrt(vvt)*stemfac
      yyt2=+2.*sqrt(vvt)*stemfac
      yyt3=+2.*sqrt(vvt)*stemfac
      yyt4=-2.*sqrt(vvt)*stemfac

      !if(i.eq.10.and.j.eq.10) print*,'aplt,oplt=',aplt,oplt
      
      ! Rotate and translate box so axis aligns with vector arrow
      
      call rotate (angstem,aplt,oplt,xxt1,yyt1,xt1,yt1)
      call rotate (angstem,aplt,oplt,xxt2,yyt2,xt2,yt2)
      call rotate (angstem,aplt,oplt,xxt3,yyt3,xt3,yt3)
      call rotate (angstem,aplt,oplt,xxt4,yyt4,xt4,yt4)

      !  If any part of the vector head is out of bounds, skip drawing the
      !  vector..., or tail
      ! MAY WANT TO BRING BACK FORMER CLIPPING CODE BECAUSE SET CALL ABOVE
      ! IS NOT CLIPPING

      xa=min(x1,x3,x4,x2)
      xz=max(x1,x3,x4,x2)
      ya=min(y1,y3,y4,y2)
      yz=max(y1,y3,y4,y2)

      if(xa.lt.px1.or.xz.gt.px2.or.ya.lt.py1.or.yz.gt.py2) goto 10

      nvec=nvec +1
      if(icolor.eq.1) then
         val=max(conrlo1,min(.999*conrhi1,slab(i,j)))
         iinc(nvec)=int((val-conrlo1)/conrinc1)+1
      else
         iinc(nvec)=1
      endif

      cv(nvec+mv0)=x1
      cv(nvec+mv1)=x2
      cv(nvec+mv2)=x3
      cv(nvec+mv3)=x4
      cv(nvec+mv4)=y1
      cv(nvec+mv5)=y2
      cv(nvec+mv6)=y3
      cv(nvec+mv7)=y4

      cvt(nvec+mvt0)=xt1
      cvt(nvec+mvt1)=xt2
      cvt(nvec+mvt2)=xt3
      cvt(nvec+mvt3)=xt4
      cvt(nvec+mvt4)=yt1
      cvt(nvec+mvt5)=yt2
      cvt(nvec+mvt6)=yt3
      cvt(nvec+mvt7)=yt4

      10 continue

   enddo
enddo

! Draw the vectors

do inc=1,nincs

   if(icolor.eq.1) then
      !colors
      ici=inc+ifllbeg
      call gsplci(ici)
   else
      call gsplci(iwcolor)
   endif

   call sflush
   do ivec=1,nvec
      if(iinc(ivec).eq.inc) then
         call frstpt(cv(ivec+mv0),cv(ivec+mv4))
         call vector(cv(ivec+mv1),cv(ivec+mv5))
         call frstpt(cv(ivec+mv2),cv(ivec+mv6))
         call vector(cv(ivec+mv0),cv(ivec+mv4))
         call vector(cv(ivec+mv3),cv(ivec+mv7))

         if(cwinds.eq.'t') then
            call frstpt(cvt(ivec+mvt0),cvt(ivec+mvt4))
            call vector(cvt(ivec+mvt1),cvt(ivec+mvt5))
            call vector(cvt(ivec+mvt2),cvt(ivec+mvt6))
            call vector(cvt(ivec+mvt3),cvt(ivec+mvt7))
            call vector(cvt(ivec+mvt0),cvt(ivec+mvt4))
         endif

      endif

   enddo
   call plotif(0.,0.,2)
enddo

deallocate (cv,cvt,iinc)

lcdname=len_trim(cdname(iolayer))-1
lcdunits=len_trim(cdunits(iolayer))-1
   
if(ipinfo.eq.1) then

   ! Plot label information for color vector field

   call set (0.,1.,0.,1.,0.,1.,0.,1.,1)
   
   call colortab_ind ('title0',ic)
   call gsplci(ic)
   call plotif(0.,0.,2)
   call gstxci(ic)
   call plotif(0.,0.,2)

   if(icolor.eq.1) then

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
      
      call gsplci(icmj)
      call plotif(0.,0.,2)
      call gstxci(icmj)
      call plotif(0.,0.,2)

      title=cdname(iolayer)(1:lcdname)//' ('//  &
            cdunits(iolayer)(1:lcdunits)//')'
      call plchhq (.03,box5,'vector colors  ',-.7,0.,-1.)
      call plchhq (.21,box5,title(1:(lcdname+lcdunits+3)),-.7,0.,-1.)
      
      sz=-.6
      write (title,'(g10.4)') varmin
      call plchhq (.56,box5,title,sz,0.,-1.)
      write(title,'(g10.4)') varmax
      call plchhq (.685,box5,title,sz,0.,-1.)
      write(title,'(g10.4)') conrinc1
      call plchhq (.795,box5,title,sz,0.,-1.)
      
   endif

   ! Plot a reference vector

   ! Draw vector information and label box

   call gsplci(ic)
   call gstxci(ic)

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
   call sflush

   box5=boxtop-.5*boxhite

   ! Compute the x and y displacements of the vector head line segments

   dxh1=hsize2*cos(angh)
   dyh1=hsize2*sin(angh)
   dxh2=hsize2*cos(-angh)
   dyh2=hsize2*sin(-angh)

   ! Find plot coordinates of the 4 endpoints of the vector parts

   aplt=.15+.5*adiff
   oplt=box5

   x1=aplt
   !x2=aplt-stemleng*adiff
   x2=aplt-adiff
   x3=aplt-dxh1
   x4=aplt-dxh2
   y1=oplt
   y2=oplt
   y3=oplt-dyh1
   y4=oplt-dyh2

   ! Draw the reference vector

   call gsplci(icmj)
   call gstxci(icmj)

   call frstpt(x1,y1)
   call vector(x2,y2)
   call frstpt(x3,y3)
   call vector(x1,y1)
   call vector(x4,y4)
   call sflush

   if (cwinds.eq.'v'.or.cwinds.eq.'t') then
      call plchhq (.03,box5,'vectors  ',-.7,0.,-1.)
      if(vert.eq.'Z') then
         write(title,'(i4,'' m/s horiz '',f4.2,'' m/s vert'')')  &
            nint(velmax/stemleng),velmax/(vexag*stemleng)
      else
         write(title,'(i4,'' m/s horiz'')') nint(velmax/stemleng)
      endif
      call plchhq (.21,box5,title,-.7,0.,-1.)
   else
      call plchhq (.03,box5,'vort vectors',-.7,0.,-1.)
      if(vert.eq.'Z') then
         write(title,'(e8.2,'' rad/s horiz  '',e8.2,'' rad/s vert'')')  &
            velmax/stemleng,velmax/(vexag*stemleng)
      else
         write(title,'(e8.2,'' rad/s horiz'')') velmax/stemleng
      endif
      call plchhq (.21,box5,title,-.7,0.,-1.)
   endif

   sz=-.7
   write (title,'(g10.4)') tilelo
   call plchhq (.56,box5,title,sz,0.,-1.)
   write (title,'(g10.4)') tilehi
   call plchhq (.685,box5,title,sz,0.,-1.)

endif

if(ipanl.ne.1) then

   ! Construct the scale vector and title for multi-panel plots

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
   
   ! put reference vector in box
   
   call colortab_ind ('title0',ic)
   call gsplci(ic)
   call gstxci(ic)

   ! Compute the x and y displacements of the vector head line segments

   dxh1=hsize2*cos(angh)
   dyh1=hsize2*sin(angh)
   dxh2=hsize2*cos(-angh)
   dyh2=hsize2*sin(-angh)

   ! Find plot coordinates of the 4 endpoints of the vector parts
  
   aplt=px2-.025
   if(vert.eq.'Z') then
      oplt=py2-.04
   else
      oplt=py1+.008
   endif
   x1=aplt
   x2=aplt-adiff
   x3=aplt-dxh1
   x4=aplt-dxh2
   y1=oplt
   y2=oplt
   y3=oplt-dyh1
   y4=oplt-dyh2
   
   ! reference vector
   
   call gsplci(icmj)
   call gstxci(icmj)

   call frstpt(x1,y1)
   call vector(x2,y2)
   call frstpt(x3,y3)
   call vector(x1,y1)
   call vector(x4,y4)
   call sflush
   
   ! label scale

   if (cwinds.eq.'v'.or.cwinds.eq.'t') then
      if(vert.eq.'Z') then
         write(title,'(i4,''m/s horiz '')') nint(velmax/stemleng)
         call plchlq (px2-.002,py2-.01,title(1:len_trim(title)),-.5,0.,1.)
         write(title,'(f4.2,''m/s vert'')') velmax/(vexag*stemleng)
         call plchlq (px2-.002,py2-.025,title(1:len_trim(title)),-.5,0.,1.)
      else
         write(title,'(i4,''m/s horiz'')') nint(velmax/stemleng)
         call plchlq (px2-.002,py1+.02,title(1:len_trim(title)),-.5,0.,1.)
      endif
   else
      if(vert.eq.'Z') then
         write(title,'(e8.2,''rad/s horiz  '')') velmax/stemleng
         call plchlq (px2-.002,py2-.01,title(1:len_trim(title)),-.5,0.,1.)
         write(title,'(e8.2,''rad/s vert'')') velmax/(vexag*stemleng)
         call plchlq (px2-.002,py2-.025,title(1:len_trim(title)),-.5,0.,1.)
      else
         write(title,'(e8.2,''rad/s horiz'')') velmax/stemleng
         call plchlq (px2-.002,py1+.02,title(1:len_trim(title)),-.5,0.,1.)
      endif
   endif
   
   if(icolor.eq.1) then

      ! Construct the title for multi-panel plots
   
      call set (0.,1.,0.,1.,0.,1.,0.,1.,1)
 
      call gsplci(icmj)
      call gstxci(icmj)

      if(abs(conrinc1).ge.10000.) then
         write(cconinc,'(e8.1)') conrinc1
      elseif(abs(conrinc1).ge.1000.) then
         write(cconinc,'(f6.0)') conrinc1
      elseif(abs(conrinc1).ge.100.) then
         write(cconinc,'(f5.0)') conrinc1
      elseif(abs(conrinc1).ge.10.) then
         write(cconinc,'(f4.0)') conrinc1
      elseif(abs(conrinc1).ge..5) then
         write(cconinc,'(f4.1)') conrinc1
      elseif(abs(conrinc1).ge..05) then
         write(cconinc,'(f5.2)') conrinc1
      else
         write(cconinc,'(e8.1)') conrinc1
      endif
      
      title=cdname(iolayer)(1:lcdname)//  &
            ' ['//cconinc(1:len_trim(cconinc))//  &
            ' '//cdunits(iolayer)(1:lcdunits)//']'
          
      if(iolayer==1.or.(iolayer==2.and.cvar(1)(1:4)=='none')) then
         if(ipanl==1) then
            call plchlq (px1,py1-.018,title(1:len_trim(title)),-.78,0.,-1.)
         else
            call plchlq (px1,py1-.01,title(1:len_trim(title)),-.55,0.,-1.)
         endif
      else
         if(ipanl==1) then
            call plchlq (px2,py1-.018,title(1:len_trim(title)),-.8,0.,1.)
         else
            call plchlq (px1,py1-.026,title(1:len_trim(title)),-.55,0.,-1.)
         endif
      endif
   
   endif
endif

if(icolor.eq.1.and.colorbar(iolayer).eq.'b'.and.nincs.ne.0) then

   ! Plot color bar
   
   call set(0.,1.,0.,1.,0.,1.,0.,1.,1)

   call gsplci(icmj)
   call gstxci(icmj)

   valuelab=conrlo1

   rif(1)=px2+.01
   if(ipanl.eq.1) then
      rif(2)=rif(1)+.03
      slz=-.63
   else
      rif(2)=rif(1)+.01
      slz=-.55
   endif
   rif(3)=rif(2)
   rif(4)=rif(1)
   rif(5)=rif(1)

   if(ipanl.eq.1) then
      step=min(1.,(py2-py1)/(px2-px1))
   else
      step=min(1.,.4*(py2-py1)/(px2-px1))
   endif
   if(colorbar(1)=='b'.and.cvar(1)(1:4).ne.'none'.and.  &
      colorbar(2)=='b') then
      if(ipanl.eq.1) then
         cbargap=0.05
      else
         cbargap=0.005
      endif
      if(iolayer==1) then
         rjf(1)=py1+(py2-py1)/2+cbargap/2
         rjf(2)=py1+(py2-py1)/2+cbargap/2
         rjf(5)=py1+(py2-py1)/2+cbargap/2
      else
         rjf(1)=py1
         rjf(2)=py1
         rjf(5)=py1
      endif
      yinc=(py2-cbargap-py1)/(float(nincs)*2)
      labstep=max(1,nint(float(nincs)/(10.*step)))
   else
      rjf(1)=py1
      rjf(2)=py1
      rjf(5)=py1
      yinc=(py2-py1)/float(nincs)
      labstep=max(1,nint(float(nincs)/(20.*step)))
   endif

   if(mod(imid,labstep).eq.0.and.ipanl.eq.1) then

      if    (conrinc1 .ge. .01 .and. conrinc1 .lt. .1) then
         write(number,'(f6.3)') valuelab
      elseif(conrinc1 .ge. .1  .and. conrinc1 .lt. 1.) then
         write (number,'(f6.2)') valuelab
      elseif(conrinc1 .ge. 1.  .and. conrinc1 .lt. 100.) then
         write(number,'(f6.1)') valuelab
      elseif(conrinc1 .ge. 100.  .and. conrinc1 .lt. 10000.) then
         write(number,'(f6.0)') valuelab
      else
         write(number,'(e8.2)') valuelab
      endif

      call deblank(number,numbr,nnn)
      if(numbr(len_trim(numbr):len_trim(numbr))=='.') then
         ln=len_trim(numbr)-1
      else
         ln=len_trim(numbr)
      endif

      call plchlq (rif(2)+.01,rjf(1),numbr(1:ln),slz,0.,-1.)

   endif

   if(ipanl.eq.1) then
      call plchlq (rif(2)+.01,rjf(2)-0.025,cdunits(iolayer)(1:lcdunits)  &
                  ,slz,0.,-1.)
   else
      call plchlq (rif(2)+.01,rjf(2)+0.01,cdunits(iolayer)(1:lcdunits)  &
                  ,slz,0.,-1.)
   endif

   do ib=1,nincs

      valuelab=valuelab+conrinc1
      
      rjf(3)=rjf(2)+yinc
      rjf(4)=rjf(3)

      !colors
      ici=ib+ifllbeg

      call sfsgfa (rif,rjf,4,dst,6,ind,8,ici)

      if(mod(imid-ib,labstep).eq.0) then

         if    (conrinc1 .ge. .01 .and. conrinc1 .lt. .1) then
            write(number,'(f6.3)') valuelab
         elseif(conrinc1 .ge. .1  .and. conrinc1 .lt. 1.) then
            write (number,'(f6.2)') valuelab
         elseif(conrinc1 .ge. 1.  .and. conrinc1 .lt. 100.) then
            write(number,'(f6.1)') valuelab
         elseif(conrinc1 .ge. 100.  .and. conrinc1 .lt. 10000.) then
            write(number,'(f6.0)') valuelab
         else
            write(number,'(e8.2)') valuelab
         endif

         call deblank(number,numbr,nnn)
         if(numbr(len_trim(numbr):len_trim(numbr))=='.') then
            ln=len_trim(numbr)-1
         else
            ln=len_trim(numbr)
         endif

         call plchlq (rif(2)+.01,rjf(3),numbr(1:ln),slz,0.,-1.)

      endif

      rjf(1)=rjf(3)
      rjf(2)=rjf(3)
      rjf(5)=rjf(3)

   enddo

endif
   
if(cwinds(1:1).eq.'v') then
   print*,'Drawn vectors'
elseif (cwinds(1:1).eq.'t') then
   print*,'Drawn vectors and turbulence'
elseif (cwinds(1:1).eq.'r') then
   print*,'Drawn vectors and relative vorticity'
endif

return
end

!***************************************************************************

subroutine strmprep (n,n2,n3,i1,i2,j1,j2,k1,k2,uplt,vplt  &
                    ,xt,zt,ztop,topt,hw)

implicit none

integer :: n,n2,n3,i1,i2,j1,j2,k1,k2
real :: ztop
real :: uplt(n,n),vplt(n,n),xt(*),zt(*),topt(n2,n3),hw(*)

integer :: i,j,k,ii1,ii2
real :: slope

do k=k1,k2
   hw(k)=1.- zt(k)/ztop
enddo

if(j1==j2) then
   j=j1
   do i=i1,i2
      ii1=max(1,i-1)
      ii2=min(n2,i+1)
      slope=(topt(ii2,j)-topt(ii1,j))/(xt(ii2)-xt(ii1))

      do k=k1,k2
         vplt(i,k)=vplt(i,k)-uplt(i,k)*slope*hw(k)
      enddo
   enddo
endif

return
end

!***************************************************************************

subroutine rotate (angle,aplt,oplt,x,y,xr,yr)

implicit none

real :: angle,aplt,oplt,x,y,xr,yr

real :: alen,theta,phi

alen=sqrt(x**2+y**2)
if(abs(x).lt.1.e-20) x=1.e-20
if(abs(y).lt.1.e-20) y=1.e-20
theta=atan2(y,x)
phi=theta+angle
xr=alen*cos(phi)+aplt
yr=alen*sin(phi)+oplt

return
end
