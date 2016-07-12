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

subroutine contour (nngd,ivvar,ilayer,ifill,px1,px2,py1,py2,slab  &
                   ,nii,njj,vert,ymin,ymax,boxtop,boxhite)

implicit none

integer :: nngd,ivvar,ilayer,ifill,nii,njj
real :: px1,px2,py1,py2,ymin,ymax,boxtop,boxhite
real, dimension(nii,njj) :: slab
character(len=*) :: vert

include 'frame.h'

integer :: ifllbeg,imax,isolid,idashes,i,j,ncl,lcdname,lcdunits  &
          ,ib,ici,ln,nnn,numboxes,imid,icmj,icmp,icmn,ic,labstep
real :: conlo,conhi,coninc,clabscl,valuelab,cbargap,xnii,xnjj  &
       ,tilelo,tilehi,clv,sz,slsum,slmin,slmax,slmean,smin,smax  &
       ,smean,sconlo,sconhi,sconinc,sclabscl,box5,yinc,step,slz
real :: c1,c2,c3
real :: rif(6),rjf(6),dst(6),ind(8)
character(len=1)  :: cpoint
character(len=80) :: title
character(len=8)  :: number,numbr
character(len=10) :: cconinc
character(len=20) :: color

integer    lrwk, liwk, lmap, nwrk, ngrps, maxncl
parameter  ( lrwk = 5000, liwk = 5000, lmap = 2000000 )
parameter  ( nwrk = 15000, ngrps = 10, maxncl = 20 )
integer    iwrk(liwk), lfin(maxncl+1)
integer    map(lmap), iarea(ngrps), igrp(ngrps)
real       rwrk(lrwk), xwrk(nwrk), ywrk(nwrk)

common /anacon/ conlo,conhi,coninc,clabscl
common /colorfill/ ifllbeg,imax

external cfill

data isolid /65535/, idashes /43690/
save

xnii=float(nii)
xnjj=float(njj)
if(vert.eq.'Z') then
   if (conttyp(ilayer).eq.'t') then
      call set (px1,px2,py1,py2,0.5,xnii+.5,ymin,ymax,1)
   else
      call set (px1,px2,py1,py2,1.0,xnii,ymin,ymax,1)
   endif
   call cpseti('MAP',3)
else
   if (conttyp(ilayer).eq.'t') then
      call set (px1,px2,py1,py2,.5,xnii+.5,.5,xnjj+.5,1)
   else
      call set (px1,px2,py1,py2,1.,xnii,1.,xnjj,1)
   endif
   call cpseti('MAP',0)
endif

! Find max and min in plotted region of field.  Used
! for min and max labels.

tilelo=1.e15
tilehi=-1.e15
do j=njj,1,-1
   do i=1,nii
      tilelo=min(tilelo,slab(i,j))
      tilehi=max(tilehi,slab(i,j))
   enddo
enddo
if(abs(tilelo).lt.1.e-20) tilelo=0.
if(abs(tilehi).lt.1.e-20) tilehi=0.
clomin(ilayer,ivvar,nngd)=min(clomin(ilayer,ivvar,nngd),tilelo)
chimax(ilayer,ivvar,nngd)=max(chimax(ilayer,ivvar,nngd),tilehi)
!print*,tilelo,tilehi,clomin(ilayer,ivvar,nngd),chimax(ilayer,ivvar,nngd)
call gsfais(1)

call cpseti('SET',0)

! set contour line values - do the calls to cprect and cppkcl twice in
! cases where we want the contours to completely cover the data range
! (when conrlo and conrhi are 0.) and when the conrlo and conrhi get
! reset when conrinc is 0
! if all 3 are specified, just use those numbers
!print*,conrlo(ilayer),conrhi(ilayer),conrinc(ilayer)
if(conrinc(ilayer).lt.-1.e-10) then
   call cpseti('CLS',nint(abs(conrinc(ilayer))))
   call cpsetr('CMN',0.)
   call cpsetr('CMX',-1.)
elseif(abs(conrlo(ilayer)).lt.1.e-10.and.  &
       abs(conrhi(ilayer)).lt.1.e-10.and.  &
       conrinc(ilayer).gt.1.e-10) then
   call cpseti('CLS',12)
   call cpsetr('CMN',0.)
   call cpsetr('CMX',-1.)
   call cpsetr('CIS',conrinc(ilayer))
   call cprect(slab,nii,nii,njj,rwrk,lrwk,iwrk,liwk)
   call cppkcl(slab,rwrk,iwrk)
   call cpgeti('NCL',ncl)
   if(ncl==0) then
      conlo=0.
      conhi=0.
      icmj=icol(ilayer) ! contour title
      goto 100
   endif
   call cpseti('PAI',1)
   call cpgetr('CLV',conlo)
   call cpseti('PAI',ncl)
   call cpgetr('CLV',conhi)
   call cpsetr('CMN',conlo-(conhi-conlo)/float(ncl-1))
   call cpsetr('CMX',conhi+(conhi-conlo)/float(ncl-1))
   call cpsetr('CIS',(conhi-conlo)/float(ncl-1))
else
   call cpsetr('CMN',conrlo(ilayer))
   call cpsetr('CMX',conrhi(ilayer))
   call cpsetr('CIS',conrinc(ilayer))
   if(abs(conrinc(ilayer)).lt.1.e-10.and.  &
      abs(conrlo(ilayer)).gt.1.e-10.and.  &
      abs(conrhi(ilayer)).gt.1.e-10) then
      call cprect(slab,nii,nii,njj,rwrk,lrwk,iwrk,liwk)
      call cppkcl(slab,rwrk,iwrk)
      call cpgeti('NCL',ncl)
      if(ncl==0) then
         conlo=0.
         conhi=0.
         icmj=icol(ilayer) ! contour title
         goto 100
      endif
      call cpseti('PAI',1)
      call cpgetr('CLV',conlo)
      call cpseti('PAI',ncl)
      call cpgetr('CLV',conhi)
      call cpsetr('CMN',conrlo(ilayer))
      call cpsetr('CMX',conrhi(ilayer))
      call cpsetr('CIS',(conhi-conlo)/float(ncl-1))
   endif
   if(abs(conrlo(ilayer)).lt.1.e-10.and.  &
      abs(conrhi(ilayer)).lt.1.e-10) then
      call cprect(slab,nii,nii,njj,rwrk,lrwk,iwrk,liwk)
      call cppkcl(slab,rwrk,iwrk)
      call cpgeti('NCL',ncl)
      if(ncl==0) then
         conlo=0.
         conhi=0.
         icmj=icol(ilayer) ! contour title
         goto 100
      endif
      call cpseti('PAI',1)
      call cpgetr('CLV',conlo)
      call cpseti('PAI',ncl)
      call cpgetr('CLV',conhi)
      call cpsetr('CMN',conlo-(conhi-conlo)/float(ncl-1))
      call cpsetr('CMX',conhi+(conhi-conlo)/float(ncl-1))
      call cpsetr('CIS',(conhi-conlo)/float(ncl-1))
   endif
endif

call cprect(slab,nii,nii,njj,rwrk,lrwk,iwrk,liwk)
call cppkcl(slab,rwrk,iwrk)
call cpgeti('NCL',ncl)
if(ncl==0) then
   conlo=0.
   conhi=0.
   icmj=icol(ilayer) ! contour title
   goto 100
endif

!print*,ilayer,cgrad(ilayer),icmeth(ilayer),icol(ilayer) &
!      ,' ' ,colorbar(ilayer),icover(ilayer),ifill

! get number of contour lines
call cpgeti('NCL',ncl)
call cpseti('PAI',1)
call cpgetr('CLV',conlo)
call cpseti('PAI',ncl)
call cpgetr('CLV',conhi)
!print*,ncl,ncl-1,conlo,conhi,(conhi-conlo)/(ncl-1)

if(ncl.gt.40) then
   print*,'WARNING: too many contour levels contour in for '  &
         ,cvar(ilayer)(1:len_trim(cvar(ilayer))),' - max=40: ',ncl
   return
endif

! get the mid point
do imid=1,ncl
   call cpseti('PAI',imid)
   call cpgetr('CLV',clv)
   !print*,imid,clv,ccent(ilayer)
   if(clv.ge.ccent(ilayer)) goto 1
enddo
1 continue
imid=imid-1
imax=ncl
!print*,'mid',ncl,imid,imax

! set the initial color index
if(icmeth(ilayer)==0) then
   ifllbeg=58
else
   ! compute fill colors based on number of lines
   if(ilayer==1) ifllbeg=95
   if(ilayer==2) ifllbeg=175
   if(conttyp(ilayer).eq.'c') then
      !print*,ifllbeg+1,ifllbeg+imid,ifllbeg+ncl
      call gks_fillcolors (ilayer,ifllbeg+1,ifllbeg+imid,ifllbeg+ncl)
   else
      !print*,ifllbeg+1,ifllbeg+imid,ifllbeg+ncl-1
      call gks_fillcolors (ilayer,ifllbeg+1,ifllbeg+imid,ifllbeg+ncl-1)
   endif
endif

! color indices for positive and negative contours and text
if(cgrad(ilayer).lt.1.e-10) then
   icmp=icol(ilayer)                           ! positive contours
   icmn=icmp                                   ! negative contours
   icmj=icmp                                   ! contour title text  
else
   if(abs(icmeth(ilayer)).ge.1.and.abs(icmeth(ilayer)).le.3) then
      call colortab_ind (fillcols(1,ilayer),icmp) ! positive contours
      icmn=icmp                                   ! negative contours
      icmj=icmp                                   ! contour title text  
   else
      call colortab_ind (fillcols(1,ilayer),icmp) ! positive contours
      call colortab_ind (fillcols(2,ilayer),icmn) ! negative contours
      if(imid.gt.imax-imid) then
         icmj=icmn                                ! contour title text
      else
         icmj=icmp
      endif
   endif
endif
!if(icmeth(ilayer).eq.0.and.conttyp(ilayer).ne.'c') then
!   call colortab_ind ('title0',icmp)           ! positive contours
!   icmn=icmp                                   ! negative contours
!   icmj=icmp                                   ! contour title text  
!endifolors
!print*,icmn,icmj,icmp

! make sure we can see the label colors
call colortab_col (color,icmj)
!print*,color,icmj
call colortab_hls ('hls',color,c1,c2,c3)
!print*,'icmj',icmj,c1,c2,c3
if(c2 < 50.) then
   icmj=icmn
   call colortab_col (color,icmj)
   !print*,color,icmj
   call colortab_hls ('hls',color,c1,c2,c3)
   !print*,'icmj',icmj,c1,c2,c3
   if(c2 < 50.) then
      icmj=icmp
   endif
endif
  
! set up labeling parameters

! contour line labels
call cpseti('LLP',3)                 ! penalty scheme
call cpseti('LBC',0)                 ! boxes filled - foreground color
call cpsetr('LLW',.005)              ! white space
call cpsetr('LLS',csize(ilayer))     ! size

! high/low labels
if(ichigh(ilayer).ge.0) then
   call cpsetc('HLT','H:B:$ZDV$:E''L:B:$ZDV$:E' )  ! box code
   call cpseti('HLB',ichigh(ilayer))               ! label, box, fill
   call cpseti('HLC',icol(ilayer))                 ! color index
   call cpsetr('HLS',csize(ilayer)+.002)           ! size
else
   call cpsetc('HLT',' ' )           ! none
endif

! info label
call cpsetc('ILT',' ')               ! none

if(conttyp(ilayer).eq.'c') then

   ! Do line contours

   ! modify individual lines if desired
   if(ncl.gt.0) then
      do i=1,ncl
         call cpseti('PAI',i)
         
         if(i.ge.imid+1) then
         
            if(cgrad(ilayer).gt.1.e-10.and.icmeth(ilayer).ne.0) then
               call cpseti('CLC',ifllbeg+i)    ! graduated line color
            else
               call cpseti('CLC',icmp)     ! fixed line color
            endif
            
            if(icdash(ilayer)==2) then
               call cpseti('CLD',idashes)  ! dashed line
            else
               call cpseti('CLD',isolid)   ! solid line
            endif
            
         else
         
            if(cgrad(ilayer).gt.1.e-10.and.icmeth(ilayer).ne.0) then
               call cpseti('CLC',ifllbeg+i)    ! graduated line color
            else
               call cpseti('CLC',icmn)     ! fixed line color
            endif
            
            if(icdash(ilayer).ge.1) then
               call cpseti('CLD',idashes)  ! dashed line
            else
               call cpseti('CLD',isolid)   ! solid line
            endif
            
         endif

         if(icline(ilayer).ge.0)  &
            call cpseti('CLU',1)           ! line only
            
         call cpsetr('CLL',cthick(ilayer)) ! width
         
         if(mod(imid-i+1,icint(ilayer)).eq.0) then  ! major lines
      
            if(icline(ilayer).ge.0) then
               call cpseti('CLU',3)               ! line and label
               call cpseti('LLB',icline(ilayer))  ! label, box, fill
            else
               call cpseti('CLU',1)               ! no line
            endif
               
            call cpsetr('CLL',1.5*cthick(ilayer)) ! width
            
            if(cgrad(ilayer).gt.1.e-10.and.icmeth(ilayer).ne.0) then
            
               call cpseti('CLC',ifllbeg+i)    ! graduated line color
               
            else
            
               if(i.ge.imid+1) then
                  call cpseti('LLC',icmp)  ! label color = line color
               else
                  call cpseti('LLC',icmn)
               endif
               
            endif
            
         endif
         
      enddo
   endif

else

   ! Do filled contours

   ! initialize area map
   call arinam(map,lmap)

   ! put contour lines in area map
   call cpclam(slab,rwrk,iwrk,map)

   ! fill contours - do only this if ifill=1
   if(ifill.ne.2) call arscam(map,xwrk,ywrk,nwrk,iarea,igrp,ngrps,cfill)
   if(ifill.eq.1) return

   ! set parameters for any contour lines and labels to be drawn
   do i=1,ncl
      call cpseti('PAI',i)
      
      if(i.ge.imid+1) then
         
         if(cgrad(ilayer).gt.1.e-10) then
            call cpseti('CLC',ifllbeg+40+i)    ! graduated line color
         else
            call cpseti('CLC',icmp)     ! fixed line color
         endif
      
         if(icdash(ilayer)==2) then
            call cpseti('CLD',idashes)  ! dashed line
         else
            call cpseti('CLD',isolid)   ! solid line
         endif
         
      else
         
         if(cgrad(ilayer).gt.1.e-10) then
            call cpseti('CLC',ifllbeg+40+i)  ! graduated line color
         else
            call cpseti('CLC',icmn)     ! fixed line color
         endif
      
         if(icdash(ilayer).ge.1) then
            call cpseti('CLD',idashes)  ! dashed line
         else
            call cpseti('CLD',isolid)   ! solid line
         endif
         
      endif
      
      if(icline(ilayer).ge.0) then
         call cpseti('CLU',1)           ! line
      else
         call cpseti('CLU',0)           ! no line
      endif
      
      call cpsetr('CLL',cthick(ilayer)) ! width
      
      if(mod(imid-i+1,icint(ilayer)).eq.0) then  ! major lines

         if(icline(ilayer).ge.0) then
            call cpseti('CLU',3)               ! line and label
            call cpseti('LLB',icline(ilayer))  ! label, box, fill
         else
            call cpseti('CLU',1)               ! no line
         endif
            
         call cpsetr('CLL',1.5*cthick(ilayer)) ! width
         
         call cpseti('LLC',icol(ilayer))       ! label color
            
         if(cgrad(ilayer).gt.1.e-10) then
        
            call cpseti('CLC',ifllbeg+40+i)    ! graduated line color
   
         else
         
            if(i.ge.imid+1) then
               call cpseti('LLC',icmp)         ! label color = line color
            else
               call cpseti('LLC',icmn)
            endif
            
         endif
         
      endif
      
   enddo

endif

! draw any contour lines
call cpcldr(slab,rwrk,iwrk)

! draw the labels
call cplbdr(slab,rwrk,iwrk)

100 continue
if(conttyp(ilayer).ne.'c'.and.ncl==0.and.ifill==1) return
      
! retrieve contouring parameters
if(ncl.gt.0) then
   call cpseti('PAI',1)
   call cpgetr('CLV',conlo)
   call cpseti('PAI',ncl)
   call cpgetr('CLV',conhi)
   coninc=(conhi-conlo)/max((ncl-1),1)
   call cpgetr('SFU',clabscl)
else
   conlo=0.
   conhi=0.
   coninc=0.
   clabscl=0.
endif

lcdname=len_trim(cdname(ilayer))-1
lcdunits=len_trim(cdunits(ilayer))-1

if(ipinfo.eq.1) then

   ! Draw boxes for the contour field labels

   call set(0.,1.,0.,1.,0.,1.,0.,1.,1)

   call colortab_ind ('title0',ic)
   call gsplci(ic)
   call gstxci(ic)

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
   call sflush

   box5=boxtop-.5*boxhite
   
   ! Make nice labels under the plot with contouring information

   call gsplci(icmj)
   call gstxci(icmj)

   if(lcdunits.eq.0) title=cdname(ilayer)(1:lcdname)
   if(lcdunits.ne.0)  &
      title=cdname(ilayer)(1:lcdname)//' ('//cdunits(ilayer)(1:lcdunits)//')'
   call plchhq (.03,box5,'contours  ',-.7,0.,-1.)
   call plchhq (.21,box5,title(1:(lcdname+lcdunits+3)),-.7,0.,-1.)

   sz=-.6
   write (title,'(g10.4)') tilelo
   call plchhq (.56,box5,title,sz,0.,-1.)
   write (title,'(g10.4)') tilehi
   call plchhq (.685,box5,title,sz,0.,-1.)
   write (title,'(g10.4)') coninc
   call plchhq (.795,box5,title,sz,0.,-1.)
   write (title,'(''1e'',i2)') nint(log10(clabscl))
   call plchhq (.920,box5,title,sz,0.,-1.)
   
endif

if(ipanl.ne.1.or.ipinfo==2) then

   ! Construct the title for multi-panel plots

   call set(0.,1.,0.,1.,0.,1.,0.,1.,1)

   call gsplci(icmj)
   call gstxci(icmj)

   if(abs(coninc).ge.10000.) then
      write(cconinc,'(e8.1)') coninc
   elseif(abs(coninc).ge.1000.) then
      write(cconinc,'(f6.0)') coninc
   elseif(abs(coninc).ge.100.) then
      write(cconinc,'(f5.0)') coninc
   elseif(abs(coninc).ge.10.) then
      write(cconinc,'(f4.0)') coninc
   elseif(abs(coninc).ge..5) then
      write(cconinc,'(f4.1)') coninc
   elseif(abs(coninc).ge..05) then
      write(cconinc,'(f5.2)') coninc
   else
      write(cconinc,'(e8.1)') coninc
   endif
   
   title=cdname(ilayer)(1:lcdname)//  &
         ' ['//cconinc(1:len_trim(cconinc))//  &
         ' '//cdunits(ilayer)(1:lcdunits)//']'
   
   if(ilayer==1.or.(ilayer==2.and.cvar(1)(1:4)=='none')) then
      if(ipanl==1) then
         call plchlq (px1,py1-.018,title(1:len_trim(title)),-.8,0.,-1.)
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

! Draw a color bar for the plot (max of two colorbars)

if(ilayer.le.2.and.colorbar(ilayer).eq.'b'.and.ncl.ne.0) then

   call set(0.,1.,0.,1.,0.,1.,0.,1.,1)

   call gsplci(icmj)
   call gstxci(icmj)
   
   if(coninc.gt.0.) then
      if(conttyp(ilayer).eq.'c') then
         numboxes=ncl
      else
         numboxes=ncl-1
      endif
      valuelab=conlo
   else
      numboxes=0
   endif
   
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
   if(colorbar(1)=='b'.and.cvar(1)(1:4).ne.'none'.and.colorbar(2)=='b') then
      if(ipanl.eq.1) then
         cbargap=0.05
      else
         cbargap=0.005
      endif
      cbargap=0.05
      if(ilayer==1) then
         rjf(1)=py1+(py2-py1)/2+cbargap/2
         rjf(2)=py1+(py2-py1)/2+cbargap/2
         rjf(5)=py1+(py2-py1)/2+cbargap/2
      else
         rjf(1)=py1
         rjf(2)=py1
         rjf(5)=py1
      endif
      yinc=(py2-cbargap-py1)/(float(numboxes)*2)
      labstep=max(1,nint(float(numboxes)/(10.*step)))
   else
      rjf(1)=py1
      rjf(2)=py1
      rjf(5)=py1
      yinc=(py2-py1)/float(numboxes)
      labstep=max(1,nint(float(numboxes)/(20.*step)))
   endif
   
   if(mod(imid,labstep).eq.0.and.conttyp(ilayer).ne.'c'.and.ipanl.eq.1) then
 
      if     (coninc .ge. .01 .and. coninc .lt. .1) then
         write(number,'(f6.3)') valuelab
      elseif(coninc .ge. .1  .and. coninc .lt. 1.) then
         write(number,'(f6.2)') valuelab
      elseif(coninc .ge. 1.  .and. coninc .lt. 100.) then
         write(number,'(f6.1)') valuelab
      elseif(coninc .ge. 100..and. coninc .lt. 10000.) then
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
      
      valuelab=valuelab+coninc
      
   endif

   if(ipanl.eq.1) then
      call plchlq (rif(2)+.01,rjf(2)-0.025,cdunits(ilayer)(1:lcdunits)  &
                  ,slz,0.,-1.)
   else
      call plchlq (rif(2)+.01,rjf(2)+0.01,cdunits(ilayer)(1:lcdunits)  &
                  ,slz,0.,-1.)
   endif
   
   do ib=1,numboxes

      rjf(3)=rjf(2)+yinc
      rjf(4)=rjf(3)
      
      !colors
      ici=ib+ifllbeg

      call sfsgfa (rif,rjf,4,dst,6,ind,8,ici)

      if(mod(imid-ib,labstep).eq.0) then
      
         if    (coninc .ge. .01 .and. coninc .lt. .1) then
            write(number,'(f6.3)') valuelab
         elseif(coninc .ge. .1  .and. coninc .lt. 1.) then
            write(number,'(f6.2)') valuelab
         elseif(coninc .ge. 1.  .and. coninc .lt. 100.) then
            write(number,'(f6.1)') valuelab
         elseif(coninc .ge. 100.  .and. coninc .lt. 10000.) then
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
      
         if(conttyp(ilayer).eq.'c') then
            call plchlq (rif(2)+.01,rjf(3)-.5*yinc,numbr(1:ln),slz,0.,-1.)
         else
            call plchlq (rif(2)+.01,rjf(3),numbr(1:ln),slz,0.,-1.)
         endif
      
      endif

      rjf(1)=rjf(3)
      rjf(2)=rjf(3)
      rjf(5)=rjf(3)
      
      valuelab=valuelab+coninc

   enddo
   
endif

print*,'Contoured ',cdname(ilayer)(1:lcdname),' ('  &
                   ,cdunits(ilayer)(1:lcdunits),')'

! compute some stats

slsum=0.
slmin=slab(1,1)
slmax=slab(1,1)
do j=1,njj
   do i=1,nii
      slsum=slsum+slab(i,j)
      slmin=min(slmin,slab(i,j))
      slmax=max(slmax,slab(i,j))
   enddo
enddo
slmean=slsum/(nii*njj)

return

!-----------------------------------------------------

entry slab_stats(smin,smax,smean)

! entry to return computed stats

smin=slmin
smax=slmax
smean=slmean

return

!-----------------------------------------------------

entry slab_conts(sconlo,sconhi,sconinc,sclabscl)

! entry to return default contouring levels

sconlo=conlo
sconhi=conhi
sconinc=coninc
sclabscl=clabscl

return
end

!***************************************************************************

subroutine cfill (xcra,ycra,ncra,iaia,igia,naia)

dimension xcra(*),ycra(*),iaia(*),igia(*)
common /colorfill/ ifllbeg,imax

! The arrays XCRA and YCRA, for indices 1 to NCRA, contain the X and Y
! coordinates of points defining a polygon.  The area identifiers in
! the array IAIA, each with an associated group identifier in the array
! IGIA, tell us whether the polygon is to be color-filled or not.

! Assume the polygon will be filled until we find otherwise.

ifll=1

! If any of the area identifiers is negative, don't fill the polygon.

do i=1,naia
   if(iaia(i).lt.0) ifll=0
enddo
if(ifll==0) return

! Otherwise, fill the polygon in the color implied by its area
! identifier relative to edge group 3 (the contour-line group).

ifll=0

do i=1,naia
   if(igia(i)==3) ifll=iaia(i)
   if(ifll==1.or.ifll==imax+1) ifll=0  ! don't fill above and below hi-lo range
enddo
if(ifll.gt.0) then
   !print*,'z',imax,ifll,ifllbeg,ifll-1+ifllbeg
   call gsfaci (ifll+ifllbeg-1)
   call gfa (ncra-1,xcra,ycra)
endif

return
end

!***************************************************************************

subroutine cpmpxy (imap,xinp,yinp,xotp,yotp)

! This version of CPMPXY implements four different mappings:
!
!   IMAP = 1 implies an EZMAP mapping.  XINP and YINP are assumed to be
!   the longitude and latitude, in degrees, of a point on the globe.
!
!   IMAP = 2 implies a polar coordinate mapping.  XINP and YINP are
!   assumed to be values of rho and theta (in degrees).
!
!   IMAP = 3:  If IVTRAN = 1 and IZSTRAN = 0 then the input Y is indexed
!     into the array of heights which is Z.  This is good for
!     irregularly spaced grid points in Z.
!     If IVTRAN = 1 and IZSTRAN = 1 then the vertical direction
!     is not only converted to Z, but also scaled to fit in only
!     the area above the topography, which is found in the array
!     ZS.
!
!   IMAP = 4 is the old EZMAP transform. It may not be necessary anymore.

common/trans/itrans,m,n,xlonl,xlonr,ylatb,ylatt,yans
common/trans2/ivtran,z(1000),izstran,zs(1000),zmodtop

! do the transformation.

if(imap.eq.1) then
   call maptrn (yinp,xinp,xotp,yotp)
elseif(imap.eq.2) then
   xotp=xinp*cos(.017453292519943*yinp)
   yotp=xinp*sin(.017453292519943*yinp)
elseif(imap.eq.3) then
   k=int(yinp)
   fctz=yinp-k
   yotp=z(k)+(z(k+1)-z(k))*fctz
   if(izstran.eq.1)then
      i=int(xinp)
      fctx=xinp-i
      zsx=zs(i)+(zs(i+1)-zs(i))*fctx
      rtg=1.-zsx/zmodtop
      yotp=yotp*rtg+zsx
   endif
   xotp=xinp
elseif(imap.eq.4) then
   xlon=xlonl+(xlonr-xlonl)*(xinp-1.)/(m-1.)
   ylat=ylatb+(ylatt-ylatb)*(yinp-1.)/(n-1.)
   call maptrn (ylat,xlon,xotp,yotp)
else
   xotp=xinp
   yotp=yinp
endif

return
end
