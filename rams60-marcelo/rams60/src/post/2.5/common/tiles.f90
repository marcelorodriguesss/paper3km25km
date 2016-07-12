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

subroutine tileplot (nngd,ivvar,ilayer,px1,px2,py1,py2,slab,fracarea  &
                    ,njb,nii,njj,npatch,xmplot,xtplot,ymplot  &
                    ,xmin,xmax,ymin,ymax,zs,zuv,zmodtop,vert,ivtype  &
                    ,boxtop,boxhite)

implicit none
                    
integer :: nngd,ivvar,ilayer,njb,nii,njj,npatch,ivtype
real :: px1,px2,py1,py2,xmin,xmax,ymin,ymax,zmodtop,boxtop,boxhite
real, dimension(nii,njj,npatch) :: slab,fracarea
real, dimension(*) :: xmplot,xtplot,ymplot,zs,zuv
character(len=*) :: vert

include 'frame.h'

integer :: isaf(18),ln,ib,nnn,numboxes,lcdunits,lcdname,i,j,ip  &
          ,ici,ipat,ic,ifllbeg,ndum,iasf(18),imid,icmj,labstep,ii
real :: rif(6),rjf(6),ric(7),rjc(7),dst(6),ind(8),yinc,valuelab,box7  &
       ,box5,box3,fracar,fracdist,slb,disty,distx,donedist,conrhi1  &
       ,conrlo1,conrinc1,conrlo2,tilelo,tilehi,sz,cbargap,clabscl  &
       ,step,slz
character(len=1)  :: cpoint
character(len=80) :: title
character(len=8)  :: number,numbr
character(len=10) :: cconinc

data iasf /18*1/

! This routine makes tile plots WITH patches

! Turn off the clipping indicator - turned off as didn't hide topo
!call gsclip (0)

! Set all the GKS aspect source flags to "individual"
call gsasf (iasf)

! Force solid fill
call gsfais (1)

call set (px1,px2,py1,py2,xmin,xmax,ymin,ymax,1)

call sfseti ('TYPE OF FILL',0)

if(ivtype.le.5) then
   ipat=1
else
   ipat=npatch
endif

! Find max and min in plotted region of field.  Used for contour scaling
! if conrlo(ilayer) and conrhi(ilayer) = 0

tilelo=1.e15
tilehi=-1.e15
do j=njj,1,-1
   do i=1,nii
      do ip=1,ipat
         if(fracarea(i,j,ip).gt.1.e-6.or.ipat.eq.1) then
            tilelo=min(tilelo,slab(i,j,ip))
            tilehi=max(tilehi,slab(i,j,ip))
         endif
      enddo
   enddo
enddo
if(cvar(ilayer)(1:10).eq.'qveg_class') then
   tilelo=0.
   tilehi=30.
endif
if(cvar(ilayer)(1:9).eq.'veg_class') then
   tilelo=0.
   tilehi=32.
endif
if(cvar(ilayer)(1:5).eq.'sltex') then
   tilelo=0.
   tilehi=12.
endif
if(abs(tilelo).lt.1.e-20) tilelo=0.
if(abs(tilehi).lt.1.e-20) tilehi=0.
clomin(ilayer,ivvar,nngd)=min(clomin(ilayer,ivvar,nngd),tilelo)
chimax(ilayer,ivvar,nngd)=max(chimax(ilayer,ivvar,nngd),tilehi)
!print*,tilelo,tilehi,clomin(ilayer,ivvar,nngd),chimax(ilayer,ivvar,nngd)

if(abs(conrlo(ilayer)).lt.1.e-10 .and.  &
   abs(conrhi(ilayer)).lt.1.e-10) then
   conrlo1=tilelo
   conrhi1=tilehi
else
   conrlo1=conrlo(ilayer)
   conrhi1=conrhi(ilayer)
endif
!print*,conrlo1,conrhi1

!print*,ilayer,cgrad(ilayer),icmeth(ilayer),icol(ilayer)  &
!      ,' ',colorbar(ilayer),ccent(ilayer)

if(abs(conrhi1-conrlo1).lt.1.e-10) then
   numboxes=0
   icmj=icol(ilayer)
   goto 100
endif

if(abs(conrinc(ilayer)).lt.1.e-10) then
   call niceinc6_mrc(conrlo1,conrhi1,ndum,conrinc1,conrlo2)
elseif(conrinc(ilayer).lt.0.) then
   conrinc1=(conrhi1-conrlo1)/abs(conrinc(ilayer))
else
   conrinc1=conrinc(ilayer)
endif

if(cvar(ilayer)(1:10).ne.'qveg_class'.and.  &
   cvar(ilayer)(1:9).ne.'veg_class'.and.  &
   cvar(ilayer)(1:5).ne.'sltex') then
   !print*,ccent(ilayer),conrinc1,conrlo1,conrhi1
   if(ccent(ilayer).eq.conrlo1) then
      conrlo1=ccent(ilayer)
   elseif(ccent(ilayer).gt.conrlo1) then
      do i=1,20000
         !print*,'a1',i,ccent(ilayer)-conrinc1*i,conrlo1
         if(ccent(ilayer)-conrinc1*i.le.conrlo1) goto 50
      enddo
      50 continue
      conrlo1=ccent(ilayer)-conrinc1*i
   else
      do i=1,20000
         !print*,'a2',i,ccent(ilayer)+conrinc1*i,conrlo1
         if(ccent(ilayer)+conrinc1*i.gt.conrlo1) goto 51
      enddo
      51 continue
      conrlo1=ccent(ilayer)+conrinc1*(i-1)
   endif
   !print*,conrlo1
   if(ccent(ilayer).eq.conrhi1) then
      conrhi1=ccent(ilayer)
   elseif(ccent(ilayer).lt.conrhi1) then
      do i=1,20000
         !print*,'b1',i,ccent(ilayer)+conrinc1*i,conrhi1
         if(ccent(ilayer)+conrinc1*i.ge.conrhi1) goto 52
      enddo
      52 continue
      conrhi1=ccent(ilayer)+conrinc1*i
   else
      do i=1,20000
         !print*,'b2',i,ccent(ilayer)-conrinc1*i,conrhi1
         if(ccent(ilayer)-conrinc1*i.lt.conrhi1) goto 53
      enddo
      53 continue
      conrhi1=ccent(ilayer)-conrinc1*(i-1)
   endif
   !print*,conrhi1
endif
!print*,conrlo1,conrinc1,conrhi1

! get number of levels
numboxes=nint((conrhi1-conrlo1)/conrinc1)

if(numboxes.gt.40) then
   print*,'WARNING: too many tile fill levels in tileplot for '  &
         ,cvar(ilayer)(1:len_trim(cvar(ilayer))),' - max=40: ',numboxes
   return
endif

! get the mid point
!print*,conrlo1,conrinc1,ccent(ilayer)
do imid=0,numboxes
   !print*,imid,conrlo1+imid*conrinc1,ccent(ilayer)
   if(conrlo1+imid*conrinc1.ge.ccent(ilayer)) goto 1
enddo
fillcols(1,ilayer)=fillcols(2,ilayer)
1 continue
!print*,numboxes,imid
                    
! set the initial color index
if(icmeth(ilayer)==0) then
   ifllbeg=58
else
   ! compute fill colors based on number of lines
   if(ilayer==1) ifllbeg=95
   if(ilayer==2) ifllbeg=175
   !print*,ifllbeg+1,ifllbeg+imid,ifllbeg+numboxes
   call gks_fillcolors (ilayer,ifllbeg+1,ifllbeg+imid,ifllbeg+numboxes)
endif

! color indices for tile title text
if(nint(cgrad(ilayer)).lt.0) then
   icmj=icol(ilayer)
else
   if(abs(icmeth(ilayer)).ge.1.and.abs(icmeth(ilayer)).le.3) then
      call colortab_ind (fillcols(1,ilayer),icmj) 
   else
      if(imid.gt.numboxes-imid) then
         call colortab_ind (fillcols(2,ilayer),icmj)
      else
         call colortab_ind (fillcols(1,ilayer),icmj)
      endif
   endif
endif
if(icmeth(ilayer).eq.0.and.conttyp(ilayer).ne.'c') then
   call colortab_ind ('title0',icmj)
endif
!print*,icmj

if(vert.eq.'Z'.and.ivtype.le.3) then

   ! Draw tile plot of field with terrain-following coordinate
   ! transformation (i.e., for vertical atmosphere cross section)
   
   do j=njj,1,-1
      if(njb==1.and.j==1) goto 15
      do i=1,nii

         rif(1)=xmplot(i)
         rif(2)=xtplot(i)
         rif(3)=xmplot(i+1)
         rif(4)=rif(3)
         rif(5)=rif(2)
         rif(6)=rif(1)

         rjf(1)=ymplot(j)  *(1.-zuv(i)/zmodtop)+zuv(i)
         rjf(2)=ymplot(j)  *(1.-zs(i)/zmodtop)+zs(i)
         rjf(3)=ymplot(j)  *(1.-zuv(i+1)/zmodtop)+zuv(i+1)
         rjf(4)=ymplot(j+1)*(1.-zuv(i+1)/zmodtop)+zuv(i+1)
         rjf(5)=ymplot(j+1)*(1.-zs(i)/zmodtop)+zs(i)
         rjf(6)=ymplot(j+1)*(1.-zuv(i)/zmodtop)+zuv(i)

         do ic=1,6
            ric(ic)=rif(ic)
            rjc(ic)=rjf(ic)
         enddo
         ric(7)=ric(1)
         rjc(7)=rjc(1)
            
         ! plot if within range
         if(slab(i,j,1).ge.conrlo1.and.slab(i,j,1).le.conrhi1) then
         
            ! colors
            slb=max(conrlo1,min(conrhi1,slab(i,j,1)))
            ici=int((slb-conrlo1)/conrinc1)+1+ifllbeg
            
            ! plot
            call sfsgfa (rif,rjf,6,dst,6,ind,8,ici)
            
         endif

      enddo
      15 continue
   enddo

else

   ! Draw tile plot of field without terrain-following coordinate
   ! transformation (i.e., for LEAF-2 variables or for horizontal
   ! atmos plot)
   
   do j=njj,1,-1
      do i=1,nii
         donedist=0.
         distx=xmplot(i+1)-xmplot(i)
         disty=ymplot(j+1)-ymplot(j)
         do ip=1,ipat

            if (ivtype .le. 5) then
               fracar = 1.
            else
               fracar = fracarea(i,j,ip)
            endif

            fracdist=fracar*distx
            rif(1)=max(xmin,xmplot(i)+donedist)
            rif(2)=min(xmax,xmplot(i)+donedist+fracdist)
            rjf(1)=max(ymin,ymplot(j))
            rjf(3)=min(ymax,ymplot(j+1))
            donedist=donedist+fracdist

            rif(3)=rif(2)
            rif(4)=rif(1)
            rif(5)=rif(1)
            rjf(2)=rjf(1)
            rjf(4)=rjf(3)
            rjf(5)=rjf(1)
            !write(*,'(2i3,10f10.0)') i,j,(rif(ii),ii=1,5),(rjf(ii),ii=1,5)
            
            ! plot if within range
            if(slab(i,j,ip).ge.conrlo1.and.slab(i,j,ip).le.conrhi1) then
            
               ! colors
               slb=max(conrlo1,min(conrhi1,slab(i,j,ip)))
               ici=int((slb-conrlo1)/conrinc1)+1+ifllbeg
               
               ! plot
               call sfsgfa (rif,rjf,4,dst,6,ind,8,ici)
               
            endif

         enddo

         ! this section to outline only grid cells, not patches
         ! the above commented section to outline patches too

         ric(1)=xmplot(i)
         ric(2)=xmplot(i+1)
         ric(3)=ric(2)
         ric(4)=ric(1)
         ric(5)=ric(1)

         rjc(1)=ymplot(j)
         rjc(2)=rjc(1)
         rjc(3)=ymplot(j+1)
         rjc(4)=rjc(3)
         rjc(5)=rjc(1)

      enddo
   enddo
endif

100 continue

lcdname=len_trim(cdname(ilayer))-1
lcdunits=len_trim(cdunits(ilayer))-1

if(ipinfo.eq.1) then

   ! Draw boxes for the tile field labels
   
   call set (0.,1.,0.,1.,0.,1.,0.,1.,1)

   boxtop = boxtop - boxhite

   call colortab_ind ('title0',ic)
   call gsplci(ic)
   call gstxci(ic)

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
 
   box3 = boxtop - .74 * boxhite
   box5 = boxtop - .5 * boxhite
   box7 = boxtop - .26 * boxhite
   
   ! Make nice labels under the plot with contouring information
 
   call gsplci(icmj)
   call gstxci(icmj)

   title=cdname(ilayer)(1:lcdname)//' ('//cdunits(ilayer)(1:lcdunits)//')'
   call plchhq (.03,box5,'tiles  ',-.7,0.,-1.)
   call plchhq (.21,box5,title(1:(lcdname+lcdunits+3)),-.7,0.,-1.)

   sz=-.6
   write(title,'(g10.4)') tilelo
   call plchhq (.56,box5,title,sz,0.,-1.)
   write(title,'(g10.4)') tilehi
   call plchhq (.685,box5,title,sz,0.,-1.)
   write(title,'(g10.4)') conrinc1
   call plchhq (.795,box5,title,sz,0.,-1.)
   write(title,'(''1e'',i2)') nint(log10(clabscl))
   call plchhq (.920,box5,title,sz,0.,-1.)
   
endif

if(ipanl.ne.1) then

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

if(colorbar(ilayer).eq.'b'.and.numboxes.ne.0) then

   ! Draw a color bar for the plot

   call set (0.,1.,0.,1.,0.,1.,0.,1.,1)

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
   if(colorbar(1)=='b'.and.colorbar(2)=='b') then
      if(ipanl.eq.1) then
         cbargap=0.05
      else
         cbargap=0.005
      endif
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

   if    (conrinc1 .ge. .01 .and. conrinc1 .lt. .1) then
      write(number,'(f6.2)') valuelab
   elseif(conrinc1 .ge. .1  .and. conrinc1 .lt. 1.) then
      write(number,'(f6.1)') valuelab
   elseif(conrinc1 .ge. 1.  .and. conrinc1 .lt. 10000.) then
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
   if(cvar(ilayer)(1:10).eq.'qveg_class'.or.  &
      cvar(ilayer)(1:9).eq.'veg_class'.or.  &
      cvar(ilayer)(1:5).eq.'sltex') then
         call plchlq (rif(2)+.01,rjf(1)+.5*yinc,numbr(1:ln),slz,0.,-1.)
   else
      if(mod(imid,labstep).eq.0)  &
         call plchlq (rif(2)+.01,rjf(1),numbr(1:ln),slz,0.,-1.)
   endif
   
   if(ipanl.eq.1.) then
      call plchlq (rif(2)+.01,rjf(2)-.025,cdunits(ilayer)(1:lcdunits)  &
                  ,slz,0.,-1.)
   else
      call plchlq (rif(2)+.01,rjf(2)+.01,cdunits(ilayer)(1:lcdunits)  &
                  ,slz,0.,-1.)
   endif

   do ib=1,numboxes

      rjf(3)=rjf(2)+yinc
      rjf(4)=rjf(3)
      
      ! colors
      ici=ib+ifllbeg
      
      valuelab=valuelab+conrinc1

      call sfsgfa (rif,rjf,4,dst,6,ind,8,ici)

      if    (conrinc1 .ge. .01 .and. conrinc1 .lt. .1) then
         write(number,'(f6.2)') valuelab
      elseif(conrinc1 .ge. .1  .and. conrinc1 .lt. 1.) then
         write(number,'(f6.1)') valuelab
      elseif(conrinc1 .ge. 1.  .and. conrinc1 .lt. 10000.) then
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

      if(cvar(ilayer)(1:10).eq.'qveg_class'.or.  &
         cvar(ilayer)(1:9).eq.'veg_class'.or.  &
         cvar(ilayer)(1:5).eq.'sltex') then
         if(ib.ne.numboxes.and.mod(ib,labstep).eq.0)  &
            call plchlq (rif(2)+.01,rjf(3)+.5*yinc,numbr(1:ln),slz,0.,-1.)
      else
         if(mod(imid-ib,labstep).eq.0)  &
            call plchlq (rif(2)+.01,rjf(3),numbr(1:ln),slz,0.,-1.)
      endif

      rjf(1)=rjf(3)
      rjf(2)=rjf(3)
      rjf(5)=rjf(3)

   enddo
endif

print*,'Tiled ',cdname(ilayer)(1:lcdname),' ('  &
               ,cdunits(ilayer)(1:lcdunits),')'

return
end
