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

subroutine landmks (px1,px2,py1,py2,xx1,xx2,yy1,yy2  &
                   ,xmin,xmax,ymin,ymax,ngd,ngrids,corner)

! Draws landmarks at the location indicated in the LANDMARKS file
! (which must be in the current runtime directory)# Limited to at most a 16 character identifier
! LANDMARKS file format (name can be up 16 characters):
!   name lat(deg min sec) lon(deg min sec) 
!   POU       41 37 32        -73 52 55
! OR
!   name    lat       lon
!   POU    41.62556  -73.88194
! Not all landmarks get plotted (if they are too close)
! The first in the list have plotting priority

implicit none

integer :: ngd,ngrids
real :: px1,px2,py1,py2,xx1,xx2,yy1,yy2,xmin,xmax,ymin,ymax
real :: corner(5,2,ngrids)

include 'frame.h'

integer :: l,nums,ic,nt,ntok,lc,idraw,m,lcm  &
          ,nval,nnxp,nnyp,nnzp,nfgpnts(4),n,ng,ing
real :: xwl,xel,ysl,ynl,xwm,xem,ysm,ynm  &
       ,plat,plon,xsw,xne,delx,ysw,yne,dely,sx,sy,textoff
integer, parameter         :: numsta=1000
character(len=16)          :: name(numsta)
character(len=256)         :: line
character(len=32)          :: tokens(20)
character(len=1)           :: cng
integer, dimension(numsta) :: iplotlm
real, dimension(numsta)    :: xsta,ysta,alatd,alatm,alats,alond,alonm,alons
real, allocatable          :: locsh(:),locsv(:)

call RAMS_get_fdata (6,1,1,plat,nval) ! won't work for global runs
call RAMS_get_fdata (7,1,1,plon,nval) ! won't work for global runs
call RAMS_get_idata (0,1,ngd,nfgpnts,nval)
nnxp=nfgpnts(1) ; nnyp=nfgpnts(2) ; nnzp=nfgpnts(3)
call RAMS_get_fdata (4,1,ngd,delx,nval)
call RAMS_get_fdata (5,1,ngd,dely,nval)
call RAMS_get_fdata (8,1,ngd,xsw,nval)
call RAMS_get_fdata (9,1,ngd,ysw,nval)
xne=xsw+(nnxp-1)*delx
yne=ysw+(nnyp-1)*dely
!print*,plon,nnxp,xsw,xne,delx
!print*,plat,nnyp,ysw,yne,dely

nums=0
open(unit=50,file='LANDMARKS',status='old',err=100)
do l=1,numsta
   read(50,'(a)',end=100,err=100)line
   if(line(1:10).eq.'          ') goto 100
   if(line(1:1).eq.'#') goto 95
   nums=nums+1
   call parse(line,tokens,ntok)
   read(tokens(1),'(a)') name(nums)
   if(ntok==7) then
      read(tokens(2),*) alatd(nums)
      read(tokens(3),*) alatm(nums)
      read(tokens(4),*) alats(nums)
      if(alatd(nums).lt.0) then
         alatd(nums)=alatd(nums)-alatm(nums)/60.-alats(nums)/3600.
      else
         alatd(nums)=alatd(nums)+alatm(nums)/60.+alats(nums)/3600.
      endif
      read(tokens(5),*) alond(nums)
      read(tokens(6),*) alonm(nums)
      read(tokens(7),*) alons(nums)
      if(alond(nums).lt.0) then
         alond(nums)=alond(nums)-alonm(nums)/60.-alons(nums)/3600.
      else
         alond(nums)=alond(nums)+alonm(nums)/60.+alons(nums)/3600.
      endif
   elseif(ntok==3) then
      read(tokens(2),*) alatd(nums)
      read(tokens(3),*) alond(nums)
   elseif(ntok==1 .and. name(nums)(1:4)=='grid') then
      print*,'drawing grid: ',name(nums)(5:5)
   else
      print*
      print*,'Warning: Incorrect LANDMARK file format for '  &
            ,line(1:len_trim(line))
      print*,'Ignoring landmark...'
      !print*,'Ignoring rest of the landmarks...'
      print*
      nums=nums-1
      !goto 100
   endif
   95 continue
enddo
100 continue
close(50)
   
call gsmksc(sizelandmk)     ! marker size
!call gslwsc(sizelandmk*1.5)  ! line and label thickness
call gsmk(itypelandmk)      ! marker type
call colortab_ind (collandmk,ic)
call gspmci(ic)  ! marker colour
call colortab_ind (collandlab,ic)
call gsplci(ic)  ! label and line colour
call gstxci(ic)  ! label colour

do l=1,nums
   
   if(ilandmk==1.and.name(l)(4:7).ne.'line'.and.name(l)(1:4).ne.'grid') then
      ! plot and label landmark
      
      call set(px1,px2,py1,py2,xx1,xx2,yy1,yy2,1)
      
      iplotlm(l)=0
      lc=len_trim(name(l))

      call maptra(alatd(l),alond(l),xsta(l),ysta(l))
   
      if(xsta(l).ge.xx1.and.xsta(l).le.xx2.and.  &
         ysta(l).ge.yy1.and.ysta(l).le.yy2) then

         xwl=xsta(l)-bufflandmk*(xx2-xx1)
         xel=xsta(l)+bufflandmk*(xx2-xx1)*(lc+1)
         ysl=ysta(l)-bufflandmk*(yy2-yy1)
         ynl=ysta(l)+bufflandmk*(yy2-yy1)

         idraw=1
         do m=1,l-1
            lcm=len_trim(name(m))
            call maptra(alatd(m),alond(m),xsta(m),ysta(m))
            xwm=xsta(m)-bufflandmk*(xx2-xx1)
            xem=xsta(m)+bufflandmk*(xx2-xx1)*(lcm+1)
            ysm=ysta(m)-bufflandmk*(yy2-yy1)
            ynm=ysta(m)+bufflandmk*(yy2-yy1)
            if(((xwm.le.xel.and.xel.le.xem).or.  &
                (xwm.le.xwl.and.xwl.le.xem)).and.  &
                ((ysm.le.ynl.and.ynl.le.ynm).or.  &
                (ysm.le.ysl.and.ysl.le.ynm)).and.  &
                 iplotlm(m).eq.1) idraw=0
         enddo

         if(idraw.eq.1) then
            iplotlm(l)=1
            if(ilandmk==1) then
               call gpm (1,xsta(l),ysta(l))
               if(ilandlab==1)  &
                  call plchlq (xsta(l),ysta(l),' '//name(l)(1:lc)  &
                              ,sizelandlab,0.,-1.)
            else
               if(ilandlab==1)  &
                  call plchlq (xsta(l),ysta(l),' '//name(l)(1:lc)  &
                              ,sizelandlab,0.,0.)
            endif
         endif
      endif
   
   endif
   
   if(ilandmk==1.and.name(l)(4:7)=='line') then
      ! draw lines between landmarks
      call gslwsc(sizelandmk*1.5)  ! line and label thickness
      call ll_xy (alatd(l),alond(l),plat,plon,sx,sy)
      !print*,alatd(l),alond(l),sx,sy
      sx=min(max(sx,xsw),xne)
      sy=min(max(sy,ysw),yne)
      !print*,sx,sy
      if(name(l)(1:7)=='begline') then
         if(allocated(locsh)) deallocate(locsh) ; allocate(locsh(nums))
         if(allocated(locsv)) deallocate(locsv) ; allocate(locsv(nums))
         locsh(1:nums)=0 ; locsv(1:nums)=0
         n=1 ; locsh(n)=sx ; locsv(n)=sy
      elseif(name(l)(1:7)=='drwline') then
         n=n+1 ; locsh(n)=sx ; locsv(n)=sy
      elseif(name(l)(1:7)=='endline') then
         call set (px1,px2,py1,py2,xmin,xmax,ymin,ymax,1)
         n=n+1 ; locsh(n)=sx ; locsv(n)=sy
         call gpl (n,locsh,locsv)
      endif
      call gslwsc(1.)  ! line and label thickness
   endif
   
   if(ilandmk==1.and.name(l)(1:4)=='grid') then
      ! draw grids
      call set (px1,px2,py1,py2,xmin,xmax,ymin,ymax,1)
      textoff=(ymax-ymin)*sizelandlab/(py2-py1)
      read(name(l)(5:5),'(i1)') ing
      do ng=1,ngrids
         if((ing==ng.or.ing==0).and.ng.gt.ngd) then
            !print*,ngd,ing,ng,ngrids
            if(allocated(locsh)) deallocate(locsh) ; allocate(locsh(5))
            if(allocated(locsv)) deallocate(locsv) ; allocate(locsv(5))
            do n=1,5
               locsh(n)=corner(n,1,ng)
               locsv(n)=corner(n,2,ng)
               !print*,n,locsh(n),locsv(n)
            enddo
            call gpl (5,locsh,locsv)
            if(ilandlab==1) then
               ! grid label
               !print*,ng,corner(2,1,ng),corner(2,2,ng),textoff
               write(cng,'(i1)') ng
               call plchlq (corner(2,1,ng),corner(2,2,ng)-textoff   &
                           ,' g'//cng,sizelandlab,0.,-1.)
            endif
         endif
         if((ing==ng.or.ing==0).and.ng==ngd.and.ilandlab==1) then
            ! grid label
            !print*,ng,corner(2,1,ng),corner(2,2,ng),textoff
            write(cng,'(i1)') ng
            call plchlq (corner(2,1,ng),corner(2,2,ng)-textoff  &
                        ,' g'//cng,sizelandlab,0.,-1.)
         endif
      enddo
   endif
enddo
      
return
end
