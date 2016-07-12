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

subroutine windbarb (xscreen,yscreen,nsta,wdir,wspd,scle,hemi)

! This routine will draw a wind barb with flags, staffs, and
! half staffs given the wind direction and speed.

!   ARGUMENTS: (ALL INPUT, NONE OUTPUT)
!   - xscreen,yscreen are the x,y screen coords for wind barb
!   - wdir is the wind direction: 0. being true north, 90. is east.
!   - wspd is the wind speed in whatever units, if meters/second are
!       passed then barb is plotted as the same.
!   --the above arguements are dimensioned by nsta
!   - scle is the scale of the barb to be plotted, based on domain
!       size (map size) of the calling program.  See end of routine for
!       help in determining scle.
!   - hemi is the hemisphere - to get the flags, staffs pointing in the
!       correct direction.  0.0 is Northern Hemisphere and 1.0 is Southern
!       all other values will cause an error message.

! This routine calls the NCAR Graphics routine CURVED

implicit none

integer :: nsta
real :: scle,hemi
real :: xscreen(*),yscreen(*),wdir(*),wspd(*)

include 'frame.h'

integer :: iflags,istafs,ihfstafs,ins,iangle,ifl,istt,ihs
real :: x1,y1,x2,y2,x3,y3,r,flinc,stinc,hsinc,round,hfact,angle,xangle
integer, parameter :: pi=3.1415928,maxpnts=3,nangles=360
real :: x(maxpnts),y(maxpnts),xcirc(nangles),ycirc(nangles)

do ins=1,nsta

   ! Draw calm circle if wdir and wspd are zero
 
   if(wdir(ins).eq.0.0 .and. wspd(ins).eq.0.0) then
      r=.1
      x1=r
      y1=0.
      do iangle=1,nangles
         xangle=float(iangle)*pi/180.
         xcirc(iangle)=(cos(xangle)*x1-sin(xangle)*y1)*scle+xscreen(ins)
         ycirc(iangle)=(sin(xangle)*x1-cos(xangle)*y1)*scle+yscreen(ins)
      enddo
   call curve(xcirc,ycirc,nangles)
   goto 999
   endif

   ! If winds not calm plot at least a barb
   
   r=1.
   angle=amod(360.0-(wdir(ins)-90.0),360.0)*pi/180.0
   !print*, 'angle is : ',angle,wdir(ins)
   if(hemi.lt.0.5) then
      hfact=-1.
   elseif(hemi.ge.0.5) then
      hfact=1.
   else
      print*, 'enter hemi as 0.0 for northern and 1.0 for south'
   endif
 
   ! Increments for what makes a flag, staff and half-staff are wired below.
   ! Shown below are flags for 5, staffs for 2 and half-staffs for 1 unit.
   ! Standard wind barbs would set the following to 50.0, 10.0, and 5.0.
   ! Variable ROUND below will help round to the correct number of flags,
   ! staffs, and half-staffs based on the increments.
 
   if(ibscale.le.1) then
      flinc=50.
      stinc=10.
      hsinc=5.
   elseif(ibscale.eq.2) then
      flinc=20.
      stinc=4.
      hsinc=2.
   elseif(ibscale.eq.3) then
      flinc=10.
      stinc=2.
      hsinc=1.
   elseif(ibscale.ge.4) then
      flinc=5.
      stinc=1.
      hsinc=.5
   endif
 
   round=.5*hsinc
 
   iflags=int((wspd(ins)+round)/flinc)
   istafs=int((wspd(ins)+round-flinc*float(iflags) )/stinc)
   ihfstafs=int(((wspd(ins)+round)-(flinc*float(iflags))-  &
                (stinc*float(istafs)))/hsinc)
   !print*, 'number of flags,staffs,hstaffs',iflags,istafs,ihfstafs
 
   ! Draw the barb (needs two points to describe)
 
   x1=0.
   y1=0.
   x2=r
   y2=0.
   x(1)=(cos(angle)*x1-sin(angle)*y1)*scle+xscreen(ins)
   y(1)=(sin(angle)*x1+cos(angle)*y1)*scle+yscreen(ins)
   x(2)=(cos(angle)*x2-sin(angle)*y2)*scle+xscreen(ins)
   y(2)=(sin(angle)*x2+cos(angle)*y2)*scle+yscreen(ins)
   !print*,'barb points are: ',x(1),y(1),x(2),y(2)
   call curve(x,y,2)
 
   ! Draw each flag individually  (needs three points to describe)
 
   do ifl=1,iflags
      x1=r
      y1=0.
      x2=r-.1
      y2=.5*hfact
      r=r-.2
      x3=r
      y3=0.
      x(1)=(cos(angle)*x1-sin(angle)*y1)*scle +xscreen(ins)
      y(1)=(sin(angle)*x1+cos(angle)*y1)*scle +yscreen(ins)
      x(2)=(cos(angle)*x2-sin(angle)*y2)*scle +xscreen(ins)
      y(2)=(sin(angle)*x2+cos(angle)*y2)*scle +yscreen(ins)
      x(3)=(cos(angle)*x3-sin(angle)*y3)*scle +xscreen(ins)
      y(3)=(sin(angle)*x3+cos(angle)*y3)*scle +yscreen(ins)
      !print*,'flag points are: ',x(1),y(1),x(2),y(2),x(3),y(3)
      call curve(x,y,3)
      r=r-.05
   enddo
 
   ! Draw each staff individually (needs two points to describe)
 
   do istt=1,istafs
      x1=r
      y1=0.
      x2=r
      y2=.5*hfact
      x(1)=(cos(angle)*x1-sin(angle)*y1)*scle+xscreen(ins)
      y(1)=(sin(angle)*x1+cos(angle)*y1)*scle+yscreen(ins)
      x(2)=(cos(angle)*x2-sin(angle)*y2)*scle+xscreen(ins)
      y(2)=(sin(angle)*x2+cos(angle)*y2)*scle+yscreen(ins)
      !print*,'staff points are: ',x(1),y(1),x(2),y(2)
      call curve(x,y,2)
      r=r-.1
   enddo

   ! Draw any half staffs (needs two points to describe)

   if(istafs.eq.0.and.iflags.eq.0) r=r-0.1
   do ihs=1,ihfstafs
      x1=r
      y1=0.
      x2=r
      y2=.25*hfact
      x(1)=(cos(angle)*x1-sin(angle)*y1)*scle+xscreen(ins)
      y(1)=(sin(angle)*x1+cos(angle)*y1)*scle+yscreen(ins)
      x(2)=(cos(angle)*x2-sin(angle)*y2)*scle+xscreen(ins)
      y(2)=(sin(angle)*x2+cos(angle)*y2)*scle+yscreen(ins)
      !print*,'hstaff points are: ',x(1),y(1),x(2),y(2)
      call curve(x,y,2)
      r=r-.1
   enddo
   999 continue

enddo

! Guidelines for variable SCLE: SCLE = ABS(LAT2-LAT1)*(0.003/8.0)
! where lat1 and lat2 are from the SW and NE corners of the map

return
end
