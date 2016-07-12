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

subroutine gks_colors (iwk,ibgnd)

! line and text colors (total of 256 colors starting at index 0)

! 0        reserved for background
! 1        reserved for foreground
! 2  - 58  reserved for standard colors
! 59 - 89  reserved for fixed array of colors
! 90 - 95  reserved for 6 gray scale colors
! 96 - 255 reserved for user specified colors

implicit none

integer :: iwk,ibgnd

integer :: i
   
! set color 0 to background and all other colors to foreground

if(abs(ibgnd).eq.1) then
   call gscr (iwk,0,0.,0.,0.)    ! black (background)
   do i=1,255
      call gscr (iwk,i,1.,1.,1.) ! white (foreground)
   enddo
else
   call gscr (iwk,0,1.,1.,1.)    ! white (background)
   do i=1,255
      call gscr (iwk,i,0.,0.,0.) ! black (foreground)
   enddo
endif
   
! put the lookup table colors in the gks color table
call colortab_init (iwk,ibgnd)

if(ibgnd.eq.1) then

   ! Colors intended for computer screens on black background

   ! use specific colors
   call colortab_add ('rgb','title0'     ,1.,1.,1. )  ! box divider lines
   call colortab_add ('rgb','title1'     ,1.,.7,.7 )  ! experiment name and grid number
   call colortab_add ('rgb','title2'     ,.0,.95,.8)  ! location, date, time, and
                                                      !   min-max-inc-lab labels
   call colortab_add ('rgb','axis'       ,1.,1.,1. )  ! axis labels, perimeter, topo,
                                                      !   vector low, tile colorbar boxes
                                                      !   and labels
   call colortab_add ('rgb','roads0'     ,.65,.65,.65)! roads on (filled)
   call colortab_add ('rgb','roads1'     ,.6,.6,.6 )  ! roads on (outline)
   call colortab_add ('rgb','land'       ,0.,.5,0. )  ! land
   call colortab_add ('rgb','water'      ,0.,0.,.5 )  ! water
   call colortab_add ('rgb','bound0'     ,0.,.3,.0 )  ! map boundaries (filled)
   call colortab_add ('rgb','bound1'     ,0.,.5,.0 )  ! map boundaries (outline)
   call colortab_add ('rgb','shadow0'    ,.0,1.,.0 )  ! map boundaries shadow (filled)
   call colortab_add ('rgb','shadow1'    ,.0,1.,.0 )  ! map boundaries shadow (outline)

   ! tile colors
   !   call hsv00 (iwk,ind,h,s,v)
   call hsv00 (iwk,59,210.,.4,1.0)    ! blues
   call hsv00 (iwk,60,210.,.7,1.0)
   call hsv00 (iwk,61,210.,1.0,1.0)
   call hsv00 (iwk,62,180.,1.0,1.0)   ! blue-greens
   call hsv00 (iwk,63,180.,1.0,.80)
   call hsv00 (iwk,64,180.,1.0,.62)
   call hsv00 (iwk,65,150.,.70,1.0)   ! greens
   call hsv00 (iwk,66,150.,.70,.70)
   call hsv00 (iwk,67,150.,.70,.55)
   call hsv00 (iwk,68, 85.,.70,1.0)   ! green-yellow
   call hsv00 (iwk,69, 60.,.70,1.0)   ! yellow
   call hsv00 (iwk,70, 45.,.70,1.0)   ! orange-yellow
   call hsv00 (iwk,71, 30.,.90,1.0)   ! oranges
   call hsv00 (iwk,72, 30.,1.0,0.7)
   call hsv00 (iwk,73, 30.,0.6,0.7)   ! tan
   call hsv00 (iwk,74, 30.,0.6,0.52)  ! browns
   call hsv00 (iwk,75, 30.,0.6,0.40)
   call hsv00 (iwk,76,  0.,.40,1.0)   ! reds
   call hsv00 (iwk,77,  0.,.70,1.0)
   call hsv00 (iwk,78,  0.,.90,.90)
   call hsv00 (iwk,79,  0.,1.0,0.6)
   call hsv00 (iwk,80,305.,.45,1.0)   ! purples
   call hsv00 (iwk,81,305.,.70,1.0)
   call hsv00 (iwk,82,305.,1.0,.85)
   call hsv00 (iwk,83,275.,.45,1.0)   ! purple-blues
   call hsv00 (iwk,84,275.,.70,1.0)
   call hsv00 (iwk,85,275.,1.0,.92)
   call hsv00 (iwk,86,0.,0.,.28)      ! gray shades
   call hsv00 (iwk,87,0.,0.,.50)
   call hsv00 (iwk,88,0.,0.,.70)
   call hsv00 (iwk,89,0.,0.,.90)
                    
   ! Define a set of 6 lighter colors for vectors
   
   call gscr (iwk,90,1., .50 , 1.)
   call gscr (iwk,91,.4, .75 , 1.)
   call gscr (iwk,92,.6, 1.0 , 0.)
   call gscr (iwk,93,1., 1.0 , 0.)
   call gscr (iwk,94,1., .75 , 0.)
   call gscr (iwk,95,1., .40 , .4)

elseif(ibgnd.eq.2.) then

   ! Colors intended for computer screens on white background
   
   ! use specific colors
   call colortab_add ('rgb','title0'     ,0.,0.,0. )  ! box divider lines
   call colortab_add ('rgb','title1'     ,.1,.0,.0 )  ! experiment name and grid number
   call colortab_add ('rgb','title2'     ,.0,.5,.2 )  ! location, date, time, and
                                                      !   min-max-inc-lab labels
   call colortab_add ('rgb','axis'       ,0.,0.,0. )  ! axis labels, perimeter, topo,
                                                      !   vector low, tile colorbar boxes
                                                      !   and labels
   call colortab_add ('rgb','roads0'     ,.65,.65,.65)! roads on (filled)
   call colortab_add ('rgb','roads1'     ,.6,.6,.6 )  ! roads on (outline)
   call colortab_add ('rgb','land'       ,.8,1.,.8 )  ! land
   call colortab_add ('rgb','water'      ,.8,.8,1. )  ! water
   call colortab_add ('rgb','bound0'     ,.45,1.,.45) ! map boundaries (filled)
   call colortab_add ('rgb','bound1'     ,.6,1.,.6 )  ! map boundaries (outline)
   call colortab_add ('rgb','shadow0'    ,.0,.0,.0 )  ! map boundaries shadow (filled)
   call colortab_add ('rgb','shadow1'    ,.0,.0,.0 )  ! map boundaries shadow (outline)

   ! tile colors
   !   call hsv00 (iwk,ind,h,s,v)
   call hsv00 (iwk,59,210.,.4,1.0)    ! blues
   call hsv00 (iwk,60,210.,.7,1.0)
   call hsv00 (iwk,61,210.,1.0,1.0)
   call hsv00 (iwk,62,180.,1.0,1.0)   ! blue-greens
   call hsv00 (iwk,63,180.,1.0,.80)
   call hsv00 (iwk,64,180.,1.0,.62)
   call hsv00 (iwk,65,150.,.70,1.0)   ! greens
   call hsv00 (iwk,66,150.,.70,.70)
   call hsv00 (iwk,67,150.,.70,.55)
   call hsv00 (iwk,68, 85.,.70,1.0)   ! green-yellow
   call hsv00 (iwk,79, 60.,.70,1.0)   ! yellow
   call hsv00 (iwk,70, 45.,.70,1.0)   ! orange-yellow
   call hsv00 (iwk,71, 30.,.90,1.0)   ! oranges
   call hsv00 (iwk,72, 30.,1.0,0.7)
   call hsv00 (iwk,73, 30.,0.6,0.7)   ! tan
   call hsv00 (iwk,74, 30.,0.6,0.52)  ! browns
   call hsv00 (iwk,75, 30.,0.6,0.40)
   call hsv00 (iwk,76,  0.,.40,1.0)   ! reds
   call hsv00 (iwk,77,  0.,.70,1.0)
   call hsv00 (iwk,78,  0.,.90,.90)
   call hsv00 (iwk,89,  0.,1.0,0.6)
   call hsv00 (iwk,80,305.,.45,1.0)   ! purples
   call hsv00 (iwk,81,305.,.70,1.0)
   call hsv00 (iwk,82,305.,1.0,.85)
   call hsv00 (iwk,83,275.,.45,1.0)   ! purple-blues
   call hsv00 (iwk,84,275.,.70,1.0)
   call hsv00 (iwk,85,275.,1.0,.92)
   call hsv00 (iwk,86,0.,0.,.28)      ! gray shades
   call hsv00 (iwk,87,0.,0.,.50)
   call hsv00 (iwk,88,0.,0.,.70)
   call hsv00 (iwk,89,0.,0.,.90)
                   
   ! Define a set of 6 darker colors for vectors
   call gscr (iwk,90,.5,.0,.5)
   call gscr (iwk,90,.0,.0,.7)
   call gscr (iwk,92,.4,.8,.0)
   call gscr (iwk,93,.7,.7,.0)
   call gscr (iwk,94,.7,.4,.0)
   call gscr (iwk,95,.7,.0,.0)
                  
elseif(ibgnd.eq.3) then

   ! Colors intended for printing on white background
   !   (may need to be modified for a specific printer)

   ! use specific colors
   call colortab_add ('rgb','title0'     ,0.,0.,0. )  ! box divider lines
   call colortab_add ('rgb','title1'     ,.1,.0,.0 )  ! experiment name and grid number
   call colortab_add ('rgb','title2'     ,.0,.5,.2 )  ! location, date, time, and
                                                      !   min-max-inc-lab labels
   call colortab_add ('rgb','axis'       ,0.,0.,0. )  ! axis labels, perimeter, topo,
                                                      !   vector low, tile colorbar boxes
                                                      !   and labels
   call colortab_add ('rgb','roads0'     ,.65,.65,.65)! roads on (filled)
   call colortab_add ('rgb','roads1'     ,.6,.6,.6 )  ! roads on (outline)
   call colortab_add ('rgb','land'       ,.8,1.,.8 )  ! land
   call colortab_add ('rgb','water'      ,.8,.8,1. )  ! water
   call colortab_add ('rgb','bound0'     ,.45,1.,.45) ! map boundaries (filled)
   call colortab_add ('rgb','bound1'     ,.6,1.,.6 )  ! map boundaries (outline)
   call colortab_add ('rgb','shadow0'    ,.0,.0,.0 )  ! map boundaries shadow (filled)
   call colortab_add ('rgb','shadow1'    ,.0,.0,.0 )  ! map boundaries shadow (outline)
   
   ! tile colors
   !   call hsv00 (iwk,ind,h,s,v)
   call hsv00 (iwk,59,210.,0.45,1.0)  ! blues
   call hsv00 (iwk,60,210.,0.65,1.0)
   call hsv00 (iwk,61,210.,1.0,1.0)
   call hsv00 (iwk,62,170.,0.45,1.0)  ! blue-greens
   call hsv00 (iwk,63,170.,0.65,1.0)
   call hsv00 (iwk,64,170.,1.0,1.0)
   call hsv00 (iwk,65,120.,0.6,1.0)   ! greens
   call hsv00 (iwk,66,120.,1.0,1.0)
   call hsv00 (iwk,67, 90.,1.0,1.0)   ! yellow-green
   call hsv00 (iwk,68, 73.,1.0,1.0)   ! green-yellow
   call hsv00 (iwk,69, 60.,1.0,1.0)   ! yellow
   call hsv00 (iwk,70, 45.,1.0,1.0)   ! orange-yellow
   call hsv00 (iwk,71, 30.,1.0,1.0)   ! oranges
   call hsv00 (iwk,72, 30.,1.0,0.7)
   call hsv00 (iwk,73, 30.,0.6,0.7)   ! tan
   call hsv00 (iwk,74, 30.,0.6,0.52)  ! browns
   call hsv00 (iwk,75, 30.,0.6,0.40)
   call hsv00 (iwk,76,  0.,0.4,1.0)   ! reds
   call hsv00 (iwk,77,  0.,0.6,1.0)
   call hsv00 (iwk,78,  0.,1.0,1.0)
   call hsv00 (iwk,79,  0.,1.0,0.6)
   call hsv00 (iwk,80,300.,0.4,1.0)   ! magentas
   call hsv00 (iwk,81,300.,0.6,1.0)
   call hsv00 (iwk,82,300.,1.0,1.0)
   call hsv00 (iwk,83,260.,0.4,1.0)   ! purples
   call hsv00 (iwk,84,260.,0.6,1.0)
   call hsv00 (iwk,85,260.,1.0,1.0)
   call hsv00 (iwk,86,0.,0.,.0)       ! gray shades
   call hsv00 (iwk,87,0.,0.,.28)
   call hsv00 (iwk,88,0.,0.,.56)
   call hsv00 (iwk,89,0.,0.,.84)
                   
   ! Define a set of 6 darker colors for vectors\
   call gscr (iwk,90,.50,.0,.5)
   call gscr (iwk,91,.00,.0,.7)
   call gscr (iwk,92,.40,.8,.0)
   call gscr (iwk,93,.70,.7,.0)
   call gscr (iwk,94,.70,.4,.0)
   call gscr (iwk,95,.70,.0,.0)

endif

return
end

!***************************************************************************

subroutine gks_fillcolors (ilayer,imin,imid,imax)

! fill and tile colors

implicit none

integer :: ilayer,imin,imid,imax

include 'frame.h'

integer :: i,ip
real :: h1,l1,s1,h2,l2,s2,h3,l3,s3,hx,lx,sx,fx
   
! get the color fill options and et the rgb and hls colors
ip=0

if(abs(icmeth(ilayer)).eq.1.or.abs(icmeth(ilayer)).eq.2.or.  &
   abs(icmeth(ilayer)).eq.3.or.abs(icmeth(ilayer)).eq.4.or.  &
   abs(icmeth(ilayer)).eq.5.or.abs(icmeth(ilayer)).eq.6.or.  &
   abs(icmeth(ilayer)).eq.7) then
   
   if(ibgnd.lt.0) fillcols(1,ilayer)='gray'
   call colortab_hls ('hls',fillcols(1,ilayer),h1,l1,s1)
   if(ip==1) print*,fillcols(1,ilayer),h1,l1,s1
   
   if(abs(icmeth(ilayer)).eq.4.or.abs(icmeth(ilayer)).eq.5.or.  &
      abs(icmeth(ilayer)).eq.6.or.abs(icmeth(ilayer)).eq.7) then
      if(ibgnd.lt.0) fillcols(2,ilayer)='gray'
      call colortab_hls ('hls',fillcols(2,ilayer),h2,l2,s2)
      if(ip==1) print*,fillcols(2,ilayer),h2,l2,s2
      
      if(abs(icmeth(ilayer)).eq.7) then
         if(ibgnd.lt.0) fillcols(3,ilayer)='gray'
         call colortab_hls ('hls',fillcols(3,ilayer),h3,l3,s3)
         if(ip==1) print*,fillcols(3,ilayer),h3,l3,s3
      endif
      
   endif
   
endif

if(ip==1) print*,imin,imid,imax,icmeth(ilayer)

if(abs(icmeth(ilayer)).eq.3) then
   if(l1.gt.75.) then
      l1=75.
      print*,'maximum lightness specification is 75 in this color scheme'  &
            ,' - adjusting lightness to 75'
   elseif(l1.lt.25.) then
      l1=25.
      print*,'minimum lightness specification is 25 in this color scheme'  &
            ,' - adjusting lightness to 25'
   endif
   do i=imin,imax
      fx=float(i-imin)/float(imax-imin)
      if(icmeth(ilayer).eq.3) then
         ! dark to light color
	   lx=(l1-25.)+40.*fx
      elseif(icmeth(ilayer).eq.-3) then
         ! light to dark color
	   lx=(l1+25.)-50.*fx
      endif
      ! fill color
      call hls00 (iwk,i,h1,lx,s1)
      if(ip==1) write(*,'(a,i3,7f8.3)') ' fill color ',i,h1,lx,s1,fx
      ! line color
      if(lx.lt.50.) then
         lx=lx+abs(cgrad(ilayer))
      else
         lx=lx-abs(cgrad(ilayer))
      endif
      call hls00 (iwk,i+40,h1,lx,s1)
      if(ip==-1) write(*,'(a,i3,7f8.3)') ' line color ',i+40,h1,lx,s1,fx
   enddo
endif
       
if(abs(icmeth(ilayer))==1.or.abs(icmeth(ilayer))==2) then

   if(abs(ibgnd).eq.1) then
   
      ! black background
      do i=imin,imax
         fx=float(i-imin)/float(imax-imin)
         if(icmeth(ilayer)==1) then
            ! black to full color
            lx=min(100.,max(0.,l1*fx**(1./cbias(ilayer))))
         elseif(icmeth(ilayer)==-1) then
            ! full color to black
            lx=min(100.,max(0.,l1-l1*fx**cbias(ilayer)))
         elseif(icmeth(ilayer)==2) then
            ! white to full color
            lx=min(100.,max(0.,100.-(100.-l1)*fx**cbias(ilayer)))
         elseif(icmeth(ilayer)==-2) then
            ! full color to white
            lx=min(100.,max(0.,l1+(100.-l1)*fx**(1./cbias(ilayer))))
         endif
         ! fill color
         call hls00 (iwk,i,h1,lx,s1)
         if(ip==1) write(*,'(a,i3,7f8.3)') ' fill color ',i,h1,lx,s1,fx
         ! line color
         if(lx.lt.50.) then
            lx=lx+abs(cgrad(ilayer))
         else
            lx=lx-abs(cgrad(ilayer))
         endif
         call hls00 (iwk,i+40,h1,lx,s1)
         if(ip==-1) write(*,'(a,i3,7f8.3)') ' line color ',i+40,h1,lx,s1,fx
      enddo

   elseif(abs(ibgnd).eq.2.or.abs(ibgnd).eq.3) then
   
      ! white background
      do i=imin,imax
         fx=float(i-imin)/float(imax-imin)
         if(icmeth(ilayer)==1) then
            ! white to full color
            lx=min(100.,max(0.,100.-(100.-l1)*fx**cbias(ilayer)))
         elseif(icmeth(ilayer)==-1) then
            ! full color to white
            lx=min(100.,max(0.,l1+(100.-l1)*fx**(1./cbias(ilayer))))
         elseif(icmeth(ilayer)==2) then
            ! black to full color
            lx=min(100.,max(0.,l1*fx**(1./cbias(ilayer))))
         elseif(icmeth(ilayer)==-2) then
            ! full color to black
            lx=min(100.,max(0.,l1-l1*fx**cbias(ilayer)))
         endif
         ! fill color
         call hls00 (iwk,i,h1,lx,s1)
         if(ip==1) write(*,'(a,i3,7f8.3)') ' fill color ',i,h1,lx,s1,fx
         ! line color
         if(lx.lt.50.) then
            lx=lx+abs(cgrad(ilayer))
         else
            lx=lx-abs(cgrad(ilayer))
         endif
         call hls00 (iwk,i+40,h1,lx,s1)
         if(ip==-1) write(*,'(a,i3,7f8.3)') ' line color ',i+40,h1,lx,s1,fx
      enddo

   endif

elseif(abs(icmeth(ilayer))==4.or.abs(icmeth(ilayer))==5) then

   if(abs(ibgnd).eq.1) then
   
      ! black background
      if(imid.eq.imin) then
         ! lower half
         if(icmeth(ilayer)==4) then
            ! full color to black
            lx=0.
         elseif(icmeth(ilayer)==-4) then
            ! black to full color
            lx=l2
         elseif(icmeth(ilayer)==5) then
            ! full color to white
            lx=100.
         elseif(icmeth(ilayer)==-5) then
            ! white to full color
            lx=l2
         endif
         ! fill color
         call hls00 (iwk,imin,h2,lx,s2)
         if(ip==1) write(*,'(a,i3,6f8.3)') ' lower fill color (min-mid) '  &
                                          ,imin,h2,lx,s2
         ! line color
         if(lx.lt.50.) then
            lx=lx+abs(cgrad(ilayer))
         else
            lx=lx-abs(cgrad(ilayer))
         endif
         call hls00 (iwk,imin+40,h2,lx,s2)
         if(ip==-1) write(*,'(a,i3,6f8.3)') ' lower line color (min-mid) '  &
                                          ,imin+40,h2,lx,s2
      else
         do i=imin,imid
            ! fill color - lower half
            fx=float(i-imin)/float(imid-imin)
            if(icmeth(ilayer)==4) then
               ! full color to black
               lx=min(100.,max(0.,l2-l2*fx**cbias(ilayer)))
            elseif(icmeth(ilayer)==-4) then
               ! black to full color
               lx=min(100.,max(0.,l2*fx**(1./cbias(ilayer))))
            elseif(icmeth(ilayer)==5) then
               ! full color to white
               lx=min(100.,max(0.,l2+(100.-l2)*fx**(1./cbias(ilayer))))
            elseif(icmeth(ilayer)==-5) then
               ! white to full color
               lx=min(100.,max(0.,100.-(100.-l2)*fx**cbias(ilayer)))
            endif
            call hls00 (iwk,i,h2,lx,s2)
            if(ip==1) write(*,'(a,i3,7f8.3)') ' lower fill color (min-mid) '  &
                                             ,i,h2,lx,s2,fx
            ! line color - lower half
            if(lx.lt.50.) then
               lx=lx+abs(cgrad(ilayer))
            else
               lx=lx-abs(cgrad(ilayer))
            endif
            call hls00 (iwk,i+40,h2,lx,s2)
            if(ip==-1) write(*,'(a,i3,7f8.3)') ' lower line color (min-mid) '  &
                                             ,i+40,h2,lx,s2,fx
         enddo
      endif
      
      if(imid+1.eq.imax) then
         ! fill color - upper half
         if(icmeth(ilayer)==4) then
            ! black to full color
            lx=0.
         elseif(icmeth(ilayer)==-4) then
            ! full color to black
            lx=l1
         elseif(icmeth(ilayer)==5) then
            ! white to full color
            lx=100.
         elseif(icmeth(ilayer)==-5) then
            ! full color to white
            lx=l1
         endif
         ! fill color
         call hls00 (iwk,imax,h1,lx,s1)
         if(ip==1) write(*,'(a,i3,6f8.3)') ' upper fill color (mid-max) '  &
                                          ,imax,h1,lx,s1
      else
         do i=imid+1,imax
            ! fill color - upper half
            fx=float(i-imid-1)/float(imax-imid-1)
            if(icmeth(ilayer)==4) then
               ! black to full color
               lx=min(100.,max(0.,l1*fx**(1./cbias(ilayer))))
            elseif(icmeth(ilayer)==-4) then
               ! full color to black
               lx=min(100.,max(0.,l1-l1*fx**cbias(ilayer)))
            elseif(icmeth(ilayer)==5) then
               ! white to full color
               lx=min(100.,max(0.,100.-(100.-l1)*fx**cbias(ilayer)))
            elseif(icmeth(ilayer)==-5) then
               ! full color to white
               lx=min(100.,max(0.,l1+(100.-l1)*fx**(1./cbias(ilayer))))
            endif
            call hls00 (iwk,i,h1,lx,s1)
            if(ip==1) write(*,'(a,i3,7f8.3)') ' upper fill color (mid-max) '  &
                                             ,i,h1,lx,s1,fx
         enddo
      endif
      if(imid+1.eq.imax+1) then
         ! line color
         if(icmeth(ilayer)==4) then
            ! black to full color
            lx=0.
         elseif(icmeth(ilayer)==-4) then
            ! full color to black
            lx=l1
         elseif(icmeth(ilayer)==5) then
            ! white to full color
            lx=100.
         elseif(icmeth(ilayer)==-5) then
            ! full color to white
            lx=l1
         endif
         if(lx.lt.50.) then
            lx=lx+abs(cgrad(ilayer))
         else
            lx=lx-abs(cgrad(ilayer))
         endif
         call hls00 (iwk,imax+1+40,h1,lx,s1)
         if(ip==-1) write(*,'(a,i3,6f8.3)') ' upper line color (mid-max) '  &
                                          ,imax+1+40,h1,lx,s1
      else
         do i=imid+1,imax+1
            ! line color - upper half
            fx=float(i-imid-1)/float(imax+1-imid-1)
            if(icmeth(ilayer)==4) then
               ! black to full color
               lx=min(100.,max(0.,l1*fx**(1./cbias(ilayer))))
            elseif(icmeth(ilayer)==-4) then
               ! full color to black
               lx=min(100.,max(0.,l1-l1*fx**cbias(ilayer)))
            elseif(icmeth(ilayer)==5) then
               ! white to full color
               lx=min(100.,max(0.,100.-(100.-l1)*fx**cbias(ilayer)))
            elseif(icmeth(ilayer)==-5) then
               ! full color to white
               lx=min(100.,max(0.,l1+(100.-l1)*fx**(1./cbias(ilayer))))
            endif
            if(lx.lt.50.) then
               lx=lx+abs(cgrad(ilayer))
            else
               lx=lx-abs(cgrad(ilayer))
            endif
            call hls00 (iwk,i+40,h1,lx,s1)
            if(ip==-1) write(*,'(a,i3,7f8.3)') ' upper line color (mid-max) '  &
                                             ,i+40,h1,lx,s1,fx
         enddo
      endif

   elseif(abs(ibgnd).eq.2.or.abs(ibgnd).eq.3) then
   
      ! white background
      if(imid.eq.imin) then
         ! lower half
         if(icmeth(ilayer)==4) then
            ! full color to white
            lx=0.
         elseif(icmeth(ilayer)==-4) then
            ! white to full color
            lx=l2
         elseif(icmeth(ilayer)==5) then
            ! full color to black
            lx=100
         elseif(icmeth(ilayer)==-5) then
            ! black to full color
            lx=l2
         endif
         ! fill color
         call hls00 (iwk,imin,h2,lx,s2)
         if(ip==1) write(*,'(a,i3,6f8.3)') ' fill color (min-mid) '  &
                                          ,imin,h2,lx,s2
         ! line color
         if(lx.lt.50.) then
            lx=lx+abs(cgrad(ilayer))
         else
            lx=lx-abs(cgrad(ilayer))
         endif
         call hls00 (iwk,imin+40,h2,lx,s2)
         if(ip==-1) write(*,'(a,i3,6f8.3)') ' line color (min-mid) '  &
                                          ,imin+40,h2,lx,s2
      else
         do i=imin,imid
            ! lower half
            fx=float(i-imin)/float(imid-imin)
            if(icmeth(ilayer)==4) then
               ! full color to white
               lx=min(100.,max(0.,l2+(100.-l2)*fx**(1./cbias(ilayer))))
            elseif(icmeth(ilayer)==-4) then
               ! white to full color
               lx=min(100.,max(0.,100.-(100.-l2)*fx**cbias(ilayer)))
            elseif(icmeth(ilayer)==5) then
               ! full color to black
               lx=min(100.,max(0.,l2-l2*fx**cbias(ilayer)))
            elseif(icmeth(ilayer)==-5) then
               ! black to full color
               lx=min(100.,max(0.,l2*fx**(1./cbias(ilayer))))
            endif
            ! fill color
            call hls00 (iwk,i,h2,lx,s2)
            if(ip==1) write(*,'(a,i3,7f8.3)') ' fill color (min-mid) '  &
                                             ,i,h2,lx,s2,fx
            ! line color
            if(lx.lt.50.) then
               lx=lx+abs(cgrad(ilayer))
            else
               lx=lx-abs(cgrad(ilayer))
            endif
            call hls00 (iwk,i+40,h2,lx,s2)
            if(ip==-1) write(*,'(a,i3,7f8.3)') ' line color (min-mid) '  &
                                             ,i+40,h2,lx,s2,fx
         enddo
      endif
      
      if(imid+1.eq.imax) then
         ! upper half
         if(icmeth(ilayer)==4) then
            ! white to full color
            lx=100.
         elseif(icmeth(ilayer)==-4) then
            ! full color to white
            lx=l1
         elseif(icmeth(ilayer)==5) then
            ! black to full color
            lx=0.
         elseif(icmeth(ilayer)==-5) then
            ! full color to black
            lx=l1
         endif
         ! fill color
         call hls00 (iwk,imid+1,h1,lx,s1)
         if(ip==1) write(*,'(a,i3,6f8.3)') ' fill color (mid-max) '  &
                                          ,imid+1,h1,lx,s1
      else
         do i=imid+1,imax
            ! fill color - upper half
            fx=float(i-imid-1)/float(imax-imid-1)
            if(icmeth(ilayer)==4) then
               ! white to full color
               lx=min(100.,max(0.,100.-(100.-l1)*fx**cbias(ilayer)))
            elseif(icmeth(ilayer)==-4) then
               ! full color to white
               lx=min(100.,max(0.,l1+(100.-l1)*fx**(1./cbias(ilayer))))
            elseif(icmeth(ilayer)==5) then
               ! black to full color
               lx=min(100.,max(0.,l1*fx**(1./cbias(ilayer))))
            elseif(icmeth(ilayer)==-5) then
               ! full color to black
               lx=min(100.,max(0.,l1-l1*fx**cbias(ilayer)))
            endif
            ! fill color
            call hls00 (iwk,i,h1,lx,s1)
            if(ip==1) write(*,'(a,i3,7f8.3)') ' fill color (mid-max) '  &
                                             ,i,h1,lx,s1,fx
         enddo
      endif
      if(imid+1.eq.imax+1) then
         ! line color
         if(icmeth(ilayer)==4) then
            ! white to full color
            lx=0.
         elseif(icmeth(ilayer)==-4) then
            ! full color to white
            lx=l1
         elseif(icmeth(ilayer)==5) then
            ! black to full color
            lx=100.
         elseif(icmeth(ilayer)==-5) then
            ! full color to black
            lx=l1
         endif
         if(lx.lt.50.) then
            lx=lx+abs(cgrad(ilayer))
         else
            lx=lx-abs(cgrad(ilayer))
         endif
         call hls00 (iwk,imid+1+40,h1,lx,s1)
         if(ip==-1) write(*,'(a,i3,6f8.3)') ' line color (mid-max) '  &
                                          ,imid+1+40,h1,lx,s1
      else
         do i=imid+1,imax+1
            ! line color - upper half
            fx=float(i-imid-1)/float(imax+1-imid-1)
            if(icmeth(ilayer)==4) then
               ! white to full color
               lx=min(100.,max(0.,100.-(100.-l1)*fx**cbias(ilayer)))
            elseif(icmeth(ilayer)==-4) then
               ! full color to white
               lx=min(100.,max(0.,l1+(100.-l1)*fx**(1./cbias(ilayer))))
            elseif(icmeth(ilayer)==5) then
               ! black to full color
               lx=min(100.,max(0.,l1*fx**(1./cbias(ilayer))))
            elseif(icmeth(ilayer)==-5) then
               ! full color to black
               lx=min(100.,max(0.,l1-l1*fx**cbias(ilayer)))
            endif
            ! line color
            if(lx.lt.50.) then
               lx=lx+abs(cgrad(ilayer))
            else
               lx=lx-abs(cgrad(ilayer))
            endif
            call hls00 (iwk,i+40,h1,lx,s1)
            if(ip==-1) write(*,'(a,i3,7f8.3)') ' line color (mid-max) '  &
                                             ,i+40,h1,lx,s1,fx
         enddo
      endif

   endif
   
elseif(abs(icmeth(ilayer)).eq.6) then

   do i=imin,imax
      fx=float(i-imin)/float(imax-imin)
      if(icmeth(ilayer).eq.6) then
         ! 1st color to 2nd color
	   hx=h2+(h1-h2)*fx**(1./cbias(ilayer))
	   lx=l2+(l1-l2)*fx**(1./cbias(ilayer))
	   sx=s2+(s1-s2)*fx**(1./cbias(ilayer))
      elseif(icmeth(ilayer).eq.-6) then
         ! 2nd color to 1st color
	   hx=h1+(h2-h1)*fx**(1./cbias(ilayer))
	   lx=l1+(l2-l1)*fx**(1./cbias(ilayer))
	   sx=s1+(s2-s1)*fx**(1./cbias(ilayer))
      endif
      ! fill color
      call hls00 (iwk,i,hx,lx,sx)
      if(ip==1) write(*,'(a,i3,7f8.3)') ' fill color ',i,hx,lx,sx,fx
      ! line color
      if(lx.lt.50.) then
         lx=lx+abs(cgrad(ilayer))
      else
         lx=lx-abs(cgrad(ilayer))
      endif
      call hls00 (iwk,i+40,hx,lx,sx)
      if(ip==-1) write(*,'(a,i3,7f8.3)') ' line color ',i+40,hx,lx,sx,fx
   enddo
   
elseif(abs(icmeth(ilayer)).eq.7) then

   if(imid.eq.imin) then
      ! 
      if(icmeth(ilayer)==7) then
         ! full color to midcolor
         hx=h3
         lx=l3
         sx=s3
      elseif(icmeth(ilayer)==-7) then
         ! midcolor to full color
         hx=h2
         lx=l2
         sx=s2
      endif
      ! fill color
      call hls00 (iwk,imin,hx,lx,sx)
      if(ip==1) write(*,'(a,i3,6f8.3)') ' lower fill color (min-mid) '  &
                                       ,imin,hx,lx,sx
      ! line color
      if(lx.lt.50.) then
         lx=lx+abs(cgrad(ilayer))
      else
         lx=lx-abs(cgrad(ilayer))
      endif
      call hls00 (iwk,imin+40,hx,lx,sx)
      if(ip==-1) write(*,'(a,i3,6f8.3)') ' lower line color (min-mid) '  &
                                       ,imin+40,h2,lx,s2
   else
      do i=imin,imid
         ! fill color - lower half
         fx=float(i-imin)/float(imid-imin)
         if(icmeth(ilayer)==7) then
            ! full color to midcolor
	      hx=h2+(h3-h2)*fx**(1./cbias(ilayer))
	      lx=l2+(l3-l2)*fx**(1./cbias(ilayer))
	      sx=s2+(s3-s2)*fx**(1./cbias(ilayer))
         elseif(icmeth(ilayer)==-7) then
            ! midcolor to full color
	      hx=h3+(h2-h3)*fx**(1./cbias(ilayer))
	      lx=l3+(l2-l3)*fx**(1./cbias(ilayer))
	      sx=s3+(s2-s3)*fx**(1./cbias(ilayer))
         endif
         !call hls00 (iwk,i,h2,lx,s2)
         call hls00 (iwk,i,hx,lx,sx)
         if(ip==1) write(*,'(a,i3,7f8.3)') ' lower fill color (min-mid) '  &
                                          ,i,hx,lx,sx,fx
         ! line color - lower half
         if(lx.lt.50.) then
            lx=lx+abs(cgrad(ilayer))
         else
            lx=lx-abs(cgrad(ilayer))
         endif
         call hls00 (iwk,i+40,hx,lx,sx)
         if(ip==-1) write(*,'(a,i3,7f8.3)') ' lower line color (min-mid) '  &
                                          ,i+40,hx,lx,sx,fx
      enddo
   endif
   
   if(imid+1.eq.imax) then
      ! fill color - upper half
      if(icmeth(ilayer)==7) then
         ! midcolor to full color
         hx=h3
         lx=l3
         sx=s3
      elseif(icmeth(ilayer)==-7) then
         ! full color to midcolor
         hx=h1
         lx=l1
         sx=s1
      endif
      ! fill color
      call hls00 (iwk,imax,hx,lx,sx)
      if(ip==1) write(*,'(a,i3,6f8.3)') ' upper fill color (mid-max) '  &
                                       ,imax,hx,lx,sx
   else
      do i=imid+1,imax
         ! fill color - upper half
         fx=float(i-imid-1)/float(imax-imid-1)
         if(icmeth(ilayer)==7) then
            ! midcolor to full color
	      hx=h3+(h1-h3)*fx**(1./cbias(ilayer))
	      lx=l3+(l1-l3)*fx**(1./cbias(ilayer))
	      sx=s3+(s1-s3)*fx**(1./cbias(ilayer))
         elseif(icmeth(ilayer)==-7) then
            ! full color to midcolor
	      hx=h1+(h3-h1)*fx**(1./cbias(ilayer))
	      lx=l1+(l3-l1)*fx**(1./cbias(ilayer))
	      sx=s1+(s3-s1)*fx**(1./cbias(ilayer))
         endif
         call hls00 (iwk,i,hx,lx,sx)
         if(ip==1) write(*,'(a,i3,7f8.3)') ' upper fill color (mid-max) '  &
                                          ,i,hx,lx,sx,fx
      enddo
   endif
   if(imid+1.eq.imax+1) then
      ! line color
      if(icmeth(ilayer)==7) then
         ! midcolor to full color
         hx=h3
         lx=l3
         sx=s3
      elseif(icmeth(ilayer)==-7) then
         ! full color to midcolor
         hx=h1
         lx=l1
         sx=s1
      endif
      if(lx.lt.50.) then
         lx=lx+abs(cgrad(ilayer))
      else
         lx=lx-abs(cgrad(ilayer))
      endif
      call hls00 (iwk,imax+1+40,hx,lx,sx)
      if(ip==-1) write(*,'(a,i3,6f8.3)') ' upper line color (mid-max) '  &
                                       ,imax+1+40,hx,lx,sx
   else
      do i=imid+1,imax+1
         ! line color - upper half
         fx=float(i-imid-1)/float(imax+1-imid-1)
         if(icmeth(ilayer)==7) then
            ! midcolor to full color
	      hx=h3+(h1-h3)*fx**(1./cbias(ilayer))
	      lx=l3+(l1-l3)*fx**(1./cbias(ilayer))
	      sx=s3+(s1-s3)*fx**(1./cbias(ilayer))
         elseif(icmeth(ilayer)==-7) then
            ! full color to midcolor
	      hx=h1+(h3-h1)*fx**(1./cbias(ilayer))
	      lx=l1+(l3-l1)*fx**(1./cbias(ilayer))
	      sx=s1+(s3-s1)*fx**(1./cbias(ilayer))
         endif
         if(lx.lt.50.) then
            lx=lx+abs(cgrad(ilayer))
         else
            lx=lx-abs(cgrad(ilayer))
         endif
         call hls00 (iwk,i+40,hx,lx,sx)
         if(ip==-1) write(*,'(a,i3,7f8.3)') ' upper line color (mid-max) '  &
                                          ,i+40,hx,lx,sx,fx
      enddo
   endif
   
endif

return
end

!***************************************************************************

subroutine colortab_init (iwk,ibgnd)

! manipulates and retrieves information from the color table

implicit none

integer :: iwk,ibgnd,icolor
real :: c1,c2,c3,h,l,s
character(len=*) :: coloradd,colorhls,colorcol,colorind,cmod

integer :: i,ic,lastchar
integer, save :: icsave,iw

integer, parameter :: maxcols=58
real, dimension(3,maxcols), save :: gksind
character(len=20), save :: gkscol(maxcols)
character(len=1) :: toksep
character(len=10) :: cind(3)

! define some general colors
integer, parameter :: icolors=40
character(len=20) :: colors(icolors)
real, dimension(3,icolors) :: colori

data (gkscol(i),i=1,maxcols) /maxcols*' '/
data (colors(i),i=1,40) /' ','white','black','grayblack','darkgray','gray'  &
                        ,'lightgray','darkred','red','midred','lightred'  &
                        ,'darkgreen','green','midgreen','lightgreen'  &
                        ,'darkblue','blue','midblue','lightblue','yellow'  &
                        ,'yellowgreen','purple','cyan','tan','shadygreen'  &
                        ,'brown','orangered','orange','gold','greenyellow'  &
                        ,'forestgreen','aqua','deepskyblue','skyblue'  &
                        ,'royalblue','slateblue','bluemagenta'  &
                        ,'darkviolet','magenta','lavender'/
     
data (colori(i,2),i=1,3)  /1.,1.,1./    ! white
data (colori(i,3),i=1,3)  /0.,0.,0./    ! black
data (colori(i,4),i=1,3)  /.3,.3,.3/    ! grayblack
data (colori(i,5),i=1,3)  /.65,.65,.65/ ! darkgray
data (colori(i,6),i=1,3)  /.5,.5,.5/    ! gray
data (colori(i,7),i=1,3)  /.8,.8,.8/    ! lightgray
data (colori(i,8),i=1,3)  /.5,0.,0./    ! darkred
data (colori(i,9),i=1,3)  /1.,0.,0./    ! red
data (colori(i,10),i=1,3) /1.,.65,.65/  ! midred
data (colori(i,11),i=1,3) /1.,.8,.8/    ! lightred
data (colori(i,12),i=1,3) /0.,.5,0./    ! darkgreen
data (colori(i,13),i=1,3) /0.,1.,0./    ! green
data (colori(i,14),i=1,3) /.65,1.,.65/  ! midgreen
data (colori(i,15),i=1,3) /.8,1.,.8/    ! lightgreen
data (colori(i,16),i=1,3) /0.,0.,.5/    ! darkblue
data (colori(i,17),i=1,3) /0.,0.,1./    ! blue
data (colori(i,18),i=1,3) /.65,.65,1./  ! midblue
data (colori(i,19),i=1,3) /.8,.8,1./    ! lightblue
data (colori(i,20),i=1,3) /1.,1.,0./    ! yellow
data (colori(i,21),i=1,3) /.5,1.,0./    ! yellowgreen
data (colori(i,22),i=1,3) /1.,0.,1./    ! purple
data (colori(i,23),i=1,3) /0.,1.,1./    ! cyan
data (colori(i,24),i=1,3) /.86,.58,.44/ ! tan
data (colori(i,25),i=1,3) /0.,.7,0./    ! shadygreen
data (colori(i,26),i=1,3) /.65,.16,.16/ ! brown
data (colori(i,27),i=1,3) /1.0,0.0,0.2/ ! orangered
data (colori(i,28),i=1,3) /1.0,.50,0.0/ ! orange
data (colori(i,29),i=1,3) /1.0,.85,0.0/ ! gold
data (colori(i,30),i=1,3) /.70,1.0,0.2/ ! greenyellow
data (colori(i,31),i=1,3) /.14,.56,.14/ ! forestgreen
data (colori(i,32),i=1,3) /0.1,1.0,.85/ ! aqua
data (colori(i,33),i=1,3) /.00,.75,1.0/ ! deepskyblue
data (colori(i,34),i=1,3) /.20,.56,.80/ ! skyblue
data (colori(i,35),i=1,3) /.25,.45,.95/ ! royalblue
data (colori(i,36),i=1,3) /.40,.35,.80/ ! slateblue
data (colori(i,37),i=1,3) /0.5,0.0,1.0/ ! bluemagenta
data (colori(i,38),i=1,3) /0.6,0.0,0.8/ ! darkviolet
data (colori(i,39),i=1,3) /1.0,0.0,1.0/ ! magenta
data (colori(i,40),i=1,3) /0.8,0.8,1.0/ ! lavender
      
data toksep /'/'/

! put the lookup table colors in the gks color table

iw=iwk
do ic=1,maxcols
   gkscol(ic)=' '
   gksind(1,ic)=0.
   gksind(2,ic)=0.
   gksind(3,ic)=0.
enddo

ic=1
gkscol(ic)='foreground'
if(abs(ibgnd).eq.1) then
   gksind(1,ic)=1.
   gksind(2,ic)=1.
   gksind(3,ic)=1.
else
   gksind(1,ic)=0.
   gksind(2,ic)=0.
   gksind(3,ic)=0.
endif

do ic=2,icolors
   if(colors(ic)(1:1).eq.' ') return
   call gscr (iw,ic,colori(1,ic),colori(2,ic),colori(3,ic))
   gkscol(ic)=colors(ic)
   gksind(1,ic)=colori(1,ic)
   gksind(2,ic)=colori(2,ic)
   gksind(3,ic)=colori(3,ic)
   icsave=ic
enddo

return
   
!-------------------------------------------

entry colortab_print ()
   
! print the color table

print*,icsave
do ic=1,icsave
   print*,ic,gksind(1,ic),gksind(2,ic),gksind(3,ic)  &
         ,' ',gkscol(ic)(1:len_trim(gkscol(ic)))
enddo

return
   
!-------------------------------------------

entry colortab_add (cmod,coloradd,c1,c2,c3)
   
! add an external color to the gks color table

do ic=1,maxcols
   if(coloradd(1:len_trim(coloradd))==  &
      gkscol(ic)(1:len_trim(gkscol(ic)))) then
      !print*,'Already have color in table: ',coloradd
      !print*,'old ',ic, gksind(1,ic),gksind(2,ic),gksind(3,ic)
      if(cmod(1:3)=='hls') then
         h=c1
         l=c2
         s=c3
         call hlsrgb (h,l,s,c1,c2,c3)
      endif
      call gscr (iw,ic,c1,c2,c3)
      gksind(1,ic)=c1
      gksind(2,ic)=c2
      gksind(3,ic)=c3
      !print*,'new ',ic,c1,c2,c3
      return
   endif
enddo

icsave=icsave+1
if(icsave.gt.maxcols) then
   print*,'Cannot allocate any more colors - limit: ',maxcols
   stop 'colortab'
endif
if(cmod(1:3)=='hls') then
   h=c1
   l=c2
   s=c3
   call hlsrgb (h,l,s,c1,c2,c3)
endif
call gscr (iw,icsave,c1,c2,c3)
gkscol(icsave)=coloradd
gksind(1,icsave)=c1
gksind(2,icsave)=c2
gksind(3,icsave)=c3
!print*,'added ',icsave,c1,c2,c3,' ',coloradd(1:len_trim(coloradd))

return

!-------------------------------------------

entry colortab_hls (cmod,colorhls,c1,c2,c3)
   
! get a color from the color table and return rgb or hls

do ic=1,maxcols
   if(colorhls==gkscol(ic)) goto 1
enddo
print*,'Could not find color: ',colorhls
stop 'gks_fillcolors'
1 continue
if(cmod(1:3)=='hls') then
   call rgbhls (gksind(1,ic),gksind(2,ic),gksind(3,ic),c1,c2,c3)
elseif(cmod(1:3)=='rgb') then
   c1=gksind(1,ic)
   c2=gksind(2,ic)
   c3=gksind(3,ic)
endif

!print*,ic,' ',colorhls
!print*,'rgb',gksind(1,ic),gksind(2,ic),gksind(3,ic)
!print*,'hls',c1,c2,c3

return
   
!-------------------------------------------

entry colortab_ind (colorind,icolor)
   
! get the gks color table index of a color

if(len_trim(gkscol(1))==0) then  ! color table not defined yet
   icolor=1
   return
endif
do ic=1,maxcols
   !print*,ic,'|',colorind(1:lastchar(colorind)),'|'  &
   !      ,gkscol(ic)(1:lastchar(gkscol(ic))),'|'
   if(colorind(1:lastchar(colorind))==  &
      gkscol(ic)(1:lastchar(gkscol(ic)))) then
      icolor=ic
      return
   endif
enddo
print*,'Could not find color: ',colorind
stop 'gks_fillcolors'

return
   
!-------------------------------------------

entry colortab_col (colorcol,icolor)
   
! get the gks color from the index of a color

if(icolor.gt.maxcols) then
   print*,'Requesting color index above maxcols: ',icolor,maxcols
   stop 'gks_fillcolors'
endif
colorcol=gkscol(icolor)
if(colorcol=='') colorcol='unset'

return
end

!***************************************************************************

subroutine gs00 (iwk,ic,ir,ig,ib)

implicit none

integer :: iwk,ic,ir,ig,ib

call gscr (iwk,ic,float(ir)/255.,float(ig)/255.,float(ib)/255.)

return
end

!***************************************************************************

subroutine hsv00 (iwk,ic,h,s,v)

implicit none

integer :: iwk,ic
real :: h,s,v

real :: r,g,b

call hsvrgb (h,s,v,r,g,b)
call gscr (iwk,ic,r,g,b)

return
end

!***************************************************************************

subroutine hls00 (iwk,ic,h,l,s)

implicit none

integer :: iwk,ic
real :: h,l,s

real :: r,g,b

call hlsrgb (h,l,s,r,g,b)
call gscr (iwk,ic,r,g,b)

return
end

!***************************************************************************

subroutine draw_colours (ncol1,ncol2,title)

! This subroutine just draws the colours in defined by the colour map
! (not including those used by the contouring package)

implicit none

integer :: ncol1,ncol2
character(len=*) :: title

integer :: nx,ny,j,i,ij,ncols,lastchar
real :: szx,szy,y0,y1,x,y,spx,spy
character(len=20) :: blab

blab=''
ncols=ncol2-ncol1+1
nx=5
ny=ncols/nx
if(mod(ncols,nx).ne.0) ny=ny+1
szx=.96/nx
szy=min(.56/ny,0.1)
y0=.10
y1=.90
spx=(1.-nx*szx)/(nx+1)
spy=(y1-y0-ny*szy)/max(ny-1,1)

do j=1,ny
   y = y0+(j-1)*(spy+szy)
   do i=1,nx
      ij=i+(j-1)*nx-1
      if(ij+1.le.ncols) then
         x = spx+(i-1)*(spx+szx)
         write(blab,'(i3)') ij+ncol1
         call drbox(x,y,szx,y0,szy,blab,ij+ncol1)
      endif
   enddo
enddo

call plchlq(.5,.95,'REVU '//title(1:lastchar(title)),.02,0,0)
call plchlq(.5,.04,'The labels indicate colour index',.012,0,0)

return
end

!***************************************************************************

subroutine drbox (x,y,szx,y0,szy,tlab,indx)

! Draw a color square with lower left corner (x,y)

implicit none

integer :: indx
real :: x,y,szx,y0,szy
character(len=*) :: tlab

integer :: itlen,ier,lastchar
real, dimension(5) :: a,b
real :: cr,cg,cb
character(len=20) :: color

call gsfaci(indx)
a(1) = x
b(1) = y
a(2) = x+szx
b(2) = y
a(3) = a(2)
b(3) = y+szy
a(4) = x
b(4) = b(3)
a(5) = a(1)
b(5) = b(1)
call gsfais(1)
call gfa(4,a,b)
call gsfais(0)

! if the color is black, draw a boundary

call gqcr(1,indx,0,ier,cr,cg,cb)
if (cr.eq.0. .and. cg.eq.0. .and. cb.eq.0.) then
   call gsplci(1)
   call gpl(5,a,b)
endif

call gsplci(1)

if(indx.le.58) then
   call colortab_col (color,indx)
   call plchlq(x,y-.011,tlab(1:lastchar(tlab))//' - '//  &
               color(1:lastchar(color)),.0098,0.,-1.)
else
   call plchlq(x+.5*szx,y-.011,tlab(1:lastchar(tlab)),.0098,0.,0.)
endif

return
end

