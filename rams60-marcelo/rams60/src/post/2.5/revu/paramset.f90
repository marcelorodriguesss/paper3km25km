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

subroutine continit ()

! initilaize plot settings

implicit none

include 'frame.h'

integer :: i

! set CFRAME_A defaults
conttyp(1)='f'
colorbar(1)='b'
conrlo(1)=0.
conrhi(1)=0.
conrinc(1)=0.
icmeth(1)=4
cbias(1)=1.5
ccent(1)=0.
icint(1)=5
icdash(1)=1
cgrad(1)=30.
cthick(1)=1.
icover(1)=1
ichigh(1)=0
icline(1)=0
csize(1)=.01
call colortab_ind ('lightgray',icol(1))
fillcols(1,1)='red'
fillcols(2,1)='blue'
fillcols(3,1)='green'

! set CFRAME_B defaults
cwinds='n'
intwindi=1
intwindj=1
ibscale=3
call colortab_ind ('yellow',iwcolor)
stemleng=-1.
velomax=0.
headleng=-1.
headleng0=-1.
headang=-1.

! set CFRAME_C defaults
do i=2,maxlayers
   cvar(i)='none'
   conttyp(i)='c'
   colorbar(i)='n'
   conrlo(i)=0.
   conrhi(i)=0.
   conrinc(i)=0.
   icmeth(i)=2
   cbias(i)=1.5
   ccent(i)=0.
   icint(i)=5
   icdash(i)=1
   cgrad(i)=30.
   cthick(i)=1.
   icover(i)=1
   ichigh(i)=0
   icline(i)=0
   csize(i)=.01
   call colortab_ind ('lightgray',icol(i))
   fillcols(1,i)='darkgreen'
   fillcols(2,i)='purple'
   fillcols(3,i)='yellow'
enddo

! set LANDMARK defaults
ilandmk=1
bufflandmk=0.02
itypelandmk=2
collandmk='lightgray'
sizelandmk=1.0
ilandlab=1
collandlab='lightgray'
sizelandlab=0.008
               
return
end

!***************************************************************************

subroutine contset (ivar,frame,cframe)

! sorts out the frame plotting attributes - parses "FRAME" string for
! the plot and put tokens into correct type variables

implicit none

integer :: ivar
character(len=*) :: frame,cframe

include 'vcomm2.h'
include 'frame.h'

integer :: i,ntokfr,itok,ntok,nclevels
character(len=1)   :: toksepfr,toksep
character(len=32)  :: tokens(20)
character(len=64)  :: frtokens(50)
character(len=20)  :: color
data toksepfr/'/'/ toksep/':'/

!print*,ivar,frame,cframe

do i=1,50
   frtokens(i)=' '
enddo
call tokenize1 (cframe,frtokens,ntokfr,toksepfr)

if(cvar(ivar)(1:10).eq.'qveg_class'.or.  &
   cvar(ivar)(1:9).eq.'veg_class'.or.  &
   cvar(ivar)(1:5).eq.'sltex') then
   conttyp(ivar)='t'
   colorbar(ivar)='b'
   icmeth(ivar)=0
   conrlo(ivar)=0.
   conrinc(ivar)=1.
   if(cvar(ivar)(1:10).eq.'qveg_class') conrhi(ivar)=31.
   if(cvar(ivar)(1:9).eq.'veg_class') conrhi(ivar)=33.
   if(cvar(ivar)(1:5).eq.'sltex') conrhi(ivar)=13.
endif

if(len_trim(frtokens(2)).ge.1) then
   conttyp(ivar)=frtokens(2)(1:1)
   if(conttyp(ivar)=='c') then
      icmeth(ivar)=0
      !cgrad(ivar)=0.
      colorbar(ivar)='n'
   elseif(conttyp(ivar)=='f'.or.conttyp(ivar)=='t') then
      colorbar(ivar)='n'
   elseif(conttyp(ivar)=='a'.or.conttyp(ivar)=='s'.or.  &
          conttyp(ivar)=='l') then
      colorbar(ivar)='n'
   elseif(conttyp(ivar)=='v'.and.frame=='CFRAME_C') then
      colorbar(ivar)='n'
   else
      print*,frame,' plotting method (conttyp) '  &
            ,'must be one of c (contour), cb '  &
            ,'(contour, show color bar), f (fill), '  &
            ,'fb (fill, show color bar), t (tile) '  &
            ,'or tb (tile, show color bar) ',frtokens(2)(1:2)
      print*,cframe
      stop 'plotspc'
   endif
endif

if(len_trim(frtokens(2)).ge.2) then
   colorbar(ivar)=frtokens(2)(2:2)
   if(conttyp(ivar)=='c') then
      icmeth(ivar)=5
      cgrad(ivar)=30.
   endif
   if(colorbar(ivar).ne.'b') then
      print*,frame,' plotting method (colorbar) '  &
            ,'must be one of c (contour), f (fill), '  &
            ,'fb (fill, show color bar), t (tile) '  &
            ,'or tb (tile, show color bar) ',frtokens(2)(1:2)
      print*,cframe
      stop 'plotspc'
   endif
endif

if(len_trim(frtokens(3)).ge.1) then
   read(frtokens(3),*,err=100) conrlo(ivar)
else
   goto 100
endif
if(len_trim(frtokens(4)).ge.1) then
   read(frtokens(4),*,err=100) conrhi(ivar)
else
   goto 100
endif
if(len_trim(frtokens(5)).ge.1) then
   read(frtokens(5),*,err=100) conrinc(ivar)
else
   goto 100
endif
goto 101
100 continue
call var_contours(cvar(ivar),conrlo(ivar),conrhi(ivar),conrinc(ivar))
101 continue
   
if(conrinc(ivar).ne.0.) then
   nclevels=nint((conrhi(ivar)-conrlo(ivar))/conrinc(ivar))+2
   if(nclevels.gt.100) then
      print*,frame,': too many contour levels requested'
      print*,'Must be <= 100: hi, low, inc, levels'
      print*,conrhi(ivar),conrlo(ivar),conrinc(ivar),nclevels
      stop 'plotspc'
   endif
endif

if(cvar(ivar)(1:10).eq.'qveg_class'.or.  &
   cvar(ivar)(1:9).eq.'veg_class'.or.  &
   cvar(ivar)(1:5).eq.'sltex') goto 203

if(len_trim(frtokens(6)).ge.1) then
   call tokenize1 (frtokens(6),tokens,ntok,toksep)
   
   do itok=1,ntok
      if(tokens(itok)(1:1).eq.'m') then
         read(tokens(itok)(2:),*,err=201) icmeth(ivar)
         if(icmeth(ivar).lt.-7.or.icmeth(ivar).gt.7) then
            print*,frame,' graduated color method '  &
                  ,'parameter (m - integer) must be '  &
                  ,'between -7 and -7',icmeth(ivar)
            print*,cframe
            stop 'plotspc'
         endif
         if(conttyp(ivar)=='c') then
            if(icmeth(ivar)==0) then
               cgrad(ivar)=0.
               colorbar(ivar)='n'
            else
               cgrad(ivar)=30.
               !colorbar(ivar)='b'
            endif
         endif
         
      elseif(tokens(itok)(1:1).eq.'b') then
         read(tokens(itok)(2:),*,err=201) cbias(ivar)
         if(cbias(ivar).lt.0.3.or.cbias(ivar).gt.3) then
            print*,frame,' graduated color bias'  &
                  ,'parameter (b - real) must be '  &
                  ,'between 0.3 and 3.0',cbias(ivar)
            print*,cframe
            stop 'plotspc'
         endif
         
      elseif(tokens(itok)(1:1).eq.'c') then
         read(tokens(itok)(2:),*,err=201) ccent(ivar)
         ! no test on value
         
      elseif(tokens(itok)(1:1).eq.'i') then
         read(tokens(itok)(2:),*,err=201) icint(ivar)
         if(icint(ivar).lt.1.or.icint(ivar).gt.1000) then
            print*,frame,' major contour line interval '  &
                  ,'parameter (i - integer) must be '  &
                  ,'between 1 and 30',icint(ivar)
            print*,cframe
            stop 'plotspc'
         endif
         
      elseif(tokens(itok)(1:1).eq.'d') then
         read(tokens(itok)(2:),*,err=201) icdash(ivar)
         if(icdash(ivar).lt.0.or.icdash(ivar).gt.2) then
            print*,frame,' line solid/dash control '  &
                  ,'parameter (d - integer) must be '  &
                  ,'between 0 and 2',icdash(ivar)
            print*,cframe
            stop 'plotspc'
         endif
         
      elseif(tokens(itok)(1:1).eq.'g') then
         read(tokens(itok)(2:),*,err=201) cgrad(ivar)
         if(abs(cgrad(ivar)).gt.50.) then
            print*,frame,' graduated contour line color '  &
                  ,'parameter (g - real) must be '  &
                  ,'between -50.0 and 50.0',cgrad(ivar)
            print*,cframe
            stop 'plotspc'
         endif
         if(cgrad(ivar).le.0..and.conttyp(ivar)=='c') colorbar(ivar)='n'
         
      elseif(tokens(itok)(1:1).eq.'t') then
         read(tokens(itok)(2:),*,err=201) cthick(ivar)
         if(cthick(ivar).lt..5.or.cthick(ivar).gt.5.) then
            print*,frame,' line thickness '  &
                  ,'parameter (t - real) must be '  &
                  ,'between 0.5 and 5.0',cthick(ivar)
            print*,cframe
            stop 'plotspc'
         endif
         
      elseif(tokens(itok)(1:1).eq.'o') then
         read(tokens(itok)(2:),*,err=201) icover(ivar)
         if(icover(ivar).lt.0.or.icover(ivar).gt.1) then
            print*,frame,' line over draw '  &
                  ,'parameter (o - integer) must be '  &
                  ,'between 0 and 1',icover(ivar)
            print*,cframe
            stop 'plotspc'
         endif
         
      elseif(tokens(itok)(1:1).eq.'l') then
         read(tokens(itok)(2:),*,err=201) icline(ivar)
         if(icline(ivar).lt.-1.or.icline(ivar).gt.3) then
            print*,frame,' line labels '  &
                  ,'parameter (l - integer) must be '  &
                  ,'between -1 and 3',icline(ivar)
            print*,cframe
            stop 'plotspc'
         endif
         
      elseif(tokens(itok)(1:1).eq.'h') then
         read(tokens(itok)(2:),*,err=201) ichigh(ivar)
         if(ichigh(ivar).lt.-1.or.ichigh(ivar).gt.3) then
            print*,frame,' high/low labels '  &
                  ,'parameter (h - integer) must be '  &
                  ,'between -1 and 3',ichigh(ivar)
            print*,cframe
            stop 'plotspc'
         endif
         
      elseif(tokens(itok)(1:1).eq.'x') then
         read(tokens(itok)(2:),*,err=201) color
         call colortab_ind (color,icol(ivar))
         ! no test on value
         
      elseif(tokens(itok)(1:1).eq.'s') then
         read(tokens(itok)(2:),*,err=201) csize(ivar)
         if(csize(ivar).lt.0.002.or.csize(ivar).gt.0.1) then
            print*,frame,' size of contour and '  &
                  ,'high/low labels parameter '  &
                  ,'(s - real) must be between 0.002 '  &
                  ,'and 0.1',csize(ivar)
            print*,cframe
            stop 'plotspc'
         endif
         
      else
         print*,frame,' unrecognized token '  &
               ,tokens(itok)(1:1)
         print*,cframe
         stop 'plotspc'
      endif
      goto 202
      
      201 continue
      print*,frame,'(',nplot,') contains bad data: '  &
            ,tokens(itok)(2:)
      print*,cframe
      stop 'plotspc'
      
      202 continue
   enddo
endif

if(len_trim(frtokens(7)).ge.1) fillcols(1,ivar)=frtokens(7)
if(len_trim(frtokens(8)).ge.1) fillcols(2,ivar)=frtokens(8)
if(len_trim(frtokens(9)).ge.1) fillcols(3,ivar)=frtokens(9)

203 continue

return
end

!***************************************************************************

subroutine windset (cframe)

! sorts out the frame plotting attributes - parses "FRAME" string for
! the plot and put tokens into correct type variables

implicit none

character(len=*) :: cframe

include 'vcomm2.h'
include 'frame.h'

integer :: i,ntokfr
character(len=1)   :: toksepfr
character(len=64)  :: frtokens(50)
character(len=20)  :: color
data toksepfr/'/'/

! get landmark attributes
               
do i=1,50
   frtokens(i)=' '
enddo
call tokenize1 (cframe,frtokens,ntokfr,toksepfr)
   
if(len_trim(frtokens(2)).ge.1) then
   read(frtokens(2),*,err=102) intwindi
else
   goto 102
endif
if(len_trim(frtokens(3)).ge.1) then
   read(frtokens(3),*,err=102) intwindj
else
   goto 102
endif
if(len_trim(frtokens(4)).ge.1) then
   read(frtokens(4),*,err=102) ibscale
else
   goto 102
endif
if(len_trim(frtokens(5)).ge.1) then
   read(frtokens(5),*,err=102) color
   call colortab_ind (color,iwcolor)
else
   goto 102
endif
if(len_trim(frtokens(6)).ge.1) then
   read(frtokens(6),*,err=105) stemleng
else
   goto 106
endif
if(len_trim(frtokens(7)).ge.1) then
   read(frtokens(7),*,err=106) velomax
else
   goto 106
endif
if(len_trim(frtokens(8)).ge.1) then
   read(frtokens(8),*,err=106) headleng
else
   goto 106
endif
if(len_trim(frtokens(9)).ge.1) then
   read(frtokens(9),*,err=106) headleng0
else
   goto 106
endif
if(len_trim(frtokens(10)).ge.1) then
   read(frtokens(10),*,err=106) headang
else
   goto 106
endif
goto 103
102 continue
!print*,'setting 2-5 cframe_b params to defaults'
intwindi=1
intwindj=1
ibscale=3
call colortab_ind ('yellow',iwcolor)
goto 103
105 continue
!print*,'setting 6 cframe_b params to defaults'
stemleng=-1
goto 103
106 continue
!print*,'setting 7-10 cframe_b params to defaults'
velomax=0.
headleng=-1.
headleng0=-1.
headang=-1.
103 continue
               
return
end

!***************************************************************************

subroutine landmkset ()

! get landmark attributes

implicit none

include 'vcomm2.h'
include 'frame.h'

integer :: i,ntokfr,itok,ntok
character(len=1)   :: toksepfr,toksep
character(len=32)  :: tokens(20)
character(len=64)  :: frtokens(50)
data toksepfr/'/'/ toksep/':'/

frtokens(1:50)=' '

call tokenize1 (landmark(nplot),frtokens,ntokfr,toksepfr)

if(len_trim(frtokens(1)).ge.1) then
   
   do i=1,2
      if(len_trim(frtokens(i)).ge.1) then
         call tokenize1 (frtokens(i),tokens,ntok,toksep)
         if(frtokens(i)(1:1).eq.'M') then
            do itok=1,ntok
 
               if(tokens(itok)(1:1).eq.'M') then
                  read(tokens(itok)(2:),*,err=501) ilandmk
                  if(ilandmk.lt.0.or.ilandmk.gt.1) then
                     print*,'LANDMARK marker on/off parameter '  &
                           ,'(M - integer) must be between 0 '  &
                           ,'and 1',ilandmk
                     print*,landmark(nplot)
                     stop 'landmkset'
                  endif
 
               elseif(tokens(itok)(1:1).eq.'b') then
                  read(tokens(itok)(2:),*,err=501) bufflandmk
                  if(bufflandmk.lt.0..or.bufflandmk.gt..1) then
                     print*,'LANDMARK marker buffer parameter '  &
                           ,'(b - real) must be between 0.0 '  &
                           ,'and 0.1',bufflandmk
                     print*,landmark(nplot)
                     stop 'landmkset'
                  endif
 
               elseif(tokens(itok)(1:1).eq.'t') then
                  read(tokens(itok)(2:),*,err=501) itypelandmk
                  if(itypelandmk.lt.1.or.itypelandmk.gt.5) then
                     print*,'LANDMARK marker type parameter '  &
                           ,'(t - integer) must be between '  &
                           ,'1 and 5',itypelandmk
                     print*,landmark(nplot)
                     stop 'landmkset'
                  endif
 
               elseif(tokens(itok)(1:1).eq.'s') then
                  read(tokens(itok)(2:),*,err=501) sizelandmk
                  if(sizelandmk.lt.0.1.or.sizelandmk.gt.10.) then
                     print*,'LANDMARK marker size parameter '  &
                           ,'(s - real) must be between 0.1 '  &
                           ,'and 10.0',sizelandmk
                     print*,landmark(nplot)
                     stop 'landmkset'
                  endif
 
               elseif(tokens(itok)(1:1).eq.'x') then
                  read(tokens(itok)(2:),*,err=501) collandmk
 
               else
                  print*,'LANDMARK marker unrecognized token '  &
                        ,tokens(itok)(1:1)
                  print*,landmark(nplot)
                  stop 'landmkset'
               endif
               goto 502

               501 continue
               print*,'LANDMARK marker contains bad data: '  &
                     ,tokens(itok)(2:)
               print*,landmark(nplot)
               stop 'landmkset'
 
               502 continue
            enddo
 
         elseif(frtokens(i)(1:1).eq.'L') then
            do itok=1,ntok
 
               if(tokens(itok)(1:1).eq.'L') then
                  read(tokens(itok)(2:),*,err=601) ilandlab
                  if(ilandlab.lt.0.or.ilandlab.gt.1) then
                     print*,'LANDMARK label on/off parameter '  &
                           ,'(L - integer) must be between 0 '  &
                           ,'and 1',ilandlab
                     print*,landmark(nplot)
                     stop 'landmkset'
                  endif
 
               elseif(tokens(itok)(1:1).eq.'s') then
                  read(tokens(itok)(2:),*,err=601) sizelandlab
                  if(sizelandlab.lt..002.or.sizelandlab.gt..05) then
                     print*,'LANDMARK label size parameter '  &
                           ,'(s - real) must be between 0.002 '  &
                           ,'and 0.05',sizelandlab
                     print*,landmark(nplot)
                     stop 'landmkset'
                  endif
 
               elseif(tokens(itok)(1:1).eq.'x') then
                  read(tokens(itok)(2:),*,err=601) collandlab
 
               else
                  print*,'LANDMARK label unrecognized token '  &
                        ,tokens(itok)(1:1)
                  print*,landmark(nplot)
                  stop 'landmkset'
               endif
               goto 602

               601 continue
               print*,'LANDMARK label contains bad data: '  &
                     ,tokens(itok)(2:)
               print*,landmark(nplot)
               stop 'landmkset'

               602 continue
            enddo
 
         else
            print*,'LANDMARK unrecognized token '  &
                  ,frtokens(i)(1:1)
            print*,landmark(nplot)
            stop 'landmkset'
         endif
       endif
   enddo
endif

return
end

!***************************************************************************

subroutine custcolors ()

! reset colors / add custom colors

implicit none

include 'vcomm2.h'
include 'frame.h'

integer :: i,j,ntokfr,itok,ntok
real :: c1,c2,c3
character(len=1)   :: toksepfr,toksep
character(len=20)  :: tokens(20)
character(len=64)  :: frtokens(50)
data toksepfr/'/'/ toksep/':'/

do i=1,50
   frtokens(i)=' '
enddo

call tokenize1 (colors(nplot),frtokens,ntokfr,toksepfr)

do i=1,ntokfr
   if(len_trim(frtokens(i)).ge.1) then
      do j=1,20
         tokens(i)=' '
      enddo
      call tokenize1 (frtokens(i),tokens,ntok,toksep)
      
      if(ntok==2) then
      
         call colortab_hls ('rgb',tokens(2),c1,c2,c3)
         call colortab_add ('rgb',tokens(1),c1,c2,c3)
      
      elseif(ntok==5) then
         if(tokens(2)(1:3)=='hls'.or.tokens(2)(1:3)=='rgb') then
            
            read(tokens(3),*,err=601) c1
            if(c1.lt.0..or.c1.gt.360.) then
               print*,'COLORS hls hue index (real) must be '  &
                     ,'between 0.0 and 360.0 ',c1
               print*,colors(nplot)
               stop 'custcolors'
            endif
            read(tokens(3),*,err=601) c2
            if(c2.lt.0..or.c2.gt.100.) then
               print*,'COLORS hls lightness index (real) must be '  &
                     ,'between 0.0 and 100.0 ',c2
               print*,colors(nplot)
               stop 'custcolors'
            endif
            read(tokens(3),*,err=601) c3
            if(c3.lt.0..or.c3.gt.100.) then
               print*,'COLORS hls saturation index (real) must be '  &
                     ,'between 0.0 and 100.0 ',c3
               print*,colors(nplot)
               stop 'custcolors'
            endif
            goto 602

            601 continue
            print*,'COLORS contains bad data - hls color indices should '  &
                  ,'be formatted /<name>:hls:<0-360>:<0-100>:<0-100>/: '  &
                  ,frtokens(i)
            print*,colors(nplot)
            stop 'custcolors'

            602 continue
         
            call colortab_add (tokens(2)(1:3),tokens(1),c1,c2,c3)
         
         elseif(tokens(2)(1:3)=='rgb') then
            
            read(tokens(3),*,err=701) c1
            if(c1.lt.0..or.c1.gt.1.) then
               print*,'COLORS rgb red index (real) must be '  &
                     ,'between 0.0 and 1.0 ',c1
               print*,colors(nplot)
               stop 'custcolors'
            endif
            read(tokens(3),*,err=701) c2
            if(c2.lt.0..or.c2.gt.1.) then
               print*,'COLORS rgb green index (real) must be '  &
                     ,'between 0.0 and 1.0 ',c2
               print*,colors(nplot)
               stop 'custcolors'
            endif
            read(tokens(3),*,err=701) c3
            if(c3.lt.0..or.c3.gt.1.) then
               print*,'COLORS rgb blue index (real) must be '  &
                     ,'between 0.0 and 1.0 ',c3
               print*,colors(nplot)
               stop 'custcolors'
            endif
            goto 702

            701 continue
            print*,'COLORS contains bad data - rgb color indices should '  &
                  ,'be formatted /<name>:hls:<0-1>:<0-1>:<0-1>/: '  &
                  ,frtokens(i)
            print*,colors(nplot)
            stop 'custcolors'

            702 continue
         
            call colortab_add (tokens(2)(1:3),tokens(1),c1,c2,c3)
            
         else
            print*,'COLORS contains bad data - custom color schemes '  &
                  ,'include rgb or hls: ',tokens(2)
            print*,colors(nplot)
            stop 'custcolors'
         endif
      else
        print*,'COLORS contains bad data - you can either redefine a '  &
              ,'color, or define a new color: ',i
        print*,colors(nplot)
        stop 'custcolors'
      endif
   endif
enddo
  
return
end
