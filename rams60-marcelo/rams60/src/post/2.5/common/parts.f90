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

subroutine parts (ilayer,px1,px2,py1,py2,xmin,xmax,ymin,ymax,icoord  &
                 ,xm,ym,zm,nxp,nyp,nzp,horiz,vert,boxtop,boxhite)

! routine to plot HYPACT particles - the particles to plot are obtained
! in HYP_fill_fld - the particles representation is contorlled in the
! '/' delimited CFRAME_A and CFRAME_B fields, which has the options; all
! particles, with ('l') or without (a) connecting lines, or particles in
! the selected slab ('s')
!   i.e. CFRAME_A(1)='/part_lag:2:3/l/' will result in all particles being
!        drawn with interconnecting lines, for species no 2, source no 3.

use hypparts

implicit none

integer          :: ilayer,icoord,nxp,nyp,nzp
real             :: px1,px2,py1,py2,xmin,xmax,ymin,ymax,boxtop,boxhite
real             :: xm(*),ym(*),zm(*)
character(len=*) :: vert,horiz

include 'interface.h'
include 'frame.h'

integer           :: i,ic,icmj,n,l,nthpart,lcdname,lcdunits,ntokfr,isrc  &
                    ,lcn,ltitle
real              :: tmax,tmin,src,spec,box5,partv,partn,parth
real, allocatable :: atph(:),atpv(:)
character(len=80) :: title
character(len=8)  :: cn,cnparts
character(len=1)  :: toksep
character(len=16) :: frtokens(50)

integer, dimension(maxlayers) :: nparts,nsources
common /cparts/ nparts,nsources

data toksep /':'/

!print*,'parts ',cvar(ilayer),' ',conttyp(ilayer)(1:1),nparts(ilayer)

call tokenize1 (cvar(ilayer)(1:len_trim(cvar(ilayer)))//':'  &
               ,frtokens,ntokfr,toksep)
read(frtokens(3)(1:len_trim(frtokens(3))),*) isrc

call set (px1,px2,py1,py2,xmin,xmax,ymin,ymax,1)

! get the normal limits for the slab

if(horiz.eq.'X'.and.vert.eq.'Y') then
   tmax=zm(min(icoord,nzp))
   tmin=zm(max(1,icoord-1))
elseif(horiz.eq.'X'.and.vert.eq.'Z') then
   tmax=ym(min(icoord,nyp))
   tmin=ym(max(1,icoord-1))
elseif(horiz.eq.'Y'.and.vert.eq.'Z') then
   tmax=xm(min(icoord,nxp))
   tmin=xm(max(1,icoord-1))
endif
if (tmax.eq.tmin.and.conttyp(ilayer)(1:1).eq.'s') then
   print*,'Need to specify a slab above gound'
   stop 'parts'
endif
      
do i=1,nsources(ilayer)
      
   ! allocate required memory
   if(allocated(atph)) deallocate(atph)
   if(allocated(atpv)) deallocate(atpv)
   allocate(atph(nparts(ilayer)))
   allocate(atpv(nparts(ilayer)))

   n=0
   do l=1,nparts(ilayer)

   ! sort out the coordinates

      if(horiz.eq.'X'.and.vert.eq.'Y') then
         parth=atp(ilayer,(l-1)*6+1)
         partv=atp(ilayer,(l-1)*6+2)
         partn=atp(ilayer,(l-1)*6+4)
      elseif(horiz.eq.'X'.and.vert.eq.'Z') then
         parth=atp(ilayer,(l-1)*6+1)
         partv=atp(ilayer,(l-1)*6+3)
         partn=atp(ilayer,(l-1)*6+2)
      elseif(horiz.eq.'Y'.and.vert.eq.'Z') then
         parth=atp(ilayer,(l-1)*6+2)
         partv=atp(ilayer,(l-1)*6+3)
         partn=atp(ilayer,(l-1)*6+1)
      endif
      
      ! set source and species

      src=atp(ilayer,(l-1)*6+5)
      spec=atp(ilayer,(l-1)*6+6)
      
      ! plot every nth particle
     
      nthpart=icint(ilayer)
      
      !print*,'mod',mod(l,nthpart)
      !print*,'src',nint(src),isrc
      !print*,'within horiz',parth,xmin,xmax
      !print*,'within vert',partv,ymin,ymax
      !print*,'parts  ',conttyp(ilayer)(1:1),partn,tmin,tmax
      
      ! add the particle to the plotting array
      
      if(mod(l,nthpart).eq.0) then
         if((nint(src).eq.isrc.or.isrc.eq.0).and.  &            ! this source
            (parth.ge.xmin.and.parth.le.xmax.and.   &           ! within horiz
             partv.ge.ymin.and.partv.le.ymax).and.  &           ! within vert
            (conttyp(ilayer)(1:1).eq.'a'.or.  &
             conttyp(ilayer)(1:1).eq.'l'.or.  &                 ! all parts
             (partn.ge.tmin.and.partn.le.tmax))) then           ! within slab
            n=n+1
            atph(n)=parth
            atpv(n)=partv
            !print*,n,parth,partv
         endif
      endif

   enddo

! draw the particles and lines bewteen if required 

   call gsmk(1)       ! marker
   call gsmksc(1.)    ! marker scale
   if(cgrad(ilayer).lt.1.e-10) then
      icmj=icol(ilayer) 
   else
      call colortab_ind (fillcols(1,ilayer),icmj)
   endif
   call gspmci(icmj)  ! marker colour
   call gsplci(icmj)  ! line colour
   call gslwsc(3.)    ! line thickness
   if(n.eq.1) then
      !print*,'plotting single pt',n
      call point(atph,atpv)    ! draws single point
   elseif(n.gt.1) then
      !print*,'plotting group of pts',n
      call gpm(n,atph,atpv)    ! draws the points
      if(conttyp(ilayer)(1:1).eq.'l') then
         !print*,'connecting pts'
         call gpl(n,atph,atpv) ! connects the points
      endif
   endif

enddo

! draw label boxes and labels

if(ipinfo.eq.1) then

   call set (0.,1.,0.,1.,0.,1.,0.,1.,1)

   call colortab_ind ('title0',ic)
   call gslwsc(1.)      ! line thickness
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

   box5=boxtop-.5*boxhite

   call plotif(0.,0.,2)
   call gsplci(icmj)
   call plotif(0.,0.,2)
   call gstxci(icmj)
   call plotif(0.,0.,2)

   lcdname=len_trim(cdname(ilayer))-1
   lcdunits=len_trim(cdunits(ilayer))-1
   write(cnparts,'(i8)') n
   call deblank(cnparts,cn,lcn)
   title=cdunits(ilayer)(1:lcdunits)//' - '//cdname(ilayer)(1:lcdname)  &
         //' ('//cn(1:lcn)//' parts)'
   ltitle=len_trim(title)
   call plchhq (.03,box5,'particles  ',-0.7,0.,-1.)
   call plchhq (.21,box5,title(1:ltitle),-0.7,0.,-1.)

endif

print*,'Drawn ',title(1:ltitle)

return
end
