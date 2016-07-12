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

subroutine RAMS_mkmap (platn,plonn,xt1,yt1,xt2,yt2,mfill  &
                      ,px1,px2,py1,py2,icplotinfo)

parameter(hires=1,region=2,ncar=3)
common/pole/xplat,xplon
data ncall/0/
real mapbnds(3,4)
dimension iaia(10),igia(10)
common/mapping/ibugout
common /mapspc/iama(300000),xcra(50000),ycra(50000)
common/misc/idcwroads,iccasroads,iccasbuild
external fill
save

!print*,'in mkmap',mfill

! gflash buffers 10 and 11 - line maps <mfill >= 1)
! gflash buffer 1 is the map fill (mfill >= 2)
! gflash buffer 2 is for roads
! gflash buffer 3 is for buildings
! gflash buffer 4 is for landmarks

if(ncall == 0) then
   ncall=1
   xt1o=0.
   xt2o=0.
   yt1o=0.
   yt2o=0.
   mfillo=-1
   py1o=-1
endif

if(mfill == mfillo.and.xt1 == xt1o.and.xt2 == xt2o.and.  &
   yt1 == yt1o.and.yt2 == yt2o.and.  &
   platn == plato.and.plonn == plono.and.  &
   py1 == py1o.and.icplotinfo == 0) then

! not sure if this is what we want here   
!   call set (ppx1,ppx2,ppy1,ppy2,xt1,xt2,yt1,yt2,1)
   call set (ppx1,ppx2,ppy1,ppy2,xx1,xx2,yy1,yy2,ittt)

else

   xplat=platn
   xplon=plonn

   call xy_ll (xlat1,xlon1,platn,plonn,xt1,yt1)
   call xy_ll (xlat2,xlon2,platn,plonn,xt2,yt2)
   call xy_ll (xlat3,xlon3,platn,plonn,xt2,yt1)
   call xy_ll (xlat4,xlon4,platn,plonn,xt1,yt2)
   
   if(xlat1 == xlat2.or.xlon1 == xlon2) then
      print*,'ERROR in RAMS_mkmap - grid has no size!'
      ibugout=1
      return
   endif
   
   ! map to use
   
   ! if a file called MARS exists in the local directory use MARS maps
   open(unit=97,file='MARS',status='old',err=101)
   close(97)
   
   ! Set up bounds for CCAS hi res maps
   mapbnds(1,1)=28.16419
   mapbnds(1,2)=-81.30000
   mapbnds(1,3)=28.81880
   mapbnds(1,4)=-79.00000
 
   ! Set up bounds for CCAS region maps
   mapbnds(2,1)=27.09676
   mapbnds(2,2)=-82.31812
   mapbnds(2,3)=29.86330
   mapbnds(2,4)=-78.72011
   mapbnds(3,1)=26.01361
   mapbnds(3,2)=-81.72486
   mapbnds(3,3)=30.00777
   mapbnds(3,4)=-79.00000
   
   ! check what map
   if(xlat1 >= mapbnds(1,1) .and. xlat2 <= mapbnds(1,3) .and.  &
      xlon1 >= mapbnds(1,2) .and. xlon2 <= mapbnds(1,4)) then
      imap = hires
      print*,'Choosing MARS HIRES map with corners and pole'
      print*,'corners- ',xt1,yt1,xt2,yt2
      print*,'pole-    ',platn,plonn
      print*,'corners- ',xlat1,xlon1,xlat2,xlon2
      print*,'bounds-  ',(mapbnds(1,i),i=1,4)
   elseif(xlat1 >= mapbnds(2,1) .and. xlat2 <= mapbnds(2,3) .and.  &
          xlon1 >= mapbnds(2,2) .and. xlon2 <= mapbnds(2,4)) then
      imap = region
      print*,'Choosing MARS REGION map with corners and pole'
      print*,'corners- ',xt1,yt1,xt2,yt2
      print*,'pole-    ',platn,plonn
      print*,'corners- ',xlat1,xlon1,xlat2,xlon2
      print*,'bounds-  ',(mapbnds(2,i),i=1,4)
   elseif(xlat1 >= mapbnds(3,1) .and. xlat2 <= mapbnds(3,3) .and.  &
          xlon1 >= mapbnds(3,2) .and. xlon2 <= mapbnds(3,4)) then
      imap = region
      print*,'Choosing MARS REGION map with corners and pole'
      print*,'corners- ',xt1,yt1,xt2,yt2
      print*,'pole-    ',platn,plonn
      print*,'corners- ',xlat1,xlon1,xlat2,xlon2
      print*,'bounds-  ',(mapbnds(3,i),i=1,4)
   else
      imap = ncar
      print*,'Choosing NCAR map with corners and pole:'
      print*,'corners- ',xt1,yt1,xt2,yt2
      print*,'pole-    ',platn,plonn
      print*,'corners- ',xlat1,xlon1,xlat2,xlon2
      print*,'bounds-  ',(mapbnds(2,i),i=1,4)
      print*,'bounds-  ',(mapbnds(3,i),i=1,4)
   endif
   goto 102
   101 continue
   imap=ncar
   102 continue

   if(abs(mfill) >= 2) then
      call gsfais (1)              ! force solid fill
      call arinam (iama,300000)    ! initialize the area map
   endif

   ! guts of map drawing

   call mappos (px1,px2,py1,py2)
   call maproj ('ST',platn,plonn,0.)
   call mapset ('CO',xlat1,xlon1,xlat2,xlon2)
   call mapsti ('GR',10)
   if(imap == ncar) then
      call mapstc ('OU','PS')
   else
      call mapstc ('OU','NO')
   endif
   call mapint
   
   ! map outline
   call gflas1 (10)

   ! set line colors and thinkness
   if(abs(mfill) >= 2) then
      call colortab_ind ('bound0',ic)
   else
      call colortab_ind ('bound1',ic)
   endif
   call gsplci (ic)
   if(mfill > 0) then
      call gslwsc (1.)
   else
      call gslwsc (1.5)
   endif

   ! draw map
   if(imap == region) then
      call addregion (mfill)
   elseif(imap == hires) then
      call addhires (mfill)
   elseif(imap == ncar) then
      call mapdrw
   endif

   call gflas2
   
   if(abs(mfill) >= 2) then
      ! map fill
      call gflas1(1)
 
      ! Fixes lack of fill at lowest level by adding an all land
      ! box out to the edges of the plot.  The subsequent call
      ! to mapbla changes some of those to water as they should
      ! be.  B.T. Jan 1997
      call mapita (xlat1,xlon1,0,iama,1,1,2)
      call mapita (xlat3,xlon3,1,iama,1,1,2)
      call mapita (xlat2,xlon2,1,iama,1,1,2)
      call mapita (xlat4,xlon4,1,iama,1,1,2)
      call mapita (xlat1,xlon1,1,iama,1,1,2)
      call mapiqa (iama,1,1,2)
      call mapbla (iama)  ! draw the map into the area map
      
      call arscam(iama,xcra,ycra,50000,iaia,igia,10,fill)
      call gflas2
   endif

   xt1o=xt1
   xt2o=xt2
   yt1o=yt1
   yt2o=yt2
   plato=platn
   plono=plonn
   mfillo=mfill
   py1o=py1
   
   call getset (ppx1,ppx2,ppy1,ppy2,xx1,xx2,yy1,yy2,ittt)

   ! road, etc
   
   ! set line colors
   idcwroads=0
   iccasroads=0
   iccasbuild=0
   if(abs(mfill) >= 2) then
      call colortab_ind ('roads0',ic)
   else
      call colortab_ind ('roads1',ic)
   endif
   
   if(imap == hires) then
      ! CCAS maps
      call gsplci (ic)
      iccasroads=1
      call drawroads(0,0)
      ! set building colors
      call colortab_ind ('yellow',ic2)
      call gsplci (ic2)
      iccasbuild=1
      call drawbuild(0,0)
   else
      if((xlat2-xlat1) < 10..and.(xlon2-xlon1) < 10.) then
         idcwroads=1
         call gsplci (ic)
         !call lndmrk(0,iimap)
         ! DTRA maps
         call dcw_map (0,iimap,xlat1,xlat2,xlon1,xlon2)
      endif
   endif

   if(mfill < 0) then
      offset=0.002
      call mappos (px1+offset,px2+offset,py1-offset,py2-offset)
   
      ! thick map outline
      call gflas1 (11)
      
      ! set line colors and thinkness
      if(abs(mfill) >= 2) then
         call colortab_ind ('shadow0',ic)
      else
         call colortab_ind ('shadow1',ic)
      endif
      call gsplci (ic)
      call gslwsc (1.5)

      ! draw map
      if(imap == region) then
         call addregion (mfill)
      elseif(imap == hires) then
         call addhires (mfill)
      elseif(imap == ncar) then
         call mapdrw
      endif

      call gflas2
      call gslwsc (1.)
      ! not sure why we had this      
      !call set (ppx1,ppx2,ppy1,ppy2,xt1,xt2,yt1,yt2,1)
   endif
   
endif

call gslwsc (1.)

return
end

!***************************************************************************

subroutine RAMS_sendmap (jmap)

! gflash map

implicit none

integer :: jmap

!print*,'gflashing map',jmap
call gflas3 (jmap)

return
end

!***************************************************************************

subroutine RAMS_sendroads ()

common/misc/idcwroads,iccasroads,iccasbuild

!print*,'gflashing other'

if(idcwroads == 1) then
   !call lndmrk(1,0)
   call dcw_map (1,0,0.,0.,0.,0.)
endif

if(iccasroads == 1) call drawroads(1,0)
if(iccasbuild == 1) call drawbuild(1,0)

return
end

!***************************************************************************

subroutine fill (xwrk,ywrk,nwrk,iarea,igrp,ngrps)

! Fills the area map

integer nwrk, ngrps
integer iarea(ngrps),igrp(ngrps)
real    xwrk(nwrk),ywrk(nwrk)

! Ignore current region if it is defined by 2 or fewer points
if(nwrk <= 3) return

! Get area ID -- group ID of 1 == map info
!  New for NCARG 4.2????  return if any iarea's are -1
idmap=-1
do i=1,ngrps
   if(iarea(i) == -1) return
   if(igrp(i) == 1) idmap=iarea(i)
enddo

! Fill the region with appropriate color
if(idmap > 0) then
   if(mapaci(idmap) == 1) then
      call colortab_ind ('water',ic)
      call gsfaci(ic)
      call gfa(nwrk-1,xwrk,ywrk)
   else
      call colortab_ind ('land',ic)
      call gsfaci(ic)
      call gfa(nwrk-1,xwrk,ywrk)
   endif
endif

return
end

!***************************************************************************

subroutine addregion (mfill)

real lat,lon, orglat, orglon
dimension iaia(10),igia(10)
character mapfile*100
common /mapspc/IAMA(300000),xcra(50000),ycra(50000)
external fill

include 'interface.h'

ierr=0

! Open the Regional CIA map file
call fegetenv('MAP_REGION',mapfile,ierr)
if(ierr.ne.0) then
  print *, 'mkmap error: Cannot get region map file name'
  return
endif
open(unit=92,file=mapfile,status='old',err=1)
goto 10
1 print *, 'mkmap error: Cannot open region map file: ',mapfile
return

! Add my map lines to area map and color it
10 read(92,11,end=20) ityp,ivects,iside
11 format(10x,i3,i5,1x,i1)
read(92,*) lat,lon
orglat=lat
orglon=lon
if(abs(mfill) >= 2) then
   if(iside.ne.1) then
      call mapita(lat,lon,0,iama,1,2,223)
   else
      call mapita(lat,lon,0,iama,1,223,2)
   endif
else
   if(iside.ne.1) then
      call mapit(lat,lon,0)
   else
      call mapit(lat,lon,0)
   endif
endif
do i=1,ivects
   read(92,*) lat,lon
   if(abs(mfill) >= 2) then
      if(iside.ne.1) then
         call mapita(lat,lon,1,iama,1,2,223)
      else
         call mapita(lat,lon,1,iama,1,223,2)
      endif
   else
      if(iside.ne.1) then
         call mapit(lat,lon,1)
      else
         call mapit(lat,lon,1)
      endif
   endif
enddo

! CLOSE the first and last if they are close together
if(abs(lat-orglat) < 0.1.and.abs(lon-orglon) < 0.1) then
   if(abs(mfill) >= 2) then
      if(iside.ne.1) then
         call mapita(orglat,orglon,1,iama,1,2,223)
      else
         call mapita(orglat,orglon,1,iama,1,223,2)
      endif
   else
      if(iside.ne.1) then
         call mapit(orglat,orglon,1)
      else
         call mapit(orglat,orglon,1)
      endif
   endif
endif

if(abs(mfill) >= 2) then
   if(iside.ne.1) then
      call mapiqa(iama,1,2,223)
   else
      call mapiqa(iama,1,223,2)
   endif
else
   if(iside.ne.1) then
      call mapiq
   else
      call mapiq
   endif
endif
goto 10

20 continue

40 continue
close(92)

return
end

!***************************************************************************

subroutine addhires (mfill)

character name*40
integer segnum,detail
real lat,lon, orglat, orglon
dimension iaia(10),igia(10)
character mapfile*100
integer idsegs(14)
common /mapspc/IAMA(300000),xcra(50000),ycra(50000)
data idsegs /1,2,3,4,5,6,7,8,-9,10,11,12,13,14/
external fill

include 'interface.h'

ierr=0

! Open the MARRS waterbodies map file
call fegetenv('MAP_HIRES',mapfile,ierr)
if(ierr.ne.0) then
   print *, 'mkmap error: Cannot get hires map file name'
   return
endif
open(unit=92,file=mapfile,status='old',err=1)
goto 100
1 print *, 'mkmap error: Cannot open hires map file: ',mapfile
return

! Add my map lines to area map and color it
100 read(92,101,err=200,end=200) segnum,detail,iright,name,xmin,xmax,ymin,ymax
101 format(3i10.0,/a40/,4f10.0)
if( idsegs(segnum) > 0 ) then
   !print 991, segnum,iright,detail,xmin,xmax,ymin,ymax
   !991 format(3(i3,1x),4f10.0)
   read(92,*,end=200) a,b,lat,lon
   orglat=lat
   orglon=lon
   if(abs(mfill) >= 2) then
      if(iright == 0) then
         call mapita(lat,lon,0,iama,1,2,223)
      else
         call mapita(lat,lon,0,iama,1,223,2)
      endif
   else
      if(iright == 0) then
         call mapit(lat,lon,0)
      else
         call mapit(lat,lon,0)
      endif
   endif
   110 read(92,*,end=200) a,b,lat,lon
   if(a.ne.-9999.) then
      if(abs(mfill) >= 2) then
         if(iright == 0) then
            call mapita(lat,lon,1,iama,1,2,223)
         else
            call mapita(lat,lon,1,iama,1,223,2)
         endif
      else
         if(iright == 0) then
            call mapit(lat,lon,1)
         else
            call mapit(lat,lon,1)
         endif
      endif
      goto 110
   endif
   if(abs(mfill) >= 2) then
      if(iright == 0) then
         call mapita(orglat,orglon,1,iama,1,2,223)
         call mapiqa(iama,1,2,223)
      else
         call mapita(orglat,orglon,1,iama,1,223,2)
         call mapiqa(iama,1,223,2)
      endif
   else
      if(iright == 0) then
         call mapit(orglat,orglon,1)
         call mapiq
      else
         call mapit(orglat,orglon,1)
         call mapiq
      endif
   endif
else
   ! Skip this set of points
   113 read(92,*) a
   if(a.ne.-9999.) goto 113
endif
goto 100

200 close(92)

return
end

!***************************************************************************

subroutine drawroads (iflash,detlev)

integer detlev,segnum,detail
real lat,lon
character mapfile*100

include 'interface.h'

ierr=0

if(detlev > 1) goto 9000

!call gqplci(ierr,ioldci)
!call gsplci(12)

if(iflash == 1) then
   !print*,'gflash-ing roads'
   call gflas3(2)
else
   call gflas1(2)
   ! Open the road map file
   call fegetenv('MAP_ROADS',mapfile,ierr)
   if(ierr.ne.0) then
      print *, 'mkmap error: Cannot get road map file name'
      return
   endif
   open(unit=92,file=mapfile,status='old',err=1)
   goto 100
   1 print *, 'mkmap error: Cannot open road map file: ',mapfile
   return

   100 continue
   read(92,111,end=200) segnum,detail
   111 format(2i10.0)
   read(92,*,end=200)
   read(92,112,end=200) xmin,xmax,ymin,ymax
   112 format(4f10.0)

   !if(detail <= detlev) then
   if(detail >= 0) then
      !print *, 'Drawing segnum ',segnum,' detail ',detail
      read(92,*,end=200)a,b,lat,lon
      call mapit(lat,lon,0)

      110 read(92,*) a,b,lat,lon

      if(a == -9999.) then
         call mapiq
      else if(a == 1.) then
         call mapiq
         read(92,*,end=200) a,b,lat,lon
         call mapit(lat,lon,0)
         goto 110
      else
         call mapit(lat,lon,1)
         goto 110
      endif
   else
      !print *, 'Skipping segnum ',segnum,' detail ',detail
      120 read(92,*) a,b,lat,lon
      if(a.ne.-9999.) goto 120
   endif

   goto 100

   200 continue
   close(92)
   call gflas2
   !bob: I think this should be off here: call gflas3(2)
   !bob: I think this should be off here: call gflas3(2)
endif

!call gsplci(ioldci)

9000 return
end

!***************************************************************************

subroutine drawbuild (iflash,detlev)

integer detlev,detail
real lat,lon
character mapfile*100

include 'interface.h'

ierr=0

if(detlev > 1) goto 9000

!call gqplci(ierr,ioldci)
!call gsplci(4)

if(iflash == 1) then
   !print*,'gflash-ing buildings'
   call gflas3(3)
else
   call gflas1(3)
   ! Open the building map file
   call fegetenv('MAP_BUILDINGS',mapfile,ierr)
   if (ierr .ne. 0) then
      print *, 'mkmap error: Cannot get building map file name'
      return
   endif
   open(unit=92,file=mapfile,status='old',err=1)
   goto 100
   1 print *, 'mkmap error: Cannot open building map file: ',mapfile
   return

   100 continue
   read(92,101,end=200) detail
   if(detail < 0) goto 200
   101 format(i10.0)
   read(92,*)

   !if (detail  >=  detlev) then
   if(detail >= 0) then
      ! DO THE FIRST POINT
      read(92,*,end=200)a,b,lat,lon
      !print *,'first point ',lat,lon
      call mapit(lat,lon,0)

      110 read(92,*) a,b,lat,lon
      if(a == -9999.) then
         call mapiq
         goto 100
      else
         call mapit(lat,lon,1)
         goto 110
      endif
   else
      120 read(92,*) a,b,lat,lon
      if(a.ne.-1.) goto 120
   endif

   goto 100

   200 continue
   close(92)
   call gflas2
   !bob: I think this should be off here:        call gflas3(3)
endif

!call gsplci(ioldci)

9000 return
end

!***************************************************************************

subroutine lndmrk (iflash,detlev)

integer detlev,detail
real lat,lon
character mapfile*100
character line*80, alatlon*15,name*80, ns*1, ew*1

include 'interface.h'

ierr=0

if(detlev > 1) goto 9000

!call gqplci(ierr,ioldci)
!call gsplci(4)

if(iflash == 1) then
   !print*,'gflash-ing landmarks'
   call gflas3(4)
else
   call gflas1(4)
   ! Open the landmarks file
   !call fegetenv('MAP_BUILDINGS',mapfile,ierr)
   if(ierr.ne.0) then
      print *, 'mkmap error: Cannot get building map file name'
      stop 404
   endif
   !open(unit=92,file=mapfile,status='old',err=1)
   open(unit=92,file='new_mexico',status='old',err=1)
   goto 100
   1 print *, 'mkmap error: Cannot open landmarks file: ',lndmrkfl
   stop 405

   100 continue
   read(92,'(a80)',end=200) line

   read(line,105,err=106)istate,icnty,ityp,alatlon,ipop,ielev,name
   105 format(i2,i3,i4,a15,i8,i7,a80)
   106 continue
   if(detail < 0) goto 200

   do ic=1,80
      if( name(ic:ic+1) == '  ') goto 107
   enddo
   107 continue
   lc=ic-1

   read(alatlon,'(3i2,a1,i3,2i2,a1)')llat,latm,lats,ns,llon,lonm,lons,ew
   alat=real(llat)+real(latm)/60.+real(lats)/3600.
   alon=real(llon)+real(lonm)/60.+real(lons)/3600.
   if(ns == 'S') alat=-alat
   if(ew == 'W') alon=-alon

   if(detail >= 0) then
      call maptra(alat,alon,xpos,ypos)
      size=0.0075
      angd=0.
      cntr=-1
      if(xpos < 999.0.and.ypos < 999.0) then
         if(ityp == 44.or.ityp == 55) then
            call plchlq (xpos,ypos,name(1:lc),size,angd,cntr)
            !print*,'plt sta@lat,lon=',name(1:lc),alat,alon,xpos,ypos
         !else
            !print*,'not plotting',ityp,name(1:lc)
         endif
      endif
      goto 100
   endif

   200 continue

  close(92)
  call gflas2
  call gflas3(4)

endif

!call gsplci(ioldci)

9000 return
end

!***************************************************************************

subroutine dcw_map (iflash,detlev,xlat1,xlat2,xlon1,xlon2)

integer detlev,detail
character*1 clat, clon
character*80 sdir, iname

include 'interface.h'

ierr=0

if(detlev > 1) goto 9000

!call gqplci(ierr,ioldci)
!call gsplci(4)

if(iflash == 1) then
   !print*,'gflash-ing dcw'
   call gflas3(4)
else
   call gflas1(4)
   ! Open the DCW database

   !========================================
   ! Selecting high resolution tiles to read

   !xminm=-175. ;xmaxm=0. ;yminm=0. ;ymaxm=90.
   !xminm=mod(xlon1,5.)*5.
   !xmaxm=mod(xlon2,5.)*5.
   !yminm=mod(xlat1,5.)*5.
   !ymaxm=mod(xlat2,5.)*5.
   xminm=xlon1
   xmaxm=xlon2
   yminm=xlat1
   ymaxm=xlat2
   !print*,'xlat1....=',xlat1,xlat2,xlon1,xlon2
   !print*,'xlmin....=',yminm,ymaxm,xminm,xmaxm

   imin=int(xminm/5.0)
   if(xminm < 0) imin=imin-1
   imax=int(xmaxm/5.0-1.e-6)
   if(xmaxm < 0) imax=imax-1
   jmin=int(yminm/5.0)
   if(yminm < 0) jmin=jmin-1
   jmax=int(ymaxm/5.0-1.e-6)
   if(ymaxm < 0) jmax=jmax-1

   jmin=max0(jmin,-13)

   !==========================================================
   ! Looping for tiles and building high resolution file names

   do jj=jmin,jmax
  
      if(jj == -13) then
         lat0=-90
      else
         lat0=5*jj
      endif

      if(lat0 >= 0) then
         clat='n'
      else
         clat='s'
      endif

      if(jj == -13) then
         i1=int(xmin/90.0)
         i2=int(xmax/90.0-1.e-6)
         klon=90
      else
         i1=imin
         i2=imax
         klon=5
      endif

      !print*,'dcw i1,i2=',i1,i2
      do ii=i1,i2
 
         lon0=klon*ii
         if(lon0 >= 0) then
            clon='e'
         else
            clon='w'
         endif

         call fegetenv('DCW_DATADIR',sdir,ierr)
         if(ierr == 1) goto 210
 
         nchs=index(sdir,' ')-1
         !nchs=len(trim(sdir))

         ! Basic map segements
         !print*,'Basic map segements'
         !write(iname,200) sdir(1:nchs),clon,iabs(lon0),clat,iabs(lat0),'.po'
         !call read_dcw(iname)

         ! Road segements
         !print*,'Road segements'
         write(iname,200) sdir(1:nchs),clon,iabs(lon0),clat,iabs(lat0),'.rd'
         call read_dcw(iname)

         ! Railrod map segements
         !print*,'Railrod map segements'
         !write(iname,200) sdir(1:nchs),clon,iabs(lon0),clat,iabs(lat0),'.rr'
         !call read_dcw(iname)

         ! Populated place areas
         !print*,'Populated place areas'
         !write(iname,200) sdir(1:nchs),clon,iabs(lon0),clat,iabs(lat0),'.pp'
         !call read_dcw(iname)

         ! Airports symbols/text
         !print*,'Airports symbols/text'
         !write(iname,200) sdir(1:nchs),clon,iabs(lon0),clat,iabs(lat0),'.apt'
         !call read_dcw(iname)

         ! Populated places symbols/text
         !print*,'Populated places symbols/text'
         !write(iname,200) sdir(1:nchs),clon,iabs(lon0),clat,iabs(lat0),'.ppt'
         !call read_dcw(iname)
 
         200 format(a,'/',a,i3.3,a,i2.2,a)

         210 continue

      enddo
   enddo

   !===========================
   !  Low resolution file names
  
   ! Basic map segements
   !print*,'Basic map segements'
   iname = sdir(1:nchs)//'/browse.po'
   !print*,iname

   ! Populated places symbols/text
   !print*,'Populated places symbols/text'
   iname = sdir(1:nchs)//'/browse.ppt'
   !print*,iname

   !=============================
   !if (detail  >=  0) then
   !    call maptra(alat,alon,xpos,ypos)
   !    size=0.0075;angd=0.;cntr=-1
   !    if(xpos < 999.0.and.ypos < 999.0)then
   !      if(ityp == 44.or.ityp == 55)then
   !        call plchlq (xpos,ypos,name(1:lc),size,angd,cntr)
   !        print*,'plt sta@lat,lon=',name(1:lc),alat,alon,xpos,ypos
   !      else
   !        print*,'not plotting',ityp,name(1:lc)
   !      endif
   !    endif
   !    goto 100
   !endif

   close(92)
   call gflas2
   !call gflas3(4)

endif

!call gsplci(ioldci)

9000 return
end

!***************************************************************************

subroutine read_dcw (iname)

character*(*) iname
dimension yr(10000), xr(10000)
character*3 ftype
character*80 filename, text
real*4 xmin,ymin

ierr=0

! Opening map files - direct access 1 word/record

1000 continue

lun_tmp=27
filename=iname
lch=lastchar(filename)
!print*,'about to open filename=',filename(1:lch)

idot=index(filename,'.')
ftype=filename(idot+1:idot+3)

! Specify the record length in bytes, with 4 bytes/word
if(ftype == 'po '.or.ftype == 'rd '.or.ftype == 'rr '.or.ftype == 'pp ') then
!  Map segements and area files
   ireclen = 1
   !print*,'ftype,irecl,MACHLEN=',ftype,ireclen,IRAN_RECSIZE()
elseif(ftype == 'ppt') then
!  Populated places text/symbol files
   ireclen = 12
   !print*,'ftype,irecl,MACHLEN=',ftype,ireclen,IRAN_RECSIZE()
else
!  All other text/symbol files
   ireclen=16
   !print*,'ftype,irecl,MACHLEN=',ftype,ireclen,IRAN_RECSIZE()
endif

ireclen=ireclen*IRAN_RECSIZE()

open(unit=lun_tmp,file=filename(1:lch),access='direct'  &
    ,recl=ireclen, form='unformatted',status='old',iostat=ierr)

if(ierr.ne.0) print*,'not finding file ',filename(1:lch)

IF(FTYPE == 'po '.OR.FTYPE == 'rd '.OR.FTYPE == 'rr '.or.ftype == 'pp ') then
                 
!=====================================
!  Reading line segment and area files

!  File Header

   read(lun_tmp,rec=1,err=210) nseg        ! Number of segments/areas in file
   !print*,'nseg=',nseg
   call dcw_swap32(nseg,1)
   read(lun_tmp,rec=2,err=210) xmin        ! S/W corner of file
   !print*,'xmin=',xmin
   call dcw_swap32(xmin,1)
   read(lun_tmp,rec=3,err=210) ymin
   !print*,'ymin=',ymin
   call dcw_swap32(ymin,1)
   !print*,'nseg,xmin,ymin=',nseg,xmin,ymin

   irec = 3

!  Read segments

   !print*,'nsegs',nseg
   do i=1,nseg
      !print*,'nseg',i
      irec=irec+1
      read(lun_tmp,rec=irec,err=110) n        ! Number of points in segment/area
      !print*,'irec,n:',irec,n
      call dcw_swap32(n,1)
      !print*,'irec,n:',irec,n
      irec=irec+1
      read(lun_tmp,rec=irec,err=110) itype    ! Segment type - used to set color
      call dcw_swap32(itype,1)
      !print*,'irec,itype:',irec,itype
                                              ! of line or area fill type
      do j=1,n
         irec=irec+1
         read(lun_tmp,rec=irec,err=110) xr(j) ! X coordinate of jth point
         call dcw_swap32(xr(j),1)
         irec=irec+1
         read(lun_tmp,rec=irec,err=110) yr(j) ! Y coordinate of jth point
         call dcw_swap32(yr(j),1)
         !print*,'num,xr,yr=',j,xr(j),yr(j)
 
!        Draw the roads

         if(j == 1) then
            call mapit(yr(j),xr(j),0)
         else
            call mapit(yr(j),xr(j),1)
         endif
      enddo

   enddo

else

!===========================
!  Reading text/symbol files

!  File Header - Number of text items and S/W corner of tile

   read(lun_tmp,rec=1,err=210) ntxt,xmin,ymin
   call dcw_swap32(ntxt,1)
   call dcw_swap32(xmin,1)
   call dcw_swap32(ymin,1)
   !print*,'ntxt,xmin,ymin=',ntxt,xmin,ymin

   irec = 1

!  Set maximum text length

   if(ireclen  ==  16*4) then
      ncht=50
   else
      ncht=40
   endif

!  Read text items
!       x     = x coordinate
!       y     = y coordinate
!       itype = color/symbol indicator
!       text  = text string

   do i=1,ntxt
      irec=irec+1
      if(ireclen == 16*4) then
         read(lun_tmp,rec=irec,err=110) x,y,ityp,text(1:ncht)
         call dcw_swap32(x,1)
         call dcw_swap32(y,1)
         call dcw_swap32(ityp,1)
         !print*,'x,y,ityp,text=',x,y,ityp,text(1:ncht)
      else
         read(lun_tmp,rec=irec,err=110) x,y,text(1:ncht)
         call dcw_swap32(x,1)
         call dcw_swap32(y,1)
         ityp=0
         !print*,'x,y,text=',x,y,text(1:ncht)
      endif
 
      if(detail >= 0) then
         call maptra(y,x,xpos,ypos)
         !print*,'y,x,xpos,ypos=',y,x,xpos,ypos
         size=0.0075
         angd=0.
         cntr=-1
         if(xpos < 999.0.and.ypos < 999.0) then
            call plchlq (xpos,ypos,text(1:ncht),size,angd,cntr)
            !print*,'plt sta@lat,lon=',text(1:ncht),y,x,xpos,ypos
         endif
      endif
 
   enddo
 
endif

210 return
110 continue
print*,'problem in read_dcw'
!110 stop 110

return
end
