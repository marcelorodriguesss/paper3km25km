!############################# Change Log ##################################
! 2.3.0.0
!
! 000823 MJB none ##
!            Added change log. ##
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000 - All Rights Reserved
!  Regional Atmospheric Modelling System - RAMS
!  Mission Research Corporation / *ASTER Division
!###########################################################################

subroutine getmappos(winx,winy,lat,lon)

!     given and gks coordinate within the window, return
!     the latitude and longitude of the map.  Points in
!     x and y that lay outside the map will be moved into
!     the edge of the map

real vl,vr,vb,vt,wl,wr,wb,wt
real tx,ty,lat,lon

tx = winx
ty = winy

!     print *, 'Finding lat and lon for ', winx,winy
!     reset viewport to that of last map settings
call maprs

!     Get those settings
call getset(vl,vr,vb,vt,wl,wr,wb,wt,lf)
!     print 932, '  Window = ',wl,wr,wb,wt
!     print 932, '  Viewpt = ',vl,vr,vb,vt

!     Move points that may be outside map region on screen
if (tx .lt. vl) tx = vl
if (tx .gt. vr) tx = vr
if (ty .lt. vb) ty = vb
if (ty .gt. vt) ty = vt

if (winx .ne. tx) print 932, '  WinChgTo ',winx,winy,tx,ty
if (winy .ne. ty) print 932, '  WinChgTo ',winx,winy,tx,ty

!     Now translate x and y to U and V
u = wl + (tx-vl)/(vr-vl) * (wr-wl)
v = wb + (ty-vb)/(vt-vb) * (wt-wb)
!     print 932, '   U & V = ',u,v

!     Now get the lat and long
call maptri(u,v,lat,lon)
!     print 932, '  LatLon = ',lat,lon

932  format(a12,4f10.5)

return
end
