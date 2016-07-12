!############################# Change Log ##################################
! 2.3.0.0
!
! 000925 MJB fgksinit ##
!            Added ICOLMETH and FILLCOLS to gks_colors call. ##
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000 - All Rights Reserved
!  Regional Atmospheric Modelling System - RAMS
!  Mission Research Corporation / *ASTER Division
!###########################################################################

subroutine fgksinit (win)

!     Need to open workstations from FORTRAN for ncar graphics calls
!     to work properly.

integer win,cgmwsid
common /cgminfo/cgmwsid

!     Set cgm ws to closed=0
cgmwsid = 0
call gopks(0,0)
call gopwk(1,win,7)
call gacwk(1)
call gopwk(2,1,3)

return
end

!***************************************************************************

subroutine fgksclose

integer cgmwsid
common /cgminfo/cgmwsid

call gdawk(2)
call gdawk(1)
call gclwk(1)
if (cgmwsid .ne. 0) then
  call clearframe(cgmwsid)
  call gdawk(cgmwsid)
  call gclwk(cgmwsid)
  close(85)
endif
call gclks

return
end

!***************************************************************************

subroutine startmeta(ibackgnd)

integer cgmwsid
common /cgminfo/cgmwsid

call gdawk(1)
if (cgmwsid .eq. 0) then
   cgmwsid = 3
   open(unit=85,file='ringi.cgm')
   call gopwk(3,85,2)
   call gks_colors(3,ibackgnd)
   call gacwk(3)
else
   call gacwk(3)
endif

return
end

!***************************************************************************

subroutine endmeta

integer cgmwsid
common /cgminfo/cgmwsid

call clearframe(3)
call gdawk(3)
call gacwk(1)

return
end

!***************************************************************************

subroutine clearframe(wsid)
integer wsid

call plotif (0., 0., 2)
call GCLRWK (wsid,1)

return
end

