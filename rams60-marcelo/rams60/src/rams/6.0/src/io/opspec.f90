!
! Copyright (C) 1991-2004  ; All Rights Reserved ; Colorado State University
! Colorado State University Research Foundation ; ATMET, LLC
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
! 5.0.0
!
!###########################################################################

SUBROUTINE OPSPEC1

! This routine checks the option specifications in the $MODEL_GRIDS
!   namelist for consistency, and overrides settings of ICLOUD,
!   IRAIN, IPRIS, ISNOW, IAGGR, IGRAUP, and IHAIL, setting them
!   all to zero if LEVEL is less than 3.

! Each error will be assigned a severity.  Fatal errors will cause
!   run to stop immediately and warning errors and informative
!   messages will be listed.

use mem_grid
use micphys

implicit none

integer :: ierr,ifm,icm,ng,ifaterr,iwarerr,infoerr  &
          ,lev4bins,lev5bins,nhemgrds


IFATERR=0
IWARERR=0
INFOERR=0

! Check number of grids and grid points in data versus maximums in
!   configuration file. (severity - F )

IF(NGRIDS.GT.MAXGRDS) THEN
  PRINT*,' FATAL - NGRIDS in data greater than MAXGRDS in config.'
  IFATERR=IFATERR+1
  goto 1000
ENDIF

IERR=0
DO NGRID=1,NGRIDS
  IF(NNXP(NGRID).GT.NXPMAX)IERR=IERR+1
  IF(NNYP(NGRID).GT.NYPMAX)IERR=IERR+1
  IF(NNZP(NGRID).GT.NZPMAX)IERR=IERR+1
ENDDO
IF(NZG.GT.NZGMAX)IERR=IERR+1
IF(IERR.GT.0) THEN
  PRINT*,' FATAL - grid points in data exceeded maximums in',  &
         ' config ',IERR,' times.'
  IFATERR=IFATERR+IERR
ENDIF

! Atmospheric levels must exceed soil levels
!   (severity - F)

DO NGRID=1,NGRIDS
  IF(NNZP(NGRID).LT.NZG+NZS+1) THEN
    PRINT*,' FATAL  - nnzp  must be at least nzg + nzs + 1.'
    IFATERR=IFATERR+1
  ENDIF
ENDDO

! Minimum values of NGRIDS, NNXP, NNYP, NNZP, NSTRATX, NSTRATY
!   (severity - F)

IF(NGRIDS.LT.1) THEN
  PRINT*,' FATAL - NGRIDS in data less than 1.'
  IFATERR=IFATERR+1
ENDIF

DO IFM=1,NGRIDS
  icm = nxtnest(ifm)
  IF(NNXP(IFM).LT.4) THEN
    PRINT*,' FATAL - NNXP must be at least 4.'
    IFATERR=IFATERR+1
  ENDIF

  IF(NNYP(IFM).LT.1) THEN
    PRINT*,' FATAL - NNYP must be at least 1.'
    IFATERR=IFATERR+1
  ENDIF

  IF(NNZP(IFM).LT.11) THEN
    PRINT*,' FATAL - NNZP must be at least 11.'
    IFATERR=IFATERR+1
  ENDIF

  IF(NSTRATX(IFM).LT.1) THEN
    PRINT*,' FATAL - NSTRATX must be at least 1.'
    IFATERR=IFATERR+1
  ENDIF

  IF(NSTRATY(IFM).LT.1) THEN
    PRINT*,' FATAL - NSTRATY must be at least 1.'
    IFATERR=IFATERR+1
  ENDIF

  if(nstraty(ifm).ne.1.and.nnyp(ifm).eq.1)then
    print*,' FATAL - nstraty must be set to 1 for 2-d run.'
    ifaterr=ifaterr+1
  endif

  if (icm .ge. 1 .and. nnyp(ifm) .eq. 1 .and.  &
     (ninest(ifm) .lt. 3 .or. njnest(ifm) .lt. 1)) then
     print*, ' FATAL - nested 2d grid must have ninest > 2 '  &
     ,'and njnest = 1 in namelist.'
      IFATERR=IFATERR+1
   ENDIF
ENDDO

! Allowable values of CENTLAT, CENTLON, POLELAT, POLELON
!   (severity - F)

if(polelat.lt.-90..or.polelat.gt.90.) then
   PRINT*,' FATAL - POLELAT outside of legal bounds.'
   IFATERR=IFATERR+1
ENDIF

if(polelon.lt.-180..or.polelon.gt.180.) then
   PRINT*,' FATAL - POLELON outside of legal bounds.'
   IFATERR=IFATERR+1
ENDIF

do ng=1,ngrids
   if(centlat(ng).lt.-90..or.centlat(ng).gt.90.) then
      PRINT*,' FATAL - CENTLAT outside of legal bounds.'
      IFATERR=IFATERR+1
   ENDIF

   if(centlon(ng).lt.-180..or.centlon(ng).gt.180.) then
      PRINT*,' FATAL - CENTLON outside of legal bounds.'
      IFATERR=IFATERR+1
   ENDIF
enddo

! Check nxtnest values for validity and whether this is a global simulation
!   (severity - F)

if (nxtnest(1) .ne. 0) then
   print*, ' FATAL - Grid # 1 must have its parent mesh'  &
      ,' designated as Grid # 0 (nxtnest(1) = 0)'
   IFATERR=IFATERR+1
ENDIF

nhemgrds = 0
do ifm = 1,ngrids
   icm = nxtnest(ifm)

   IF (icm .GE. ifm) THEN
      PRINT 1, ifm
      1 FORMAT (' FATAL - Nest #',I3,' has specified parent'  &
               ,' mesh of equal or higher number')
      IFATERR=IFATERR+1
   ENDIF

   if (icm .lt. 0) then
      PRINT 2, ifm
      2 FORMAT (' FATAL - Nest #',I3,' has specified parent'  &
               ,' mesh of number less than 0')
      IFATERR=IFATERR+1
   endif

   if (icm .eq. 0) then
      nhemgrds = nhemgrds + 1
      nhemgrd2 = ifm
   endif

enddo

if (nhemgrds .gt. 2) then
   print*, ' FATAL - more than two grids have grid # 0 specified'  &
         ,' as their parent'
   IFATERR=IFATERR+1
endif

! If this is a global simulation, print message that DELTAX and DELTAZ will be
! redefined by nnxp(1) and nnyp(1).  Check that nnxp, nnyp, and nnzp of the
! two top grids are identical, and that nnxp and nnyp are equal to each
! other.  Check to make sure that cyclic lateral boundary conditions are
! not specified.

!print*, 'nhemgrd2,nhemgrds',nhemgrd2,nhemgrds
!print*, 'nnxp(1),nnxp(nhemgrd2)',nnxp(1),nnxp(nhemgrd2)
!print*, 'nnyp(1),nnyp(nhemgrd2)',nnyp(1),nnyp(nhemgrd2)
!print*, 'nnzp(1),nnzp(nhemgrd2)',nnzp(1),nnzp(nhemgrd2)

if (nhemgrds .eq. 2) then
   if (nnxp(1) .ne. nnxp(nhemgrd2) .or.  &
       nnyp(1) .ne. nnyp(nhemgrd2) .or.  &
       nnzp(1) .ne. nnzp(nhemgrd2)) then
      print*, ' FATAL - For a global simulation, nnxp, nnyp, and nnzp'
      print*, ' must be identical between both hemispheric grids'
     IFATERR=IFATERR+1
   endif

   if (nnxp(1) .ne. nnyp(1)) then
      print*, ' '
      print*, 'FATAL - For a global simulation, nnxp must equal'
      print*, ' nnyp in both hemispheric grids'
      IFATERR = IFATERR + 1
   endif

   if (ibnd .eq. 4 .or. jbnd .eq. 4) then
      print*, ' '
      print*, 'FATAL - For a global simulation, ibnd and jbnd'
      print*, ' must not be set to 4'
      IFATERR = IFATERR + 1
   endif

   print*, ' '
   print*, 'Because two values of NXTNEST are set to 0, this'
   print*, ' is configured as a global simulation.'
   print*, 'Consequently, ihtran will automatically be set'
   print*, ' to 1, and DELTAX and DELTAY will be redefined'
   print*, ' in terms of nnxp(1) and nnyp(1), ignoring'
   print*, ' the values specified in the namelist.'

endif

! Check to make sure that top grids have NSTTOP and NSTBOT set to 1
!   (severity - F)

IF (NNSTTOP(1) .NE. 1 .or. nnsttop(nhemgrd2) .ne. 1) THEN
  PRINT*,'NSTTOP not set to 1 for a hemispheric grid'
  IFATERR=IFATERR+1
ENDIF

IF (NNSTBOT(1) .NE. 1 .or. nnstbot(nhemgrd2) .ne. 1) THEN
  PRINT*,'NSTBOT not set to 1 for a hemispheric grid'
  IFATERR=IFATERR+1
ENDIF

! If LEVEL is less than 3, set microphysics parameters to zero.
! If LEVEL equals 3, check for values of microphysics parameters
! that are out of bounds.  If LEVEL is equal to 4, set microphysics
! parameters other than ICLOUD to zero.

if (level .le. 2) then

   icloud = 0
   irain = 0
   ipris = 0
   isnow = 0
   iaggr = 0
   igraup = 0
   ihail = 0

elseif (level .eq. 3) then

   if (icloud .lt. 0 .or. icloud .gt. 7) THEN
       print*,'FATAL - ICLOUD OUT OF RANGE'
     IFATERR = IFATERR + 1
 endif
   if (irain .lt. 0 .or. irain .gt. 5) THEN
       print*,'FATAL - IRAIN OUT OF RANGE'
     IFATERR = IFATERR + 1
 endif
   if (ipris .lt. 0 .or. ipris .gt. 7) THEN
       print*,'FATAL - IPRIS OUT OF RANGE'
     IFATERR = IFATERR + 1
 endif
   if (isnow .lt. 0 .or. isnow .gt. 5) THEN
       print*,'FATAL - ISNOW OUT OF RANGE'
     IFATERR = IFATERR + 1
 endif
   if (iaggr .lt. 0 .or. iaggr .gt. 5) THEN
       print*,'FATAL - IAGGR OUT OF RANGE'
     IFATERR = IFATERR + 1
 endif
   if (igraup .lt. 0 .or. igraup .gt. 5) THEN
       print*,'FATAL - IGRAUP OUT OF RANGE'
     IFATERR = IFATERR + 1
 endif
   if (ihail .lt. 0 .or. ihail .gt. 5) THEN
       print*,'FATAL - IHAIL OUT OF RANGE'
     IFATERR = IFATERR + 1
 endif

elseif (level .eq. 4) then

   ipris = 0
   isnow = 0
   iaggr = 0
   igraup = 0
   ihail = 0

endif

! If LEVEL is 4, make sure that NADDSC is large enough for number
!   of bins specified in ICLOUD.

if (level .eq. 4) then
   if (IRAIN .eq. 0) then
      lev4bins = 2 * icloud + 1
      if (naddsc .lt. lev4bins) then
         print*, 'FATAL - NADDSC is not large enough for ICLOUD'
         print*, 'value with LEVEL = 4.'
         print*, 'NADDSC must be at least ',lev4bins
       IFATERR = IFATERR + 1
      endif
   else
      lev4bins = 2 * icloud + 1 + 67
      if (naddsc .lt. lev4bins) then
         print*, 'FATAL - NADDSC is not large enough for ICLOUD'
         print*, 'value with LEVEL = 4 and IRAIN = 1.'
         print*, 'NADDSC must be at least ',lev4bins
       IFATERR = IFATERR + 1
      endif
   endif
endif

! If LEVEL is 5, make sure that NADDSC is large enough for number
!   of bins specified in ICLOUD.

if (level .eq. 5) then
   if (IRAIN .eq. 0) then
      lev5bins = 2 * (icloud + ipris + iaggr + igraup) + 5
      if (naddsc .lt. lev5bins) then
         print*, 'FATAL - NADDSC is not large enough for ICLOUD,'
         print*, 'IPRIS, IAGGR, and IGRAUP values with LEVEL = 5.'
         print*, 'NADDSC must be at least ',lev5bins
       IFATERR = IFATERR + 1
      endif
   else
      lev5bins = 2 * (icloud + ipris + iaggr + igraup) + 5 + 67
      if (naddsc .lt. lev5bins) then
         print*, 'FATAL - NADDSC is not large enough for ICLOUD,'
         print*, 'IPRIS, IAGGR, and IGRAUP values with LEVEL = 5'
         print*, 'and IRAIN = 1.'
         print*, 'NADDSC must be at least ',lev5bins
       IFATERR = IFATERR + 1
    endif
   endif
endif

1000 continue

! Stop the run if there are any fatal errors.  List how many
!   warning and informative errors.

PRINT*,' -----------opspec1--------------------------'
PRINT*,' FATAL     errors - ',IFATERR
PRINT*,' WARNING   errors - ',IWARERR
PRINT*,' INFORM  messages - ',INFOERR
PRINT*,' -----------------------------------------------'

IF(IFATERR.GT.0) STOP 'OPSPEC1'

RETURN
END

!*******************************************************************************

SUBROUTINE OPSPEC2

! Check that fine mesh is a valid subset of its coarser mesh.
!   and that top and bottom boundary flags are set correctly.
!   (severity - F)

use mem_grid
use mem_varinit

implicit none

integer :: icm,ifm,ifaterr,iwarerr,infoerr,ncx,ncy,nfx,nfxp,nfy,nfyp
integer :: ng,nesta,nfz,kc

IFATERR=0
IWARERR=0
INFOERR=0

DO ifm=1,NGRIDS

   icm = nxtnest(ifm)
   if (icm .ge. 1) then

      NCX=(NNXP(ifm)-2)/NSTRATX(ifm)
      NCY=(NNYP(ifm)-2)/NSTRATY(ifm)

      IF ((NNYP(ifm).EQ.1.AND.NNYP(icm).NE.1).OR.  &
          (NNYP(ifm).NE.1.AND.NNYP(icm).EQ.1)) THEN
         PRINT*,' FATAL - Grids must be either all 3-D or all 2-D'
         IFATERR=IFATERR+1
      ENDIF

      IF (NINEST(ifm).LT.3) THEN
         PRINT 11, ifm
         11 FORMAT (' FATAL - Nest #',I3,' too close to western'  &
                   ,' boundary of coarser mesh')
         IFATERR=IFATERR+1
      ENDIF

      IF (NJNEST(ifm).LT.3.AND.NNYP(ifm).GT.1) THEN
         PRINT 12, ifm
         12 FORMAT (' FATAL - Nest #',I3,' too close to southern'  &
                   ,' boundary of coarser mesh')
         IFATERR=IFATERR+1
      ENDIF

      IF (NKNEST(ifm).LT.3.AND.NNSTBOT(ifm).EQ.0) THEN
         PRINT 13, ifm
         13 FORMAT (' FATAL - Nest #',I3,' too close to lower'  &
                   ,' boundary of coarser mesh or NNSTBOT incorrect')
         IFATERR=IFATERR+1
      ENDIF

      IF (NKNEST(ifm).NE.1.AND.NNSTBOT(ifm).EQ.1) THEN
         PRINT 14, ifm
         14 FORMAT (' FATAL - Nest #',I3,' not to lower boundary of'  &
                   ,' coarser mesh or NNSTBOT flag set incorrectly')
         IFATERR=IFATERR+1
      ENDIF

      IF (NINEST(ifm)+NCX.GT.NNXP(icm)-3) THEN
         PRINT 15, ifm
         15 FORMAT (' FATAL - Nest #',I3,' too close to eastern'  &
                   ,' boundary of coarser mesh')
         IFATERR=IFATERR+1
      ENDIF

      IF (NJNEST(ifm)+NCY.GT.NNYP(icm)-3.AND.NNYP(ifm).GT.1) THEN
         PRINT 16, ifm
         16 FORMAT (' FATAL - Nest #',I3,' too close to northern'  &
                   ,' boundary of coarser mesh')
         IFATERR=IFATERR+1
      ENDIF

      IF (NCX.LT.2.OR.(NCY.LT.2.AND.NNYP(ifm).NE.1)) THEN
         PRINT 17, ifm
         17 FORMAT (' FATAL - Nest #',I3,' dimensioned too small'  &
                   ,' in at least one horizontal direction')
         IFATERR=IFATERR+1
      ENDIF

      NFX=NCX*NSTRATX(ifm)+2
      IF (NNXP(ifm).NE.NFX) THEN
         NFXP=NFX+NSTRATX(ifm)
         PRINT 18, ifm,NFXP,NFX
         18 FORMAT (' FATAL - Nest #',I3,' NNXP incompatible with'  &
                   ,' NSTRATX:  May increase NNXP to',I5  &
                   ,' or decrease it to',I5)
         IFATERR=IFATERR+1
      ENDIF

      NFY=NCY*NSTRATY(ifm)+2
      IF (NNYP(ifm).NE.NFY.AND.NNYP(ifm).GT.1) THEN
         NFYP=NFY+NSTRATY(ifm)
         PRINT 19, ifm,NFYP,NFY
         19 FORMAT (' FATAL - Nest #',I3,' NNYP incompatible with'  &
                   ,' NSTRATY:  May increase NNYP to',I5  &
                   ,' or decrease it to',I5)
         IFATERR=IFATERR+1
      ENDIF

      IF (NNSTBOT(ifm).EQ.1.AND.NNSTBOT(icm).NE.1) THEN
         PRINT 20, ifm
         20 FORMAT (' FATAL - Nest #',I3,' NNSTBOT flag'  &
                   ,' incompatible with coarser mesh')
         IFATERR=IFATERR+1
      ENDIF

      IF (NNSTTOP(ifm).EQ.1.AND.NNSTTOP(icm).NE.1) THEN
         PRINT 21, ifm
         21 FORMAT (' FATAL - Nest #',I3,' NNSTTOP flag incompatible'  &
                   ,' with coarser mesh')
         IFATERR=IFATERR+1
      ENDIF

   endif
ENDDO

nesta=abs(nestz1)
if(nestz1.ne.0.and.nesta.le.ngrids)then
   nfz=nnzp(nesta)-2
   kc=nknest(nesta)
     1002    continue
      kc=kc+1
      nfz=nfz-nrz(kc,nesta)
      if(nfz.lt.0)then
         print 195,nesta
         195 format(' FATAL - vertically nested grid #',i3,  &
                    ' has illegal number of levels for given NSTRATZ values')
         IFATERR=IFATERR+1
      ENDIF
   if(nfz.gt.0)go to 1002
   if(nfz.eq.0)then
      if(kc.gt.nnzp(nxtnest(nesta))-3.and.nnsttop(nesta).ne.1)then
         PRINT 22, nesta
         22 FORMAT (' FATAL - Nest #',I3,' too high'  &
                   ,' or NNSTTOP flag set incorrectly')
         IFATERR=IFATERR+1
      ENDIF

      if(kc.ne.nnzp(nxtnest(nesta))-1.and.nnsttop(nesta).eq.1)then
         PRINT 23, nesta
         23 FORMAT (' FATAL - Nest #',I3,' not to upper boundary of'  &
                   ,' coarser mesh or NNSTTOP flag set incorrectly')
         IFATERR=IFATERR+1
      ENDIF
   endif
endif

DO ifm = 1,NGRIDS
   icm = nxtnest(ifm)
   if (ifm < nesta .and. icm .ge. 1)then
      if(nnzp(ifm).gt.nnzp(icm)-nknest(ifm)-1.and.nnsttop(ifm).eq.0)then
         PRINT 24, ifm
         24 FORMAT (' FATAL - Nest #',I3,' NNZP incompatible with'  &
                   ,' parent grid or NNSTTOP flag set incorrectly')
         IFATERR=IFATERR+1
      ENDIF

      if(nnzp(ifm).ne.nnzp(icm)-nknest(ifm)+1.and.nnsttop(ifm).eq.1)then
         PRINT 25, ifm
         25 FORMAT (' FATAL - Nest #',I3,' not to upper boundary of'  &
                   ,' coarser mesh or NNSTTOP flag set incorrectly')
         IFATERR=IFATERR+1
      ENDIF
   endif
enddo

! This need to be done here since VARFILES are filled before OPSPEC3
if (VWAITTOT.lt.VWAIT1) then
   print*,'Total wait time must be <= individual VARFILE wait'
   print*,'      resetting VWAITTOT to ',VWAIT1
   VWAITTOT=VWAIT1
   IWARERR=IWARERR+1
endif

! Stop the run if there are any fatal errors.  List how many
!   warning and informative errors.

PRINT*,' -----------opspec2--------------------------'
PRINT*,' FATAL     errors - ',IFATERR
PRINT*,' WARNING   errors - ',IWARERR
PRINT*,' INFORM  messages - ',INFOERR
PRINT*,' -----------------------------------------------'

IF(IFATERR.GT.0) STOP 'OPSPEC2'

RETURN
END

!*******************************************************************************

SUBROUTINE OPSPEC3

use mem_varinit
use mem_grid
use micphys
use io_params
use mem_radiate
use mem_cuparm
use mem_turb
use mem_leaf

implicit none

integer :: ip,k,ifaterr,iwarerr,infoerr,ng,ngr

IFATERR=0
IWARERR=0
INFOERR=0

! Check that moisture is turned on if radiation is used.
!   (severity - F)

  IF(ilwrtyp+iswrtyp.GT.0.AND.LEVEL.EQ.0)THEN
    PRINT*,' FATAL  - radiation scheme must be run with moisture.'
    IFATERR=IFATERR+1
  ENDIF
  
! Microphysics flags and parameter settings

IF((irain.GE.2.AND.irain.LE.4.AND.Rparm.LE.0.)  &
 .OR.(icloud.GE.2.AND.icloud.LE.5.AND.cparm.LE.0.)  &
 .OR.(ipris.GE.2.AND.ipris.LE.4.AND.Pparm.LE.0.)  &
 .OR.(isnow.GE.2.AND.isnow.LE.4.AND.Sparm.LE.0.)  &
 .OR.(igraup.GE.2.AND.igraup.LE.4.AND.gparm.LE.0.)  &
 .OR.(iaggr.GE.2.AND.iaggr.LE.4.AND.Aparm.LE.0.)  &
 .OR.(ihail.GE.2.AND.ihail.LE.4.AND.hparm.LE.0.)) THEN
   PRINT 26,ng,Rparm,pparm,sparm,gparm,aparm,hparm
   26 FORMAT (' FATAL - Microphysics - xPARM must be positive'  &
             ,' if micro flags are set to 2, 3, or 4,'  &
             ,' or up to 5 for icloud. ',i3,5f10.7)
   IFATERR=IFATERR+1
ENDIF
  
! Convective parameterization flags and parameter settings

DO NG=1,NGRIDS
  IF(NNQPARM(NG).GT.0.AND.LEVEL.EQ.0) THEN
    PRINT 27
    27 FORMAT (' FATAL - LEVEL must be at least'  &
              ,' 1 for the cumulus parameterization')
    IFATERR=IFATERR+1
  ENDIF
ENDDO

DO NG=1,NGRIDS
  IF(NNQPARM(NG) == 2 .AND.LEVEL /= 3) THEN
    PRINT 127
    127 FORMAT (' FATAL - LEVEL must be set to'  &
              ,' 3 for the Kain-Fritsch cumulus parameterization')
    IFATERR=IFATERR+1
  ENDIF
ENDDO

! Moving grids and topography interpolation

do ng=1,ngrids
  if((abs(gridu(ng)) .gt. 1.e-20 .or. abs(gridv(ng)) .gt. 1.e-20)  &
     .and. itoptflg(ng) .ne. 0) then
    print 28
    28 format (' FATAL - nested grid topography must be interpolated'  &
              ,' from parent grid if nested grid moves')
    ifaterr=ifaterr+1
  endif
enddo

! Check horizontal and vertical grid spacings.

IF(DZMAX.LT.DELTAZ)THEN
  PRINT*,' WARNING - DELTAZ is being reduced by a low value',  &
    ' of DZMAX.'
  IWARERR=IWARERR+1
ENDIF

IF(DZRAT.GT.1.2)THEN
  PRINT*,' WARNING - Large vertical stretch ratios sacrifice',  &
    ' second order accuracy in the vertical differencing.'
  IWARERR=IWARERR+1
ENDIF

! Check numerical schemes.

IF((SSPCT.LT.0.2.OR.SSPCT.GT.1.0).AND.SSPCT.NE.0.0)THEN
  PRINT*,' WARNING - SSPCT should normally range from 0.2 to 1.0'
  IWARERR=IWARERR+1
ENDIF

IF(NFPT.GT.NNZP(1))THEN
  PRINT*,' FATAL - NFPT must be less than nnzp(1).'
  IFATERR=IFATERR+1
ENDIF

IF(IADVL.NE.2.AND.IADVL.NE.4)then
  PRINT*,' FATAL - IADVL must be 2 or 4'
  IFATERR=IFATERR+1
ENDIF

IF(IADVF.NE.2.AND.IADVF.NE.6)then
  PRINT*,' FATAL - IADVF must be 2 or 6'
  IFATERR=IFATERR+1
ENDIF

! Check turbulence parameterization.

DO NGR=1,NGRIDS

!_STC
!_STC.............................................
!_STC Correction to account for the new options
!_STC for the E-l and E-eps closures: IDIFFK <= 6 
!_STC  (S. Trini Castelli)
!_STC.............................................
!_STC  IF(IDIFFK(NGR).LT.1.OR.IDIFFK(NGR).GT.4)THEN
!_STC    PRINT*,' FATAL - IDIFFK must be 1, 2, 3,or 4.'
!_STC
  IF(IDIFFK(NGR).LT.1.OR.IDIFFK(NGR).GT.6)THEN
    PRINT*,' FATAL - IDIFFK must be 1, 2, 3, 4, 5 or 6.'
    IFATERR=IFATERR+1
  ENDIF
ENDDO
! Check that diffusion flags are compatible if using ihorgrad=1

if(ihorgrad.eq.2)then
  if(IDIFFK(NGR) >= 3)then
    print*,' FATAL - Cant use IHORGRAD=2 if IDIFFK >= 3'
    IFATERR=IFATERR+1
  endif
endif

! Check whether the soil model will be run and make sure that the
!   number of soil levels are correct.(severity - F,I )

IF(ISFCL.EQ.0.AND.NZG.GT.1)THEN
  PRINT*,' INFO  - more soil levels specified than needed.'
  INFOERR=INFOERR+1
ENDIF

if (isfcl == 0 .and. npatch /= 2) then
  print*, ' fatal  - When isfcl = 0, npatch must be 2. '
  ifaterr = ifaterr + 1
endif

IF(ISFCL.GT.0.AND.NZG.LE.2)THEN
  PRINT*,  &
    ' FATAL  - at least 2 soil levels are needed for soil'  &
   ,' model.'
  IFATERR=IFATERR+1
ENDIF

do k=1,nzg
   if (slz(k) .gt. -.001) then
      print*, 'FATAL - Soil level',k,' not (enough) below ground'  &
         ,' level'
     IFATERR=IFATERR+1
   endif
enddo

do k=1,nzg-1
   if (slz(k)-slz(k+1) .gt. .001) then
      print*, 'FATAL - Soil level',k,' not (enough) deeper than'  &
      ,' soil level',k+1
     IFATERR=IFATERR+1
   endif
enddo

! If the soil model will be run with no radiation, make a suggestion
!   that the radiation be turned on. (severity - F )

DO NGR=1,NGRIDS
  IF(ISFCL.GT.0.AND.ilwrtyp+iswrtyp.EQ.0)THEN
    PRINT*,' FATAL  - radiation scheme must be run with soil',  &
        ' model.'
    IFATERR=IFATERR+1
  ENDIF
ENDDO


! Make sure that if nudging, nudging time scales are greater than
! the model coarse grid timestep, and that Rayleigh friction nudging
! is not done with variable initialization.

if (initial .eq. 1) then
   if (nfpt .gt. 0 .and. distim .gt. 0. .and.  &
                         distim .lt. dtlongn(1)) then
      print*, 'Rayleigh friction nudging is being done'
      print*, 'and DISTIM is less than DTLONGN(1).'
      print*, 'This nudging is too strong.'
      IFATERR=IFATERR+1
   endif
endif

if (initial .eq. 2) then

   if (nfpt .gt. 0 .and. distim .gt. 0.) then
      print*, 'Rayleigh friction nudging may not be used when'
      print*, 'variable initialization is used.'
      IFATERR=IFATERR+1
   endif

   if (nudlat .ge. 1 .and. tnudlat .gt. 0. .and.  &
                           tnudlat .lt. dtlongn(1)) then
      print*, 'Lateral boundary nudging is being done'
      print*, 'and TNUDLAT is less than DTLONGN(1).'
      print*, 'This nudging is too strong.'
      IFATERR=IFATERR+1
   endif

   if (tnudcent .gt. 0. .and. tnudcent .lt. dtlongn(1)) then
      print*, 'Center nudging is being done'
      print*, 'and TNUDCENT is less than DTLONGN(1).'
      print*, 'This nudging is too strong.'
      IFATERR=IFATERR+1
   endif
   if (tnudtop .gt. 0. .and. tnudtop .lt. dtlongn(1)) then
      print*, 'Top boundary nudging is being done'
      print*, 'and TNUDTOP is less than DTLONGN(1).'
      print*, 'This nudging is too strong.'
      IFATERR=IFATERR+1
   endif

endif


!     Check the averaging and analysis frequencies for consistency.

if (abs(AVGTIM).gt.0.0.and.FRQMEAN.le.0.0.and.FRQBOTH.le.0.) then
   print*,'Have FRQMEAN=0 & FRQBOTH=0 even though AVGTIM=',AVGTIM
   print*,'Respecifying AVGTIM=0.'
   AVGTIM=0.
   IWARERR=IWARERR+1
endif
if (FRQLITE.gt.0.) then
!   if ( nl3d(1)+nl2d(1)+nl3ds(1).eq.0)then
!      print*,'Have no LITE variables even though FRQLITE=',frqlite
!      print*,'Respecify in VTABLES or set frqlite=0 in namelist'
!      IFATERR=IFATERR+1
!   endif
endif
if (FRQMEAN.gt.0.0.and.abs(AVGTIM).gt.0.) then
   if ( abs(AVGTIM).gt.FRQMEAN ) then
      print*,'AVGTIM must be <= FRQMEAN'
      IFATERR=IFATERR+1
   endif
!   if ( nm3d(1)+nm2d(1)+nm3ds(1).eq.0)then
!      print*,'Have no MEAN variables even though FRQMEAN=',frqmean
!      print*,'Respecify in VTABLES or set frqmean=0 in namelist'
!      IFATERR=IFATERR+1
!   endif
endif
if (FRQBOTH.gt.0.0.and.abs(AVGTIM).gt.0.) then
   if ( abs(AVGTIM).gt.FRQBOTH ) then
      print*,'AVGTIM must be <= FRQBOTH'
      IFATERR=IFATERR+1
   endif
!   if ( nb3d(1)+nb2d(1)+nb3ds(1).eq.0)then
!      print*,'Have no BOTH variables even though FRQBOTH=',frqboth
!      print*,'Respecify in VTABLES or set frqboth=0 in namelist'
!      IFATERR=IFATERR+1
!   endif
endif
if (FRQMEAN.gt.0.0.and.FRQBOTH.gt.0.0.and.abs(AVGTIM).gt.0.) then
   if ( (FRQMEAN.gt.FRQBOTH.and.mod(FRQMEAN,FRQBOTH).ne.0.).or.  &
        (FRQMEAN.lt.FRQBOTH.and.mod(FRQBOTH,FRQMEAN).ne.0.) ) then
      print*,'FRQMEAN must be a multiple of FRQBOTH or vice versa'
      IFATERR=IFATERR+1
   endif
endif

! Check printout parameters.

DO NGR=1,NGRIDS
  DO IP=1,NPLT
    IF(IXSCTN(IP).EQ.1.AND.ISBVAL(IP).GT.NNYP(NGR))THEN
      PRINT 1,IP,NGR
      1 FORMAT (' FATAL - ISBVAL(',I2,') is out of bounds in'  &
               ,' y-direction for grid number ',I2,'.')
      IFATERR=IFATERR+1
    ELSEIF(IXSCTN(IP).EQ.2.AND.ISBVAL(IP).GT.NNXP(NGR))THEN
      PRINT 2,IP,NGR
      2 FORMAT (' FATAL - ISBVAL(',I2,') is out of bounds in'  &
               ,' x-direction for grid number ',I2,'.')
      IFATERR=IFATERR+1
    ELSEIF(IXSCTN(IP).EQ.3.AND.ISBVAL(IP).GT.NNZP(NGR))THEN
      PRINT 3,IP,NGR
      3 FORMAT (' FATAL - ISBVAL(',I2,') is out of bounds in'  &
               ,' z-direction for grid number ',I2,'.')
      IFATERR=IFATERR+1
    ENDIF
  ENDDO
ENDDO

! Stop the run if there are any fatal errors.  List how many
!   warning and informative errors.

PRINT*,' -----------opspec3--------------------------'
PRINT*,' FATAL     errors - ',IFATERR
PRINT*,' WARNING   errors - ',IWARERR
PRINT*,' INFORM  messages - ',INFOERR
PRINT*,' -----------------------------------------------'

IF(IFATERR.GT.0) STOP 'OPSPEC3'

RETURN
END

