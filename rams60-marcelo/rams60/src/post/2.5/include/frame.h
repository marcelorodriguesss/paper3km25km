!f90
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

!---------------------------------------------------------------------------
integer, parameter :: maxlayers=2,maxframes=30,maxgds=8
!---------------------------------------------------------------------------
character(len=24), dimension(maxlayers)   :: cvar
character(len=1),  dimension(maxlayers)   :: conttyp,colorbar
character(len=20), dimension(3,maxlayers) :: fillcols
character(len=1)                          :: cwinds
character(len=24), dimension(maxlayers)   :: cdname,cdunits
character(len=24), dimension(maxlayers,maxframes,maxgds) :: cdnames,cdunitss
common /cframe/ cvar,conttyp,colorbar,fillcols,cwinds,cdname,cdunits  &
               ,cdnames,cdunitss
!---------------------------------------------------------------------------
real, dimension(maxlayers)    :: conrinc,conrlo,conrhi,cbias,ccent  &
                                ,cgrad,cthick,csize
real, dimension(maxlayers,maxframes,maxgds) :: chimax,clomin
real                          :: velomax,stemleng,headleng0,headleng  &
                                ,headang
integer                       :: iwk,mfill,ibgnd,ipinfo,ipanl  &
                                ,iwcolor,intwindi,intwindj,ibscale
integer, dimension(maxlayers) :: icmeth,icint,icdash,icline,ichigh,icol  &
                                ,icover
common /vframe/ iwk,ibgnd,mfill,ipinfo,ipanl,conrinc,conrlo,conrhi  &
               ,icmeth,cbias,ccent,cgrad,icint,icdash,cthick,icline  &
               ,ichigh,csize,intwindi,intwindj,ibscale,velomax,stemleng  &
               ,headleng0,headleng,headang,icol,icover,chimax,clomin  &
               ,iwcolor
!---------------------------------------------------------------------------
integer           :: ilandmk,ilandlab,itypelandmk
real              :: bufflandmk,sizelandmk,sizelandlab
character(len=20) :: collandmk,collandlab
common /landmarks/ ilandmk,ilandlab,itypelandmk,bufflandmk,sizelandmk  &
                  ,sizelandlab,collandmk,collandlab
!---------------------------------------------------------------------------
