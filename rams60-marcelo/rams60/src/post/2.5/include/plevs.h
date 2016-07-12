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

! Pressure levels for interpolation. Assumes fewer levels than any grid's nnzp
! This is used by vcomm2.h as well as on its own

integer, parameter :: nplevs=11
integer, save :: iplevs(nplevs),zplevz(nplevs)
data iplevs/1000, 925, 850, 700, 500, 400, 300,  250,  200,  150,  100/
data zplevz/100 , 500,1500,3000,5600,7200,9200,10500,11800,14000,16200/
