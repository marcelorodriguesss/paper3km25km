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

! v5df.h

! Include file for using v5d functions from FORTRAN programs

! Function prototypes.  See the README file for details.  These are
! the functions you'll want to use for writing v5d file converters.


! 5-D grid limits, must match those in v5d.h

integer, parameter :: MAXVARS=30
integer, parameter :: MAXTIMES=400
integer, parameter :: MAXROWS=200
integer, parameter :: MAXCOLUMNS=200
integer, parameter :: MAXLEVELS=100

! Missing values
real,    parameter :: MISSING=1.0E35
integer, parameter :: IMISSING=-987654
