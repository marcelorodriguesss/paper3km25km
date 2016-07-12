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


Module grid_dims

!  This module contains very basic specification of grid dimensions and other 
!     parameters that will be used to dimension arrays and allocate memory.

!==============================================================================
!        Set maximum values of parameters:
!          MAXGRDS - Maximum number of grids
!          NXPMAX  - Maximum number of points in x-direction
!          NYPMAX  - Maximum number of points in y-direction
!          NZPMAX  - Maximum number of points in z-direction
!          NZGMAX  - Maximum number of soil levels
!          MAXSCLR - Maximum number of additional scalars
!          MAXHP   - Maximum number of u, v, OR t points in a single vertical
!                       level interpolated from opposite hemispheric grid

   integer, parameter :: maxgrds=8
   integer, parameter :: nxpmax=303, nypmax=303, nzpmax=132, nzgmax=20
   integer, parameter :: maxsclr=150, maxhp=1000

!        Set MAXDIM to the largest of NXPMAX,NYPMAX,NZPMAX+10,NZGMAX

   integer, parameter :: maxdim=303

!        maxmach is the maximum number of processors that can be used in a 
!          parallel run

   integer, parameter :: maxmach=64
!==============================================================================

!        Computed parameters

   integer, parameter :: maxdimp=maxdim+2
   integer, parameter :: nxyzpm=nzpmax*nxpmax*nypmax
   

End Module grid_dims
