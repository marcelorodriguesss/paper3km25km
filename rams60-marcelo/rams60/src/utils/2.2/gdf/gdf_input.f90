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
! 2.2.0
!###########################################################################

module gdf_input

! Surface variables

integer, parameter :: max_sfc_vars=12

type gdf_sfc_obs
   integer :: iqflags(max_sfc_vars,3),ihgtflg
   character(len=16) :: id
   real :: lat,lon,elev,hgt,ff,dd,t,td,p
   integer :: jyear,jmonth,jdate,jtime
end type

type(gdf_sfc_obs) :: rsfc_obs

! Upper air variables

integer, parameter :: max_up_levs=100,max_upa_vars=12

type gdf_upa_obs
   real, dimension(max_up_levs) :: p,t,z,r,zz,dz,fz
   character(len=16) :: id
   real :: lat,lon,elev
   integer :: lp,lz
   integer :: iqflagsp(max_up_levs,4,3),iqflagsz(max_up_levs,3,3)
   integer :: jyear,jmonth,jdate,jtime
end type

type(gdf_upa_obs) :: rupa_obs


! Header info

integer, parameter :: max_head_vars=10

type obs_header
   character(len=80) :: head_string(max_head_vars),sfc_string(max_sfc_vars)  &
                       ,therm_string(max_upa_vars),wind_string(max_upa_vars)  &
                       ,sfc_units(max_sfc_vars) 
   integer :: iun,iver,nhead,nvsfc,nvtherm,nvwind
end type

type(obs_header) :: header(1)


Contains

subroutine gdf_file_type (iunit,ftype)

implicit none

integer :: iunit,iver
character(len=*) :: ftype

logical, external :: isnumber
integer :: marker

character(len=64) :: token
logical :: isnum

ftype='none'

! Read the third line. If it begins with a numeric, then it is upper air.
!  Any errors will leave it set to 'none'.

read(31,*,end=100,err=100) marker, iver
read(31,*,end=100,err=100)
read(31,*,end=100,err=100) token

isnum = isnumber(token)
print*,'+++++++++++++++',isnum,token

if (isnum) then
   ftype='upa'
else
   ftype='sfc'
endif

return

100 continue

return
end subroutine

end module
