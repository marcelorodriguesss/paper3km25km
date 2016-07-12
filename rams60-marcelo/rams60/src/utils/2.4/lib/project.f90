!
! Copyright (C) 1991-2005  ; All Rights Reserved ; ATMET, LLC
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

Module project
   
   type proj_params
      integer :: proj
      real :: orig_lat,orig_lon,stdlat1,stdlat2
   end type
   
   type(proj_params) :: proj

   real, parameter :: erad=6.367e6,erad2=1.2734e7,pi180=3.14159265/180.

Contains

subroutine ll_proj(qlat,qlon,x,y)

! Convert from lat-lon to x-y in the chosen projection

implicit none

real :: qlat,qlon,x,y


if (proj%proj == 1) then

   call ll_xy(qlat,qlon,proj%orig_lat,proj%orig_lon,x,y)
   
elseif (proj%proj == 2) then

   call ll_lc2(qlat,qlon,proj%orig_lat,proj%orig_lon  &
                        ,proj%stdlat1,proj%stdlat2,x,y)

else

   print*,'ll_proj: Unknown projection type:',proj%proj
   stop 'll_proj: Unknown projection type'

endif

return
end subroutine

!---------------------------------------------------------------------

subroutine proj_ll(qlat,qlon,x,y)

! Convert from  x-y in the chosen projection to lat-lon

implicit none

real :: qlat,qlon,x,y


if (proj%proj == 1) then

   call xy_ll(qlat,qlon,proj%orig_lat,proj%orig_lon,x,y)
   
elseif (proj%proj == 2) then

   call lc_ll2(qlat,qlon,proj%orig_lat,proj%orig_lon  &
                        ,proj%stdlat1,proj%stdlat2,x,y)

else

   print*,'proj_ll: Unknown projection type:',proj%proj
   stop 'proj_ll: Unknown projection type'

endif

return
end subroutine


!---------------------------------------------------------------------

subroutine uvll_uvproj(uproj,vproj,ull,vll,qlat,qlon)

! Convert from earth relative components to u/v in the chosen projection

implicit none

real :: uproj,vproj,ull,vll,qlat,qlon,x,y


if (proj%proj == 1) then

   call uevetouv(uproj,vproj,ull,vll,qlat,qlon  &
                        ,proj%orig_lat,proj%orig_lon)
   
elseif (proj%proj == 2) then

   call uvll_uvlc2(uproj,vproj,ull,vll,qlat,qlon  &
                        ,proj%orig_lat,proj%orig_lon  &
                        ,proj%stdlat1,proj%stdlat2)

else

   print*,'uvll_uvproj: Unknown projection type:',proj%proj
   stop 'uvll_uvproj: Unknown projection type'

endif

return
end subroutine

!---------------------------------------------------------------------

subroutine uvproj_uvll(uproj,vproj,ull,vll,qlat,qlon)

! Convert from u/v in the chosen projection to earth relative components

implicit none

real :: uproj,vproj,ull,vll,qlat,qlon,x,y


if (proj%proj == 1) then

   call uvtoueve(uproj,vproj,ull,vll,qlat,qlon  &
                        ,proj%orig_lat,proj%orig_lon)
   
elseif (proj%proj == 2) then

   call uvlc_uvll2(uproj,vproj,ull,vll,qlat,qlon  &
                        ,proj%orig_lat,proj%orig_lon  &
                        ,proj%stdlat1,proj%stdlat2)

else

   print*,'uvproj_uvll: Unknown projection type:',proj%proj
   stop 'uvproj_uvll: Unknown projection type'

endif

return
end subroutine

!---------------------------------------------------------------------

end Module






