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


Module grid_struct

Type grid_def

integer :: nxp,nyp,nzp,nzg,nzs,npatch
real :: plat, plon
real, pointer, dimension(:) :: xtn,ytn,xmn,ymn,ztn,zmn
real, pointer, dimension(:,:) :: topo

End Type

Contains

!------------

subroutine alloc_grid_def(grd,nx,ny,nz)

implicit none

type(grid_def) :: grd

integer :: nx,ny,nz

allocate (grd%xtn(nx),grd%ytn(ny),grd%xmn(nx),grd%ymn(ny),grd%ztn(nz),grd%zmn(nz))
allocate (grd%topo(nx,ny))


return
end subroutine

!------------

subroutine dealloc_grid_def(grd)

implicit none

type(grid_def) :: grd

deallocate (grd%xtn,grd%ytn,grd%xmn,grd%ymn,grd%ztn,grd%zmn)
deallocate (grd%topo)


return
end subroutine

!------------

subroutine fill_grid_def(grd,nx,ny,nz,ng,ns,np,plt,pln  &
                           ,xt,xm,yt,ym,zt,zm,topo)

! Fill grid_def structure - must call alloc_grid_def first


implicit none

type(grid_def) :: grd

integer :: nx,ny,nz,ng,ns,np                    
real    :: plt,pln, xt(nx),yt(ny),zt(nz)  &
                   ,xm(nx),ym(ny),zm(nz),topo(nx,ny)


grd%nxp=nx   ; grd%nyp=ny   ; grd%nzp=nz 
grd%nzg=ng   ; grd%nzs=ns   ; grd%npatch=np
grd%plat=plt ; grd%plon=pln

grd%xtn(1:nx)=xt(1:nx)
grd%xmn(1:nx)=xm(1:nx)
grd%ytn(1:ny)=yt(1:ny)
grd%ymn(1:ny)=ym(1:ny)
grd%ztn(1:nz)=zt(1:nz)
grd%zmn(1:nz)=zm(1:nz)

grd%topo(1:nx,1:ny)=topo(1:nx,1:ny)

return
end subroutine

!------------

subroutine compare_grid_def(g1,g2,string,ierr)

! Compare two grids to see if they are the same structure 
!   Input: g1,g2 - grid_def structures filled with call to fill_grid_def
!          string - identifier string to be printed on errors

implicit none

type(grid_def) :: g1, g2
integer :: ierr
character(len=*) :: string

integer :: ict,nd
integer, external :: check_real

ict=0
ierr=0

if (g1%nxp /= g2%nxp ) then
   ict=ict+1
   print*,'Compare_grid:',trim(string),':nxp different',g1%nxp,g2%nxp
endif

if (g1%nyp /= g2%nyp ) then
   ict=ict+1
   print*,'Compare_grid:',trim(string),':nyp different',g1%nyp,g2%nyp
endif

if (g1%nzp /= g2%nzp ) then
   ict=ict+1
   print*,'Compare_grid:',trim(string),':nzp different',g1%nzp,g2%nzp
endif

if (g1%nzg /= g2%nzg ) then
   ict=ict+1
   print*,'Compare_grid:',trim(string),':nzg different',g1%nzg,g2%nzg
endif

if (g1%nzs /= g2%nzs ) then
   ict=ict+1
   print*,'Compare_grid:',trim(string),':nzs different',g1%nzs,g2%nzs
endif

if (g1%npatch /= g2%npatch ) then
   ict=ict+1
   print*,'Compare_grid:',trim(string),':npatch different',g1%npatch,g2%npatch
endif

nd = check_real(g1%plat,g2%plat,1)
if ( nd > 0 ) then
   ict=ict+1
   print*,'Compare_grid:',trim(string),':plat different',g1%plat,g2%plat
endif

nd =check_real(g1%plon,g2%plon,1)
if ( nd > 0 ) then
   ict=ict+1
   print*,'Compare_grid:',trim(string),':plon different',g1%plon,g2%plon
endif

nd = check_real(g1%xtn,g2%xtn,g1%nxp)
if ( nd > 0 ) then
   ict=ict+1
   print*,'Compare_grid:',trim(string),':xtn different',g1%xtn(nd),g2%xtn(nd)
endif

nd =check_real(g1%ytn,g2%ytn,g1%nyp)
if ( nd > 0 ) then
   ict=ict+1
   print*,'Compare_grid:',trim(string),':ytn different',g1%ytn(nd),g2%ytn(nd)
endif

if (g1%nzp == g2%nzp) then
   nd =check_real(g1%ztn(1),g2%ztn(1),g1%nzp)
   if ( nd > 0 ) then
      ict=ict+1
      print*,'Compare_grid:',trim(string),':ztn different',g1%ztn(nd),g2%ztn(nd)
   endif
else
   print*,'Compare_grid: ',trim(string),'not checking: ztn, nzp different'
endif

if(g1%nxp == g2%nxp .and. g1%nyp == g2%nyp) then
   nd =check_real(g1%topo(1,1),g2%topo(1,1),g1%nxp*g1%nyp)
   if ( nd > 0 ) then
      ict=ict+1
      print*,'Compare_grid:',trim(string),':topo different'
   endif
else
   print*,'Compare_grid: ',trim(string),'not checking: topo, nxp or nyp different'
endif

if (ict > 0) then
   print*
   print*,'Compare_grid:',trim(string),':',ict,' mismatches'
   ierr=1
endif

return
end subroutine

End Module   
