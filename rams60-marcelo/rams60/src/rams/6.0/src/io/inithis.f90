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

subroutine inithis()

use var_tables
use an_header
use mem_basic
use mem_grid
use rconstants
use ref_sounding
use io_params
use mem_scratch
use micphys

use mem_leaf

use hdf5_utils

implicit none

integer :: ngrids1,ioutput1,nzg1,nzs1,npatch1
real :: time1,ztop1
integer, allocatable, dimension(:) :: nnxp1,nnyp1,nnzp1
real, allocatable, dimension(:) :: platn1,plonn1
real, allocatable, dimension(:,:) :: xmn1,xtn1,ymn1,ytn1,zmn1,ztn1
real, allocatable, dimension(:) :: u01dn1,v01dn1,rt01dn1,th01dn1  &
                                  ,pi01dn1,dn01dn1

real, allocatable, dimension(:,:) :: topt1
real, allocatable, dimension(:,:) :: parea

integer :: iyr,imn,idy,itm,ie,maxarr,maxarr2,ngr,maxx1,maxy1,maxz1
character (len=80) :: hnameinh,prefix
character (len=256) :: hname
character (len=1) :: type,cgrid
character (len=10) :: post
character (len=3) :: fmt
character (len=2) :: cng
integer, external :: cio_i,cio_f
integer,save :: iunhd=11,inhunt=10

integer :: npts,nptsh,nv,nvh,i,k,nzpg1,nc,ndims,idims(5)
real, allocatable :: scr(:),scr2(:),scr3(:),scr4(:)

type (head_table), allocatable,save :: hr_table(:)


! Open the input history header file and read some of the info.

!nc=len_trim(hfilin)
!hnameinh=hfilin(1:nc-9)//'.vfm'

print*,'********************************: start inithis:',ngrid

call rams_f_open(iunhd,hfilin,'FORMATTED','OLD','READ',0)

ie=cio_i(iunhd,1,'ngrids',ngrids1,1)

allocate (nnxp1(ngrids1),nnyp1(ngrids1),nnzp1(ngrids1))
allocate (platn1(ngrids1),plonn1(ngrids1))

ie=cio_i(iunhd,1,'nnxp',nnxp1,ngrids1)
ie=cio_i(iunhd,1,'nnyp',nnyp1,ngrids1)
ie=cio_i(iunhd,1,'nnzp',nnzp1,ngrids1)
ie=cio_i(iunhd,1,'npatch',npatch1,1)
ie=cio_i(iunhd,1,'nzg',nzg1,1)
ie=cio_i(iunhd,1,'nzs',nzs1,1)
ie=cio_i(iunhd,1,'ioutput',ioutput1,1)
ie=cio_f(iunhd,1,'time',time1,1)
ie=cio_f(iunhd,1,'ztop',ztop1,1)
ie=cio_f(iunhd,1,'platn',platn1,ngrids1)
ie=cio_f(iunhd,1,'plonn',plonn1,ngrids1)


if (nzg /= nzg1 .or. nzs /= nzs1 .or. npatch /= npatch1) then
	print*,'LEAF parameters must be same for initial-history start'
	print*,'npatch: ',npatch,npatch1
	print*,'nzg: ',nzg,nzg1
	print*,'nzs: ',nzs,nzs1
	stop 'LEAF hist-init'
endif

! Find maximum size of any array on history file. Allocate scratch array of
! this size.

maxarr=0
maxarr2=0
maxx1=0
maxy1=0
maxz1=0
do ngr=1,ngrids1
   maxarr=max(maxarr,nnxp1(ngr)*nnyp1(ngr)*nnzp1(ngr)  &
         ,nnxp1(ngr)*nnyp1(ngr)*nzg1*npatch1 &
         ,nnxp1(ngr)*nnyp1(ngr)*nzs1*npatch1)
   maxarr2=max(maxarr2,nnxp1(ngr)*nnyp1(ngr))
   maxx1=max(maxx1,nnxp1(ngr))
   maxy1=max(maxy1,nnyp1(ngr))
   maxz1=max(maxz1,nnzp1(ngr))
enddo

allocate(xmn1(maxx1,ngrids1),xtn1(maxx1,ngrids1))
allocate(ymn1(maxy1,ngrids1),ytn1(maxy1,ngrids1))
allocate(zmn1(maxz1,ngrids1),ztn1(maxz1,ngrids1))

do ngr=1,ngrids1
   write(cng,'(i2.2)') ngr
   ie=cio_f(iunhd,1,'xmn'//cng,xmn1(1,ngr),nnxp1(ngr))
   ie=cio_f(iunhd,1,'xtn'//cng,xtn1(1,ngr),nnxp1(ngr))
   ie=cio_f(iunhd,1,'ymn'//cng,ymn1(1,ngr),nnyp1(ngr))
   ie=cio_f(iunhd,1,'ytn'//cng,ytn1(1,ngr),nnyp1(ngr))
   ie=cio_f(iunhd,1,'zmn'//cng,zmn1(1,ngr),nnzp1(ngr))
   ie=cio_f(iunhd,1,'ztn'//cng,ztn1(1,ngr),nnzp1(ngr))
enddo

allocate (topt1(maxarr2,ngrids1))
allocate (parea(maxarr,ngrids1))

allocate (scr(maxarr),scr2(maxarr),scr3(maxarr),scr4(maxarr))

!call rams_f_open(inhunt,hnameinh,'UNFORMATTED','OLD','READ',0)

!  Read variable header info

rewind(iunhd)

read(iunhd,*) nvbtab
allocate (hr_table(nvbtab))
do nv=1,nvbtab
   read(iunhd,*)  hr_table(nv)%string   &
                 ,hr_table(nv)%npointer  &
                 ,hr_table(nv)%idim_type  &
                 ,hr_table(nv)%ngrid  &
                 ,hr_table(nv)%nvalues
enddo

! Go through file and get all grids' TOPT's
do ngr=1,ngrids1

   ! Open file
   write(cgrid,'(i1)') ngr
   nc=len_trim(hfilin)
   hname=hfilin(1:nc-9)//'-g'//cgrid//'.h5'

   call shdf5_open(hname,'R')

!   call shdf5_info('TOPT',ndims,idims)
   call shdf5_irec('TOPT',rvara=topt1(1,ngr))
!   call shdf5_info('PATCH_AREA',ndims,idims)
   call shdf5_irec('PATCH_AREA',rvara=parea(1,ngr))

   call shdf5_close()

enddo


! Go through file and get all grids' TOPT's
!   Don't know how else to get this before processing a field...
!do nvh=1,nvbtab 
!   ngr=hr_table(nvh)%ngrid
!   nptsh=hr_table(nvh)%nvalues
!
!   read(inhunt)(scr(i),i=1,nptsh)
!
!   if (hr_table(nvh)%string == 'TOPT') then
!      call atob(nptsh, scr(1),topt1(1,ngr))
!   elseif (hr_table(nvh)%string == 'PATCH_AREA') then
!      call atob(nptsh, scr(1),parea(1,ngr))
!   endif
!enddo

!rewind inhunt


!!!!!!!!!!!!!!!!  Need wind rotation for the general case!!!!!!!!!!!!!!!!!!!!!!



! Loop through all variables
!do nvh=1,nvbtab
   ! Read a variable
!   nptsh=hr_table(nvh)%nvalues

!   read(inhunt)(scr(i),i=1,nptsh)
   
   !  See if this variable is active in the current run and interpolate 
   !    to new grid stucture. For the leaf variables, we will interpolate 
   !    everything except TOPxx, SOIL_TEXT, PATCH_AREA, and LEAF_CLASS.
   
!   ngr=hr_table(nvh)%ngrid
   
!      print*,'inithis: start: ' &
!            ,nvh,ngr,hr_table(nvh)%string,hr_table(nvh)%idim_type,nptsh
            
!   do nv = 1,num_var(1)
!      npts=vtab_r(nv,1)%npts
!      if(hr_table(nvh)%string == vtab_r(nv,1)%name) then
         
!         print*,'inithis: interpolating: ',ngr,' '  &
!                 ,nv,1,vtab_r(nv,1)%name,npts,vtab_r(nv,1)%idim_type

do ngr=1,ngrids1

   ! Open file
   write(cgrid,'(i1)') ngr
   nc=len_trim(hfilin)
   hname=hfilin(1:nc-9)//'-g'//cgrid//'.h5'

   call shdf5_open(hname,'R')

   ! Loop through all variables
   varloop: do nvh=1,nvbtab
      if(ngr /= hr_table(nvh)%ngrid) cycle varloop

      ! See if variable should be read and stored
      do nv = 1,num_var(1)

         if(hr_table(nvh)%string == vtab_r(nv,1)%name) then
            print*,'read : ', ngr, trim(hr_table(nvh)%string)

            ! We want it...read, maybe rearrange, and store it
!            call shdf5_info(hr_table(nvh)%string,ndims,idims)
            call shdf5_irec(trim(hr_table(nvh)%string),rvara=scr4)

   !  See if this variable is active in the current run and interpolate
   !    to new grid stucture. For the leaf variables, we will interpolate
   !    everything except TOPxx, SOIL_TEXT, PATCH_AREA, and LEAF_CLASS.

!            print*,'inithis: start: ' &
!            ,nvh,ngr,hr_table(nvh)%string,hr_table(nvh)%idim_type,nptsh


!            print*,'inithis: interpolating: ',ngr,' '  &
!                 ,nv,1,vtab_r(nv,1)%name,npts,vtab_r(nv,1)%idim_type

            select case(hr_table(nvh)%idim_type)
               case(2,6) ; call atob(hr_table(nvh)%nvalues,scr,scr4)
               case(3)
                  call unarrange(nnzp1(ngr),nnxp1(ngr),nnyp1(ngr) &
                                 ,scr4,scr)
               case(4)
                  call unarrange_p(nnxp1(ngr),nnyp1(ngr),nzg1,npatch1 &
                                 ,scr4,scr)
               case(5)
                  call unarrange_p(nnxp1(ngr),nnyp1(ngr),nzs1,npatch1 &
                                 ,scr4,scr)
            end select
                 
         if (vtab_r(nv,1)%idim_type == 2 .and.  &
                 hr_table(nvh)%string /= 'TOPT' .and.   &
                 hr_table(nvh)%string /= 'TOPTA' .and.   &
                 hr_table(nvh)%string /= 'TOPMA' .and.   &                 
                 hr_table(nvh)%string /= 'GLAT' .and.   &
                 hr_table(nvh)%string /= 'GLON' &
               ) then
            call hi_interp(1,nnxp1(ngr),nnyp1(ngr),1,scr(1)  &
                             ,xmn1(1,ngr),xtn1(1,ngr)  &
                             ,ymn1(1,ngr),ytn1(1,ngr)  &
                             ,zmn1(1,ngr),ztn1(1,ngr)  &
                             ,platn1(ngr),plonn1(ngr)  &
                             ,topt1(1,ngr),ztop1  &
                             ,1,nnxp(1),nnyp(1),1  &
                             ,vtab_r(nv,1)%var_p  &
                             ,1,ngr,vtab_r(nv,1)%name,2)

         elseif (vtab_r(nv,1)%idim_type == 3) then
            call hi_interp(nnzp1(ngr),nnxp1(ngr),nnyp1(ngr),1,scr(1)  &
                             ,xmn1(1,ngr),xtn1(1,ngr)  &
                             ,ymn1(1,ngr),ytn1(1,ngr)  &
                             ,zmn1(1,ngr),ztn1(1,ngr)  &
                             ,platn1(ngr),plonn1(ngr)  &
                             ,topt1(1,ngr),ztop1  &
                             ,nnzp(1),nnxp(1),nnyp(1),1  &
                             ,vtab_r(nv,1)%var_p  &
                             ,1,ngr,vtab_r(nv,1)%name,3)

         elseif (vtab_r(nv,1)%idim_type == 4 .and.  &
                 hr_table(nvh)%string /= 'SOIL_TEXT' ) then

            ! First, interpolate patch 1.
            call hi_interp(nzg1,nnxp1(ngr),nnyp1(ngr),1,scr(1)  &
                             ,xmn1(1,ngr),xtn1(1,ngr)  &
                             ,ymn1(1,ngr),ytn1(1,ngr)  &
                             ,zmn1(1,ngr),ztn1(1,ngr)  &
                             ,platn1(ngr),plonn1(ngr)  &
                             ,topt1(1,ngr),ztop1  &
                             ,nzg,nnxp(1),nnyp(1),1  &
                             ,vtab_r(nv,1)%var_p  &
                             ,1,ngr,vtab_r(nv,1)%name,4)
            
            ! Copy grid 1, patch 2 to scr3 - Irrelevant if this is the coarsest
            !   grid on the history file, this will contain the land average
            !   if we have interpolated from the coarser grids. 
            !   We will overwrite points as we interpolate from finer grids.
            
            call patch_land_copy2(nzg,nnxp(1),nnyp(1),npatch  &
                                 ,vtab_r(nv,1)%var_p,scr3(1))
                                 
            ! Then average over history grid patches and interpolate
            call patch_land_average(nzg1,nnxp1(ngr),nnyp1(ngr),npatch1  &
                              ,parea(1,ngr),scr(1),scr2(1))
            call hi_interp(nzg1,nnxp1(ngr),nnyp1(ngr),1,scr2(1)  &
                             ,xmn1(1,ngr),xtn1(1,ngr)  &
                             ,ymn1(1,ngr),ytn1(1,ngr)  &
                             ,zmn1(1,ngr),ztn1(1,ngr)  &
                             ,platn1(ngr),plonn1(ngr)  &
                             ,topt1(1,ngr),ztop1  &
                             ,nzg,nnxp(1),nnyp(1),1  &
                             ,scr3(1)  &
                             ,1,ngr,vtab_r(nv,ngr)%name,4)
            call patch_land_unaverage(nzg,nnxp(1),nnyp(1),npatch  &
                              ,scr3(1),vtab_r(nv,1)%var_p)

         elseif (vtab_r(nv,1)%idim_type == 5 ) then
         
            call hi_interp(nzs1,nnxp1(ngr),nnyp1(ngr),1,scr(1)  &
                             ,xmn1(1,ngr),xtn1(1,ngr)  &
                             ,ymn1(1,ngr),ytn1(1,ngr)  &
                             ,zmn1(1,ngr),ztn1(1,ngr)  &
                             ,platn1(ngr),plonn1(ngr)  &
                             ,topt1(1,ngr),ztop1  &
                             ,nzs,nnxp(1),nnyp(1),1  &
                             ,vtab_r(nv,1)%var_p  &
                             ,1,ngr,vtab_r(nv,1)%name,5)
            
            ! Copy patch 2 to scr3 - This will contain the land average
            call patch_land_copy2(nzs,nnxp(1),nnyp(1),npatch  &
                                 ,vtab_r(nv,1)%var_p,scr3(1))
            call patch_land_average(nzs1,nnxp1(ngr),nnyp1(ngr),npatch1  &
                              ,parea(1,ngr),scr(1),scr2(1))
            call hi_interp(nzs1,nnxp1(ngr),nnyp1(ngr),1,scr2(1)  &
                             ,xmn1(1,ngr),xtn1(1,ngr)  &
                             ,ymn1(1,ngr),ytn1(1,ngr)  &
                             ,zmn1(1,ngr),ztn1(1,ngr)  &
                             ,platn1(ngr),plonn1(ngr)  &
                             ,topt1(1,ngr),ztop1  &
                             ,nzs,nnxp(1),nnyp(1),1  &
                             ,scr3(1)  &
                             ,1,ngr,vtab_r(nv,1)%name,5)
            call patch_land_unaverage(nzs,nnxp(1),nnyp(1),npatch  &
                              ,scr3(1),vtab_r(nv,1)%var_p)

         elseif (vtab_r(nv,1)%idim_type == 6 .and.  &
                 hr_table(nvh)%string /= 'LEAF_CLASS' .and.   &
                 hr_table(nvh)%string /= 'PATCH_AREA' ) then
            
            call hi_interp(1,nnxp1(ngr),nnyp1(ngr),1,scr(1)  &
                             ,xmn1(1,ngr),xtn1(1,ngr)  &
                             ,ymn1(1,ngr),ytn1(1,ngr)  &
                             ,zmn1(1,ngr),ztn1(1,ngr)  &
                             ,platn1(ngr),plonn1(ngr)  &
                             ,topt1(1,ngr),ztop1  &
                             ,1,nnxp(1),nnyp(1),1  &
                             ,vtab_r(nv,1)%var_p  &
                             ,1,ngr,vtab_r(nv,1)%name,6)
            
            ! Copy patch 2 to scr3 - This will contain the land average
            call patch_land_copy2(1,nnxp(1),nnyp(1),npatch  &
                                 ,vtab_r(nv,1)%var_p,scr3(1))
            
            call patch_land_average(1,nnxp1(ngr),nnyp1(ngr),npatch1  &
                              ,parea(1,ngr),scr(1),scr2(1))
            call hi_interp(1,nnxp1(ngr),nnyp1(ngr),1,scr2(1)  &
                             ,xmn1(1,ngr),xtn1(1,ngr)  &
                             ,ymn1(1,ngr),ytn1(1,ngr)  &
                             ,zmn1(1,ngr),ztn1(1,ngr)  &
                             ,platn1(ngr),plonn1(ngr)  &
                             ,topt1(1,ngr),ztop1  &
                             ,1,nnxp(1),nnyp(1),1  &
                             ,scr3(1)  &
                             ,1,ngr,vtab_r(nv,1)%name,6)
           call patch_land_unaverage(1,nnxp(1),nnyp(1),npatch  &
                              ,scr3(1),vtab_r(nv,1)%var_p)
         endif
         exit         
      endif
      enddo

   enddo varloop
   call shdf5_close()

enddo

! Close the input history file

!close(inhunt)

deallocate(scr,scr2,scr3,scr4,hr_table)

!  Prepare 1D reference sounding

nzpg1=nnzp1(1)
allocate(u01dn1(nzpg1), v01dn1(nzpg1),rt01dn1(nzpg1)  &
       ,th01dn1(nzpg1),pi01dn1(nzpg1),dn01dn1(nzpg1) )

cng='01'
ie=cio_f(iunhd,1,'u01dn'//cng,  u01dn1(1),nnzp1(1))
ie=cio_f(iunhd,1,'v01dn'//cng,  v01dn1(1),nnzp1(1))
ie=cio_f(iunhd,1,'pi01dn'//cng,pi01dn1(1),nnzp1(1))
ie=cio_f(iunhd,1,'th01dn'//cng,th01dn1(1),nnzp1(1))
ie=cio_f(iunhd,1,'dn01dn'//cng,dn01dn1(1),nnzp1(1))
ie=cio_f(iunhd,1,'rt01dn'//cng,rt01dn1(1),nnzp1(1))


call htint(nzpg1,th01dn1,ztn1(1,1),nnzp(1),vctr1,ztn(1,1))
call htint(nzpg1,u01dn1,ztn1(1,1) ,nnzp(1),u01dn(1,1),ztn(1,1))
call htint(nzpg1,v01dn1,ztn1(1,1) ,nnzp(1),v01dn(1,1),ztn(1,1))

if (level .ge. 1) then
   call htint(nzpg1,rt01dn1,ztn1(1,1),nnzp(1),rt01dn(1,1),ztn(1,1))
else
   rt01dn(1:nnzp(ngrid),1) = 0.
endif

do k = 1,nnzp(ngrid)
   th01dn(k,1) = vctr1(k) * (1. + .61 * rt01dn(k,1))
enddo
u01dn(1,1) = u01dn(2,1)
v01dn(1,1) = v01dn(2,1)
rt01dn(1,1) = rt01dn(2,1)
th01dn(1,1) = th01dn(2,1)

pi01dn(1,1) = pi01dn1(1) + g * (ztn1(1,1) - ztn(1,1))  &
   / (.5 * (th01dn(1,1) + th01dn1(1)*(1.+.61*rt01dn1(1)) ) )
do k = 2,nnzp(1)
   pi01dn(k,1) = pi01dn(k-1,1) - g / (dzmn(k-1,1)  &
      * .5 * (th01dn(k,1) + th01dn(k-1,1)))
enddo

do k = 1,nnzp(1)
   vctr4(k) = (pi01dn(k,1) / cp) ** cpor * p00
   dn01dn(k,1) = cp * vctr4(k)  &
      / (rgas * th01dn(k,1) * pi01dn(k,1))
enddo


close(iunhd)

! Compute 3d reference state for grid 1

call newgrid(1)
call refs3d(nnzp(1),nnxp(1),nnyp(1)  &
  ,basic_g(1)%pi0  (1,1,1)  ,basic_g(1)%dn0  (1,1,1)  &
  ,basic_g(1)%dn0u (1,1,1)  ,basic_g(1)%dn0v (1,1,1)  &
  ,basic_g(1)%th0  (1,1,1)  ,grid_g(1)%topt  (1,1)    &
  ,grid_g(1)%rtgt  (1,1)                                )


return
end

!---------------------------------------------------------------------
subroutine prtptr(str,a,n)
character(len=*) :: str
dimension a(n)
print*,str,a(1:n)
return
end 







subroutine sfcinit_hstart()
                     
use mem_leaf
use mem_basic
use mem_grid
use leaf_coms
use rconstants


implicit none

integer :: i,j,ifm,ipat,k2,k,nveg
real :: c1
real, allocatable, dimension(:,:) :: hpis,hprss

! This subroutine fills the LEAF arrays for a history-initial start.

! Refill many of the LEAF variables, as the interpolated values may not be relevant.

c1 = .5 * cpi

do ifm=1,ngrids

allocate(hpis(nnxp(ifm),nnyp(ifm)),hprss(nnxp(ifm),nnyp(ifm)))

do j = 1,nnyp(ifm)
   do i = 1,nnxp(ifm)
      k2=grid_g(ifm)%lpw(i,j)
      hpis(i,j) = c1 * (basic_g(ifm)%pi0(k2-1,i,j) + basic_g(ifm)%pi0(k2,i,j)   &
                      + basic_g(ifm)%pp(k2-1,i,j) + basic_g(ifm)%pp(k2,i,j))
    !  airtemp = basic_g(ifm)%theta(k2,i,j) * pis(i,j)
      hprss(i,j) = hpis(i,j) ** cpor * p00

      leaf_g(ifm)%patch_rough(i,j,1) = 0.001
     ! leaf_g(ifm)%can_temp(i,j,1) = airtemp
     ! leaf_g(ifm)%can_rvap(i,j,1) = basic_g(ifm)%rv(k2,i,j)

      do ipat = 2,npatch

         nveg = nint(leaf_g(ifm)%leaf_class(i,j,ipat))
         if(nveg == 0) then
            leaf_g(ifm)%leaf_class(i,j,ipat)=1.
            nveg = nint(leaf_g(ifm)%leaf_class(i,j,ipat))
         endif
 
         leaf_g(ifm)%soil_rough(i,j,ipat) = zrough
         leaf_g(ifm)%patch_rough(i,j,ipat) = max(zrough,grid_g(ifm)%topzo(i,j))

         leaf_g(ifm)%veg_rough(i,j,ipat) = .13 * veg_ht(nveg)
         leaf_g(ifm)%veg_height(i,j,ipat) = veg_ht(nveg)
         
         leaf_g(ifm)%stom_resist(i,j,ipat) = 1.e6

      !   leaf_g(ifm)%veg_temp(i,j,ipat) = airtemp
      !   leaf_g(ifm)%can_temp(i,j,ipat) = airtemp

      !   leaf_g(ifm)%veg_water(i,j,ipat) = 0.
      !   leaf_g(ifm)%can_rvap(i,j,ipat) = rv(k2,i,j)


         do k = 1,nzs

            leaf_g(ifm)%sfcwater_nlev(i,j,ipat) = 0.
            if (leaf_g(ifm)%sfcwater_mass(k,i,j,ipat) > 0.)  &
               leaf_g(ifm)%sfcwater_nlev(i,j,ipat) = float(k)

         enddo
        


         if (ipat >= 2) call vegndvi(ifm   &
            ,leaf_g(ifm)%patch_area  (i,j,ipat) &
            ,leaf_g(ifm)%leaf_class(i,j,ipat)  &
            ,leaf_g(ifm)%veg_fracarea(i,j,ipat) &
            ,leaf_g(ifm)%veg_lai   (i,j,ipat)  &
            ,leaf_g(ifm)%veg_tai     (i,j,ipat) &
            ,leaf_g(ifm)%veg_rough (i,j,ipat)  &
            ,leaf_g(ifm)%veg_height  (i,j,ipat) &
            ,leaf_g(ifm)%veg_albedo(i,j,ipat)  &
            ,leaf_g(ifm)%veg_ndvip   (i,j,ipat) &
            ,leaf_g(ifm)%veg_ndvic (i,j,ipat)  &
            ,leaf_g(ifm)%veg_ndvif   (i,j,ipat)        )

         call grndvap(  &
             leaf_g(ifm)%soil_energy    (nzg,i,j,ipat) &
             ,leaf_g(ifm)%soil_water    (nzg,i,j,ipat)  &
            ,leaf_g(ifm)%soil_text      (nzg,i,j,ipat) &
            ,leaf_g(ifm)%sfcwater_energy(nzs,i,j,ipat)  &
            ,leaf_g(ifm)%sfcwater_nlev  (i,j,ipat)      &
            ,leaf_g(ifm)%ground_rsat    (i,j,ipat)     &
            ,leaf_g(ifm)%ground_rvap    (i,j,ipat)      &
            ,leaf_g(ifm)%can_temp       (i,j,ipat)     &
            ,leaf_g(ifm)%can_rvap       (i,j,ipat)      &
            ,hprss       (i,j)                          )

      enddo
   enddo
enddo
print*,'si-hs:',maxval(leaf_g(ifm)%soil_text),minval(leaf_g(ifm)%soil_text)
deallocate(hpis,hprss)

enddo

return
end

!---------------------------------------------------------------------

subroutine hi_interp(n1,n2,n3,n4,vn,xm1,xt1,ym1,yt1,zm1,zt1  &
                    ,plat1,plon1,topt1,ztop1  &
                    ,m1,m2,m3,m4,vm,ngm,ngr1,vname,idim)
                      
use mem_grid
use mem_scratch

implicit none

integer :: n1,n2,n3,n4,ngr1,idim
real, dimension(n1,n2,n3,n4) :: vn
real, dimension(n2,n3) :: topt1
real, dimension(n1) :: zm1,zt1
real, dimension(n2) :: xm1,xt1
real, dimension(n3) :: ym1,yt1
real :: plat1,plon1,ztop1

integer :: m1,m2,m3,m4,ngm
real, dimension(m1,m2,m3,m4) :: vm

character (len=*) :: vname

integer :: i,j,k,np,ii,jj
real :: xxm,yym,fixxm,fiyym,topoh,rtgth
real, allocatable :: scr3(:,:,:,:)

! This routine will interpolate the new run's coarse grid only

!print*,'nssssss1:',n1,n2,n3,n4,vname
!print*,'mssssss1:',m1,m2,m3,m4,allocated(scr3)

if(allocated(scr3)) deallocate(scr3) ; allocate(scr3(n2,n3,n1,n4))

! We are going to swap indices due to excessive memory copies of
!     non-contiguous memory

do np=1,n4
   do k=1,n1
      scr3(1:n2,1:n3,k,np)=vn(k,1:n2,1:n3,np)
   enddo
enddo
   
! I'm going to interpolate to T points, then average to velocity points

do j=1,m3
   do i=1,m2

      ! Find real grid point x,y relative to history file
      call ll_xy (grid_g(ngm)%glat(i,j),grid_g(ngm)%glon(i,j) &
                 ,plat1,plon1,xxm,yym)
      
      ! See if point is on this grid. 
      if (xxm < xm1(1) .or. xxm > xm1(n2-1) .or. &
          yym < ym1(1) .or. yym > ym1(n3-1) ) then 
          
         if (ngr1 == 1) then
           
            ! We are on the input coarse grid and point is not on this grid. 
            !    Stop immediately...
            print*,grid_g(ngm)%glat(i,j),grid_g(ngm)%glon(i,j) &
                 ,plat1,plon1,xxm,yym
            print*,xm1(1),xm1(n2-1),xxm
            print*,ym1(1),ym1(n3-1),yym
            print*, 'His_init: grid point not on history file grids'
            stop 'his_init: point OB'
         else
            ! Otherwise, go to next point
            cycle
         endif
         
      endif
          
      ! We are okay horizontally, now interpolate vertical column from 
      !     field 
      
      ! Find x,y grid point locations on input field. 
      !     Assuming constant spacing and deal with stagger
      
      if(vname == 'UP' .or. vname == 'UC') then
         fixxm=1.+(xxm-xm1(1))/(xm1(2)-xm1(1))
      else
         fixxm=1.+(xxm-xt1(1))/(xt1(2)-xt1(1))
      endif
      if(vname == 'VP' .or. vname == 'VC') then
         fiyym=1.+(yym-ym1(1))/(ym1(2)-ym1(1))
      else
         fiyym=1.+(yym-yt1(1))/(yt1(2)-yt1(1))
      endif
      
      do np=1,n4
         do k=1,n1
            call gdtost2(scr3(1,1,k,np),n2,n3,fixxm,fiyym,vctr1(k))    
         enddo      

      if (idim == 3) then
         ! Interpolate this column vertically to actual grid if 3d variable
         call gdtost2(topt1(1,1),n2,n3,fixxm,fiyym,topoh)    
         rtgth=1.-topoh/ztop1
         do k=1,m1
            ! Actual grid level heights
            vctr2(k)= grid_g(ngm)%topt(i,j)   &
                      + ztn(k,1) *grid_g(ngm)%rtgt(i,j)
         enddo
         do k=1,n1
            ! History grid level heights
            vctr3(k)= topoh + zt1(k) *rtgth
         enddo

         ! Interpolate vertically
         
         call htint(n1,vctr1(1),vctr3(1),m1,vctr10(1),vctr2(1))
         vm(1:m1,i,j,np)=vctr10(1:m1)
      elseif (idim == 2) then
         vm(1,i,j,np)=vctr1(1)
      elseif (idim == 4 .or. idim == 5) then
         vm(1:m1,i,j,np)=vctr1(1:m1)
      elseif (idim == 6) then
         vm(1,i,j,np)=vctr1(1)
      endif

      enddo

   enddo
enddo

if     (vname == 'UP' .or. vname == 'UC') then
   call hi_avgu(m1,m2,m3,vm(1,1,1,1))
elseif (vname == 'VP' .or. vname == 'VC') then
   call hi_avgv(m1,m2,m3,vm(1,1,1,1))
elseif (vname == 'WP' .or. vname == 'WC') then
   call hi_avgw(m1,m2,m3,vm(1,1,1,1))
endif

deallocate(scr3)

return
end

!-------------------------------------------------------------------------

subroutine hi_avgu(m1,m2,m3,u)
implicit none
integer :: m1,m2,m3
real :: u(m1,m2,m3)
integer :: i,j,k

do k=1,m1
   do j=1,m3
      do i=1,m2-1
         u(k,i,j)=(u(k,i,j)+u(k,i+1,j))*.5
      enddo
      u(k,m2,j)=u(k,m2-1,j)
   enddo
enddo

return
end
subroutine hi_avgv(m1,m2,m3,v)
implicit none
integer :: m1,m2,m3
real :: v(m1,m2,m3)
integer :: i,j,k

do k=1,m1
   do i=1,m2
      do j=1,m3-1
         v(k,i,j)=(v(k,i,j)+v(k,i,j+1))*.5
      enddo
      v(k,i,m3)=v(k,i,m3-1)
   enddo
enddo

return
end

subroutine hi_avgw(m1,m2,m3,w)
implicit none
integer :: m1,m2,m3
real :: w(m1,m2,m3)
integer :: i,j,k

do j=1,m3
   do i=1,m2
      do k=1,m1-1
         w(k,i,j)=(w(k,i,j)+w(k+1,i,j))*.5
      enddo
      w(m1,i,j)=w(m1-1,i,j)
   enddo
enddo

return
end

!-------------------------------------------------------------------------

subroutine patch_land_average(n1,n2,n3,n4,parea,pa1,paa)

implicit none

integer :: n1,n2,n3,n4
real, dimension (n1,n2,n3,n4) :: pa1
real, dimension (n1,n2,n3)    :: paa
real, dimension (n2,n3,n4)    :: parea

integer :: i,j,k,np
real :: sw

print*,'plandav:',n1,n2,n3,n4
! Compute average of patch variable across all land patches.
!     Recall water patch is first patch, all others are land.

do j=1,n3
   do i=1,n2
      do k=1,n1
         
         sw=0.
         paa(k,i,j)=0.
         do np=2,n4
            paa(k,i,j)=paa(k,i,j)+pa1(k,i,j,np)*parea(i,j,np)
            sw=sw+parea(i,j,np)
         enddo
         
         if(sw > 0.) then
            paa(k,i,j)=paa(k,i,j)/sw
         else
            paa(k,i,j)=pa1(k,i,j,2)
         endif
         
      enddo
   enddo
enddo

return
end


subroutine patch_land_unaverage(n1,n2,n3,n4,paa,pa1)

implicit none

integer :: n1,n2,n3,n4
real, dimension (n1,n2,n3,n4) :: pa1
real, dimension (n1,n2,n3)    :: paa

integer :: i,j,k,np
real :: sw

! Assign average patch variable to all land patches.
!     Recall water patch is first patch, all others are land.

do j=1,n3
   do i=1,n2
      do k=1,n1
         
         do np=2,n4
            pa1(k,i,j,np)=paa(k,i,j)
         enddo
         
      enddo
   enddo
enddo
print*,'unav:',maxval(paa),minval(paa)
return
end

subroutine patch_land_copy2(n1,n2,n3,n4,pa1,paa)

implicit none

integer :: n1,n2,n3,n4
real, dimension (n1,n2,n3,n4) :: pa1
real, dimension (n1,n2,n3)    :: paa

integer :: i,j,k,np
real :: sw

! Assign average patch variable to all land patches.
!     Recall water patch is first patch, all others are land.

do j=1,n3
   do i=1,n2
      do k=1,n1
            paa(k,i,j)=pa1(k,i,j,2)
      enddo
   enddo
enddo

return
end


