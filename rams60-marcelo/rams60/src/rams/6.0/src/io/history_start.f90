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
! 6.0.0
!
!###########################################################################


subroutine history_start(name_name)

! This routine initializes the model from the history file

use grid_dims
use var_tables
use io_params
use mem_grid
use ref_sounding

implicit none

character (len=*) :: name_name

integer :: ngrids1  &
          ,nnxp1(maxgrds),nnyp1(maxgrds),nnzp1(maxgrds),nzg1,nzs1,npatch1

integer :: iyr,imn,idy,itm,ie,maxarr,ngr,nc
character(len=2) :: cng
integer, external :: cio_i,cio_f
integer,save :: iunhd=11


! Open the input history header file and read some of the info.

call rams_f_open(iunhd,trim(hfilin),'FORMATTED','OLD','READ',0)

ie=cio_i(iunhd,1,'ngrids',ngrids1,1)
ngridsh=ngrids1
ie=cio_i(iunhd,1,'nnxp',nnxp1,ngrids1)
ie=cio_i(iunhd,1,'nnyp',nnyp1,ngrids1)
ie=cio_i(iunhd,1,'nnzp',nnzp1,ngrids1)
ie=cio_i(iunhd,1,'npatch',npatch1,1)
ie=cio_i(iunhd,1,'nzg',nzg1,1)
ie=cio_i(iunhd,1,'nzs',nzs1,1)
ie=cio_f(iunhd,1,'time',time,1)

! Get the 1-d reference state

do ngr=1,ngridsh
   write(cng,1) ngr
1       format(i2.2)
   ie=cio_f(iunhd,1,'u01dn'//cng,u01dn(1,ngr),nnzp(ngr))
   ie=cio_f(iunhd,1,'v01dn'//cng,v01dn(1,ngr),nnzp(ngr))
   ie=cio_f(iunhd,1,'pi01dn'//cng,pi01dn(1,ngr),nnzp(ngr))
   ie=cio_f(iunhd,1,'th01dn'//cng,th01dn(1,ngr),nnzp(ngr))
   ie=cio_f(iunhd,1,'dn01dn'//cng,dn01dn(1,ngr),nnzp(ngr))
   ie=cio_f(iunhd,1,'rt01dn'//cng,rt01dn(1,ngr),nnzp(ngr))
enddo

! Put these into regular arrays (for moving grids)
ie=cio_i(iunhd,1,'ninest',ninest,ngrids1)
ie=cio_i(iunhd,1,'njnest',njnest,ngrids1)


! Find maximum size of any array on history file. Allocate scratch array of
! this size.

maxarr=0
do ngr=1,ngridsh
   maxarr=max(maxarr,nnxp1(ngr)*nnyp1(ngr)*nnzp1(ngr)  &
         ,nnxp1(ngr)*nnyp1(ngr)*nzg1*npatch1 &
         ,nnxp1(ngr)*nnyp1(ngr)*nzs1*npatch1)
enddo

! read stuff here

call hist_read(maxarr,hfilin,iunhd)

print*,'back from read'
close(iunhd)


return
end


!*******************************************************************************

subroutine hist_read(maxarr,hnamein,iunhd)

use an_header
use var_tables
use mem_grid

use hdf5_utils

implicit none

integer :: maxarr,iunhd

character (len=*) :: hnamein
integer :: ngr,npts,nc,nv,nvh,ndims,idims(4)
character(len=1) :: cgrid
character(len=128) :: hname
real, allocatable :: scr(:)

type (head_table), allocatable,save :: hr_table(:)

allocate (scr(maxarr))


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


do ngr=1,ngridsh

   ! Open file
   write(cgrid,'(i1)') ngr
   nc=len_trim(hnamein)
   hname=hnamein(1:nc-9)//'-g'//cgrid//'.h5'

   call shdf5_open(hname,'R')

   ! Loop through all variables
   varloop: do nvh=1,nvbtab
      if(ngr /= hr_table(nvh)%ngrid) cycle varloop
      
      ! See if variable should be read and stored
      do nv = 1,num_var(ngr)
         if(hr_table(nvh)%string == vtab_r(nv,ngr)%name) then
            ! there is a match on this grid. see if hist flag is set...
            print*,'found: ', trim(hr_table(nvh)%string)
            if (vtab_r(nv,ngr)%ihist /= 1) cycle varloop
            print*,'read : ', trim(hr_table(nvh)%string)
            
            ! We want it...read, maybe rearrange, and store it
        !!!!    call shdf5_info(hr_table(nvh)%string,ndims,idims)
        !!!!    call shdf5_irec(ndims,idims,trim(hr_table(nvh)%string),rvara=scr)
            call shdf5_irec(trim(hr_table(nvh)%string),rvara=scr)
            
            !npts = product(idims(1:ndims))
            npts = vtab_r(nv,ngr)%npts
            
            select case(vtab_r(nv,ngr)%idim_type)
               case(2,6) ; call atob(npts,scr,vtab_r(nv,ngr)%var_p)
               case(3)
                  call unarrange(nnzp(ngr),nnxp(ngr),nnyp(ngr) &
                                 ,scr,vtab_r(nv,ngr)%var_p)
               case(4)
                  call unarrange_p(nnxp(ngr),nnyp(ngr),nzg,npatch &
                                 ,scr,vtab_r(nv,ngr)%var_p)
               case(5)
                  call unarrange_p(nnxp(ngr),nnyp(ngr),nzs,npatch &
                                 ,scr,vtab_r(nv,ngr)%var_p)
            end select
            cycle varloop
         endif
      enddo
   enddo varloop
   
   call shdf5_close()

enddo

deallocate(scr,hr_table)

return
end
