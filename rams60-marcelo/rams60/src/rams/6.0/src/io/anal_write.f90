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

subroutine anal_write(restart,vtype)

use an_header
use var_tables
use mem_scratch
use mem_basic
use mem_turb
use mem_grid
use io_params

use anal_extra
use hdf5_utils

implicit none

! This routine writes the chosen variables on the analysis file.

character*(*) restart,vtype

character(len=256) :: anamel,anamelh,command
character(len=2)  :: cgrid
character(len=25) :: subaname
character(len=16) :: varn
character(len=1)  :: vnam
logical exans
integer, save :: ioaunt=10,ncall_head=0,nvtota=0,nvtoth=0,nvtotl=0  &
                ,nvtot
integer :: ngr,nv,nvcnt,lenl,npointer,n3d,indwrt,n2d,iwrite,npts,idtype
real :: timeold
logical :: first_call=.true.
character(len=256), save :: anameold
real, save :: time_save

integer :: ndims,idims(4)

real, pointer :: v_pointer

type (head_table), allocatable,save :: aw_table(:)

if (ioutput == 0) return

if (ncall_head == 0) then
   ! Get "extra" variable count
   call anal_extra_init()

   !  Find total number of fields to be written
   do ngr=1,ngrids
      do nv = 1,num_var(ngr)
         if ( vtab_r(nv,ngr)%ianal == 1 .or.  &
              vtab_r(nv,ngr)%ihist == 1) nvtota=nvtota+1
         if ( vtab_r(nv,ngr)%ilite == 1) nvtotl=nvtotl+1
      enddo
   enddo
   nvtot=max( nvtota + ngrids*num_extra_anal, nvtotl)
   allocate (aw_table(nvtot))
   ncall_head=1
endif

!print*,'^^^^^^^^^:',nvtota,num_extra_anal,nvtotl

timeold=time
if(vtype == 'MEAN'.or.vtype == 'BOTH') time=min(time,time-avgtim/2.)


! Construct header file name

if(vtype == 'INST') vnam='A'
if(vtype == 'LITE') vnam='L'
if(vtype == 'MEAN') vnam='M'
if(vtype == 'BOTH') vnam='B'
call makefnam(anamelh,afilepref,time,iyear1,imonth1,idate1,  &
     itime1*100,vnam,'head','txt')

! Loop through each nest

nvcnt=0

do ngr=1,ngrids
   
   ! If this routine is called, then 1 or more grids will get written.
   !     See if this isn't one of them...
   
   if(vtype == 'INST') then
      ! If this routine is called, then 1 or more grids will get written.
      !     See if this isn't one of them...
      if(mod(time,frqstate(ngr)) > dtlongn(1).and.  &
                         time  <  timmax - .01*dtlongn(1) .and.  &
                         iflag == 0) cycle 
   else
      ! If it isn't the instantaneous files, then we will be outputting all
      !  grids. We would need grid dependent frqmean, etc, and maybe averaging
      !  times also to vary the grids for other write types.
   endif
                        
      
   write(cgrid,'(a1,i1)') 'g',ngr
   call makefnam(anamel,afilepref,time,iyear1,imonth1,idate1,  &
           itime1*100,vnam,cgrid,'h5')

   lenl = len_trim(anamel)

   inquire(file=anamel,exist=exans)
   if(exans.and.iclobber == 0) then
      print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      print*,'!!!   trying to open file name :'
      print*,'!!!       ',anamel
      print*,'!!!   but it already exists. run is ended.'
      print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      stop 'anal_write'
   endif
   
   print*,'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
   print*,'+ anal_write: open file:',trim(anamel)
   print*,'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
   call shdf5_open(anamel,'W',iclobber)
   npointer=0

!  Loop through the main variable table and write those variables
!     with the correct flag set

   do nv = 1,num_var(ngr)
      !print*,'rio:',vtab_r(nv,ngr)%name,vtab_r(nv,ngr)%ilite
      
      iwrite=0
      if(vtype == 'INST' .and. &
            (vtab_r(nv,ngr)%ianal == 1.or.vtab_r(nv,ngr)%ihist == 1)) then
         iwrite=1
         v_pointer => vtab_r(nv,ngr)%var_p
      elseif(vtype == 'LITE' .and. vtab_r(nv,ngr)%ilite == 1) then
         iwrite=1
         v_pointer => vtab_r(nv,ngr)%var_p
      elseif(vtype == 'MEAN' .and. &
            (vtab_r(nv,ngr)%ianal == 1.or.vtab_r(nv,ngr)%ihist == 1)) then
         iwrite=1
         v_pointer => vtab_r(nv,ngr)%var_m
      elseif(vtype == 'BOTH' .and. vtab_r(nv,ngr)%ilite == 1) then
         iwrite=1
         v_pointer => vtab_r(nv,ngr)%var_m
      endif

      if(iwrite == 1) then
      
         varn= vtab_r(nv,ngr)%name
         print '(a,a,$)','writing: ',varn
         
         ndims=2 ; idims(1)=nnxp(ngr) ; idims(2)=nnyp(ngr)
         
         if(vtab_r(nv,ngr)%idim_type == 3) then
            !  Rearrange 3-d variables to (i,j,k)
            call rearrange(nnzp(ngr),nnxp(ngr),nnyp(ngr)  &
                          ,v_pointer,scratch%scr2(1))
            v_pointer =>scratch%scr2(1)
            ndims=3 ; idims(3)=nnzp(ngr)
      
         elseif(vtab_r(nv,ngr)%idim_type == 4) then
            !  Rearrange 4-d leaf%soil variables to (i,j,k,ip)
            call rearrange_p(nnxp(ngr),nnyp(ngr),nzg,npatch  &
                            ,v_pointer,scratch%scr2(1))
            v_pointer =>scratch%scr2(1)
            ndims=4 ; idims(3)=nzg ; idims(4)=npatch
      
         elseif(vtab_r(nv,ngr)%idim_type == 5) then
            !  Rearrange 4-d leaf%sfcwater variables to (i,j,k,ip)
            call rearrange_p(nnxp(ngr),nnyp(ngr),nzs,npatch  &
                            ,v_pointer,scratch%scr2(1))
            v_pointer =>scratch%scr2(1)
            ndims=4 ; idims(3)=nzs ; idims(4)=npatch
         
         elseif(vtab_r(nv,ngr)%idim_type == 6) then
            ndims=3 ; idims(3)=npatch
         endif
      
         nvcnt=nvcnt+1
         aw_table(nvcnt)%string=varn
         aw_table(nvcnt)%npointer=npointer
         aw_table(nvcnt)%idim_type=vtab_r(nv,ngr)%idim_type
         aw_table(nvcnt)%ngrid=ngr
         aw_table(nvcnt)%nvalues=vtab_r(nv,ngr)%npts
         
         call shdf5_orec(ndims,idims,varn,rvars=v_pointer)
         !!!!!! We are calling this "rvars" because it's a scalar pointer.
         !!!!!! Probably need to mod vtables at some point to have a pointer array...
         print*,nvcnt,varn,' : done'
      endif
   
   enddo
   
   ! Now loop through the "extra" variables, putting each in scratch%scr2...
   
   do nv = 1, num_extra_anal
      varn=an_extra(nv)%name
         print '(a,a,$)','writing: ',varn
           
      call anal_extra_comp(nv,idtype,scratch%scr1,ngr)
      
      idims(1)=nnxp(ngr) ; idims(2)=nnyp(ngr)
      
      if(idtype == 2) then
         ndims=2 
         npts=product(idims(1:ndims))
         call atob(npts,scratch%scr1,scratch%scr2)
      elseif(idtype == 3) then
         ndims=3 ; idims(3)=nnzp(ngr)
         npts=product(idims(1:ndims))
         call rearrange(nnzp(ngr),nnxp(ngr),nnyp(ngr)  &
                       ,scratch%scr1,scratch%scr2)
      
      !elseif(anal_extra(nv)%idim_type == 4) then
      !   !  Rearrange 4-d leaf%soil variables to (i,j,k,ip)
      !   call rearrange_p(nnxp(ngr),nnyp(ngr),nzg,npatch  &
      !                   ,v_pointer,scratch%scr2(1))
      !   ndims=4 ; idims(3)=nzg ; idims(4)=npatch
      
      !elseif(anal_extra(nv)%idim_type == 5) then
      !   !  Rearrange 4-d leaf%sfcwater variables to (i,j,k,ip)
      !   call rearrange_p(nnxp(ngr),nnyp(ngr),nzs,npatch  &
      !                   ,v_pointer,scratch%scr2(1))
      !   ndims=4 ; idims(3)=nzs ; idims(4)=npatch
      
      !elseif(anal_extra(nv)%idim_type == 6) then
      !   ndims=3 ; idims(3)=npatch
      endif
      
      nvcnt=nvcnt+1
      aw_table(nvcnt)%string=an_extra(nv)%name
      aw_table(nvcnt)%npointer=1 ! not needed for hdf
      aw_table(nvcnt)%idim_type=idtype
      aw_table(nvcnt)%ngrid=ngr
      aw_table(nvcnt)%nvalues=npts

      call shdf5_orec(ndims,idims,varn,rvara=scratch%scr2)
         print*,nvcnt,varn,' : done'
   enddo
   
   call shdf5_close()

enddo

! Write the header information out to the file.

call rams_f_open(ioaunt,anamelh,  &
     'FORMATTED','REPLACE','WRITE',iclobber)

write(ioaunt,110) nvcnt
do nv=1,nvcnt
   write(ioaunt,120) aw_table(nv)%string   &
                    ,aw_table(nv)%npointer  &
                    ,aw_table(nv)%idim_type  &
                    ,aw_table(nv)%ngrid  &
                    ,aw_table(nv)%nvalues
enddo

110 format(i6)
120 format(a16,1x,i12,i3,i3,1x,i9)

call commio('ANAL','WRITE',ioaunt)
close(ioaunt)

if(vtype == 'LITE')then
   subaname='  Analysis lite write'
elseif(vtype == 'MEAN')then
   subaname='  Averaged analysis write    '
elseif(vtype == 'BOTH')then
   subaname='  Averaged analysis lite write    '
else
   subaname='  Analysis write         '
endif

print 12,subaname,time,anamelh
12 format(/,1X,79('*'),/,  &
       A35,'  Time = ',F9.0,/,'      Header file name - ',A60  &
    ,/,1X,79('*'))

! Reset the time back to the original
if(vtype == 'MEAN'.or.vtype == 'BOTH')  time=timeold


! See if previous state file should be deleted. This feature is for writing
!   frequent files for checkpointing, but only saving a subset of them.
!   DO NOT remove the old state file if IFLAG is set.

if(frqstate_keep > 0.) then
   if(.not.first_call .and. iflag == 0 .and.  &
                            mod(time_save,frqstate_keep) /= 0.) then
      command = 'rm -f '//anameold(1:len_trim(anameold)-5)//'*'
      print*,'*************************************************'
      print*,'*  Removing state file with command: '
      print*,'*    ',trim(command)
      print*,'*************************************************'
      call system (command)
   endif
   anameold = anamel
   time_save = time
   first_call = .false.
endif

return
end
