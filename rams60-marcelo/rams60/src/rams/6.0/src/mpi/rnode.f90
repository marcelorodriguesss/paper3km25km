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

subroutine rams_node(name_name)

use mem_grid
use node_mod

implicit none

character(len=*) :: name_name

integer :: isendflg,isendlite,isendmean,isendboth,nt,npass,icm,ifm,nfeed
real :: wstart,totcpu,t1,w1,t6,w6,begtime
real, external :: walltime

real,allocatable ::  b(:)

ipara=1
!          Call routine to initialize input parameters
!               and namelist settings
!          -----------------------------------
call init_params(1)

!          Allocate memory needed on node

call rams_mem_alloc(2)

!          Routine to get fields from master and finish initialization
!          -----------------------------------------------------------
call init_fields(1)

isendflg=0
isendlite = 0
isendmean = 0
isendboth = 0

call par_pause(master_num,777)

!------------------------------------------------
!     Loop through total number of long timesteps
!------------------------------------------------

wstart=walltime(0.)
nt = 0

do while (time .lt. timmax)

   totcpu=0
   w1=walltime(wstart)
   call timing(1,t1)

   nt = nt + 1
   begtime=time

!!!! This part is for the dynamic balancing and sending new varfile/sst info

   if(isendflg.eq.1) then

      if(load_bal == 1) then
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         ! Dynamic balance experimental for now until OS memory issues can be solved
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         call nodeget_grid_dimens(ngrids)
         call rams_mem_alloc(3)  ! Dealloc, then allocate new vars
      endif
      
      call init_fields(0) ! Get fields from master
      
   endif
  ! call timing(1,t2)
  ! w2=walltime(wstart)

!    Receive message from master containing ISENDFLG, new dt's, etc.

   call node_getdtsched(isendflg,isendlite,isendmean,isendboth)

  ! call timing(1,t3)
  ! w3=walltime(wstart)

!----------------------------------------------------------------------------
!                  Loop through all grids and advance a 'DTLONG' timestep.
!----------------------------------------------------------------------------

!                  Start the timestep schedule

   do npass=1,nsubs
!            print*,'-->starting schedule :',mynum,npass,nt,nsubs

      isstp=isched(npass,3)
      ngrid=isched(npass,1)
      call newgrid(ngrid)

      call node_index()

!----------------------------------------------------------------------------
!                  Advance this grid forward by the appropriate timestep.

      time=begtime + (isched(npass,5)-1) * dtlt

!      Call main timestep driver
!      ------------------------------

      call timestep()

      ngbegun(ngrid)=1

!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!                  Is it time to send the coarse domain points to the nested
!                  nodes to interpolate a nested grid's boundaries?

      if(isched(npass,2).ne.0) then
         ngrid=isched(npass,2)
         call newgrid(ngrid)
         isstp=isched(npass,3)
         icm=nxtnest(ngrid)

         call node_sendnbc(ngrid,icm)
!                 -----------------------------------------------------------
!                  All boundary nodes on the fine grid must receive
!                     boundaries. Get them here for now.

         call node_getnbc(ngrid,nxtnest(ngrid) )
      endif
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!                  Is it time to feedback fields from fine grids to
!                     coarser grids?

      if(isched(npass,4).ne.0) then
         ngrid=isched(npass,1)
         do nfeed=1,isched(npass,4)
            call newgrid(ngrid)
            call node_sendfeed(ngrid)
            call node_getfeed(nxtnest(ngrid),ngrid)
            ngrid=nxtnest(ngrid)
         enddo
      endif
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!cc         print*,'-->ending schedule:',mynum,npass,nt,nsubs,ntsteps
   enddo

 !  call timing(1,t4)
 !  w4=walltime(wstart)

!----------------------------------------------------------------------------
!        Also, average each of the analysis variables over time
   do ngrid=1,ngrids
     call newgrid(ngrid)

!          THETAM and RVM have not been updated after nesting feedback
!             This means that these variables are really a timestep
!             behind the instantaneous variables.

!          Calculate the means
     call anlavg(mzp,mxp,myp,nzg)

   enddo
  ! call timing(1,t5)
  ! w5=walltime(wstart)

!----------------------------------------------------------------------------
! Send timing info/CFL numbers back to master.

   do ifm = 1,ngrids
      call newgrid(ifm)
      call cfl(mzp,mxp,myp,mi0(ifm),mj0(ifm),mynum)
   enddo

   call timing(2,t6)
   w6=walltime(wstart)
   totcpu=totcpu+t6-t1
   
 !  if (mynum==14) then
 !     print*,'time 1:',t2-t1,w2-w1
 !     print*,'time 2:',t3-t2,w3-w2
 !     print*,'time 3:',t4-t3,w4-w3
 !     print*,'time 4:',t5-t4,w5-w4
 !     print*,'time 5:',t6-t5,w6-w5
 !  endif
   
   call node_putcflcpu(t6-t1,w6-w1)

!----------------------------------------------------------------------------
! Send entire subdomains back to master every now and then.

   if(isendflg.eq.1) then
         call node_sendall()
         print*, 'calling par_pause',99999
         call par_pause(master_num,99999)
   endif

   if(isendlite.eq.1) then
         call node_sendanl('LITE')
         print*, 'calling par_pause',99998
         call par_pause(master_num,99998)
   endif

   if(isendmean.eq.1) then
         call node_sendanl('MEAN')
         print*, 'calling par_pause',99997
         call par_pause(master_num,99997)
   endif

   if(isendboth.eq.1) then
         call node_sendanl('BOTH')
         print*, 'calling par_pause',99996
         call par_pause(master_num,99996)
   endif

!---------------------------------------------------------------------------
!                   Update main time variable by a long timestep.

   time=begtime+dtlongn(1)

enddo

call par_exit()

return
end
!
!     ****************************************************************
!
subroutine init_params(init)

use mem_grid
use node_mod
use mem_oda

implicit none

integer, intent(in) :: init

!          get all initialization info from the master process
!          -------------------------------------------------------

call par_pause(master_num,777)
print*,mynum,'=========== done node ready1:'

call nodeget_processid(init)
print*,mynum,'=========== done nodeget_processid'

call nodeget_nl
print*,mynum,'=========== done nodeget_nl'

call nodeget_gridinit
print*,mynum,'=========== done nodeget_gridinit'

if (ibnd .eq. 4 .or. jbnd .eq. 4) then
   call ipaths_cyc_alloc(nnxp(1),nnyp(1),ibnd,jbnd,maxmach)
endif

call nodeget_grid_dimens()
print*,mynum,'=========== done nodeget_dimens'

call nodeget_gridset
print*,mynum,'=========== done nodeget_gridset'

call nodeget_cofnest()
print*,mynum,'=========== done nodeget_cofnest'

call nodeget_micphys
print*,mynum,'=========== done nodeget_micphys'

if (if_oda == 1) call nodeget_oda()
print*,mynum,'=========== done nodeget_oda'

call nodeget_misc
print*,mynum,'=========== done nodeget_misc'

return
end
!
!     ****************************************************************
!
subroutine init_fields(init)

use mem_grid
use node_mod

use var_tables

implicit none

integer :: init

integer :: ng,nm,itype,i1,j1,i2,j2,memf,npvar,nv


!          Initialize surface constants.
!          -------------------------------------------------------
if(init == 1) then
   call sfcdata
endif

!          Get all necessary fields from master.
!          -------------------------------------------------------

call node_getinit()


!     Can we use existing memory for the nesting communication buffers?
!       If not, allocate new buffers or compute buffer sizes.

!       Check feedback buffer.

itype=6
nbuff_feed=0
do ng=1,ngrids
   do nm=1,nmachs
      i1=ipaths(1,itype,ng,nm)
      i2=ipaths(2,itype,ng,nm)
      j1=ipaths(3,itype,ng,nm)
      j2=ipaths(4,itype,ng,nm)
      memf=(i2-i1+1)*(j2-j1+1)*(nnzp(ng))  &
           *(4+num_scalar(ng))
      nbuff_feed=max(nbuff_feed,memf)
   enddo
enddo

!____________________________________________
!
!    Allocate long time step send and receive buffers

if(init /= 1) then
   do nm=1,nmachs
      if (associated(node_buffs(nm)%lbc_send_buff) ) &
          deallocate(node_buffs(nm)%lbc_send_buff)
      if (associated(node_buffs(nm)%lbc_recv_buff) ) &
          deallocate(node_buffs(nm)%lbc_recv_buff)
   enddo
endif

do nm=1,nmachs
   if (node_buffs(nm)%nsend > 0) &
       allocate(node_buffs(nm)%lbc_send_buff(node_buffs(nm)%nsend))
   if (node_buffs(nm)%nrecv > 0) &
       allocate(node_buffs(nm)%lbc_recv_buff(node_buffs(nm)%nrecv))
   print'(a,6i10)','LBC node alloc:',mynum,nm &
       ,node_buffs(nm)%nsend, node_buffs(nm)%nrecv
enddo

! If using cyclic boundary conditions, initialize parallel communication
! for them
!  Find number of lbc variables to be communicated.
   npvar=0
   do nv = 1,num_var(1)
      if(vtab_r(nv,1)%impt1 == 1 ) then
         npvar=npvar+1 
      endif
   enddo

if (ibnd .eq. 4 .or. jbnd .eq. 4) then
   call node_cycinit(nnzp(1),nnxp(1),nnyp(1),npvar,nmachs,ibnd,jbnd,mynum)
endif

return
end

!     ****************************************************************

subroutine node_index()

use node_mod

implicit none

ia_1=max(ia-1,1)
ia_2=max(ia-2,1)
ia_3=max(ia-3,1)
ia1=ia+1
ia2=ia+2
ia3=ia+3
iz_1=iz-1
iz_2=iz-2
iz_3=iz-3
iz1=min(iz+1,mxp)
iz2=min(iz+2,mxp)
iz3=min(iz+3,mxp)

izu=iz
if(iand(ibcon,2).ne.0) izu=iz-1

if(myp.gt.1) then
   ja_1=max(ja-1,1)
   ja_2=max(ja-2,1)
   ja_3=max(ja-3,1)
   ja1=ja+1
   ja2=ja+2
   ja3=ja+3
   jz_1=jz-1
   jz_2=jz-2
   jz_3=jz-3
   jz1=min(jz+1,myp)
   jz2=min(jz+2,myp)
   jz3=min(jz+3,myp)

   jzv=jz
   if(iand(ibcon,8).ne.0) jzv=jz-1

else
   print*,'Trying to do 2-dimensional run ??????'
   stop 'no parallel 2d'
   ja_1=1
   ja_2=1
   ja_3=1
   ja1=1
   ja2=1
   ja3=1
   jz_1=1
   jz_2=1
   jz_3=1
   jz1=1
   jz2=1
   jz3=1
   jzv=1
endif

return
end
