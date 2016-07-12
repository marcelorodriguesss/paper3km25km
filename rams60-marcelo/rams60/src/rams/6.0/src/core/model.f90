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


subroutine model()

use mem_grid

implicit none

!   +------------------------------------------------------------------
!   ! This routine drives the entire time integration process
!   !   for a non-parallel run.
!   +------------------------------------------------------------------

integer :: npass,nndtflg,icm,ifm,nfeed,mynum
real :: wtime_start,begtime,t1,wtime1,wtime2,t2,wtime_tot
real, external :: walltime

 print*, 'starting subroutine MODEL'

wtime_start=walltime(0.)
istp = 0

!         Start the timesteps

do while (time < timmax)

   istp = istp + 1
   begtime=time

!            CPU timing information
   call timing(1,t1)
   wtime1=walltime(wtime_start)

! Examine Courant numbers in case model needs to be stopped
! or (if ideltat < 0), to update dtlongn, nndtrat,
! nnacoust, sspct and isched.

   call dtset(nndtflg)
   if (nndtflg .gt. 0) then
      call modsched(isched,maxsched,ngrids,nxtnest,nndtrat,nsubs)
   endif

! Start the timestep schedule to loop through all grids and advance them
! in time an increment equal to dtlongn(1).

   do npass=1,nsubs

      isstp=isched(npass,3)
      ngrid=isched(npass,1)
      call newgrid(ngrid)

!---------------------------------------------------------------------

      time=begtime + (isched(npass,5)-1) * dtlt

      call onenode()

      call timestep()

      ngbegun(ngrid)=1

      if(isched(npass,2).ne.0) then
         call newgrid(isched(npass,2))
         isstp=isched(npass,3)
         icm=nxtnest(ngrid)
         call prgintrp(nnzp(icm),nnxp(icm),nnyp(icm)  &
              ,nnzp(icm),nnxp(icm),nnyp(icm),0,0,ngrid,0,mynum)
      endif

      if(isched(npass,4).ne.0) then
         ifm = isched(npass,1)
         icm = nxtnest(ifm)
         do nfeed = 1,isched(npass,4)
            call newgrid(ifm)
            call nstfeed(ifm,icm)
        !    call movenest(ifm,icm)  ! doesn't work for now in 5.0
            ifm = icm
            icm = nxtnest(ifm)
         enddo
      endif

   enddo

   time=begtime+dtlongn(1)

! At this point, all grids have been advanced forward by time increment DTLONG,
! and all nesting operations (interpolation and feedback) have been carried
! out.  If there are two hemispheric grids in this simulation, now is the
! time to carry out the communication between them.  Subroutine hemintrp
! will return immediately if nhemgrd2 is not greater than 1.

   call hemintrp

! Compute Courant numbers cflxy and cflz and do averaging.

   do ifm = 1,ngrids
      call newgrid(ifm)
      call cfl(nzp,nxp,nyp,0,0,1)

!           THETAM and RVM have not been updated after nesting feedback
!              This means that these variables are really a timestep
!              behind the instantaneous variables.

!           Calculate the means
      call anlavg(nzp,nxp,nyp,nzg)
   enddo

   wtime2=walltime(wtime_start)
   CALL TIMING(2,T2)
   PRINT 201, ISTP,TIME,T2-T1,wtime2-wtime1
201     FORMAT(' Timestep-',I5,'   Sim time(sec)=',F9.1  &
        ,'  CPU(sec)=',F7.2,'  Wall(sec)=',F7.2)

   call rams_output()

enddo

wtime_tot=walltime(wtime_start)
print '(//,a,f10.0)'  &
     ,' -----Total elapsed time: ',wtime_tot

RETURN
END

!     *****************************************************************

subroutine par_model()

use mem_grid
use rpara

implicit none
!   +------------------------------------------------------------------
!   ! This routine drives the entire time integration process
!   !   for a parallel run.
!   +------------------------------------------------------------------

integer :: isendflg,isendlite,isendmean,isendboth,nndtflg,ntsend,nmach,n
real :: wtime_start,t1,wtime1,wtime2,t2,wtime_tot,pcpu,pwall
real, external :: walltime


 print*, 'starting subroutine par_MODEL', time,timmax

wtime_start=walltime(0.)

isendflg=0
isendlite = 0
isendmean = 0
isendboth = 0

istp = 0

!         Start the main timestep loop

do while (time .lt. timmax)

   istp = istp + 1

!!!! Reinitialize subdomains - based on Isendflg computed below
!!!! This part is for the dynamic balancing and sending new varfile/sst info

   if(isendflg.eq.1) then
   
      if (load_bal == 1) then
         call node_decomp(0)
         call masterput_grid_dimens
      endif
      
      call master_sendinit()
      
   endif

!            CPU timing information

   call timing(1,t1)
   wtime1=walltime(wtime_start)

!            ISENDFLG designates whether nodes should send back
!               stuff things it normally doesn't have to
!               at the end of timestep for history/analysis write,
!               load balancing, etc.

!               Determines whether nodes send stuff back at the END of the
!               timestep!!!!!

   call comm_time (isendflg,isendlite,isendmean,isendboth)

!            Examine Courant numbers in case model needs to be stopped
!            or (if ideltat < 0), to update dtlongn, nndtrat,
!            nnacoust, sspct and isched.

   call dtset(nndtflg)
   if (iflag > 0) then
      isendflg = 1
      isendlite = 1
      isendmean = 1
      isendboth = 1
   endif
   if (nndtflg > 0) then
      call modsched(isched,maxsched,ngrids,nxtnest,nndtrat,nsubs)
   endif
   
   ! Send timestep schedule and timesteps to nodes only if they have changed.
   !   Need to do it in first timestep regardless...
   ntsend=0
   if(istp == 1 .or. nndtflg > 0) ntsend=1
   call master_putdtsched(isendflg,isendlite,isendmean,isendboth,ntsend)


!---------------------------------------------------------------------
!  Bypass the timestep schedule, then update the main time variable.

!         do npass=1,nsubs
!         enddo

   time=time+dtlongn(1)

! Wait for cpu time, wallclock time, and Courant numbers
! cflxy and cflz from nodes.

   call master_getcflcpu()


   if(isendflg.eq.1) then

!                  Wait for whole subdomains from nodes

      call master_getall

      print*,'calling par_ready-',99999
      call par_ready(nmachs,machnum,99999)
   endif

   if(isendlite.eq.1) then
      call master_getanl('LITE')
      call par_ready(nmach,machnum,99998)
   endif

   if(isendmean.eq.1) then
      call master_getanl('MEAN')
      call par_ready(nmach,machnum,99997)
   endif

   if(isendboth.eq.1) then
      call master_getanl('BOTH')
      call par_ready(nmach,machnum,99996)
   endif

   wtime2=walltime(wtime_start)
   CALL TIMING(2,T2)

      PRINT 200, ISTP,TIME,T2-T1,wtime2-wtime1
200        FORMAT(' Master step-',I5,'  Sim time(sec)=',F9.1  &
           ,'  CPU(sec)=',F6.2,'  Wall(sec)=',F6.2)
      pcpu=0
      pwall=0
      do n=1,nmachs
         pcpu=pcpu+ptimes(n,1)
         pwall=pwall+ptimes(n,2)
      enddo
      print 201,pcpu,(ptimes(n,1),n=1,nmachs)
!            print 202,pwall/nmachs,(ptimes(n,2),n=1,nmachs)
201       format(' Node---CPU--total=',f7.2,'__machs=',1000f8.3)
202       format(' Node--wall----avg=',f7.2,'__machs=',1000f8.3)
      print*,'----------------------------------------------'


      call rams_output()
 enddo

  wtime_tot=walltime(wtime_start)
   print '(//,a,f10.0)'  &
     ,' -----Total elapsed time: ',wtime_tot

RETURN
END
