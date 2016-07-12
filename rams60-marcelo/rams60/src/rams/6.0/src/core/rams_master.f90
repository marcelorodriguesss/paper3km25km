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

subroutine rams_master(iparall,nproc,taskids,master_num,name_name)

use rpara
use mem_grid
use mem_oda, only:if_oda

implicit none

integer taskids(*)
character*(*) name_name
integer :: iparall,nproc,master_num

integer :: i,ifm,ns,nndtflg
real :: w1,w2,w3,t1,t2,wtime_start
real, external :: walltime

wtime_start=walltime(0.)
w1=walltime(wtime_start)
do ns=1,nproc
   print*,'CPU:',taskids(ns),master_num,iparall
enddo
   print*,'FILES:',name_name(1:len_trim(name_name))

iparallel=iparall

call opngks     ! call this just in case we put in graphical stuff
                !    for debugging

!          Read grid point and options information
!          ------------------------------------------------
call read_nl(name_name)


!          Reset timmax for an ERROR run
if ( trim(runtype) == 'ERROR') timmax = 0.

!          Read HYPACT options information
!          ------------------------------------------------
!if (index(runtype,'HYP') /= 0)  &
!   CALL hyp_namelist_read(name_name)


call eng_params  ! Various option settings that should normally 
                 !    not be changed

!          Reset parallel flag if necessary
!          --------------------------------------
if ( (runtype(1:4) == 'MAKE' ) .and. iparallel /= 0) then
   print*,'Resetting IPARALLEL=0 for a ',trim(runtype),' run'
   iparallel=0
endif

!          Print initial banner
!          ------------------------------------------------
!
write(6,'(a1,78a1)') ' ',('*',i=1,78)
write(6,'(2a1,a42)') ' ','*','    RAMS - Version 6.0'
write(6,'(2a1)') ' ','*'
write(6,'(2a1,a,a64)') ' ','*','   ',trim(expnme)
write(6,'(2a1)') ' ','*'
write(6,'(2a1,a,a64)') ' ','*','   RUNTYPE = ',trim(runtype)
write(6,'(a1,78a1)') ' ',('*',i=1,78)

! First check of options, mainly for numbers of grid points

call opspec1        

! Basic grid coordinate setup

call grid_setup(1)

! Additional checks, mainly for nesting 

call opspec2()      

! Check sfc,sst,ndvi files; remake if needed, even if RUNTYPE=ERROR

call make_sfcfiles()

! Exit if a MAKESFC run

if (runtype(1:7) == 'MAKESFC') then
   print*, 'MAKESFC run complete'
   go to 1000
endif

! If we are doing a "MAKEVFILE" run, call ISAN, then exit.

if(runtype(1:9) == 'MAKEVFILE') then
   call isan_driver(name_name)
   print*,' ISAN complete '
   go to 1000
endif   

!-----------------------------------------------------------
! If we got here, we are doing an actual 
!    simulation (RAMS or HYPACT)
!-----------------------------------------------------------

! Initialize micro arrays. May need to change some settings which affect memory.
call jnmbinit()

! Allocate main memory

print*, '---------------------------------------------------'
if (iparallel == 0) then
   call rams_mem_alloc(0) !     Allocate new data types
else
   call rams_mem_alloc(1) !     Allocate new data types
endif
print*, '---------------------------------------------------'

!          Call the main initialization driver
!          -----------------------------------

call initlz(name_name)

! Compute Courant numbers cflxy and cflz.

do ifm = 1,ngrids
   call newgrid(ifm)
   call cfl(nzp,nxp,nyp,0,0,0)
enddo

! Initialize dtlongn, nndtrat, and nnacoust, and compute the timestep
! schedule for all grid operations.

call dtset(nndtflg)
call modsched(isched,maxsched,ngrids,nxtnest,nndtrat,nsubs)

!  Initialize HYPACT configuration and sources

if (index(runtype,'HYP') /= 0)  then
  ! call hyp_source_times(timmax,ihour1)
  ! call emission_spec(nsumpart,a(ioff+itoptn(1)),nih,njh  &
  !                ,iyear1,imonth1,idate1,ihour1)
endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!   stop'hyp-okay'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

call timing(1,t1)
w2=walltime(wtime_start)
print'(a,2f12.3)','++++++++++ CPU - wall time: master initialization: '  &
     ,t1,w2-w1

if(iparallel == 1) then

!   ---------------------------------------------------------------
!     Initialize parallel processes with all relevant information
!   --------------------------------------------------------------
      print*, '++++rams_master start init',nzg,nzs,npatch,nnzp(1)

   call par_ready(nmachs,machnum,777)
      print*, '++++rams_master-ready1',nzg,nzs,npatch,nnzp(1)

   call masterput_processid(nproc,taskids,master_num)
      print*, '++++rams_master1-processid',nzg,nzs,npatch,nnzp(1)

   call masterput_nl()
      print*, '++++rams_master-nl',nzg,nzs,npatch,nnzp(1)

   call masterput_gridinit()
      print*, '++++rams_master-gridinit',nzg,nzs,npatch,nnzp(1)

   call node_decomp(1)
      print*, '++++rams_master-decomp',nzg,nzs,npatch,nnzp(1)

   call masterput_grid_dimens()
      print*, '++++rams_master-grid_dimens'

   call masterput_gridset()
      print*, '++++rams_master-gridset',nzg,nzs,npatch,nnzp(1)

   call masterput_cofnest()
      print*, '++++rams_master-cofnest',nzg,nzs,npatch,nnzp(1)

   call masterput_micphys()
      print*, '++++rams_master-micphys',nzg,nzs,npatch,nnzp(1)

   if (if_oda == 1) call masterput_oda()
      print*, '++++rams_master-oda',nzg,nzs,npatch,nnzp(1)

   call masterput_misc()
      print*, '++++rams_master-misc',nzg,nzs,npatch,nnzp(1)

   call master_sendinit()
      print*, '++++rams_master-sendinit',nzg,nzs,npatch,nnzp(1)

   call par_ready(nmachs,machnum,777)
      print*, '++++rams_master-ready2',nzg,nzs,npatch,nnzp(1)

endif

call timing(1,t2)
w3=walltime(wtime_start)
print '(a,2f12.3)','++++++++++ CPU - wall time: node initialization: '  &
                   ,t2-t1,w3-w2
print*, 'rams_master-done init'

! Exit if doing a zero time run
if (time >= timmax .or. trim(runtype) == 'ERROR') go to 1000

!  Call the model time integration driver
!  --------------------------------------

if(iparallel == 1) then
   call par_model()
else
   call model()
endif

!  RAMS finished, clean up some last things...
!  -----------------------------------------------------------

1000 continue

if ( trim(runtype) == 'ERROR') then
   print*
   print*,'---------------------------------------------------------'
   print*,'|  ERROR run completed successfully. No fatal errors.'
   print*,'---------------------------------------------------------'
   print*
endif


if (iparallel == 1) call par_exit()

call clsgks     ! call this just in case we put in graphical stuff
                !   for debugging

return
end

!-------------------------------------------------------------------------

subroutine comm_time(isendflg,isendlite,isendmean,isendboth)

use mem_varinit
use mem_cuparm
use io_params
use mem_grid

implicit none

integer :: isendflg,isendlite,isendmean,isendboth

real :: timemf
integer :: ifm

!         ISENDFLG designates whether nodes should send back
!            stuff things it normally doesn't have to
!            at the end of timestep for history/analysis write,
!            load balancing, etc.

!         isendflg  = the usual RAMS stuff
!         isendlite = the "lite" variables
!         isendmean = the "mean" variasbles
!         isendboth = Both the "mean" and "lite" variables

!            Determines whether nodes send stuff back at the END of the
!            timestep!!!!!

timemf = time + dtlongn(1)
isendflg = 0
isendlite = 0
isendmean = 0
isendboth = 0

if(frqlite > 0.) then
   if (mod(timemf,frqlite) < dtlongn(1)) isendlite=1
endif

if (frqmean > 0.) then
   if(avgtim > 0.)then
      if(mod(timemf-avgtim/2.,frqmean) < dtlongn(1) .and.  &
         timemf >= avgtim) isendmean=1
   elseif(avgtim < 0.)then
      if(mod(timemf,frqmean) < dtlongn(1)) isendmean=1
   endif
endif

if (frqboth > 0.) then
   if(avgtim > 0.)then
      if(mod(timemf-avgtim/2.,frqboth) < dtlongn(1) .and.  &
         timemf >= avgtim) isendboth=1
   elseif(avgtim < 0.)then
      if(mod(timemf,frqboth) < dtlongn(1)) isendboth=1
   endif
endif

if (ioutput  /=  0) then
   do ifm = 1,ngrids
      if ( mod(timemf,frqstate(ifm))  <  dtlongn(1) ) then
            isendflg = 1
            return
      endif
   enddo
endif

if( timemf  >=  timmax - .01*dtlongn(1) ) then
   isendflg = 1
   return
endif

if  ( nud_type == 2 .and. timemf  >=  vtime2  &
    .and. timemf  <  timmax) then
   isendflg = 1
   return
endif

if  ( nud_type == 1 .and. timemf  >=  htime2  &
    .and. timemf  <  timmax) then
   isendflg = 1
   return
endif

if  ( nud_cond == 1 .and. timemf  >=  condtime2  &
    .and. timemf  <  timmax) then
   isendflg = 1
   return
endif

if (mod(timemf,frqprt)  <  dtlongn(1) ) then
   isendflg = 1
   return
endif

if (iupdsst  ==  1 ) then
   do ifm = 1,ngrids
      if (isstflg(ifm)  ==  1) then
         if (timemf  >=  ssttime2(ifm) .and.  &
            timemf  <  timmax) then
            isendflg = 1
            return
         endif
      endif
   enddo
endif

if (iupdndvi  ==  1 ) then
   do ifm = 1,ngrids
      if (ndviflg(ifm)  ==  1) then
         if (timemf  >=  ndvitime2(ifm) .and.  &
            timemf  <  timmax) then
            isendflg = 1
            return
         endif
      endif
   enddo
endif

if (if_cuinv  ==  1 ) then
   do ifm = 1,ngrids
      if (timemf  >=  cu_times(ncufl+1) .and.  &
            timemf  <  timmax) then
            isendflg = 1
            return
      endif
   enddo
endif

return
end

!-------------------------------------------------------------------------

subroutine rams_output()

use mem_leaf
use mem_varinit
use mem_cuparm
use io_params
use mem_grid

implicit none

integer :: ierr,ifm,ifileok
logical :: analwrite
real :: timmaxr


do ifm = 1,ngrids
   call newgrid(ifm)
   timmaxr = timmax - .01*dtlongn(ifm)

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! 1 Dec 1998 - implement THERMO call for THETA and RV if calling PRTOUT
! or ANLWRT.

!        For testing the mean fields, skip this rediagnosis
!        GOTO 10

   if (ioutput  ==  0) go to 10

   analwrite=.false.
   if(mod(time,frqstate(ifm)) <  dtlongn(ifm)               .or.  &
             mod(time,frqprt) <  dtlongn(1)                 .or.  &
                        time  >= timmaxr                    .or.  &
                        iflag == 1                                &
      ) analwrite=.true.
   
   
   if (analwrite) then
      call thermo(nzp,nxp,nyp,1,1,1,nyp,'THRM_ONLY')
      call thermo(nzp,nxp,nyp,nxp,nxp,1,nyp,'THRM_ONLY')
      if (jdim  ==  1) then
         call thermo(nzp,nxp,nyp,1,nxp,1,1,'THRM_ONLY')
         call thermo(nzp,nxp,nyp,1,nxp,nyp,nyp,'THRM_ONLY')
      endif
   endif

!        get rid of boundary gradients in the mean diagnosed variables.
!         if(mod(time,frqmean) < dtlongn(1).or.
!     +      mod(time,frqboth) < dtlongn(1) ) then
!            call zeromeangrad(a,ifm,nzp,nxp,nyp,1,1,1,nyp)
!            call zeromeangrad(a,ifm,nzp,nxp,nyp,nxp,nxp,1,nyp)
!            if (jdim  ==  1) then
!               call zeromeangrad(a,ifm,nzp,nxp,nyp,1,nxp,1,1)
!               call zeromeangrad(a,ifm,nzp,nxp,nyp,1,nxp,nyp,nyp)
!            endif
!         endif

10      continue


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

   if(mod(time,frqprt) < dtlongn(1).or.iflag == 1)then
      call prtout()
!c            call uwcomp(a)
   endif

enddo

timmaxr = timmax - .01*dtlongn(1)

analwrite=.false.
do ifm = 1,ngrids
   if(mod(time,frqstate(ifm)) < dtlongn(1).or.  &
      time  >=  timmaxr .or.  &
      iflag == 1) analwrite=.true.
enddo
if (analwrite) call anal_write('no','INST')

!     Call the analysis writing routine again for the other var types
if(frqlite > 0.) then
   if( mod(time,frqlite) < dtlongn(1).or.  &
                      time  >=  timmaxr ) then
    call anal_write('no','LITE')
   endif
endif

!print*, 'check avg',time,frqmean,avgtim,dtlongn(1)
if (frqmean > 0.) then
   if(avgtim > 0.0.and.mod(time-avgtim/2.,frqmean) < dtlongn(1)  &
      .and.time >= avgtim) then
      !print*,'call1'
      call anal_write('no','MEAN')
   endif
      
   if(avgtim < 0.0.and.mod(time,frqmean) < dtlongn(1)) then
      !print*,'call2'
      call anal_write('no','MEAN')
   endif
endif

if (frqboth>0.) then
   if(avgtim > 0.0.and.mod(time-avgtim/2.,frqboth) < dtlongn(1)  &
      .and.time >= avgtim)call anal_write('no','BOTH')
   if(avgtim < 0.0.and.mod(time,frqboth) < dtlongn(1))  &
      call anal_write('no','BOTH')
endif

timmaxr = time+.00001

if (iupdsst  ==  1 .and. timmaxr < timmax) then
   do ifm = 1,ngrids
      call sst_read(3,ifm,ierr)
   enddo
endif

if (iupdndvi  ==  1 .and. timmaxr < timmax) then
   do ifm = 1,ngrids
      call ndvi_read(3,ifm,ierr)
   enddo   
endif


if( nud_type == 1 .and. time >= htime2 .and. timmaxr < timmax) then
   call nud_read(2)
endif

if( nud_cond == 1 .and. time >= condtime2 .and. timmaxr < timmax) then
   call cond_read(2)
endif

if(if_cuinv == 1 .and. time >= cutime2 .and. timmaxr < timmax) then
   call cu_read(2)
endif


if(nud_type == 2 .and. time >= vtime2 .and. timmaxr < timmax) then
   call varf_read(2)
endif

if (iflag  ==  1) stop 'IFLAG'

return
end
