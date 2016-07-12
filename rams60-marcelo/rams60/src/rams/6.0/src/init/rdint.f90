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

subroutine initlz (name_name)

use mem_leaf
use mem_grid
use mem_scratch
use mem_basic
use mem_micro
use var_tables
use mem_varinit
use mem_cuparm
use mem_oda
use mem_turb, only:if_urban_canopy
use io_params
use micphys

implicit none

character(len=*) :: name_name

!---------------------------------------------------------------------
!     *** This routine is the driver of all initialization packages
!---------------------------------------------------------------------

character(len=8) :: rest
integer :: ifm,icm,ifileok,mynum,ihm,ngr,nv,ierr

!     Set version number for common blocks.
iversion = 2

!     Unit numbers for some I/O files.
iopunt=6

if (runtype(1:7) == 'INITIAL' .or. runtype(1:5) == 'ERROR') then

   time=0.
   ngbegun(1:ngrids) = 0

!----------------------------------------------------------------------
!                 Initial startup
!----------------------------------------------------------------------
   
   print*,'  Initial start:', runtype, initial


   ! Read surface, topo, sst, and ndvi files for all grids. All the files
   !   were checked earlier, so they must be correct.,

   do ifm = 1,ngrids
      call top_read(ifm)
   enddo

   do ifm = 1,ngrids
      call sfc_read(ifm)
   enddo

   !     Define grid topography, transform, latitude-longitude,
   !        and map factor arrays.

   call grid_setup(2)
   
   ! read SST files

   call sst_read(1,ifm,ierr)
   if (ierr /= 0) then
      print*,'rdint: Error in  sst surface files'
      stop 'rdint: sst surface file error'
   endif

   ! read NDVI files

   call ndvi_read(1,ifm,ierr)
   if (ierr /= 0) then
      print*,'rdint: Error in  ndvi surface files'
      stop 'rdint: ndvi surface file error'
   endif

   ! Initialize snowcover arrays

   do ifm = 1,ngrids
      call snowinit(nnxp(ifm),nnyp(ifm)  &
         ,leaf_g(ifm)%snow_mass(1,1),leaf_g(ifm)%snow_depth(1,1))
   enddo
   
   ! The following things will be done for INITIAL = 1 or 3...
   
   if ( initial == 1 .or. initial == 3) then

      ! If horizontally homogeneous initialization, 
      !    subroutine INITHH loops through all grids and initializes 
      !    those for which nxtnest = 0.

      if(initial == 1) then
         print*,'Horizontally-homogeneous-INITIAL start of grid- 1' 
         call inithh()
      endif
   
      ! If "history" initialization, call INITHIS.  
      !      This will define initial fields and reference state on grid 1 from
      !      history file. Other grids will be interpolated as in a INITIAL=1 start.

      if (initial == 3) then
         print*,'History-INITIAL start of grid- 1'
         call inithis()
      endif


      !  On all fine grids, initialize the surface layer characteristics,
      !  the 1-D reference state arrays, the 3-D reference state arrays,
      !  and the prognostic atmospheric fields by interpolation.

      call fmrefs1d(2,ngrids)

      do ifm = 2,ngrids
         icm = nxtnest(ifm)
         if (icm  >=  1) then
            call fmrefs3d(ifm)

            call prgintrp(nnzp(icm),nnxp(icm),nnyp(icm)  &
               ,nnzp(icm),nnxp(icm),nnyp(icm),0,0,ifm,1,mynum)

            call fmdn0(ifm)
            
            print*,'Initial interpolation of grid-',ifm
         endif
      enddo
 

   elseif(initial == 2) then
   
      ! If "variable initialization", do it all here
      
      call varf_read(0)

   endif

!     Initialize past time level velocity and perturbation Exner function
!     on all grids.

   do ifm=1,ngrids
      call newgrid(ifm)
      call fldinit(1)
      call negadj1(nzp,nxp,nyp)
      call thermo(nzp,nxp,nyp,1,nxp,1,nyp,'THRM_ONLY')

      if (level  ==  3) then
         call initqin(nzp,nxp,nyp        &
            ,micro_g(ifm)%q2    (1,1,1)  &
            ,micro_g(ifm)%q6    (1,1,1)  &
            ,micro_g(ifm)%q7    (1,1,1)  &
            ,basic_g(ifm)%pi0   (1,1,1)  &
            ,basic_g(ifm)%pp    (1,1,1)  &
            ,basic_g(ifm)%theta (1,1,1)  &
            ,basic_g(ifm)%dn0   (1,1,1)  )
         if(icloud == 7) call initqin2(nzp,nxp,nyp        &
            ,micro_g(ifm)%cccnp (1,1,1)  )
         if(ipris == 7) call initqin3(nzp,nxp,nyp        &
            ,micro_g(ifm)%cifnp (1,1,1)  &
            ,basic_g(ifm)%dn0   (1,1,1))

! This section only if bin model spawned from bulk model fields
!            elseif (level  ==  4or5) then
!               call xmic_init(nzp,nxp,nyp,nb  &
!                 ,basic_g(ifm)%dn0 (1,1,1)  &
!                 ,basic_g(ifm)%rtp (1,1,1)  &
!                 ,basic_g(ifm)%rv  (1,1,1)  &
!                 ,sclp(2))  &
!                 ,sclp(nb+2))  &
!                 ,vctr1,vctr2)
      endif

   enddo

! If initializing some fields from previous runs...

   if (ipastin == 1) then
      call recycle()
   endif
   
! Fill land surface data for all grids that have no standard input files

   call sfcdata


! Initialize various LEAF variables.

   if (ipastin == 0) call geonest_nofile(1,ngrids)
   if (initial == 3) call sfcinit_hstart()

!hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh

elseif(runtype(1:7) == 'HISTORY') then

!hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh

!                  History file start
!                  -----------------

   call history_start(name_name)

   call grid_setup(1)
   
   ! Check surface,topo,sst,ndvi files. Remake if necessary.
   
   call make_sfcfiles()

   ! Read surface and topo files for any added grids

   do ifm = ngridsh+1,ngrids
      call sfc_read(ifm)
   enddo
   
   do ifm = ngridsh+1,ngrids
      call top_read(ifm)
   enddo

   call grid_setup(2)

   ! Read in sst and ndvi files for all grids

   call sst_read(1,ifm,ierr)

   call ndvi_read(1,ifm,ierr)

   do ifm = 1,ngrids
      icm = nxtnest(ifm)
      if (icm  ==  0) then
         call newgrid(ifm)
         call refs3d (nzp,nxp,nyp  &
         ,basic_g(ifm)%pi0  (1,1,1),basic_g(ifm)%dn0  (1,1,1)  &
         ,basic_g(ifm)%dn0u (1,1,1),basic_g(ifm)%dn0v (1,1,1)  &
         ,basic_g(ifm)%th0  (1,1,1),grid_g(ifm)%topt  (1,1)    &
         ,grid_g(ifm)%rtgt  (1,1)  )
      endif
   enddo

   do ifm = 1,min(ngrids,ngridsh)
      icm = nxtnest(ifm)
      if (icm  >  0) call fmrefs3d(ifm)
      !!!!!!call negadj1(nzp,nxp,nyp)
   enddo

   ! If initializing some fields from previous runs...

   if (ipastin == 1) call recycle()
   
   call sfcdata

!     If any grids are being added for this run, initialize their
!     surface layer variables, 1-D reference state variables, and
!     prognostic atmospheric and soil model fields.

   if (ngrids  >  ngridsh) then
      print*,' +-------------------------------------'
      print*,'            !      New grids will be added.       '
      print*,'            !'
      print*,'            ! ',ngridsh,' grid(s) on history file.'
      print*,'            ! ',ngrids, ' grids to be run.        '
      print*,' +-------------------------------------'
      call fmrefs1d(ngridsh+1,ngrids)
      do ifm = ngridsh+1,ngrids
         icm = nxtnest(ifm)
         if (icm  ==  0) then
            print*, 'Attempted to add a hemispheric grid on'  &
               ,' a history restart; this cannot be done.'
            stop 'addgrid'
         endif
         call fmrefs3d(ifm)
         call prgintrp(nnzp(icm),nnxp(icm),nnyp(icm)  &
              ,nnzp(icm),nnxp(icm),nnyp(icm),0,0,ifm,1,mynum)
         print*,'History start interpolation of added grid-',ifm

         call fmdn0(ifm)
         call newgrid(ifm)
         call fldinit(0)
         call negadj1(nzp,nxp,nyp)
         call thermo(nzp,nxp,nyp,1,nxp,1,nyp,'THRM_ONLY')
         if (level  ==  3) then
            call initqin(nzp,nxp,nyp        &
               ,micro_g(ifm)%q2    (1,1,1)  &
               ,micro_g(ifm)%q6    (1,1,1)  &
               ,micro_g(ifm)%q7    (1,1,1)  &
               ,basic_g(ifm)%pi0   (1,1,1)  &
               ,basic_g(ifm)%pp    (1,1,1)  &
               ,basic_g(ifm)%theta (1,1,1)  &
               ,basic_g(ifm)%dn0   (1,1,1) )
         endif
      enddo

      ! Fill land surface data for all grids that have no standard input files
      call geonest_nofile(ngridsh+1,ngrids)
      
   elseif (ngrids  <  ngridsh) then
      print*,' +-------------------------------------'
      print*,'            !      Grids will be subtracted.       '
      print*,'            !'
      print*,'            ! ',NGRIDSH,' grid(s) on history file.'
      print*,'            ! ',NGRIDS, ' grids to be run.        '
      print*,' +-------------------------------------'
   endif
   

else
      print*,'WRONG RUNTYPE IN INITLZ'
      stop 'WRONG RUNTYPE IN INITLZ'

endif

! For a global model domain, initialize interpolation table values for
! communication between hemispheric grids.  Subroutine hemintrp_cof will
! return immediately if nhemgrd2 is not greater than 1.

call newgrid(1)
ihm = nnxyp(1)
call hemintrp_cof (nnxp(1),nnyp(1)  &
   ,scratch%scr1(1)      ,scratch%scr1(1+ihm)  ,scratch%scr1(1+2*ihm)  &
   ,scratch%scr1(1+3*ihm),scratch%scr1(1+4*ihm),scratch%scr1(1+5*ihm))

call micro_master()

!       Fill latitude-longitude, map factor, and Coriolis arrays.

do ifm = 1,ngrids
   call newgrid(ifm)
   call fcorio(nxp,nyp           &
      ,basic_g(ifm)%fcoru (1,1)  &
      ,basic_g(ifm)%fcorv (1,1)  &
      ,grid_g(ifm)%glat   (1,1)  )
enddo


!  If we are doing one-way nesting or varfile nudging, inventory, 
!     prepare history/varfile files
!     and fill past/future nudging arrays for start of simulation

if ( nud_type == 1 ) then
   call nud_read(1)
elseif(nud_type == 2) then
   call varf_read(1)
endif

! Do same if doing condensate nudging

if ( nud_cond == 1 ) call cond_read(1)

! Process and read observations for ODA - observational data assimilation

if (if_oda == 1) call oda_read(nnzp(1),nnxp(1),nnyp(1) &
                              ,basic_g(1)%pi0(1,1,1)  &
                              ,scratch%scr1(1))

! Read cumulus heating fields

if (if_cuinv == 1) call cu_read(1)

! Initialize urban canopy drag coefficients

if (if_urban_canopy == 1) call urb_drag_init()


! Print locations of all grids

call gridloc_prt()

!                  Save initial fields on history and analysis files
!                  -------------------------------------------------
iinput=ioutput
rest  ='no'
if (runtype  ==  'HISTORY') rest = 'yes'
call anal_write(rest,'INST')
if(frqlite > 0.) call anal_write(rest,'LITE')

!                  Save initial fields into the averaged arrays
!                  -------------------------------------------------
if(avgtim /= 0.)then

   do ngr=1,ngrids

      do nv=1,num_var(ngr)
         if(vtab_r(nv,ngr)%imean == 1) then
            call atob(vtab_r(nv,ngr)%npts,vtab_r(nv,ngr)%var_p  &
                     ,vtab_r(nv,ngr)%var_m)
         endif 
      enddo
   enddo
endif


!                  Print the header information and initial fields
!                  -----------------------------------------------
ngrid=1

call prtopt(6)

call nameout()

if (initfld  ==  1) then
   do ifm = 1,ngrids
      call newgrid(ifm)
      call prtout()
   enddo
endif

call opspec3()

return
end


!     ****************************************************************

subroutine read_nl(file)

use mem_grid
use io_params
implicit none

character(len=*) :: file

real :: tfact
integer :: nf,n


! Initialize some variables to known values so we can reset them
!     after the read if they are not set in the namelist

frqstate(1:maxgrds)=-1.

! Read grid point and options information

open(1,status='OLD',file=file)

call namein(1,'$MODEL_GRIDS',1)
call namein(1,'$MODEL_FILE_INFO',1)

call namein(1,'$MODEL_OPTIONS',1)
call namein(1,'$MODEL_SOUND',1)
call namein(1,'$MODEL_PRINT',0)
close(1)

!         Change some input time specifications into seconds

if(timeunit == 'd'.or.timeunit == 'D') tfact=86400.
if(timeunit == 'h'.or.timeunit == 'H') tfact=3600.
if(timeunit == 'm'.or.timeunit == 'M') tfact=60.
if(timeunit == 's'.or.timeunit == 'S') tfact=1.

timmax=timmax*tfact

! Set some variables if they weren't specified in namelist

! frqstate:
!----------------------------------
nf=0
do n = 1,ngrids
   print*,'frqstate(n)',frqstate(n)
   if (frqstate(n) > 0.) nf = n
enddo

if(ioutput > 0) then
   if(nf == 0) then
      print*,'No good values for frqstate'
      stop 'read_nl: frqstate bad'
   elseif (nf /= ngrids) then
      print*,'Defaulting frqstate for grids ',nf+1,' to ',ngrids  &
            ,' to coarser grid value:',frqstate(nf)
      do n = nf+1 , ngrids
         frqstate(n) = frqstate(nf)
      enddo
   endif
   
   !do n = 1 , ngrids
   !   print*,'read_nl: grid#,frqstate,dtlong:',n,frqstate(n),dtlong
   !   if (mod(frqstate(n),dtlong) > .01) then
   !      print*,'read_nl: FRQSTATE must be multiple of DTLONG.'
   !      print*,'read_nl: grid#,frqstate,dtlong:',n,frqstate(n),dtlong
   !      !stop 'read_nl:1'
   !   endif
   !enddo
endif
!----------------------------------

return
end
