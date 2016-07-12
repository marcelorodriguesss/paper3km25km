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


subroutine sst_read(runflag,ifm,ierr)

use mem_grid
use mem_leaf
use io_params

implicit none

integer :: runflag,ifm,ierr

character(len=14), save  :: totdate_start,totdate_init
character(len=14)  :: totdate,totdatem
integer :: iyears,imonths,idates,ihours,nf,ng
real(kind=8) :: secs_init,secs1,secs2

ierr = 0

if (runflag == 1 .or. runflag == 2) then   ! Initialization(1) or file check(2)

   ! Inventory all sst surface files. 
   call sst_file_inv (sstfpfx,ierr)
   if(ierr == 1) then
      if(runflag == 2) return
      if(runflag == 1) stop 'sst_read: error on init'
   endif

   ! Find init and start date
   call date_make_big(iyear1,imonth1,idate1,itime1*100  &
                ,totdate_init)
   if (runtype == 'HISTORY') then
      call date_add_to_big(totdate_init,time,'s',totdate_start)
   elseif (runtype == 'INITIAL' .or. runtype == 'ERROR') then
      totdate_start=totdate_init
   elseif (runtype == 'MAKEVFILE') then
      totdate_start=totdate_init
   endif

   ! Do some checks on times
   do ng = 1, ngrids
      if(itotdate_sst(1,ng) > totdate_start .and. isstcycdata == 0) then
         print*, 'sst_read: initial sst file time for grid ',ng  &
               ,' later than beginning of run'
         ierr = 1
      endif
         
      if(iupdsst == 1 .and. nsstfiles(ng) == 1) then
         print*, 'sst_read: updating SST values but only one SST file'  &
                  ,' for grid',ng
         ierr = 1
      endif
      
      call date_add_to_big(totdate_init,timmax,'s',totdatem)
      if(iupdsst == 1 .and. isstcycdata == 0 .and. &
               itotdate_sst(nsstfiles(ng),ng) < totdatem) then
         print*, 'sst_read: final sst file time for grid ',ng  &
                  ,'earlier than end of run '
         ierr = 1
      endif
   enddo
   
   ! Return if errors
   if(ierr == 1 .and. runflag == 1) return
   
   ! If we are only checking, we're done.
   if(runflag == 2) return 

   do ng = 1, ngrids
   
      ! Change the sst file dates to the current year. We will increment
      !   when we need to read a future file.
      if(isstcycdata == 1) then
         do nf=1,nsstfiles(ng)
            itotdate_sst(nf,ng)(1:4)=totdate_start(1:4)
         enddo
      endif
         
      ! Find past time file. The files are ordered in time, so we only 
      !    need to find when start time is greater than a file.
      isstflp(ng)=0
      do nf=nsstfiles(ng),1,-1
         if(totdate_start >= itotdate_sst(nf,ng) ) then
            isstflp(ng)=nf
            exit
         endif
      enddo
      
      isstflf(ng)=isstflp(ng)+1
   
      ! If we are cyclic, we possibly didn't find a start time later than
      !   a file time. I think it is safe to assume that we will use the 
      !   last file for the start file. Also see if future time will be on 
      !   next cycle.
      if(isstcycdata == 1 ) then
         if(isstflp(ng) == 0) isstflp(ng) = nsstfiles(ng)
         if(isstflf(ng) > nsstfiles(ng)) isstflf(ng)=1
      endif
   
      print*,'sst starting at file number:',ng,isstflp(ng),isstflf(ng)

      ! Read past time sst field

      call sst_update(0,isstflp(ng))
      
      if(iupdsst == 1 ) then
         ! Read future time sst field if updating
         call sst_update(1,isstflf(ng))
     
         ! Compute times as number of seconds past 1 Jan 1900
         call date_abs_secs(totdate_init,secs_init)
         
         ! Get model time of past file
         totdatem=itotdate_sst(isstflp(ng),ng)
         if(isstcyclic == 1) then
            ! If month of past file > current month, subtract a year
            if(totdatem(5:6) > totdate_start(5:6))   &
               call date_add_to_big(totdatem,-365.,'d',totdatem)
         endif
         call date_abs_secs(totdatem,secs1)
   
         totdatem=itotdate_sst(isstflf(ng),ng)
         if(isstcyclic == 1) then
            if(totdatem < totdate_start) then
               ! Future file is in next year. Update all file names for a new year
               call date_add_to_big(totdatem, 365.,'d',totdatem)
               do nf=1,nsstfiles(ng)
                  itotdate_sst(nf,ng)(1:4)=totdatem(1:4)
               enddo
            endif
         endif
         call date_abs_secs(totdatem,secs2)
      
         ssttime1(ng) = secs1 - secs_init
         ssttime2(ng) = secs2 - secs_init
         print*,'=======3',ssttime1(ng),ssttime2(ng),secs1,secs2,secs_init
      else 
         leaf_g(ng)%seatp(1:nnxp(ng),1:nnyp(ng))=  &
         leaf_g(ng)%seatf(1:nnxp(ng),1:nnyp(ng))
         ssttime1(ng) = 0.
         ssttime2(ng) = 0.
      endif
   enddo
   
   return

elseif (runflag == 3) then   ! Runtime file increment
   
   !print*,'checking:',time,'---',ssttime2(ifm)
   if ( time >= ssttime2(ifm) ) then
   
      ! Update sst fields
      isstflp(ifm) = isstflf(ifm)
      isstflf(ifm) = isstflp(ifm) + 1
      
      ! Compute times as number of seconds past 1 Jan 1900
      !   If cyclic, modify file date 
      call date_abs_secs(totdate_init,secs_init)
      call date_add_to_big(totdate_init,time,'s',totdate)

      totdatem=itotdate_sst(isstflp(ifm),ifm)
      call date_abs_secs(totdatem,secs1)
      
      ! Need to deal with cyclic. 
      if(isstcyclic == 1 .and. isstflf(ifm) > nsstfiles(ifm)) then
         isstflf(ifm)=1
         ! Update all file names for a new year
         totdatem=itotdate_sst(isstflf(ifm),ifm)
         call date_add_to_big(totdatem, 365.,'d',totdatem)
         do nf=1,nsstfiles(ifm)
            itotdate_sst(nf,ifm)(1:4)=totdatem(1:4)
         enddo
      endif
         
      totdatem=itotdate_sst(isstflf(ifm),ifm)
      call date_abs_secs(totdatem,secs2)
      
      ssttime1(ifm) = secs1 - secs_init
      ssttime2(ifm) = secs2 - secs_init
      
      ! Finally read the actual field           
      call sst_update(1,isstflf(ifm))

      print*,'Switched sst files:',ssttime1(ifm),ssttime2(ifm)  &
               ,secs1,secs2,secs_init
   
   else
   
      return
   
   endif
   
endif

return
end



subroutine sst_file_inv (sfilin,ierr)

use mem_grid
use io_params

implicit none

character(len=*) :: sfilin
integer :: ierr

integer :: nc,nf,lnf,nftot,ng
integer :: inyear,inmonth,indate,inhour

integer, parameter :: maxfiles=1000
character(len=128), dimension(maxfiles) :: fnames
character(len=14)  :: itotdate
character(len=1)  :: cgrid
real(kind=8) :: secs_init,secs_file

ierr=0

! Get abs seconds of run start

call date_abs_secs2(iyear1,imonth1,idate1,itime1*100,secs_init)

! Go through sst files and make inventory. We unfortunately have to do this
!   for all grids.

isstcyclic=0
isstcycdata=0

do ng = 1, ngrids
   
   nftot = -1
   write(cgrid,'(i1)') ng
   call RAMS_filelist(fnames,trim(sfilin)//'-W-*-g'//cgrid//'.h5',nftot)
   
   if(nftot <= 0) then
      print*,'No sst files for grid '//cgrid
      ierr=1
      return
   endif   
   
   if(nftot > maxsstfiles) then
      print*,'too many sst files'
      stop 'sst_file_inv: lots_of_sst'
   endif
   

   ! We need to see if the data is to be considered "cyclic". Count how many files
   !  have the year 0000. If all of them do not, we probably have an error.
   !  isstcycdata = 1 when data is cyclic
   !  isstcyclic =1 when we are have cyclic data and will be updating in time

   nsstfiles(ng)=0
   do nf=1,nftot
      lnf=len_trim(fnames(nf))
      read(fnames(nf)(lnf-22:lnf-6),20) inyear,inmonth,indate,inhour
      20 format(i4,1x,i2,1x,i2,1x,i6)

      ! Check the file headers
      call sst_check_header(ng,fnames(nf),ierr)
      if(ierr == 1) return
   
      if(inyear == 0) isstcycdata=isstcycdata+1

      call date_make_big(inyear,inmonth,indate,inhour,itotdate)

      nsstfiles(ng)=nsstfiles(ng)+1
      fnames_sst(nsstfiles(ng),ng)=fnames(nf)
      itotdate_sst(nsstfiles(ng),ng)=itotdate

   enddo

   call RAMS_dintsort(nsstfiles(ng),itotdate_sst(1,ng),fnames_sst(1,ng))


   !  start printing section
   !--------------------------------------------------------------

   print*,' '
   print*,' '
   print*,' '
   print*,'-------------------------------------------------------------'
   print*,'-----------  SST Input File Inventory: Grid '//cgrid
   print*,'-------------------------------------------------------------'
   do nf=1,nsstfiles(ng)
      print*,  itotdate_sst(nf,ng),'   ',trim(fnames_sst(nf,ng))
   enddo
   print*,'------------------------------------------------------'

enddo

! Check the cyclic data condition. 
!   WE ARE ONLY ALLOWING CYCLIC ON ALL GRIDS
if(isstcycdata > 0) then
   if(isstcycdata /= sum(nsstfiles(1:ngrids))) then
      print*, 'All sst surface files do not have year 0000'
      print*, 'This confuses the gods and can not occur.'
      stop 'sst_inv'
   endif
   isstcycdata=1
else
   isstcycdata=0
endif


! Set the main cyclic flag. Only relevant if we are updating in time.
if(iupdsst == 1 .and. isstcycdata == 1) isstcyclic=1

return
end


!--------------------------------------------------------------

subroutine sst_update(iswap,nfile)

use mem_grid
use mem_leaf
use io_params

use hdf5_utils

implicit none

integer :: iswap,nfile

integer,save :: iun=25

integer :: ng,nc
character(len=1) :: cgrid
character(len=128) :: flnm
integer :: ndims,idims(4)


! Put new fields into future arrays. If iswap == 1, 
!     swap future into past first

if (iswap == 1) then
   do ng=1,ngrids
      leaf_g(ng)%seatp(1:nnxp(ng),1:nnyp(ng))=  &
         leaf_g(ng)%seatf(1:nnxp(ng),1:nnyp(ng))
   enddo
endif


! Open the input file for each grid and read field.
do ng=1,ngrids
   ! Contruct file name for this grid
   write(cgrid, '(i1)') ng
   flnm=fnames_sst(nfile,ng)
   nc=len_trim(flnm)-3
   flnm(nc:nc)=cgrid

   call shdf5_open(flnm,'R')
   ndims=2 ; idims(1)=nnxp(ng) ; idims(2)=nnyp(ng)
   call shdf5_irec('SEATF',rvara=leaf_g(ng)%seatf)
   call shdf5_close()

enddo


return
end

!****************************************************************************

subroutine sst_check_header(ifm,flnm,ierr)

use mem_grid

use hdf5_utils

! This subroutine checks for the existence of an sst file for
! grid number ifm, and if it exists, also checks for agreement of
! grid configuration between the file and the current model run.
! If the file does not exist or does not match grid configuration,
! the flag ierr is returned with a value of 1.  If the file 
! exists and is ok, ierr is returned with a value of 0.

implicit none
integer :: ifm,ierr
character*(*) flnm

integer :: nsfx,nsfy
real :: sfdx,sfdy,sfplat,sfplon,sflat,sflon,glatr,glonr
integer :: ndims,idims(4)

ierr = 0

print*,'------------------------------------------------------------------'
print '(a,i3,1x,a,a,$)',' ---> Check grid:',ifm,' sst filename:',trim(flnm)


call xy_ll(glatr,glonr,platn(ifm),plonn(ifm),xtn(1,ifm),ytn(1,ifm))


call shdf5_open(flnm,'R')
ndims=1 ; idims(1)=1
call shdf5_irec('nx',ivars=nsfx)
call shdf5_irec('ny',ivars=nsfy)
call shdf5_irec('dx',rvars=sfdx)
call shdf5_irec('dy',rvars=sfdy)
call shdf5_irec('polelat',rvars=sfplat)
call shdf5_irec('polelon',rvars=sfplon)
call shdf5_irec('sw_lat',rvars=sflat)
call shdf5_irec('sw_lon',rvars=sflon)
call shdf5_close()

if (nsfx                   .ne. nnxp(ifm) .or.  &
    nsfy                   .ne. nnyp(ifm) .or.  &
    abs(sfdx-deltaxn(ifm)) .gt. .001      .or.  &
    abs(sfdy-deltayn(ifm)) .gt. .001      .or.  &
    abs(sfplat-platn(ifm)) .gt. .001      .or.  &
    abs(sfplon-plonn(ifm)) .gt. .001      .or.  &
    abs(sflat-glatr)       .gt. .001      .or.  &
    abs(sflon-glonr)       .gt. .001) then

   ierr = 1

   print*
   print*,'SSTfile mismatch on grid:',ifm
   print*,'Values: model, file'
   print*,'-------------------'
   print*,'nnxp:',nnxp(ifm),nsfx
   print*,'nnyp:',nnyp(ifm),nsfy
   print*,'deltax:',deltaxn(ifm),sfdx
   print*,'deltay:',deltayn(ifm),sfdy
   print*,'platn:',platn(ifm),sfplat
   print*,'plonn:',plonn(ifm),sfplon
   print*,'SW lat:',glatr,sflat
   print*,'SW lon:',glonr,sflon
   print*,'-------------------'

else

   ierr = 0
   print*,' ---> good '
   print*,'------------------------------------------------------------------'

endif

return
end

