
integer function RAMS_getvar (string,ngrd,a,b,flnm)

#if defined (RAMS_SUPPORT)
use an_header
use hdf5_utils
#endif
#if defined (GRIB_SUPPORT)
use grib_utils
#endif
#if defined (WRF_SUPPORT)
use cdf_utils
#endif

implicit none

#if defined (WRF_SUPPORT)
include 'netcdf.inc'
#endif

real :: a(*),b(*)
integer :: itype,ngrd,rams_c_pos,ncid,nid
character(len=*) :: flnm,string
character(len=1) :: cgrid
character(len=256) :: flng,errmsg
character(len=32) :: gstring
real :: gdefault
logical :: there,h5,fgrib,vfm,cdf
integer :: ni,npts,npts_chk,iword,n,levtype,nc
integer :: ndims,idims(4),nent,ierr

!print*,'getvar:hhhhhhh:',string
!print*,'getvar:hhhhhhh:',flnm

! First see if data file for this grid/time exists...

write(cgrid,'(i1)') ngrd

! Look for a standard name .h5 file
flng=trim(flnm)//'-g'//cgrid//'.h5'
!print*,'getvar:hhhhhhh:',flng
inquire(file=flng,exist=there)
if (there) then
   h5=.true. ; fgrib=.false. ; vfm = .false. ; cdf = .false.
   go to 100
endif

! Look for a standard name .vfm file
flng=trim(flnm)//'-g'//cgrid//'.vfm'
inquire(file=flng,exist=there)
if(there) then
   h5=.false. ; fgrib=.false. ; vfm = .true. ; cdf = .false.
   go to 100
endif

! See if this file is a grib or netcdf file.
flng=trim(flnm)
inquire(file=flng,exist=there)
if (there) then
   open(1,file=trim(flng),form='formatted',status='old')
   read(1,'(a3)') gstring
   close(1)
   if (gstring(1:3) == 'GRI') then
      h5=.false. ; fgrib=.true. ; vfm = .false. ; cdf = .false.
      go to 100
   elseif (gstring(1:3) == 'CDF') then
      h5=.false. ; fgrib=.false. ; vfm = .false. ; cdf = .true.
      go to 100
   else
      there=.false.
   endif
endif

if(.not.there) then
   errmsg='File not found - '//flng
   call error_mess(errmsg)
   RAMS_getvar=2
   return
endif

100 continue

! Read field depending on file type
RAMS_getvar=1
if ( vfm .or. h5 ) then

#if defined (RAMS_SUPPORT)
   ! Search table for variable
   do ni=1,nvbtab
      !print*,ni,anal_table(ni)%string,anal_table(ni)%ngrid
      if(string == anal_table(ni)%string .and.  &
         ngrd == anal_table(ni)%ngrid) then
   
         npts=anal_table(ni)%nvalues
         itype=anal_table(ni)%idim_type
         iword=anal_table(ni)%npointer

         if (h5) then
            print*,'Opening HDF5 file for reading: ',trim(flng)
            call shdf5_open(trim(flng),'R')
            call shdf5_info(string,ndims,idims)
            npts_chk=product(idims(1:ndims))
 
            if (npts /= npts_chk) then
               print*,'No. of points in anal table and in hdf5 file do not match.'
               print*,'   anal field:',string
               print*,'   anal table:',npts
               print*,'   hdf5 file :',npts_chk
               stop 'getvar: bad points'
            endif
            call shdf5_irec(trim(string),rvara=a)
            call shdf5_close()
         else
            call RAMS_c_open(trim(flng)//char(0),'r'//char(0))
            call vfirecr(10,a,npts,'LIN',b,iword)
            call RAMS_c_close()
         endif

         RAMS_getvar=0
         
         call an_setvar_info(class=itype, name=string, grid=ngrd, nvals=npts)
         !print*,')))))))))))))))))))))))))))) set class:',itype, trim(string)
         exit

      endif
   enddo
#else
   print*,'RAMS file found, but program not compiled with RAMS support'
   stop 'no RAMS'
#endif

elseif (fgrib) then

#if defined (GRIB_SUPPORT)

   ! Separate the variable name and the type of level. If latter is not 
   !  specified, defualt to pressure(100).
         !call grib_translate(string,gstring,levtype,gdefault)
   
   if (string(1:4) /= 'NULL') then
      nc=index(string,':')
      if (nc > 0) then
         gstring = string(1:nc-1)
         read(string(nc+1:),*) levtype
      else
         gstring=string
         levtype=100
      endif
      ! Get dimension info
      call grib_info(gstring,levtype,ngrd,ndims,idims,nent,ierr)
      if (ierr == 0) then
         ! Read variable
         call grib_irec(trim(flng),ndims,idims,nent,a)
         RAMS_getvar=0
      else
         RAMS_getvar=1
      endif
   endif
#else
   print*,'GRIB file found, but program not compiled with GRIB support'
   stop 'no GRIB'
#endif

elseif (cdf) then

#if defined (WRF_SUPPORT)
   print*,'Opening netcdf file for reading: ',trim(flng)
   ierr=nf_open(trim(flng),nf_nowrite,ncid)
   call cdf_varinfo(ncid,string,nid,ndims,idims)
   call cdf_irec(ncid,nid,ndims,idims,a)
   ierr=nf_close(ncid)
   RAMS_getvar=0
#else
   print*,'NETCDF file found, but program not compiled with WRF support'
   stop 'no WRF'
#endif
endif
print '(a,i2,1x,a,1x,a)','getvar RAMS_getvar (0=good):'  &
                  ,RAMS_getvar,trim(string)

if (RAMS_getvar == 1) then
   errmsg='Variable not available in this run - '//trim(string)
   call error_mess(errmsg)
   return
endif

!print*,'getvar good:',string,' ',gstring

return
end
