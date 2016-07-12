module cdf_utils

integer :: icode

!===============================================================================

Contains

!===============================================================================

subroutine cdf_varinfo(ncid,string,nid,ndims,idims)

implicit none

#if defined (WRF_SUPPORT)
include 'netcdf.inc'
#else
integer :: nf_inq_varid,nf_inq_varndims,nf_inq_vardimid,nf_inq_dimlen
#endif

integer :: ncid,nid,ndims,dimid(4),idims(4),n
character(len=*) :: string

icode=nf_inq_varid(ncid,trim(string),nid)
icode=nf_inq_varndims(ncid,nid,ndims)
icode=nf_inq_vardimid(ncid,nid,dimid)
do n=1,ndims
   icode=nf_inq_dimlen(ncid,dimid(n),idims(n))
enddo

return
end subroutine

!===============================================================================

subroutine cdf_irec(ncid,nid,ndims,idims,a)

implicit none

#if defined (WRF_SUPPORT)
include 'netcdf.inc'
#else
integer :: nf_get_var_real
#endif

integer :: ncid,nid,ndims,idims(4)
real :: a(idims(1),idims(2),idims(3))

! Fill array with missing value

a = -9999.

icode=nf_get_var_real(ncid,nid,a)

return
end subroutine

end module
