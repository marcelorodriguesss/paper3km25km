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

subroutine isenio (inout,n1,n2)

use isan_coms
use hdf5_utils

implicit none

integer :: n1,n2
character(len=*) :: inout

integer :: npts,nlt,nx3,ny3,ninn,l,levnn(maxisn)
integer :: ndims,idims(4)

if(inout == 'IN') THEN

   ndims=1 ; idims(1)=1
   call shdf5_irec('isen_year',ivars=iyy)
   call shdf5_irec('isen_month',ivars=imm)
   call shdf5_irec('isen_date',ivars=idd)
   call shdf5_irec('isen_hour',ivars=ihh)
   call shdf5_irec('isen_nx',ivars=nx3)
   call shdf5_irec('isen_ny',ivars=ny3)
   call shdf5_irec('isen_nisn',ivars=ninn)
   ndims=1 ; idims(1)=ninn
   call shdf5_irec('isen_levth',ivara=levnn)

   if(nx3.ne.n1.or.ny3.ne.n2.or.ninn.ne.nisn) then
      print*,'Isentropic stage grid dimensions do not match'
      print*,'   configuration file on read !'
      print*,' File dimens - ',nx3,ny3,ninn
      print*,' Run  dimens - ',n1,n2,nisn
      stop 'IO3-2'
   endif

   npts=n1*n2*nisn
   ndims=3 ; idims(1)=n1 ; idims(2)=n2 ; idims(3)=nisn
   call shdf5_irec('isen_u',rvara=pi_u)
   call vmissr(pi_u(1,1,nlt),npts,1e30,-9998.)
   call shdf5_irec('isen_v',rvara=pi_v)
   call vmissr(pi_v(1,1,nlt),npts,1e30,-9998.)
   call shdf5_irec('isen_s',rvara=pi_s)
   call vmissr(pi_p(1,1,nlt),npts,1e30,-9998.)
   call shdf5_irec('isen_p',rvara=pi_p)
   call vmissr(pi_s(1,1,nlt),npts,1e30,-9998.)
   call shdf5_irec('isen_r',rvara=pi_r)
   call vmissr(pi_r(1,1,nlt),npts,1e30,-9998.)

   call shdf5_irec('sfc_u',rvara=rs_u)
   call vmissr(rs_u,npts,1e30,-9998.)
   call shdf5_irec('sfc_v',rvara=rs_v)
   call vmissr(rs_v,npts,1e30,-9998.)
   call shdf5_irec('sfc_p',rvara=rs_p)
   call vmissr(rs_p,npts,1e30,-9998.)
   call shdf5_irec('sfc_t',rvara=rs_t)
   call vmissr(rs_t,npts,1e30,-9998.)
   call shdf5_irec('sfc_r',rvara=rs_r)
   call vmissr(rs_r,npts,1e30,-9998.)
   call shdf5_irec('sfc_s',rvara=rs_s)
   call vmissr(rs_s,npts,1e30,-9998.)
   call shdf5_irec('sfc_topo',rvara=rs_top)
   call vmissr(rs_top,npts,1e30,-9998.)
   call shdf5_irec('sfc_qual',rvara=rs_qual)
   call vmissr(rs_qual,npts,1e30,-9998.)
   
   call shdf5_irec('sfc_slp',rvara=rs_slp)
   call vmissr(rs_slp,npts,1e30,-9998.)
   call shdf5_irec('sfc_sfp',rvara=rs_sfp)
   call vmissr(rs_sfp,npts,1e30,-9998.)
   call shdf5_irec('sfc_sft',rvara=rs_sft)
   call vmissr(rs_sft,npts,1e30,-9998.)
   call shdf5_irec('sfc_snow',rvara=rs_snow)
   call vmissr(rs_snow,npts,1e30,-9998.)
   call shdf5_irec('sfc_sst',rvara=rs_sst)
   call vmissr(rs_sst,npts,1e30,-9998.)

   print 201,' *****  Isentropic file input *****************'  &
        ,iyear,imonth,idate,ihour,n1,n2,nisn  &
        ,(levth(l),l=1,nisn)
   201 format(//,a,//  &
        ,' *',7X,' Date (year,month,day,hour)  - ',4I5,/  &
        ,' *',7X,' Number of X,Y points        - ',2I5,/  &
        ,' *',7X,' Number of isentropic levels - ',I5,/  &
        ,' *',7X,' Isentropic levels (K)       - '/,(32X,8I5))
   print '(a)',' **********************************************'

endif

if(inout.eq.'out') then

   ndims=1 ; idims(1)=1
   call shdf5_orec(ndims,idims,'isen_year',ivars=iyear)
   call shdf5_orec(ndims,idims,'isen_month',ivars=imonth)
   call shdf5_orec(ndims,idims,'isen_date',ivars=idate)
   call shdf5_orec(ndims,idims,'isen_hour',ivars=ihour)
   call shdf5_orec(ndims,idims,'isen_nx',ivars=n1)
   call shdf5_orec(ndims,idims,'isen_ny',ivars=n2)
   call shdf5_orec(ndims,idims,'isen_nisn',ivars=nisn)
   ndims=1 ; idims(1)=nisn
   call shdf5_orec(ndims,idims,'isen_levth',ivara=levth)

   npts=n1*n2*nisn
   ndims=3 ; idims(1)=n1 ; idims(2)=n2 ; idims(3)=nisn
   call vmissw(pi_u,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'isen_u',rvara=pi_scra)
   call vmissw(pi_v,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'isen_v',rvara=pi_scra)
   call vmissw(pi_p,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'isen_p',rvara=pi_scra)
   call vmissw(pi_s,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'isen_s',rvara=pi_scra)
   call vmissw(pi_r,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'isen_r',rvara=pi_scra)

   npts=n1*n2
   ndims=2 ; idims(1)=n1 ; idims(2)=n2
   call vmissw(rs_u,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sfc_u',rvara=pi_scra)
   call vmissw(rs_v,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sfc_v',rvara=pi_scra)
   call vmissw(rs_p,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sfc_p',rvara=pi_scra)
   call vmissw(rs_t,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sfc_t',rvara=pi_scra)
   call vmissw(rs_r,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sfc_r',rvara=pi_scra)
   call vmissw(rs_s,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sfc_s',rvara=pi_scra)
   call vmissw(rs_top,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sfc_topo',rvara=pi_scra)
   call vmissw(rs_qual,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sfc_qual',rvara=pi_scra)
   
   call vmissw(rs_slp,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sfc_slp',rvara=pi_scra)
   call vmissw(rs_sfp,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sfc_sfp',rvara=pi_scra)
   call vmissw(rs_sft,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sfc_sft',rvara=pi_scra)
   call vmissw(rs_snow,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sfc_snow',rvara=pi_scra)
   call vmissw(rs_sst,npts,pi_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sfc_sst',rvara=pi_scra)


   print 201,' *****  Isentropic file written *************'  &
        ,iyear,imonth,idate,ihour,n1,n2,nisn  &
        ,(levth(l),l=1,nisn)

   print 303,igridfl,gobsep,gobrad
   303 format(/,  &
         ' Grid flag (IGRIDFL)               -',I4,/  &
        ,' Grid-obs separation in degrees    -',F5.2,/  &
        ,' Grid-obs radius influence degrees -',F5.2)

endif

return
end

!***************************************************************************

subroutine sigzio (inout,n1,n2)

use isan_coms
use hdf5_utils

implicit none

integer :: n1,n2
character(len=*) :: inout

integer :: npts,nlt,l,ninn,nx3,ny3
integer :: ndims,idims(4)

if(inout.eq.'IN') then
   ndims=1 ; idims(1)=1
   call shdf5_irec('isen_year',ivars=iyy)
   call shdf5_irec('isen_month',ivars=imm)
   call shdf5_irec('isen_date',ivars=idd)
   call shdf5_irec('isen_hour',ivars=ihh)
   call shdf5_irec('isen_nx',ivars=nx3)
   call shdf5_irec('isen_ny',ivars=ny3)
   call shdf5_irec('sigz_nsigz',ivars=ninn)
   ndims=1 ; idims(1)=ninn
   call shdf5_irec('sigz_sigz',rvara=sigz)

   if(nx3.ne.n1.or.ny3.ne.n2.or.ninn.ne.nsigz)then
      print*,'Sigma-z grid dimensions do not match'
      print*,'   input data on read !'
      print*,' File  dimensions - ',nx3,ny3,ninn
      print*,' Input dimensions - ',n1,n2,nsigz
      stop 'iO3-2'
   endif

   npts=n1*n2*nsigz
   ndims=3 ; idims(1)=n1 ; idims(2)=n2 ; idims(3)=nsigz
   call shdf5_irec('sigz_u',rvara=ps_u)
   call vmissr(ps_u(1,1,nlt),npts,1e30,-998.)
   call shdf5_irec('sigz_v',rvara=ps_v)
   call vmissr(ps_v(1,1,nlt),npts,1e30,-998.)
   call shdf5_irec('sigz_p',rvara=ps_p)
   call vmissr(ps_p(1,1,nlt),npts,1e30,-.5)
   call shdf5_irec('sigz_t',rvara=ps_t)
   call vmissr(ps_t(1,1,nlt),npts,1e30,-.5)
   call shdf5_irec('sigz_r',rvara=ps_r)
   call vmissr(ps_r(1,1,nlt),npts,1e30,-.5)

   print 201,' *****  Sigma-z file input *****************'  &
        ,iyear,imonth,idate,ihour,n1,n2,nsigz  &
        ,(sigz(l),l=1,nsigz)
   201 format(//,a,//  &
        ,' *',7X,' Date (year,month,day,hour)  - ',4I5,/  &
        ,' *',7X,' Number of X,Y points        - ',2I5,/  &
        ,' *',7X,' Number of sigma-z levels    - ',I5,/  &
        ,' *',7X,' Sigma-z levels (m)          - '/,(32X,7F8.1))
   print '(a)',' **********************************************'

endif

if(inout.eq.'OUT') then
   
   ndims=1 ; idims(1)=1
   call shdf5_orec(ndims,idims,'sigz_nsigz',ivars=nsigz)
   ndims=1 ; idims(1)=nsigz
   call shdf5_orec(ndims,idims,'sigz_sigz',rvara=sigz)

   npts=n1*n2*nsigz
   ndims=3 ; idims(1)=n1 ; idims(2)=n2 ; idims(3)=nsigz
   call vmissw(ps_u,npts,ps_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sigz_u',rvara=ps_scra)
   call vmissw(ps_v,npts,ps_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sigz_v',rvara=ps_scra)
   call vmissw(ps_p,npts,ps_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sigz_p',rvara=ps_scra)
   call vmissw(ps_t,npts,ps_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sigz_t',rvara=ps_scra)
   call vmissw(ps_r,npts,ps_scra,1E30,-9999.)
   call shdf5_orec(ndims,idims,'sigz_r',rvara=ps_scra)

   print 201,' *****  Sigma-z file written *************'  &
        ,iyear,imonth,idate,ihour,n1,n2,nsigz   &
        ,(sigz(l),l=1,nsigz)

endif

return
end

!***************************************************************************

subroutine vmissw (af,n,as,fm,fx)
implicit none

integer :: n
real :: af(*),as(*),fm,fx

integer :: i

do i=1,n
   as(i)=af(i)
   if(af(i).ge.fm) as(i)=fx
enddo

return
end

!***************************************************************************

subroutine vmissr (af,n,fm,fx)
implicit none

integer :: n
real :: af(*),fm,fx

integer :: i

do i=1,n
   if(af(i).le.fx) af(i)=fm
enddo

return
end

