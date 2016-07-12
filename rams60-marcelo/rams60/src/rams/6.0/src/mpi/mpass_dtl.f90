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

subroutine master_getcflcpu()

use mem_grid
use rpara

implicit none

integer :: mess_len,ngr,nm,ibytes,msgtyp,ihostnum
real, save, allocatable :: buff(:),buff1(:),buff2(:)
integer, save :: ncall=0,nwords

if (ncall==0) then
   nwords = 2 + 2 * maxgrds
   allocate (buff(nwords))
   allocate (buff1(maxgrds))
   allocate (buff2(maxgrds))
   ncall=1
endif

do ngr = 1,ngrids
   cflxy(ngr) = 0.
   cflz(ngr) = 0.
enddo

do nm=1,nmachs
   call par_get_new(buff,nwords,999,ibytes,msgtyp,ihostnum)
   call par_get_float(ptimes(ihostnum,1),1)
   call par_get_float(ptimes(ihostnum,2),1)
   call par_get_float(buff1,maxgrds)
   call par_get_float(buff2,maxgrds)
   do ngr = 1,ngrids
      cflxy(ngr) = max(cflxy(ngr),buff1(ngr))
      cflz(ngr) = max(cflz(ngr),buff2(ngr))
   enddo
!                  print*,'master got:',nnm,nm,ptimes(nnm,1)
!     +                 ,ptimes(nnm,2)
enddo

return
end

!     *****************************************************************

subroutine node_putcflcpu(totcpu,totwall)

use mem_grid
use node_mod

implicit none
real :: totcpu,totwall

real, save, allocatable :: buff(:)
integer, save :: ncall=0,nwords

if (ncall==0) then
   nwords = 2 + 2 * maxgrds
   allocate (buff(nwords))
   ncall=1
endif

call par_init_put(buff,nwords)
call par_put_float(totcpu,1)
call par_put_float(totwall,1)
call par_put_float(cflxy,maxgrds)
call par_put_float(cflz,maxgrds)
call par_send(master_num,999)

return
end

!     *****************************************************************

subroutine master_putdtsched(isendflg,isendlite,isendmean  &
                            ,isendboth,ntsend)
use mem_grid
use rpara

implicit none
integer :: isendflg,isendlite,isendmean,isendboth,ntsend

real, save, allocatable :: buff(:)
integer, save :: ncall=0,nwords

integer :: nm

if (ncall==0) then
   nwords = 7 + 3 * maxgrds + maxsched * maxschent
   allocate (buff(nwords))
   ncall=1
endif

call par_init_put(buff,nwords)

call par_put_int(isendflg,1)
call par_put_int(isendlite,1)
call par_put_int(isendmean,1)
call par_put_int(isendboth,1)

call par_put_int(ntsend,1)
if(ntsend == 1) then
   call par_put_int(nnacoust,maxgrds)
   call par_put_int(nndtrat,maxgrds)
   call par_put_int(isched,maxsched*maxschent)
   call par_put_int(nsubs,1)
   call par_put_float(dtlongn,maxgrds)
   call par_put_float(sspct,1)
endif

do nm=1,nmachs
   call par_send(machnum(nm),44)
enddo

return
end

!     *****************************************************************

subroutine node_getdtsched(isendflg,isendlite,isendmean,isendboth)

use mem_grid
use node_mod

implicit none
integer :: isendflg,isendlite,isendmean,isendboth,ntsend

real, save, allocatable :: buff(:)
integer, save :: ncall=0,nwords
integer :: ibytes,msgtype,ihostnum

if (ncall==0) then
   nwords = 7 + 3 * maxgrds + maxsched * maxschent
   allocate (buff(nwords))
   ncall=1
endif

call par_get_new(buff,nwords,44,ibytes,msgtype,ihostnum)

call par_get_int(isendflg,1)
call par_get_int(isendlite,1)
call par_get_int(isendmean,1)
call par_get_int(isendboth,1)
call par_get_int(ntsend,1)

if(ntsend == 1) then
   call par_get_int(nnacoust,maxgrds)
   call par_get_int(nndtrat,maxgrds)
   call par_get_int(isched,maxsched*maxschent)
   call par_get_int(nsubs,1)
   call par_get_float(dtlongn,maxgrds)
   call par_get_float(sspct,1)
endif

return
end




