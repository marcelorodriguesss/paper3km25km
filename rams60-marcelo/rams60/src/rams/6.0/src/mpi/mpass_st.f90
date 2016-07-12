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

subroutine node_sendst(isflag)

use mem_grid
use node_mod

use mem_scratch
use mem_basic

implicit none

integer :: isflag

integer :: itype,nm,i1,i2,j1,j2,mtp

if (isflag.ge.2.and.isflag.le.4) itype=isflag
if (isflag.ge.5.and.isflag.le.6) itype=1

!---------------------------------------------------
!
!   First, before we send anything, let's post the receives


do nm=1,nmachs
   if (iget_paths(itype,ngrid,nm).ne.0) then
      call par_get_noblock(node_buffs(nm)%lbc_recv_buff(1)  &
          ,node_buffs(nm)%nrecv ,10000+isflag,machs(nm),irecv_req(nm) )
   endif
enddo

!---------------------------------------------------
!
!   Now we can actually go on to sending the stuff

do nm=1,nmachs

   if(ipaths(1,itype,ngrid,nm).ne.0) then

      call par_init_put(node_buffs(nm)%lbc_send_buff(1)  &
                       ,node_buffs(nm)%nsend )
      i1=ipaths(1,itype,ngrid,nm)
      i2=ipaths(2,itype,ngrid,nm)
      j1=ipaths(3,itype,ngrid,nm)
      j2=ipaths(4,itype,ngrid,nm)

      call par_put_int(i1,1)
      call par_put_int(i2,1)
      call par_put_int(j1,1)
      call par_put_int(j2,1)
      call par_put_int(mynum,1)

      if(isflag.eq.2) then
         call mkstbuff(mzp,mxp,myp,basic_g(ngrid)%up(1,1,1)  &
             ,scratch%scr1(1),i1-i0,i2-i0,j1-j0,j2-j0,mtp)
         call par_put_float(scratch%scr1(1),mtp)
      elseif (isflag.eq.3) then
         call mkstbuff(mzp,mxp,myp,basic_g(ngrid)%vp(1,1,1)  &
             ,scratch%scr1(1),i1-i0,i2-i0,j1-j0,j2-j0,mtp)
         call par_put_float(scratch%scr1(1),mtp)
      elseif (isflag.eq.4) then
         call mkstbuff(mzp,mxp,myp,basic_g(ngrid)%pp(1,1,1)  &
             ,scratch%scr1(1),i1-i0,i2-i0,j1-j0,j2-j0,mtp)
         call par_put_float(scratch%scr1(1),mtp)
      elseif(isflag.eq.5) then
         call mkstbuff(mzp,mxp,myp,basic_g(ngrid)%up(1,1,1)  &
             ,scratch%scr1(1),i1-i0,i2-i0,j1-j0,j2-j0,mtp)
         call par_put_float(scratch%scr1(1),mtp)
         call mkstbuff(mzp,mxp,myp,basic_g(ngrid)%vp(1,1,1)  &
             ,scratch%scr1(1),i1-i0,i2-i0,j1-j0,j2-j0,mtp)
         call par_put_float(scratch%scr1(1),mtp)
      elseif (isflag.eq.6) then
         call mkstbuff(mzp,mxp,myp,basic_g(ngrid)%wp(1,1,1)  &
             ,scratch%scr1(1),i1-i0,i2-i0,j1-j0,j2-j0,mtp)
         call par_put_float(scratch%scr1(1),mtp)
         call mkstbuff(mzp,mxp,myp,basic_g(ngrid)%pp(1,1,1)  &
             ,scratch%scr1(1),i1-i0,i2-i0,j1-j0,j2-j0,mtp)
         call par_put_float(scratch%scr1(1),mtp)

      endif

      call par_send_noblock(ipaths(5,itype,ngrid,nm)  &
           ,10000+isflag,isend_req(nm) )

   endif

enddo


return
end
!
!     ****************************************************************
!
subroutine node_getst(isflag)

use mem_grid
use node_mod

use mem_scratch
use mem_basic

implicit none

integer :: isflag

integer :: itype,nm,ibytes,msgid,ihostnum,i1,i2,j1,j2,mtp,mtc  &
          ,node_src,nptsxy

if (isflag.ge.2.and.isflag.le.4) itype=isflag
if (isflag.ge.5.and.isflag.le.6) itype=1

!_____________________________________________________________________
!
!  First, let's make sure our sends are all finished and de-allocated

do nm=1,nmachs
   if(ipaths(1,itype,ngrid,nm).ne.0) then
      call par_wait(isend_req(nm),ibytes,msgid,ihostnum)
   endif
enddo
!_____________________________________________________________________
!
!  Now, let's wait on our receives

do nm=1,nmachs
   if (iget_paths(itype,ngrid,nm).ne.0) then
      call par_wait(irecv_req(nm),ibytes,msgid,ihostnum)
   endif
enddo
!_____________________________________________________________________
!
!  We got all our stuff. Now unpack it into appropriate space.


do nm=1,nmachs

   if (iget_paths(itype,ngrid,nm).ne.0) then

      call par_assoc_buff(node_buffs(nm)%lbc_recv_buff(1)  &
                         ,node_buffs(nm)%nrecv)

      call par_get_int(i1,1)
      call par_get_int(i2,1)
      call par_get_int(j1,1)
      call par_get_int(j2,1)
      call par_get_int(node_src,1)

      nptsxy=(i2-i1+1)*(j2-j1+1)

      mtp=nzp * nptsxy

      if(isflag.eq.2) then
         call par_get_float(scratch%scr1(1),mtp)
         call exstbuff(mzp,mxp,myp,basic_g(ngrid)%up(1,1,1)  &
             ,scratch%scr1(1),i1-i0,i2-i0,j1-j0,j2-j0,mtc)
      elseif(isflag.eq.3) then
         call par_get_float(scratch%scr1(1),mtp)
         call exstbuff(mzp,mxp,myp,basic_g(ngrid)%vp(1,1,1)  &
             ,scratch%scr1(1),i1-i0,i2-i0,j1-j0,j2-j0,mtc)
      elseif(isflag.eq.4) then
         call par_get_float(scratch%scr1(1),mtp)
         call exstbuff(mzp,mxp,myp,basic_g(ngrid)%pp(1,1,1)  &
             ,scratch%scr1(1),i1-i0,i2-i0,j1-j0,j2-j0,mtc)
      elseif(isflag.eq.5) then
         call par_get_float(scratch%scr1(1),mtp)
         call exstbuff(mzp,mxp,myp,basic_g(ngrid)%up(1,1,1)  &
             ,scratch%scr1(1),i1-i0,i2-i0,j1-j0,j2-j0,mtc)
         call par_get_float(scratch%scr1(1),mtp)
         call exstbuff(mzp,mxp,myp,basic_g(ngrid)%vp(1,1,1)  &
             ,scratch%scr1(1),i1-i0,i2-i0,j1-j0,j2-j0,mtc)
      elseif(isflag.eq.6) then
         call par_get_float(scratch%scr1(1),mtp)
         call exstbuff(mzp,mxp,myp,basic_g(ngrid)%wp(1,1,1)  &
             ,scratch%scr1(1),i1-i0,i2-i0,j1-j0,j2-j0,mtc)
         call par_get_float(scratch%scr1(1),mtp)
         call exstbuff(mzp,mxp,myp,basic_g(ngrid)%pp(1,1,1)  &
             ,scratch%scr1(1),i1-i0,i2-i0,j1-j0,j2-j0,mtc)
      endif

   endif

enddo

return
end

!*********************************************************************

subroutine mkstbuff(n1,n2,n3,a,b,il,ir,jb,jt,ind)
implicit none
integer :: n1,n2,n3,il,ir,jb,jt,ind
real :: a(n1,n2,n3),b(*)

integer :: i,j,k

ind=0
do j=jb,jt
   do i=il,ir
      do k=1,n1
         ind=ind+1
         b(ind)=a(k,i,j)
      enddo
   enddo
enddo

return
end

!*********************************************************************

subroutine mkstbuffs(n1,n2,n3,a,b,il,ir,jb,jt,ind)
implicit none
integer :: n1,n2,n3,il,ir,jb,jt,ind
real :: a(n1,n2,n3),b(*)

integer :: i,j,k

ind=0
do j=jb,jt
   do i=il,ir
      do k=1,n3
         ind=ind+1
         b(ind)=a(i,j,k)
      enddo
   enddo
enddo

return
end

!*********************************************************************

subroutine exstbuff(n1,n2,n3,a,b,il,ir,jb,jt,ind)
implicit none
integer :: n1,n2,n3,il,ir,jb,jt,ind
real :: a(n1,n2,n3),b(*)

integer :: i,j,k

ind=0
do j=jb,jt
   do i=il,ir
      do k=1,n1
         ind=ind+1
         a(k,i,j)=b(ind)
      enddo
   enddo
enddo

return
end

!*********************************************************************

subroutine exstbuffs(n1,n2,n3,a,b,il,ir,jb,jt,ind)
implicit none
integer :: n1,n2,n3,il,ir,jb,jt,ind
real :: a(n1,n2,n3),b(*)

integer :: i,j,k

ind=0
do j=jb,jt
   do i=il,ir
      do k=1,n3
         ind=ind+1
         a(i,j,k)=b(ind)
      enddo
   enddo
enddo

return
end


