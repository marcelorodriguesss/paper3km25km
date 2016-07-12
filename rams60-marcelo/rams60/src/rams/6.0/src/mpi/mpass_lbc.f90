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

subroutine node_sendlbc()

use mem_grid
use node_mod

use var_tables
use mem_scratch

implicit none

integer :: itype,nm,i1,i2,j1,j2,nv,mtp

itype=1

!______________________
!
!   First, before we send anything, let's post the receives. Also, make sure
!     any pending sends are complete.

do nm=1,nmachs
   if (iget_paths(itype,ngrid,nm).ne.0) then
      call par_get_noblock(node_buffs(nm)%lbc_recv_buff(1)  &
          ,node_buffs(nm)%nrecv ,20000+ngrid,machs(nm),irecv_req(nm) )

   endif
enddo


!______________________
!
!   Now we can actually go on to sending the stuff

do nm=1,nmachs

   if(ipaths(1,itype,ngrid,nm).ne.0) then

      i1=ipaths(1,itype,ngrid,nm)
      i2=ipaths(2,itype,ngrid,nm)
      j1=ipaths(3,itype,ngrid,nm)
      j2=ipaths(4,itype,ngrid,nm)

      call par_init_put(node_buffs(nm)%lbc_send_buff(1)  &
                       ,node_buffs(nm)%nsend )
      call par_put_int(i1,1)
      call par_put_int(i2,1)
      call par_put_int(j1,1)
      call par_put_int(j2,1)
      call par_put_int(mynum,1)

      do nv = 1,num_var(ngrid)
         if ( vtab_r(nv,ngrid)%impt1 == 1) then
            if ( vtab_r(nv,ngrid)%idim_type == 3) then
               call mkstbuff(mzp,mxp,myp,vtab_r(nv,ngrid)%var_p  &
                   ,scratch%vt3dp(1),i1-i0,i2-i0,j1-j0,j2-j0,mtp)
               call par_put_float(scratch%vt3dp(1),mtp)
            elseif ( vtab_r(nv,ngrid)%idim_type == 2) then
               call mkstbuff(1,mxp,myp,vtab_r(nv,ngrid)%var_p  &
                   ,scratch%vt3dp(1),i1-i0,i2-i0,j1-j0,j2-j0,mtp)
               call par_put_float(scratch%vt3dp(1),mtp)
            endif
         endif
      enddo

      call par_send_noblock(ipaths(5,itype,ngrid,nm)  &
           ,20000+ngrid,isend_req(nm))

   endif

enddo

return
end

!     ****************************************************************

subroutine node_getlbc()

use mem_grid
use node_mod

use var_tables
use mem_scratch

implicit none

integer :: itype,nm,ibytes,msgid,ihostnum,i1,i2,j1,j2  &
          ,nmp,nv,node_src,mtc,mtp,nptsxy

itype=1

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

      do nv = 1,num_var(ngrid)
         if ( vtab_r(nv,ngrid)%impt1 == 1) then
            if ( vtab_r(nv,ngrid)%idim_type == 3) then
               mtp=nnzp(ngrid) * nptsxy
               call par_get_float(scratch%vt3dp(1),mtp)
               call exstbuff(mzp,mxp,myp,vtab_r(nv,ngrid)%var_p  &
                   ,scratch%vt3dp(1),i1-i0,i2-i0,j1-j0,j2-j0,mtc)
            elseif ( vtab_r(nv,ngrid)%idim_type == 2) then
               mtp= nptsxy
               call par_get_float(scratch%vt3dp(1),mtp)
               call exstbuff(1,mxp,myp,vtab_r(nv,ngrid)%var_p  &
                   ,scratch%vt3dp(1),i1-i0,i2-i0,j1-j0,j2-j0,mtc)
            endif
         endif
      enddo
      
   endif

enddo

return
end
