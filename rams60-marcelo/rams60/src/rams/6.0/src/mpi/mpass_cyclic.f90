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

subroutine ipaths_cyc_alloc(nxp,nyp,ibnd,jbnd,maxmach)
use cyclic_mod
implicit none
integer :: nxp,nyp,ibnd,jbnd,maxmach

npts_cyc = 8 * maxmach
if (ibnd .eq. 4) npts_cyc = npts_cyc + nyp * 4
if (jbnd .eq. 4) npts_cyc = npts_cyc + nxp * 4
if (ibnd .eq. 4 .and. jbnd .eq. 4) npts_cyc = npts_cyc + 16

allocate (ipathst_cyc(8,npts_cyc))
allocate (ipathsu_cyc(8,npts_cyc))
allocate (ipathsv_cyc(8,npts_cyc))

call azero(8*npts_cyc,ipathst_cyc(1,1))
call azero(8*npts_cyc,ipathsu_cyc(1,1))
call azero(8*npts_cyc,ipathsv_cyc(1,1))


return
end

!*****************************************************************************

subroutine node_cycinit(nzp,nxp,nyp,npvar,nmachs,ibnd,jbnd,mynum)
use cyclic_mod
implicit none
integer :: nmachs,mynum,icypts,nzp,nxp,nyp,icycpts,iadd,mdn,msn,ndn,nsn  &
   ,npvar,ibnd,jbnd,nm,maxijrecv_cyc

integer, save, allocatable :: ijcount(:),jdn(:),jsn(:)

integer :: idn,isn,ii,iijj

! This subroutine is called by each node process.  It uses information from
! the master cyclic parallel array [ipaths_cyc] to construct integer scalars 
! and arrays that the node needs for parallel data sends and receives for 
! cyclic boundary conditions

allocate (ndn_cyc(nmachs),nsn_cyc(nmachs),msn_cyc(nmachs),mdn_cyc(nmachs)  &
   ,nijsendt_cyc(nmachs),nijsendu_cyc(nmachs),nijsendv_cyc(nmachs)  &
   ,nijrecvt_cyc(nmachs),nijrecvu_cyc(nmachs),nijrecvv_cyc(nmachs)  &
   ,ijcount(nmachs)  &
   ,jdn(nmachs),jsn(nmachs),isend_req_cyc(6,nmachs),irecv_req_cyc(6,nmachs))

do nm = 1,nmachs
   nijsendt_cyc(nm) = 0.
   nijsendu_cyc(nm) = 0.
   nijsendv_cyc(nm) = 0.
   nijrecvt_cyc(nm) = 0.
   nijrecvu_cyc(nm) = 0.
   nijrecvv_cyc(nm) = 0.
   isend_req_cyc(1:6,nm)=-999
   irecv_req_cyc(1:6,nm)=-999
enddo

do icycpts = 1,npts_cyc

   msn = ipathst_cyc(1,icycpts)
   mdn = ipathst_cyc(4,icycpts)

   if (msn .eq. mynum .and. mdn .gt. 0) then
      nijsendt_cyc(mdn) = nijsendt_cyc(mdn) + 1
   endif
   
   if (mdn .eq. mynum .and. msn .gt. 0) then
      nijrecvt_cyc(msn) = nijrecvt_cyc(msn) + 1
   endif
enddo

do icycpts = 1,npts_cyc
   msn = ipathsu_cyc(1,icycpts)
   mdn = ipathsu_cyc(4,icycpts)

   if (msn .eq. mynum .and. mdn .gt. 0) then
      nijsendu_cyc(mdn) = nijsendu_cyc(mdn) + 1
   endif
   if (mdn .eq. mynum .and. msn .gt. 0) then
      nijrecvu_cyc(msn) = nijrecvu_cyc(msn) + 1
   endif
enddo

do icycpts = 1,npts_cyc
   msn = ipathsv_cyc(1,icycpts)
   mdn = ipathsv_cyc(4,icycpts)

   if (msn .eq. mynum .and. mdn .gt. 0) then
      nijsendv_cyc(mdn) = nijsendv_cyc(mdn) + 1
   endif
   if (mdn .eq. mynum .and. msn .gt. 0) then
      nijrecvv_cyc(msn) = nijrecvv_cyc(msn) + 1
   endif

enddo

nsn = 0
ndn = 0
maxijsendt_cyc = 0
maxijsendu_cyc = 0
maxijsendv_cyc = 0
maxijrecvt_cyc = 0
maxijrecvu_cyc = 0
maxijrecvv_cyc = 0

do nm = 1,nmachs
   if (nijsendt_cyc(nm) .gt. 0) then
      ndn = ndn + 1
      ndn_cyc(nm) = ndn
      mdn_cyc(ndn) = nm
      nijsendt_cyc(ndn) = nijsendt_cyc(nm)
      nijsendu_cyc(ndn) = nijsendu_cyc(nm)
      nijsendv_cyc(ndn) = nijsendv_cyc(nm)
      maxijsendt_cyc = max(maxijsendt_cyc,nijsendt_cyc(ndn))
      maxijsendu_cyc = max(maxijsendu_cyc,nijsendu_cyc(ndn))
      maxijsendv_cyc = max(maxijsendv_cyc,nijsendv_cyc(ndn))
   endif

   if (nijrecvt_cyc(nm) .gt. 0) then
      nsn = nsn + 1
      nsn_cyc(nm) = nsn
      msn_cyc(nsn) = nm
      nijrecvt_cyc(nsn) = nijrecvt_cyc(nm)
      nijrecvu_cyc(nsn) = nijrecvu_cyc(nm)
      nijrecvv_cyc(nsn) = nijrecvv_cyc(nm)
      maxijrecvt_cyc = max(maxijrecvt_cyc,nijrecvt_cyc(nsn))
      maxijrecvu_cyc = max(maxijrecvu_cyc,nijrecvu_cyc(nsn))
      maxijrecvv_cyc = max(maxijrecvv_cyc,nijrecvv_cyc(nsn))
   endif
enddo

ndns_cyc = ndn
nsns_cyc = nsn

maxijrecv_cyc = max(maxijrecvt_cyc,maxijrecvu_cyc,maxijrecvv_cyc)

allocate (ijsendt_cyc(6,maxijsendt_cyc,ndns_cyc) &
         ,ijsendu_cyc(6,maxijsendu_cyc,ndns_cyc) &
         ,ijsendv_cyc(6,maxijsendv_cyc,ndns_cyc) &
         ,ijrecv_cyc(6,maxijrecv_cyc))

do ndn = 1,ndns_cyc
   ijcount(ndn) = 0
enddo

do icycpts = 1,npts_cyc
   msn = ipathst_cyc(1,icycpts)
   mdn = ipathst_cyc(4,icycpts)
   if (msn .eq. mynum .and. mdn .gt. 0) then
      ndn = ndn_cyc(mdn)
      ijcount(ndn) = ijcount(ndn) + 1
      ijsendt_cyc(1,ijcount(ndn),ndn) = ipathst_cyc(2,icycpts)
      ijsendt_cyc(2,ijcount(ndn),ndn) = ipathst_cyc(3,icycpts)
      ijsendt_cyc(3,ijcount(ndn),ndn) = ipathst_cyc(5,icycpts)
      ijsendt_cyc(4,ijcount(ndn),ndn) = ipathst_cyc(6,icycpts)
      ijsendt_cyc(5,ijcount(ndn),ndn) = ipathst_cyc(7,icycpts)
      ijsendt_cyc(6,ijcount(ndn),ndn) = ipathst_cyc(8,icycpts)
   endif
   
enddo

do ndn = 1,ndns_cyc
   ijcount(ndn) = 0
enddo

do icycpts = 1,npts_cyc
   msn = ipathsu_cyc(1,icycpts)
   mdn = ipathsu_cyc(4,icycpts)
   if (msn .eq. mynum .and. mdn .gt. 0) then
      ndn = ndn_cyc(mdn)
      ijcount(ndn) = ijcount(ndn) + 1
      ijsendu_cyc(1,ijcount(ndn),ndn) = ipathsu_cyc(2,icycpts)
      ijsendu_cyc(2,ijcount(ndn),ndn) = ipathsu_cyc(3,icycpts)
      ijsendu_cyc(3,ijcount(ndn),ndn) = ipathsu_cyc(5,icycpts)
      ijsendu_cyc(4,ijcount(ndn),ndn) = ipathsu_cyc(6,icycpts)
      ijsendu_cyc(5,ijcount(ndn),ndn) = ipathsu_cyc(7,icycpts)
      ijsendu_cyc(6,ijcount(ndn),ndn) = ipathsu_cyc(8,icycpts)
   endif
enddo

do ndn = 1,ndns_cyc
   ijcount(ndn) = 0
enddo

do icycpts = 1,npts_cyc
   msn = ipathsv_cyc(1,icycpts)
   mdn = ipathsv_cyc(4,icycpts)
   if (msn .eq. mynum .and. mdn .gt. 0) then
      ndn = ndn_cyc(mdn)
      ijcount(ndn) = ijcount(ndn) + 1
      ijsendv_cyc(1,ijcount(ndn),ndn) = ipathsv_cyc(2,icycpts)
      ijsendv_cyc(2,ijcount(ndn),ndn) = ipathsv_cyc(3,icycpts)
      ijsendv_cyc(3,ijcount(ndn),ndn) = ipathsv_cyc(5,icycpts)
      ijsendv_cyc(4,ijcount(ndn),ndn) = ipathsv_cyc(6,icycpts)
      ijsendv_cyc(5,ijcount(ndn),ndn) = ipathsv_cyc(7,icycpts)
      ijsendv_cyc(6,ijcount(ndn),ndn) = ipathsv_cyc(8,icycpts)
   endif
enddo

! allocate cyclic buffers

nbuffsend_cyc = max(maxijsendt_cyc * (max(2,npvar) * nzp + 6) + 1  &
                   ,maxijsendu_cyc * (nzp + 6) + 1                 &
                  + maxijsendv_cyc * (nzp + 6) + 1                 ) 

allocate (buffsend_cyc(nbuffsend_cyc,ndns_cyc))

nbuffrecv_cyc = max(maxijrecvt_cyc * (max(2,npvar) * nzp + 6) + 1  &
                   ,maxijrecvu_cyc * (nzp + 6) + 1                 &
                  + maxijrecvv_cyc * (nzp + 6) + 1                 ) 

allocate (buffrecv_cyc(nbuffrecv_cyc,nsns_cyc))

if (ibnd .eq. 4 .or. jbnd .eq. 4) then
   allocate(lstart_cyc(npvar+4,nxp,nyp))
endif

return
end

!*****************************************************************************

subroutine node_sendcyclic(isflag)

use mem_grid
use var_tables
use cyclic_mod
use mem_basic
use mem_scratch
use node_mod

implicit none

integer :: nsn,icycpts,msn,mdn,isn,jsn,ind,nmp,isflag,ndn
integer :: ibob,ii,nm,iijj,nv,ijr,ibytes,msgid  &
   ,ihostnum

if (ibnd .ne. 4 .and. jbnd .ne. 4) return

!   First, before we send anything, let's post the receives

do nsn = 1,nsns_cyc
   msn = msn_cyc(nsn)
!                                IN              IN
   call par_get_noblock(buffrecv_cyc(1,nsn),nbuffrecv_cyc  &
!                      IN                            IN  
      ,21000+100*machs(msn)+10*machs(mynum)+isflag,machs(msn)  &
!              OUT
      ,irecv_req_cyc(isflag,msn))
enddo

!   Now we can actually go on to sending the stuff

do ndn = 1,ndns_cyc

   mdn = mdn_cyc(ndn)
   call par_init_put(buffsend_cyc(1,ndn),nbuffsend_cyc)
   
   if (isflag == 1 .or. isflag == 4 .or. isflag == 6) then   
      call par_put_int(nijsendt_cyc(ndn),1)
      call par_put_int(ijsendt_cyc(1,1,ndn),6*nijsendt_cyc(ndn))

      do icycpts = 1,nijsendt_cyc(ndn)

         isn = ijsendt_cyc(1,icycpts,ndn) - mi0(1)
         jsn = ijsendt_cyc(2,icycpts,ndn) - mj0(1)

         if (isflag == 1) then

            do nv = 1,num_var(1)
               if ( vtab_r(nv,1)%impt1 == 1) then
                  if ( vtab_r(nv,1)%idim_type == 3) then
                     call mkcycbuff(mmzp(1),mmxp(1),mmyp(1)  &
                        ,vtab_r(nv,1)%var_p,scratch%vt3dp(1)  &
                        ,isn,jsn)

                     call par_put_float(scratch%vt3dp(1),mmzp(1))
                        
                  elseif ( vtab_r(nv,1)%idim_type == 2) then
                     call mkcycbuff(1,mmxp(1),mmyp(1)  &
                        ,vtab_r(nv,1)%var_p,scratch%vt3dp(1)  &
                        ,isn,jsn)
                     call par_put_float(scratch%vt3dp(1),1)
                  endif
               endif
            enddo

         elseif (isflag == 4) then

            call mkcycbuff(mmzp(1),mmxp(1),mmyp(1)  &
               ,basic_g(1)%pp(1,1,1),scratch%vt3dp(1)  &
               ,isn,jsn)
            call par_put_float(scratch%vt3dp(1),mmzp(1))

         elseif (isflag == 6) then

            call mkcycbuff(mmzp(1),mmxp(1),mmyp(1)   &
               ,basic_g(1)%wp(1,1,1),scratch%vt3dp(1)  &
               ,isn,jsn)
            call par_put_float(scratch%vt3dp(1),mmzp(1))
            call mkcycbuff(mmzp(1),mmxp(1),mmyp(1)  &
               ,basic_g(1)%pp(1,1,1),scratch%vt3dp(1)  &
               ,isn,jsn)
            call par_put_float(scratch%vt3dp(1),mmzp(1))

         endif
      enddo
            
   elseif (isflag == 2) then   

      call par_put_int(nijsendu_cyc(ndn),1)
      call par_put_int(ijsendu_cyc(1,1,ndn),6*nijsendu_cyc(ndn))

      do icycpts = 1,nijsendu_cyc(ndn)

         isn = ijsendu_cyc(1,icycpts,ndn) - mi0(1)
         jsn = ijsendu_cyc(2,icycpts,ndn) - mj0(1)

         call mkcycbuff(mmzp(1),mmxp(1),mmyp(1)  &
            ,basic_g(1)%up(1,1,1),scratch%vt3dp(1)  &
            ,isn,jsn)
         call par_put_float(scratch%vt3dp(1),mmzp(1))

      enddo

   elseif (isflag == 3) then

      call par_put_int(nijsendv_cyc(ndn),1)
      call par_put_int(ijsendv_cyc(1,1,ndn),6*nijsendv_cyc(ndn))

      do icycpts = 1,nijsendv_cyc(ndn)

         isn = ijsendv_cyc(1,icycpts,ndn) - mi0(1)
         jsn = ijsendv_cyc(2,icycpts,ndn) - mj0(1)

         call mkcycbuff(mmzp(1),mmxp(1),mmyp(1)  &
            ,basic_g(1)%vp(1,1,1),scratch%vt3dp(1)  &
            ,isn,jsn)
         call par_put_float(scratch%vt3dp(1),mmzp(1))

      enddo

   elseif (isflag == 5) then

      call par_put_int(nijsendu_cyc(ndn),1)
      call par_put_int(ijsendu_cyc(1,1,ndn),6*nijsendu_cyc(ndn))

      do icycpts = 1,nijsendu_cyc(ndn)

         isn = ijsendu_cyc(1,icycpts,ndn) - mi0(1)
         jsn = ijsendu_cyc(2,icycpts,ndn) - mj0(1)

         call mkcycbuff(mmzp(1),mmxp(1),mmyp(1)  &
            ,basic_g(1)%up(1,1,1),scratch%vt3dp(1)  &
            ,isn,jsn)
         call par_put_float(scratch%vt3dp(1),mmzp(1))

      enddo
      

      call par_put_int(nijsendv_cyc(ndn),1)
      call par_put_int(ijsendv_cyc(1,1,ndn),6*nijsendv_cyc(ndn))

      do icycpts = 1,nijsendv_cyc(ndn)

         isn = ijsendv_cyc(1,icycpts,ndn) - mi0(1)
         jsn = ijsendv_cyc(2,icycpts,ndn) - mj0(1)

         call mkcycbuff(mmzp(1),mmxp(1),mmyp(1)  &
            ,basic_g(1)%vp(1,1,1),scratch%vt3dp(1)  &
            ,isn,jsn)
         call par_put_float(scratch%vt3dp(1),mmzp(1))

      enddo

   endif

!                         IN          IN
   call par_send_noblock(mdn,21000+100*machs(mynum)+10*machs(mdn)+isflag  &
!             OUT
      ,isend_req_cyc(isflag,mdn))
      
enddo

return
end

!*****************************************************************************

subroutine node_getcyclic(isflag)

use mem_grid
use var_tables
use cyclic_mod
use mem_basic
use mem_scratch
use node_mod

implicit none

integer :: ndn,nsn,mdn,msn,numcols,icol,mtp,mtc,ind,ibytes,msgid  &
   ,ihostnum,nmp,isflag,mijrecv,ijr,istart,jstart,ivar,idn,jdn,iijj &
   ,nv,kg,nid

if (ibnd .ne. 4 .and. jbnd .ne. 4) return

!  First, let's make sure our sends are all finished and de-allocated

do ndn = 1,ndns_cyc
   mdn = mdn_cyc(ndn)
      
!                    input                   out   out     out
   call par_wait(isend_req_cyc(isflag,mdn),ibytes,msgid,ihostnum)
      
enddo

!  Now, let's wait on our receives

!  Initialize start flags.

    lstart_cyc(:,:,:) = 0

mtp = nnzp(1)

do nsn = 1,nsns_cyc
   msn = msn_cyc(nsn)
      
!                    input                  out   out     out
   call par_wait(irecv_req_cyc(isflag,msn),ibytes,msgid,ihostnum)
      
!  We got all our stuff.  Now unpack it into appropriate space.

   call par_assoc_buff(buffrecv_cyc(1,nsn),nbuffrecv_cyc)

   if (isflag == 1 .or. isflag == 4 .or. isflag == 6) then   
      call par_get_int(mijrecv,1)
      call par_get_int(ijrecv_cyc(1,1),6*mijrecv)

      do ijr = 1,mijrecv

         if (isflag == 1) then

            ivar = 4
            do nv = 1,num_var(1)
               if ( vtab_r(nv,1)%impt1 == 1) then
                  ivar = ivar + 1
                  if ( vtab_r(nv,1)%idim_type == 3) then
                     call par_get_float(scratch%vt3dp(1),mtp)
                     call cyclic_para(mmzp(1),mmxp(1),mmyp(1)  &
                        ,vtab_r(nv,ngrid)%var_p,scratch%vt3dp(1)  &
                        ,ijr,mi0(1),mj0(1),ivar,mynum)
                  elseif ( vtab_r(nv,1)%idim_type == 2) then
                     call par_get_float(scratch%vt3dp(1),1)
                     call cyclic_para(1,mmxp(1),mmyp(1)  &
                        ,vtab_r(nv,ngrid)%var_p,scratch%vt3dp(1)  &
                        ,ijr,mi0(1),mj0(1),ivar,mynum)
                  endif
               endif
            enddo
         elseif (isflag == 4) then
            call par_get_float(scratch%vt3dp(1),mtp)
            call cyclic_para(mmzp(1),mmxp(1),mmyp(1)  &
               ,basic_g(1)%pp(1,1,1),scratch%vt3dp(1),ijr,mi0(1),mj0(1),4,mynum)
         elseif (isflag == 6) then
            call par_get_float(scratch%vt3dp(1),mtp)
            call cyclic_para(mmzp(1),mmxp(1),mmyp(1)  &
               ,basic_g(1)%wp(1,1,1),scratch%vt3dp(1),ijr,mi0(1),mj0(1),3,mynum)
            call par_get_float(scratch%vt3dp(1),mtp)
            call cyclic_para(mmzp(1),mmxp(1),mmyp(1)  &
            ,basic_g(1)%pp(1,1,1),scratch%vt3dp(1),ijr,mi0(1),mj0(1),4,mynum)
         endif
      enddo
 
   elseif (isflag == 2) then

      call par_get_int(mijrecv,1)
      call par_get_int(ijrecv_cyc(1,1),6*mijrecv)

      do ijr = 1,mijrecv
         call par_get_float(scratch%vt3dp(1),mtp)
         call cyclic_para(mmzp(1),mmxp(1),mmyp(1)  &
            ,basic_g(1)%up(1,1,1),scratch%vt3dp(1),ijr,mi0(1),mj0(1),1,mynum)
      enddo

   elseif (isflag .eq. 3) then
      call par_get_int(mijrecv,1)
      call par_get_int(ijrecv_cyc(1,1),6*mijrecv)

      do ijr = 1,mijrecv
         call par_get_float(scratch%vt3dp(1),mtp)
         call cyclic_para(mmzp(1),mmxp(1),mmyp(1)  &
            ,basic_g(1)%vp(1,1,1),scratch%vt3dp(1),ijr,mi0(1),mj0(1),2,mynum)
      enddo
      
   elseif (isflag .eq. 5) then
      call par_get_int(mijrecv,1)
      call par_get_int(ijrecv_cyc(1,1),6*mijrecv)

      do ijr = 1,mijrecv
         call par_get_float(scratch%vt3dp(1),mtp)
         call cyclic_para(mmzp(1),mmxp(1),mmyp(1)  &
            ,basic_g(1)%up(1,1,1),scratch%vt3dp(1),ijr,mi0(1),mj0(1),1,mynum)
      enddo

      call par_get_int(mijrecv,1)
      call par_get_int(ijrecv_cyc(1,1),6*mijrecv)

      do ijr = 1,mijrecv
         call par_get_float(scratch%vt3dp(1),mtp)
         call cyclic_para(mmzp(1),mmxp(1),mmyp(1)  &
            ,basic_g(1)%vp(1,1,1),scratch%vt3dp(1),ijr,mi0(1),mj0(1),2,mynum)
      enddo
   endif

enddo

return
end

!*****************************************************************************

subroutine cyclic_para(m1,m2,m3,af,bf,ijr,i0,j0,ivar,mynum)
use cyclic_mod

implicit none
integer :: m1,m2,m3,ijr,i0,j0,ivar,mynum
real, dimension(m1,m2,m3) :: af
real, dimension(*) :: bf

integer :: idn,jdn,k,iijj,i,j,isn,jsn
real :: sum,self,other

integer, save :: ncallc = 0

isn = ijrecv_cyc(1,ijr)
jsn = ijrecv_cyc(2,ijr)
idn = ijrecv_cyc(3,ijr) - i0
jdn = ijrecv_cyc(4,ijr) - j0
sum = float(ijrecv_cyc(5,ijr) + ijrecv_cyc(6,ijr))
self = float(ijrecv_cyc(5,ijr)) / sum
other = 1./ sum

if (lstart_cyc(ivar,idn,jdn) .eq. 0) then
   lstart_cyc(ivar,idn,jdn) = 1
   do k = 1,m1
      af(k,idn,jdn) = af(k,idn,jdn) * self
   enddo
endif

do k = 1,m1
   af(k,idn,jdn) = af(k,idn,jdn) + bf(k) * other
enddo

return
end

!****************************************************************

subroutine mkcycbuff(n1,n2,n3,a,b,i,j)
dimension a(n1,n2,n3),b(*)

do k = 1,n1
   b(k) = a(k,i,j)
enddo

return
end

