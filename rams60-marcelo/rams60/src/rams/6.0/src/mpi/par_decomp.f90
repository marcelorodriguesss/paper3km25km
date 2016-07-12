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

subroutine PAR_decomp(nxp,nyp  &
   ,nodes,work,workrow,nblocks,workload,workblock,workcol  &
   ,jrows,jrow,ixb,ixe,iyb,iye)
implicit none
integer :: nxp,nyp,nodes

real :: work(nxp,*),workrow(*),workload(*),workblock(*),workcol(*)
integer :: jrows(*),jrow(*),nblocks(*),ixb(*),ixe(*),iyb(*),iye(*)

! default relspeed = 1.0 for nodes of uniform speed.
real :: relspeed(256)
data relspeed/256*1./

integer :: inode,i,j,islab,jnodes,nslabs,min_blocks,nbigslabs,iblock &
          ,jnode,knode
real :: anodes,aslabs,totspeed,workdom,workaccum,worksofar &
       ,slabspeed,workslab

! This routine decomposes grid domains of size (nnxp,nnyp) into a number,
! specified by nodes, of rectangular subdomains.  The convention is followed
! that any internal boundaries (between subdomains) that are parallel to
! the x-axis run continuously across the full domain, while boundaries
! parallel to the y-axis may or may not run the full distance across the
! domain.  For convenience, regions of the domain bounded by adjacent
! east-west internal boundaries are termed "slabs", while smaller divisions
! within each slab are termed "blocks".  Each block is required to have
! a minimum dimension of 6 by 6 grid cells.  If this cannot be satisfied
! with the given input parameters, the subroutine stops.


! Estimate the number of slabs to be used (aslabs), and compute a final
! nearest integer value (nslabs) which is limited to allowable values.
! Zero out array for accumulating number of columns for each node.

   anodes = float(nodes)
   aslabs = sqrt(anodes * float(nyp) / float(nxp))
   nslabs = min(nodes,max(1,nint(aslabs)))

!          print*, 'nslabs',nslabs

   totspeed = 0.
   do inode = 1,nodes
      ixe(inode) = 0
      totspeed = totspeed + relspeed(inode)
   enddo

!          print*, 'totspeed',totspeed

! Compute total work load over each row and over entire domain.

   workdom = 0.
   do j = 1,nyp
      workrow(j) = 0.
      do i = 1,nxp
         workrow(j) = workrow(j) + work(i,j)
      enddo
      workdom = workdom + workrow(j)

!          print*, 'j,workdom,workrow(j)',j,workdom,workrow(j)

   enddo
   workrow(2) = workrow(2) + workrow(1)
   workrow(nyp-1) = workrow(nyp-1) + workrow(nyp)

! Determine number of blocks and the average workload for each slab.

   min_blocks = nodes / nslabs
   nbigslabs = nodes - min_blocks * nslabs
   inode = 0
   do islab = 1,nslabs
      workload(islab) = 0.
      nblocks(islab) = min_blocks
      if (islab .le. nbigslabs) nblocks(islab) = min_blocks + 1
      do iblock = 1,nblocks(islab)
         inode = inode + 1
         workload(islab) = workload(islab)  &
            + workdom * relspeed(inode) / totspeed

!           print*, 'islab,iblock,workload(islab),workdom,inode'
!           print*,  islab,iblock,workload(islab),workdom,inode

      enddo
   enddo

! Assign all j-rows to their respective slabs in a way that balances the work
! load among slabs according to their respective numbers of nodes (blocks).
! The array jrows counts the number of rows in each slab, and the array
! jrow is the index of the southernmost row in each slab.

   do islab = 1,nslabs
      jrows(islab) = 0
   enddo

   workaccum = 0.
   worksofar = 0.
   islab = 0

   do j = 2,nyp-1
      workaccum = workaccum + workrow(j)
      if (workaccum - .5 * workrow(j) .gt. worksofar .and.  &
              islab .lt. nslabs) then
         islab = islab + 1
         jrow(islab) = j
         worksofar = worksofar + workload(islab)
      endif
      jrows(islab) = jrows(islab) + 1
   enddo

   inode = 0
   jnode = 0
   knode = 0
   do islab = 1,nslabs

! Compute the total work load for each slab and for each i-column in the
! slab.

      slabspeed = 0.
      workslab = 0.
      do i = 1,nxp
         workcol(i) = 0.
         do j = jrow(islab),jrow(islab)+jrows(islab)-1
            workcol(i) = workcol(i) + work(i,j)
         enddo
         workslab = workslab + workcol(i)
      enddo
      workcol(2) = workcol(2) + workcol(1)
      workcol(nxp-1) = workcol(nxp-1) + workcol(nxp)

! Determine average workload for each block.

      do iblock = 1,nblocks(islab)
         jnode = jnode + 1
         slabspeed = slabspeed + relspeed(jnode)

!           print*, 'r1:iblock,jnode,slabspeed,relspeed(jnode)'
!           print*,     iblock,jnode,slabspeed,relspeed(jnode)

      enddo
      do iblock = 1,nblocks(islab)
         knode = knode + 1
         workblock(iblock) = workslab  &
            * relspeed(knode) / slabspeed

!       print*, 'islab,iblock,workblock,workslab,relspeed,slabspeed'
!       print*, islab,iblock,workblock(iblock),workslab,relspeed(knode)
!     +       ,slabspeed
!       print*, 'knode',knode

      enddo

! Assign the i-columns of each slab to their respective blocks in a way that
! balances the work load among the blocks.  The array ncols counts the number
! of i-columns on each node, and the array ncol is the index of the
! westernmost i-column on each node.

      workaccum = 0.
      worksofar = 0.

      iblock = 0
      do i = 2,nxp-1
         workaccum = workaccum + workcol(i)

!        print*, 'islab',islab
!        print*, 'i,workaccum,workcol(i),worksofar,iblock,nblocks'
!        print*, i,workaccum,workcol(i),worksofar,iblock,nblocks(islab)

         if (workaccum - .5 * workcol(i) .gt. worksofar .and.  &
                iblock .lt. nblocks(islab)) then
            iblock = iblock + 1

!ccccccc defining node variables here ccccccccccccccccccccccccccccccccccc
            inode = inode + 1
            iyb(inode) = jrow(islab)
            ixb(inode) = i
            iye(inode) = iyb(inode) + jrows(islab) - 1

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            worksofar = worksofar + workblock(iblock)
         endif
         ixe(inode) = ixe(inode) + 1
      enddo
   enddo

!ccccccc defining node variable here ccccccccccccccccccccccccccccccccccc
   do jnode = 1,nodes
      ixe(jnode) = ixb(jnode) + ixe(jnode) - 1

!           print*, 'jnode,ixb,ixe',jnode,ixb(jnode),ixe(jnode)
!     +        ,(ixe(jnode)-ixb(jnode)+1),(iye(jnode)-iyb(jnode)+1)
!     +        ,(ixe(jnode)-ixb(jnode)+1)*(iye(jnode)-iyb(jnode)+1)

   enddo
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

! Check to make sure that each subdomain has at least 2 interior 
! rows and columns.

   do jnode = 1,nodes
      if (iye(jnode) - iyb(jnode) .lt. 1 .or.  &
          ixe(jnode) - ixb(jnode) .lt. 1) then
         print*, 'grid:',nxp,nyp,'  subdomain too small on node ',jnode
         print*, '(ixb,ixe,iyb,iye) = '  &
            ,ixb(jnode),ixe(jnode),iyb(jnode),iye(jnode)
         stop 'small_nodes'
      endif
   enddo

return
end

!     ******************************************************************

subroutine PAR_est_time(nxp,nyp,work,cput,init)
implicit none
integer :: nxp,nyp,init
real :: work(nxp,nyp),cput(nxp,nyp)

integer :: i,j
real :: bfact

! Sample routine to fill work elements with values proportional to the time
! required to perform model operations.


if(init==1) then
   do j = 2,nyp-1
      do i = 2,nxp-1
         work(i,j) = 1.
      enddo
   enddo
else
   do j = 2,nyp-1
      do i = 2,nxp-1
         work(i,j) = cput(i,j)
      enddo
   enddo
endif

! Fill real boundaries with .2 of interior points

bfact=.2
do j = 1,nyp
   work(1,j) = bfact * work(2,j)
   work(nxp,j) = bfact * work(nxp-1,j)
enddo

do i = 1,nxp
   work(i,1) = bfact * work(i,2)
   work(i,nyp) = bfact * work(i,nyp-1)
enddo

call ezcntr(work,nxp,nyp)

return
end

!     *****************************************************************

subroutine PAR_node_paths(maxgrds,ngrids,nxpmax,nypmax,nnxp,nnyp  &
   ,nxtnest,maxmach,nodes,node_id,ibcflg,nxbeg,nxend,nybeg,nyend  &
   ,ipm,jpm,ixb,ixe,iyb,iye,ipaths,igetpaths,iwid,ibnd,jbnd)

use cyclic_mod
implicit none
integer :: maxgrds,ngrids,nxpmax,nypmax,maxmach,nodes

integer :: nnxp(*),nnyp(*),nxtnest(*),node_id(*),ibcflg(maxmach,*)  &
     ,nxbeg(maxmach,*),nxend(maxmach,*),nybeg(maxmach,*)  &
     ,nyend(maxmach,*),ipm(nxpmax,*),jpm(nypmax,*),ixb(maxmach,*)  &
     ,ixe(maxmach,*),iyb(maxmach,*),iye(maxmach,*)  &
     ,ipaths(5,7,maxgrds,maxmach,maxmach)  &
     ,igetpaths(6,maxgrds,maxmach,maxmach)  &
     ,is0t(3),is0u(3),is0v(3),js0t(3),js0u(3),js0v(3)

integer :: ngr,isend_type,idn,isn,i,j,iwid,ibnd,jbnd,info,nnn &
          ,indt,indu,indv,nxp,nyp,id,jd,nijst,nijsu,nijsv &
          ,iselft,iselfu,iselfv,mijs,is,js

! if using cyclic boundary conditions, allocate ipaths_cyc array

if (ibnd .eq. 4 .or. jbnd .eq. 4) then
   call ipaths_cyc_alloc(nnxp(1),nnyp(1),ibnd,jbnd,maxmach)
endif

! Zero out ipaths array

do ngr=1,ngrids
   do isend_type = 1,6
      do idn=1,nodes
         do isn = 1,nodes
            igetpaths(isend_type,ngr,isn,idn) = 0
            do info = 1,5
               ipaths(info,isend_type,ngr,idn,isn) = 0
            enddo
         enddo
      enddo
   enddo
enddo


do ngr=1,ngrids
   nnn = nxtnest(ngr)

   do idn=1,nodes

      do isn = 1,nodes

         if (isn .eq. idn) go to 6

! Long timestep overlap regions

         if(nxbeg(idn,ngr) .gt. ixe(isn,ngr) .or.  &
            nybeg(idn,ngr) .gt. iye(isn,ngr) .or.  &
            nxend(idn,ngr) .lt. ixb(isn,ngr) .or.  &
            nyend(idn,ngr) .lt. iyb(isn,ngr)) go to 6
         igetpaths(1,ngr,isn,idn) = node_id(isn)

         ipaths(1,1,ngr,idn,isn)=max(ixb(isn,ngr),nxbeg(idn,ngr))
         ipaths(2,1,ngr,idn,isn)=min(ixe(isn,ngr),nxend(idn,ngr))
         ipaths(3,1,ngr,idn,isn)=max(iyb(isn,ngr),nybeg(idn,ngr))
         ipaths(4,1,ngr,idn,isn)=min(iye(isn,ngr),nyend(idn,ngr))
         ipaths(5,1,ngr,idn,isn)=node_id(idn)

!c Expand ipaths to include [coarse] grid external boundary points.

!cb               if (nnn .eq. 0) then
            if (ipaths(1,1,ngr,idn,isn) .eq. 2)  &
                ipaths(1,1,ngr,idn,isn) = 1
            if (ipaths(2,1,ngr,idn,isn) .eq. nnxp(ngr)-1)  &
                ipaths(2,1,ngr,idn,isn) = nnxp(ngr)
            if (ipaths(3,1,ngr,idn,isn) .eq. 2)  &
                ipaths(3,1,ngr,idn,isn) = 1
            if (ipaths(4,1,ngr,idn,isn) .eq. nnyp(ngr)-1)  &
                ipaths(4,1,ngr,idn,isn) = nnyp(ngr)
!cb               endif

! Small timestep overlap regions for u

         if (ixb(idn,ngr)-1 .eq. ixe(isn,ngr) .and.  &
             iye(idn,ngr)   .ge. iyb(isn,ngr) .and.  &
             iyb(idn,ngr)   .le. iye(isn,ngr)) then

            igetpaths(2,ngr,isn,idn) = node_id(isn)

            ipaths(1,2,ngr,idn,isn)=ixe(isn,ngr)
            ipaths(2,2,ngr,idn,isn)=ixe(isn,ngr)
            ipaths(3,2,ngr,idn,isn)=max(iyb(isn,ngr),iyb(idn,ngr))
            ipaths(4,2,ngr,idn,isn)=min(iye(isn,ngr),iye(idn,ngr))
            ipaths(5,2,ngr,idn,isn)=node_id(idn)

         endif

! Small timestep overlap regions for v

         if (iyb(idn,ngr)-1 .eq. iye(isn,ngr) .and.  &
             ixe(idn,ngr)   .ge. ixb(isn,ngr) .and.  &
             ixb(idn,ngr)   .le. ixe(isn,ngr)) then

            igetpaths(3,ngr,isn,idn) = node_id(isn)

            ipaths(1,3,ngr,idn,isn)=max(ixb(isn,ngr),ixb(idn,ngr))
            ipaths(2,3,ngr,idn,isn)=min(ixe(isn,ngr),ixe(idn,ngr))
            ipaths(3,3,ngr,idn,isn)=iye(isn,ngr)
            ipaths(4,3,ngr,idn,isn)=iye(isn,ngr)
            ipaths(5,3,ngr,idn,isn)=node_id(idn)

         endif

! Small timestep overlap regions for pi'

         if (ixe(idn,ngr)+1 .eq. ixb(isn,ngr) .and.  &
             iye(idn,ngr)   .ge. iyb(isn,ngr) .and.  &
             iyb(idn,ngr)   .le. iye(isn,ngr)) then

            igetpaths(4,ngr,isn,idn) = node_id(isn)

            ipaths(1,4,ngr,idn,isn)=ixb(isn,ngr)
            ipaths(2,4,ngr,idn,isn)=ixb(isn,ngr)
            ipaths(3,4,ngr,idn,isn)=max(iyb(isn,ngr),iyb(idn,ngr))
            ipaths(4,4,ngr,idn,isn)=min(iye(isn,ngr),iye(idn,ngr))
            ipaths(5,4,ngr,idn,isn)=node_id(idn)

         elseif (iye(idn,ngr)+1 .eq. iyb(isn,ngr) .and.  &
                 ixe(idn,ngr)   .ge. ixb(isn,ngr) .and.  &
                 ixb(idn,ngr)   .le. ixe(isn,ngr)) then

            igetpaths(4,ngr,isn,idn) = node_id(isn)

            ipaths(1,4,ngr,idn,isn)=max(ixb(isn,ngr),ixb(idn,ngr))
            ipaths(2,4,ngr,idn,isn)=min(ixe(isn,ngr),ixe(idn,ngr))
            ipaths(3,4,ngr,idn,isn)=iyb(isn,ngr)
            ipaths(4,4,ngr,idn,isn)=iyb(isn,ngr)
            ipaths(5,4,ngr,idn,isn)=node_id(idn)

         endif

6             continue

! Coarse grid to fine grid interpolation communication

         if (nnn .eq. 0) go to 26

         if(ipm(ixb(idn,ngr)-1,ngr)-2 .gt. ixe(isn,nnn).or.  &
            jpm(iyb(idn,ngr)-1,ngr)-2 .gt. iye(isn,nnn).or.  &
            ipm(ixe(idn,ngr)+1,ngr)+1 .lt. ixb(isn,nnn).or.  &
            jpm(iye(idn,ngr)+1,ngr)+1 .lt. iyb(isn,nnn)) go to 16
         igetpaths(5,ngr,isn,idn) = node_id(isn)

         ipaths(1,5,ngr,idn,isn) = max(ixb(isn,nnn)  &
            ,ipm(ixb(idn,ngr)-1,ngr)-2)
         ipaths(2,5,ngr,idn,isn) = min(ixe(isn,nnn)  &
            ,ipm(ixe(idn,ngr)+1,ngr)+1)
         ipaths(3,5,ngr,idn,isn) = max(iyb(isn,nnn)  &
            ,jpm(iyb(idn,ngr)-1,ngr)-2)
         ipaths(4,5,ngr,idn,isn) = min(iye(isn,nnn)  &
            ,jpm(iye(idn,ngr)+1,ngr)+1)
         ipaths(5,5,ngr,idn,isn) = node_id(idn)

16            continue

! Fine grid to coarse grid averaging communication

!c rtimh - micphys: in the following lines: change ixb, ixe, iyb, and iye
!c to nxbeg, nxend, nybeg, and nyend on coarse grid destination nodes
!c in order to keep internal CG boundaries up to date, since they have
!c already been updated by their CG neighboring nodes??

         if(nxbeg(idn,nnn) .gt. ipm(ixe(isn,ngr),ngr).or.  &
            nybeg(idn,nnn) .gt. jpm(iye(isn,ngr),ngr).or.  &
            nxend(idn,nnn) .lt. ipm(ixb(isn,ngr),ngr).or.  &
            nyend(idn,nnn) .lt. jpm(iyb(isn,ngr),ngr)) go to 26
         igetpaths(6,ngr,isn,idn) = node_id(isn)

         ipaths(1,6,ngr,idn,isn) = max(ipm(ixb(isn,ngr),ngr)  &
            ,nxbeg(idn,nnn))
         ipaths(2,6,ngr,idn,isn) = min(ipm(ixe(isn,ngr),ngr)  &
            ,nxend(idn,nnn))
         ipaths(3,6,ngr,idn,isn) = max(jpm(iyb(isn,ngr),ngr)  &
            ,nybeg(idn,nnn))
         ipaths(4,6,ngr,idn,isn) = min(jpm(iye(isn,ngr),ngr)  &
            ,nyend(idn,nnn))
         ipaths(5,6,ngr,idn,isn) = node_id(idn)

! A second index value of 7 of the ipaths array is used to determine
! the loop limits in fdbackp for averaging the fm over the overlap
! between the cm node and fm node, rather than always over the full
! fm node.  It is not used for actually sending stuff.  The
! ipaths(*,6,*,*,*) part of the array is still used for sending the
! block of averaged cm points from the fm node to the cm node.

         do i = ixb(isn,ngr),ixe(isn,ngr)
            if (ipm(i,ngr) .eq. ipaths(1,6,ngr,idn,isn)) then
               ipaths(1,7,ngr,idn,isn) = i
               go to 21
            endif
         enddo
21            continue

         do i = ixe(isn,ngr),ixb(isn,ngr),-1
            if (ipm(i,ngr) .eq. ipaths(2,6,ngr,idn,isn)) then
               ipaths(2,7,ngr,idn,isn) = i
               go to 22
            endif
         enddo
22            continue

         do j = iyb(isn,ngr),iye(isn,ngr)
            if (jpm(j,ngr) .eq. ipaths(3,6,ngr,idn,isn)) then
               ipaths(3,7,ngr,idn,isn) = j
               go to 23
            endif
         enddo
23            continue

         do j = iye(isn,ngr),iyb(isn,ngr),-1
            if (jpm(j,ngr) .eq. ipaths(4,6,ngr,idn,isn)) then
               ipaths(4,7,ngr,idn,isn) = j
               go to 24
            endif
         enddo
24            continue


26            continue

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!        do ig = 1,6
!           print*, ' '
!           print*, '   ih   ig  ngr  idn  isn   iget/ipath'
!           write(6,3038)ig,ngr,idn,isn,igetpaths(ig,ngr,isn,idn)
!           print*, ' '
!           do ih = 1,5
!              write(6,3039)ih,ig,ngr,idn,isn,ipaths(ih,ig,ngr,idn,isn)
!           enddo
!        enddo
! 3038   format(5x,4i5,i10)
! 3039   format(5i5,i10)
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      enddo
   enddo
enddo

! Cyclic boundary conditions (grid 1 only)

indt = 0
indu = 0
indv = 0
nxp = nnxp(1)
nyp = nnyp(1)

! Loop over all destination ij columns

do jd = 1,nyp
   do id = 1,nxp
      if (id <= 2 .or. id >= nxp-1 .or. jd <= 2 .or. jd >= nyp-1) then

! Determine 1, 2, or 3 source ij columns for this destination ij column                

         nijst = 0
         nijsu = 0
         nijsv = 0

         if (ibnd == 4) then
! t
            if (id <= 2 .and. jd >= 2 .and. jd <= nyp-1) then
               call fill_cyc(nijst,is0t,js0t,id+nxp-3,jd)
            elseif (id >= nxp-1 .and. jd >= 2 .and. jd <= nyp-1) then
               call fill_cyc(nijst,is0t,js0t,id-nxp+3,jd)
            endif
! u
            if (id == 1 .and. jd >= 2 .and. jd <= nyp-1) then
               call fill_cyc(nijsu,is0u,js0u,id+nxp-3,jd)
            elseif (id == nxp-1 .and. jd >= 2 .and. jd <= nyp-1) then
               call fill_cyc(nijsu,is0u,js0u,id-nxp+3,jd)
            endif
! v
            if (id <= 2 .and. jd >= 2 .and. jd < nyp-1) then
               call fill_cyc(nijsv,is0v,js0v,id+nxp-3,jd)
            elseif (id >= nxp-1 .and. jd >= 2 .and. jd < nyp-1) then
               call fill_cyc(nijsv,is0v,js0v,id-nxp+3,jd)
            endif

         endif
          
         if (jbnd == 4) then
!t
            if (jd <= 2 .and. id >= 2 .and. id <= nxp-1) then
               call fill_cyc(nijst,is0t,js0t,id,jd+nyp-3)
            elseif (jd >= nyp-1 .and. id >= 2 .and. id <= nxp-1) then
               call fill_cyc(nijst,is0t,js0t,id,jd-nyp+3)
            endif
!u
            if (jd <= 2 .and. id >= 2 .and. id < nxp-1) then
               call fill_cyc(nijsu,is0u,js0u,id,jd+nyp-3)
            elseif (jd >= nyp-1 .and. id >= 2 .and. id < nxp-1) then
               call fill_cyc(nijsu,is0u,js0u,id,jd-nyp+3)
            endif
!v
            if (jd == 1 .and. id >= 2 .and. id <= nxp-1) then
               call fill_cyc(nijsv,is0v,js0v,id,jd+nyp-3)
            elseif (jd == nyp-1 .and. id >= 2 .and. id <= nxp-1) then
               call fill_cyc(nijsv,is0v,js0v,id,jd-nyp+3)
            endif

         endif
          
         if (ibnd == 4 .and. jbnd == 4) then     ! group of 2x2 pts each corner
!t
            if (id <= 2 .and. jd <= 2) then
               call fill_cyc(nijst,is0t,js0t,id+nxp-3,jd+nyp-3)
            elseif (id >= nxp-1 .and. jd <= 2) then
               call fill_cyc(nijst,is0t,js0t,id-nxp+3,jd+nyp-3)
            elseif (id <= 2 .and. jd >= nyp-1) then
               call fill_cyc(nijst,is0t,js0t,id+nxp-3,jd-nyp+3)
            elseif (id >= nxp-1 .and. jd >= nyp-1) then
               call fill_cyc(nijst,is0t,js0t,id-nxp+3,jd-nyp+3)
            endif
!u
            if (id == 1 .and. jd <= 2) then
               call fill_cyc(nijsu,is0u,js0u,id+nxp-3,jd+nyp-3)
            elseif (id == nxp-1 .and. jd <= 2) then
               call fill_cyc(nijsu,is0u,js0u,id-nxp+3,jd+nyp-3)
            elseif (id == 1 .and. jd >= nyp-1) then
               call fill_cyc(nijsu,is0u,js0u,id+nxp-3,jd-nyp+3)
            elseif (id == nxp-1 .and. jd >= nyp-1) then
               call fill_cyc(nijsu,is0u,js0u,id-nxp+3,jd-nyp+3)
            endif
!v
            if (id <= 2 .and. jd == 1) then
               call fill_cyc(nijsv,is0v,js0v,id+nxp-3,jd+nyp-3)
            elseif (id >= nxp-1 .and. jd == 1) then
               call fill_cyc(nijsv,is0v,js0v,id-nxp+3,jd+nyp-3)
            elseif (id <= 2 .and. jd == nyp-1) then
               call fill_cyc(nijsv,is0v,js0v,id+nxp-3,jd-nyp+3)
            elseif (id >= nxp-1 .and. jd == nyp-1) then
               call fill_cyc(nijsv,is0v,js0v,id-nxp+3,jd-nyp+3)
            endif

         elseif (ibnd .eq. 4) then                ! single pt each corner
!tuv
            if (id == 1 .and. (jd == 1 .or. jd == nyp)) then
               call fill_cyc(nijst,is0t,js0t,id+nxp-3,jd)
               call fill_cyc(nijsu,is0u,js0u,id+nxp-3,jd)
               call fill_cyc(nijsv,is0v,js0v,id+nxp-3,jd)
            elseif (id == nxp .and. (jd == 1 .or. jd == nyp)) then
               call fill_cyc(nijst,is0t,js0t,id-nxp+3,jd)
               call fill_cyc(nijsu,is0u,js0u,id-nxp+3,jd)
               call fill_cyc(nijsv,is0v,js0v,id-nxp+3,jd)
            endif

         elseif (jbnd .eq. 4) then                ! single pt each corner
!tuv
            if ((id == 1 .or. id == nxp) .and. jd == 1) then
               call fill_cyc(nijst,is0t,js0t,id,jd+nyp-3)
               call fill_cyc(nijsu,is0u,js0u,id,jd+nyp-3)
               call fill_cyc(nijsv,is0v,js0v,id,jd+nyp-3)
             elseif ((id == 1 .or. id == nxp) .and. jd == nyp) then
               call fill_cyc(nijst,is0t,js0t,id,jd-nyp+3)
               call fill_cyc(nijsu,is0u,js0u,id,jd-nyp+3)
               call fill_cyc(nijsv,is0v,js0v,id,jd-nyp+3)
            endif

         endif

         if (id == 1 .or. id == nxp .or. jd == 1 .or. jd == nyp) then
            iselft = 0
            iselfu = 0
            iselfv = 0
         elseif (id == nxp - 1) then
            iselft = 1
            iselfu = 0
            iselfv = 1
            if (jd == nyp - 1) iselfv = 0
         elseif (jd == nyp - 1) then
            iselft = 1
            iselfu = 1
            iselfv = 0
         else
            iselft = 1
            iselfu = 1
            iselfv = 1
         endif
                  
! Loop over all destination nodes and check if current ij destination column 
! is in it

         do idn = 1,nodes
            if (id >= nxbeg(idn,1) .and. id <= nxend(idn,1) .and. &
                jd >= nybeg(idn,1) .and. jd <= nyend(idn,1)) then
                
! Loop over up to 3 source ij columns

               do mijs = 1,nijst
                  is = is0t(mijs)
                  js = js0t(mijs)

! Loop over all source nodes and check if current ij source column is in it

                  do isn = 1,nodes
                     if (is >= ixb(isn,1) .and. is <= ixe(isn,1) .and.  &
                         js >= iyb(isn,1) .and. js <= iye(isn,1)) then
                
                        indt = indt + 1
                        ipathst_cyc(1,indt) = isn        ! source node #
                        ipathst_cyc(2,indt) = is         ! source node i
                        ipathst_cyc(3,indt) = js         ! source node j
                        ipathst_cyc(4,indt) = idn        ! destination node #
                        ipathst_cyc(5,indt) = id         ! destination node i
                        ipathst_cyc(6,indt) = jd         ! destination node j
                        ipathst_cyc(7,indt) = iselft     ! self count
                        ipathst_cyc(8,indt) = nijst      ! other point count

                     endif
                  enddo

               enddo

               do mijs = 1,nijsu
                  is = is0u(mijs)
                  js = js0u(mijs)

! Loop over all source nodes and check if current ij source column is in it

                  do isn = 1,nodes
                     if (is >= ixb(isn,1) .and. is <= ixe(isn,1) .and.  &
                         js >= iyb(isn,1) .and. js <= iye(isn,1)) then

                        indu = indu + 1
                        ipathsu_cyc(1,indu) = isn        ! source node #
                        ipathsu_cyc(2,indu) = is         ! source node i
                        ipathsu_cyc(3,indu) = js         ! source node j
                        ipathsu_cyc(4,indu) = idn        ! destination node #
                        ipathsu_cyc(5,indu) = id         ! destination node i
                        ipathsu_cyc(6,indu) = jd         ! destination node j
                        ipathsu_cyc(7,indu) = iselfu     ! self count
                        ipathsu_cyc(8,indu) = nijsu      ! other point count

                     endif
                  enddo

               enddo

               do mijs = 1,nijsv
                  is = is0v(mijs)
                  js = js0v(mijs)

! Loop over all source nodes and check if current ij source column is in it

                  do isn = 1,nodes
                     if (is >= ixb(isn,1) .and. is <= ixe(isn,1) .and.  &
                         js >= iyb(isn,1) .and. js <= iye(isn,1)) then

                        indv = indv + 1
                        ipathsv_cyc(1,indv) = isn        ! source node #
                        ipathsv_cyc(2,indv) = is         ! source node i
                        ipathsv_cyc(3,indv) = js         ! source node j
                        ipathsv_cyc(4,indv) = idn        ! destination node #
                        ipathsv_cyc(5,indv) = id         ! destination node i
                        ipathsv_cyc(6,indv) = jd         ! destination node j
                        ipathsv_cyc(7,indv) = iselfv     ! self count
                        ipathsv_cyc(8,indv) = nijsv      ! other point count

                     endif
                  enddo

               enddo

            endif
         enddo

      endif
   enddo
enddo

return
end

!*****************************************************************

subroutine fill_cyc(nijs,is0,js0,is,js)
implicit none
integer :: nijs,is,js
integer, dimension(*) :: is0,js0

nijs = nijs + 1
is0(nijs) = is
js0(nijs) = js

return
end

!     *****************************************************************

subroutine decomp_plot(ngr)

use mem_grid
use rpara

implicit none
integer :: ngr
character*2 cnum
integer :: nm,nb,nn,ixxb,ixxe,iyyb,iyye,ng
real :: pxb,pxe,pyb,pye,px,py

call set(.1, .9,.1,.9,  &
     xtn(1,ngr),xtn(nnxp(ngr),ngr)  &
     ,ytn(1,ngr),ytn(nnyp(ngr),ngr),1)
call perim(nnxp(ngr)-1,1,nnyp(ngr)-1,1)

do nm=1,nmachs

   nb=2
   if(ngr.eq.ngrids) nb=1

   do nn=1,nb
      if(nn.eq.1) ng=ngr
      if(nn.eq.2) ng=ngr+1

         ixxb=ixb(nm,ng)-1
         ixxe=ixe(nm,ng)
         iyyb=iyb(nm,ng)-1
         iyye=iye(nm,ng)
         pxb=xtn(ixxb,ng)
         pxe=xtn(ixxe,ng)
         pyb=ytn(iyyb,ng)
         pye=ytn(iyye,ng)

      call line(pxb,pyb,pxe,pyb)
      call line(pxe,pyb,pxe,pye)
      call line(pxe,pye,pxb,pye)
      call line(pxb,pye,pxb,pyb)

      px=.5*(pxb+pxe)
      py=.5*(pyb+pye)
      write(cnum,'(i2)') nm
      call pwritx(px,py,cnum,2,15,0,0.)
   enddo

enddo

call frame
return
end
