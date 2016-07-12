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

subroutine oda_sta_count (plat,plon,ngds)

use mem_oda
use gdf_input

implicit none

real :: plat,plon
integer :: ngds
 
integer :: ierr,ns,nf,iexist,ngd,ifile,nloc
real :: stx,sty
 
logical there


! get id's 

num_oda_sfc=0
ntimes_sfc(1:maxodasta)=0

do nf=1,nsfcfiles

   print*,'Opening sfc obs file: ',fnames_sfc(nf)(1:len_trim(fnames_sfc(nf)))
   
   open(unit=31,file=fnames_sfc(nf),status='old')
      
   ifile=1
   header(ifile)%iun=31
   call gdf_read_sfc_ver (ifile)

   do while (.TRUE.)

      call gdf_read_sfc_obs (ifile,'no',ierr)
      if(ierr==1) exit
      
         ! Check if station already exits
      iexist=0
      do ns=1,num_oda_sfc
         if(trim(rsfc_obs%id) == trim(staid_sfc(ns))) then
            iexist=1
            ntimes_sfc(ns)=ntimes_sfc(ns)+1
            exit
         endif
      enddo
      
      if(iexist==0) then
                  
         ! check if location within bounds
              !   print*,'here'
         call ll_xy (rsfc_obs%lat,rsfc_obs%lon,plat,plon,stx,sty)
               !  print*,'here',sfc_lat,sfc_lon,plat,plon,stx,sty
         call findgrid (stx,sty,ngd) 
              !   print*,'here',ngd,sfc_id(1:len_trim(sfc_id))

         ! only use if on grid 1

         if(ngd <= 0) cycle
         
         num_oda_sfc=num_oda_sfc+1

         if(num_oda_sfc > maxodasta) then
            print*,'num_oda_sfc exceeds maxodasta'
            print*,'Increase mem_oda variable maxodasta'
            stop 'maxodasta exceeded'
         endif
         
         staid_sfc(num_oda_sfc)=rsfc_obs%id
         ntimes_sfc(num_oda_sfc)=ntimes_sfc(num_oda_sfc)+1
         print*,'new station:',num_oda_sfc,rsfc_obs%id,ntimes_sfc(num_oda_sfc)

      endif
      
      
   enddo
   
   close(31)

enddo

maxtimes_sfc=1   
if(num_oda_sfc >= 1) maxtimes_sfc=maxval(ntimes_sfc(1:num_oda_sfc))

print*,'Number of sfc locations',num_oda_sfc,maxtimes_sfc

! Now do upper air data

num_oda_upa=0
ntimes_upa(1:maxodasta)=0

do nf=1,nupafiles

   print*,'Opening upa obs file: ',trim(fnames_upa(nf))
   
   open(unit=31,file=fnames_upa(nf),status='old')
      
   ifile=1
   header(ifile)%iun=31
   call gdf_read_upa_ver (ifile)

   do while (.TRUE.)
   
      call gdf_read_upa_obs(ifile,'no',ierr)
      if(ierr == 1) exit
                 
         ! Check if station already exits
      iexist=0
      do ns=1,num_oda_upa
         if(trim(rupa_obs%id) == trim(staid_upa(ns))) then
            iexist=1
            ntimes_upa(ns)=ntimes_upa(ns)+1
            exit
         endif
      enddo
      
      if(iexist == 0) then
                  
         ! check if location within bounds
         call ll_xy (rupa_obs%lat,rupa_obs%lon,plat,plon,stx,sty)
         call findgrid (stx,sty,ngd)

         ! only use if on grid 1

         if(ngd <= 0) cycle
         
         num_oda_upa=num_oda_upa+1
         if(num_oda_upa > maxodasta) then
            print*,'num_oda_sfc exceeds maxodasta'
            print*,'Increase mem_oda variable maxodasta'
            stop 'maxodasta exceeded'
         endif
         
         staid_upa(num_oda_upa)=rupa_obs%id
         ntimes_upa(ns)=ntimes_upa(ns)+1
         print*,'new upa station:',num_oda_upa,rupa_obs%id,ngd,stx,sty


      endif
         
   enddo
   
   close(31) 
   
enddo

maxtimes_upa=1   
if(num_oda_upa >= 1) maxtimes_upa=maxval(ntimes_upa(1:num_oda_upa))

print*,'Number of upa locations',num_oda_upa,maxtimes_upa
   
return
end

!***************************************************************************

subroutine findgrid (xfx,yfy,ngr)

use mem_grid

! not set up for Z locations

implicit none

real :: xfx,yfy
integer :: ngr

integer :: ng
                   
do ng=ngrids,1,-1
   if(xfx <= xmn(nnxp(ng)-1,ng).and.xfx >= xmn(1,ng) .and.  &
      yfy <= ymn(nnyp(ng)-1,ng).and.yfy >= ymn(1,ng) ) then
      ngr=ng
      return
   endif
enddo
ngr=-1

return
end
