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

subroutine oda_sta_input (plat,plon,ngds)

use mem_oda
use gdf_input
use mem_grid

implicit none

real :: plat,plon
integer :: ngds

integer :: ierr,ns,nf,iexist,ngd,ifile,nloc,idsta,ntimes,ng,nlevs,k
real :: stx,sty,xy_ij
real(kind=8) :: secs_init,secs_obs

integer, parameter :: num_convars=5
character(len=8) :: varn(num_convars)
data varn/'ue','ve','tempc','dewptc','press_pa'/
real :: vars(num_convars),varp1(1000),varp2(1000)
  
! Read all observation data

! Transfer station id's to structure and assign integer id

do ns=1,num_oda_sfc
   oda_sfc_info(ns)%id=staid_sfc(ns)
   oda_sfc_info(ns)%intid=ns
   oda_sfc_info(ns)%ntimes=0
enddo

! Upper air
do ns=1,num_oda_upa
   oda_upa_info(ns)%id=staid_upa(ns)
   oda_upa_info(ns)%intid=ns
   oda_upa_info(ns)%ntimes=0
enddo

! Get abs seconds of run start

call date_abs_secs2(iyear1,imonth1,idate1,itime1*100,secs_init)

! Process surface files

do nf=1,nsfcfiles

   print*,'Opening sfc obs file: ',trim(fnames_sfc(nf))
   
   open(unit=31,file=fnames_sfc(nf),status='old')
      
   ifile=1
   header(ifile)%iun=31
   call gdf_read_sfc_ver (ifile)
   
   do while (.TRUE.)
      call gdf_read_sfc_obs (ifile,'no',ierr)
      if(ierr==1) exit
                 
         ! Find station integer id
      do ns=1,num_oda_sfc
      
         if(trim(rsfc_obs%id) == trim(staid_sfc(ns))) then

            idsta=ns
            oda_sfc_info(ns)%ntimes=oda_sfc_info(ns)%ntimes+1
            ntimes=oda_sfc_info(ns)%ntimes
            
            if(ntimes > maxtimes_sfc) then
               print*,'Surface obs exceeds memory allocation, stopping'
               print*,'idsta,ntimes:',ns,idsta,ntimes
               stop 'too many times for sfc obs'
            endif
            
            if(ntimes == 1) then
               ! Compute locations relative to grids
               call ll_xy (rsfc_obs%lat,rsfc_obs%lon,plat,plon,stx,sty)
               !print*,'---',rsfc_obs%lat,rsfc_obs%lon,plat,plon,stx,sty
               oda_sfc_info(ns)%xsta=stx
               oda_sfc_info(ns)%ysta=sty
               oda_sfc_info(ns)%xlat=rsfc_obs%lat
               oda_sfc_info(ns)%xlon=rsfc_obs%lon
               oda_sfc_info(ns)%stopo=rsfc_obs%elev
               call findgrid (stx,sty,ngd)
               
               ! Assume telescoping grids
               oda_sfc_info(ns)%iactive(1:ngds)=0
               oda_sfc_info(ns)%iactive(1:ngd)=1
               
               ! Find i,j on all active grids
               do ng=1,ngds
                  if (oda_sfc_info(ns)%iactive(ng) == 1) then
                     oda_sfc_info(ns)%xista(ng)=  &
                                      xy_ij (nnxp(ng),xtn(1,ng),stx)
                     oda_sfc_info(ns)%xjsta(ng)=  &
                                      xy_ij (nnyp(ng),ytn(1,ng),sty)
                  else
                     oda_sfc_info(ns)%xista(ng)= 0.
                     oda_sfc_info(ns)%xjsta(ng)= 0.
                  endif
               enddo
               !print*,'sta:',ns
               !print*,oda_sfc_info(ns)%xsta,oda_sfc_info(ns)%ysta
               !print*,oda_sfc_info(ns)%xista(1),oda_sfc_info(ns)%xjsta(1)
            endif
            
            ! Find time in seconds relative to run start         
            call date_abs_secs2(rsfc_obs%jyear,rsfc_obs%jmonth  &
                               ,rsfc_obs%jdate,rsfc_obs%jtime*100,secs_obs)
            oda_sfc_obs(ns)%time(ntimes)=secs_obs - secs_init

            ! Fill data arrays
            
            call gdf_sfc_data_convert(vars,varn,num_convars)
            
            oda_sfc_obs(ns)%temp(ntimes)=vars(3)
            oda_sfc_obs(ns)%dewpt(ntimes)=vars(4)
            oda_sfc_obs(ns)%us(ntimes)=vars(1)
            oda_sfc_obs(ns)%vs(ntimes)=vars(2)
            oda_sfc_obs(ns)%ps(ntimes)=vars(5)
            
            oda_sfc_obs(ns)%u(ntimes)=-999.
            oda_sfc_obs(ns)%v(ntimes)=-999.
            if(vars(1) > -998. .and. vars(2) > -998.) then
               call uevetouv(oda_sfc_obs(ns)%u(ntimes)  &
                         ,oda_sfc_obs(ns)%v(ntimes)  &
                         ,vars(1),vars(2)  &
                         ,rsfc_obs%lat,rsfc_obs%lon,plat,plon)
            endif
            !if(ns==461) print*,'obs_in:',ntimes,trim(rsfc_obs%id)  &
           !,oda_sfc_info(ns)%xista(1),oda_sfc_info(ns)%xjsta(1) &
           !,oda_sfc_obs(ns)%time(ntimes),oda_sfc_obs(ns)%us(ntimes)  &
           !,oda_sfc_obs(ns)%vs(ntimes),oda_sfc_obs(ns)%temp(ntimes) &
           !,oda_sfc_obs(ns)%u(ntimes),oda_sfc_obs(ns)%v(ntimes) &
           !,oda_sfc_obs(ns)%dewpt(ntimes),rsfc_obs%ff,rsfc_obs%dd
            exit
         endif  
     enddo   
   enddo
   
         
   close(31)

enddo
   
! Process upper air files

   print*,'Opening upa obs file: ',nupafiles
ufiles : do nf=1,nupafiles

   print*,'Opening upa obs file: ',fnames_upa(nf)(1:len_trim(fnames_upa(nf)))
   
   open(unit=31,file=fnames_upa(nf),status='old')
      
   ifile=1
   header(ifile)%iun=31
   call gdf_read_upa_ver (ifile)
   
   numobs: do while (.TRUE.)
      call gdf_read_upa_obs (ifile,'no',ierr)
      if(ierr==1) exit
                 
         ! Find station integer id
      find_sta: do ns=1,num_oda_upa
         if(trim(rupa_obs%id) == trim(staid_upa(ns))) then

            idsta=ns
            oda_upa_info(ns)%ntimes = oda_upa_info(ns)%ntimes + 1
            ntimes=oda_upa_info(ns)%ntimes
            
            if(ntimes > maxtimes_upa) then
               print*,'Upa obs exceeds memory allocation, stopping'
               print*,'idsta,ntimes:',ns,idsta,ntimes
               stop 'too many times for obs'
            endif
            
            if(ntimes == 1) then
               ! Compute locations relative to grids
               call ll_xy (rupa_obs%lat,rupa_obs%lon,plat,plon,stx,sty)
               oda_upa_info(ns)%xsta=stx
               oda_upa_info(ns)%ysta=sty
               oda_upa_info(ns)%xlat=rupa_obs%lat
               oda_upa_info(ns)%xlon=rupa_obs%lon
               oda_upa_info(ns)%stopo=rupa_obs%elev
               call findgrid (stx,sty,ngd)
               
               ! Assume telescoping grids
               oda_upa_info(ns)%iactive(1:ngds)=0
               oda_upa_info(ns)%iactive(1:ngd)=1
               
               
               ! Find i,j on all active grids
               do ng=1,ngds
                  if (oda_upa_info(ns)%iactive(ng) == 1) then
                     oda_upa_info(ns)%xista(ng)=  &
                                      xy_ij (nnxp(ng),xtn(1,ng),stx)
                     oda_upa_info(ns)%xjsta(ng)=  &
                                      xy_ij (nnyp(ng),ytn(1,ng),sty)
                  else
                     oda_upa_info(ns)%xista(ng)= 0.
                     oda_upa_info(ns)%xjsta(ng)= 0.
                  endif
               enddo
            endif
            
            ! Find time in seconds relative to run start         
            call date_abs_secs2(rupa_obs%jyear,rupa_obs%jmonth  &
                               ,rupa_obs%jdate,rupa_obs%jtime*100,secs_obs)
            oda_upa_obs(ns)%time(ntimes)=secs_obs - secs_init

            ! Fill data arrays
            
            call gdf_upa_get_profile(varp1,nlevs,'ue','z')
            call gdf_upa_get_profile(varp2,nlevs,'ve','z')
            if (nlevs > maxupalevs) then
               print*, 'ODA error: maxupalevs exceeded for winds:',nlevs
               stop 'ODA maxupalevs winds'
            endif
            do k=1,nlevs
               if(varp1(k) > -998. .and. varp2(k) > -998.) then
                  call uevetouv(oda_upa_obs(ns)%u(k,ntimes)  &
                               ,oda_upa_obs(ns)%v(k,ntimes)  &
                               ,varp1(k),varp2(k)  &
                               ,rupa_obs%lat,rupa_obs%lon,plat,plon)
               endif
            enddo

            call gdf_upa_get_profile(oda_upa_obs(ns)%zz(1,ntimes),nlevs,'zz','z')
            oda_upa_obs(ns)%us(1:nlevs,ntimes)=varp1(1:nlevs)
            oda_upa_obs(ns)%vs(1:nlevs,ntimes)=varp2(1:nlevs)
            oda_upa_obs(ns)%lz(ntimes)=nlevs
            
            ! For thermo variables, unlike surface obs, we usually have 
            !    pressure reported in standard raobs. 
            !    Convert temp,prs,rh => theta,pi,r
            !
            ! First get geopotential height, 
            call gdf_upa_get_profile(varp2,nlevs,'geo','p')
            if (nlevs > maxupalevs) then
               print*, 'ODA error: maxupalevs exceeded for prs:',nlevs
               stop 'ODA maxupalevs press'
            endif
            oda_upa_obs(ns)%lp(ntimes)=nlevs
            oda_upa_obs(ns)%zgeo(1:nlevs,ntimes)=varp2(1:nlevs)
            
            call gdf_upa_get_profile(varp1,nlevs,'theta','p')
            oda_upa_obs(ns)%theta(1:nlevs,ntimes)=varp1(1:nlevs)
            
            call gdf_upa_get_profile(varp1,nlevs,'pi','p')
            oda_upa_obs(ns)%pi(1:nlevs,ntimes)=varp1(1:nlevs)
            
            call gdf_upa_get_profile(varp1,nlevs,'mixrat','p')
            oda_upa_obs(ns)%rv(1:nlevs,ntimes)=varp1(1:nlevs)
            
            ! Recompute sounding heights hydrostatically
            call upa_hyd (nlevs  &
                           ,oda_upa_obs(ns)%zgeo(1:nlevs,ntimes)  &
                           ,oda_upa_obs(ns)%theta(1:nlevs,ntimes) &
                           ,oda_upa_obs(ns)%pi(1:nlevs,ntimes)    &
                           ,oda_upa_obs(ns)%rv(1:nlevs,ntimes), ierr )
                           
            if (ierr /= 0) then
               oda_upa_info(ns)%ntimes =  oda_upa_info(ns)%ntimes - 1
            endif
            
!print*,plat,plon,ns,ntimes
!do k=1,oda_upa_obs(ns)%lp(ntimes)
!   print '(i3,5f12.3)',k,oda_upa_obs(ns)%pi(k,ntimes)  &
!               ,oda_upa_obs(ns)%zgeo(k,ntimes) &
!               ,oda_upa_obs(ns)%theta(k,ntimes) &
!               ,oda_upa_obs(ns)%rv(k,ntimes)
!enddo
!do k=1,oda_upa_obs(ns)%lz(ntimes)
!   print '(i3,5f12.3)',k,oda_upa_obs(ns)%u(k,ntimes)  &
!               ,oda_upa_obs(ns)%v(k,ntimes) &
!               ,oda_upa_obs(ns)%us(k,ntimes) &
!               ,oda_upa_obs(ns)%vs(k,ntimes) &
!               ,oda_upa_obs(ns)%zz(k,ntimes)
!enddo

            exit
         endif  
      enddo find_sta
      
   enddo numobs
        
   close(31)


enddo ufiles

return
end

!-------------------------------------------------------------------

subroutine upa_hyd (lp, zp,tp,pp,rp,ierr)

use rconstants

implicit none

integer :: lp,ierr
real, dimension(lp) :: zp,tp,pp,rp

integer :: k,lbc
real :: bchyd,pio,zso,tho
real, dimension(lp) :: tpv

ierr = 0

! Need to find one good height for a boundary condition.
!      First look for a good level at or below 500 mb

bchyd=cp*(50000./p00)**rocp
do k=lp,1,-1
   if(pp(k) >= bchyd .and. tp(k) > -998. .and. zp(k) > -998.) then
      lbc=k
      go to 52
   endif
enddo

! Next start at 500 mb and go up...
do k=1,lp
   if(pp(k) < bchyd .and. tp(k) > -998. .and. zp(k) > -998.) then
      lbc=k
      go to 52
   endif
enddo


print*,' Could not find good rawind z  boundary level-'
ierr = 1
return

52      continue
! Found a boundary condition

! Compute virtual theta. If any moisture value is missing, just use dry theta.
do k = 1, lp
   tpv(k) = -999.
   if (tp(k) > 0.) then
      if (rp(k) > 0.) then   
         tpv(k) = tp(k) * (1.+ 0.61*rp(k))
      else
         tpv(k) = tp(k)
      endif
   endif
enddo

! Integrate from the bc level to model top. If theta or pi is missing, assign
!   missing height value.

pio=pp(lbc)
zso=zp(lbc)
tho=tpv(lbc)
do k=lbc+1,lp
   zp(k)=-999.
   if (pp(k) > -998. .and. pio > -998. .and. tpv(k) > .998 .and. tho > -998.) then
      zp(k) = zso + (tpv(k)+tho)*.5/g * (pio-pp(k))
      zso=zp(k) ; pio=pp(k) ; tho=tpv(k)
   elseif (zp(k) > -998.) then
      zso=zp(k) ; pio=pp(k) ; tho=tpv(k)
   endif
enddo

! Now go from bc level to ground

zso=zp(lbc)
pio=pp(lbc)
tho=tpv(lbc)
do k=lbc-1,1,-1
   zp(k)=-999.
   if (pp(k) > -998. .and. pio > -998. .and. tpv(k) > .998 .and. tho > -998.) then
      zp(k) = zso + (tpv(k)+tho)*.5/g * (pio-pp(k))
      zso=zp(k) ; pio=pp(k) ; tho=tpv(k)
   elseif (zp(k) > -998.) then
      zso=zp(k) ; pio=pp(k) ; tho=tpv(k)
   endif
enddo

! Check the computed heights for large gaps. If a gap is larger than
!    2000. m, return an error.

do k=1, lp
   if(zp(k) > -998.) then
      lbc=k
      go to 53
   endif
enddo

print*,' Could not find good rawind height-'
ierr = 1
return

53 continue
!print*,'lbc2:',lbc

zso = zp(lbc)
do k = lbc, lp
   if ( zp(k) > -998. .and. zso > -998. .and. (zp(k)-zso) > 2000. ) then
      print*,' Large gap in rawind heights-',(zp(k)-zso)
      ierr = 1
      return
   elseif (zp(k) > -998.) then
      zso = zp(k)
   endif
enddo

return
end


!-------------------------------------------------------------------

real function xy_ij (nih,xh,x)

! gets i (or j) grid point index from x (or y) value in m

implicit none

integer :: nih
real :: xh(nih),x

integer :: inti,m
real :: reali

if(x < xh(1) .or. x > xh(nih))then
   print*, 'x, y, or z value exceeds grid limits'
   stop 'xy_ij'
endif

!print*,x
!print*,xh

do m=2,nih
   if (x >= xh(m-1) .and. x < xh(m)) then
      inti=m-1
      reali=(x-xh(m-1))/(xh(m)-xh(m-1))
      xy_ij=float(inti)+reali
      !print*,'i,j:',inti,reali,xy_ij
      exit
   endif
enddo

return
end

