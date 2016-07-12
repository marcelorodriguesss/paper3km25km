!
! Copyright (C) 1991-2004  ; All Rights Reserved ; ATMET, LLC
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
! 2.2.0
!###########################################################################

subroutine gdf_read_upa_ver (ifile)

! Reads Ralph sfc file version and header

use gdf_input

implicit none

integer :: ifile

integer :: imarker,nvar,nh,nt,nw

header(ifile)%head_string(1:max_head_vars)=''
header(ifile)%therm_string(1:max_upa_vars)=''
header(ifile)%wind_string(1:max_upa_vars)=''

read(header(ifile)%iun,*) imarker
rewind header(ifile)%iun

if(imarker.eq.999999) then
   read(header(ifile)%iun,*) imarker,header(ifile)%iver
else
   header(ifile)%iver=1
endif

if(header(ifile)%iver==3) then

   read(header(ifile)%iun,*) header(ifile)%nhead
   do nh=1,header(ifile)%nhead
      read(header(ifile)%iun,*) header(ifile)%head_string(nh)
   enddo
   read(header(ifile)%iun,*) header(ifile)%nvtherm
   do nt=1,header(ifile)%nvtherm
      read(header(ifile)%iun,*) header(ifile)%therm_string(nt)
   enddo
   read(header(ifile)%iun,*) header(ifile)%nvwind
   do nw=1,header(ifile)%nvwind
      read(header(ifile)%iun,*) header(ifile)%wind_string(nw)
   enddo
endif

return
end

!***************************************************************************

subroutine gdf_read_upa_obs (ifile,qcheck,ierr)

! Reads one upper air obs from Ralph upper air file
!   need to detect if there is an additional header

use gdf_input

implicit none

integer :: ifile,ierr
character(len=*) :: qcheck

integer :: ntok,iqfl(5)
character(len=80) :: var_string
character(len=16) :: cflags,tokens(100)
character(len=256) :: line
integer :: jd,n,i,j,iq,iqf,kl

ierr=0

read(header(ifile)%iun,'(a)',end=20,err=20) line
if(len_trim(line)==0) goto 10
   
! Check to see if this is a header line. If so, read header and following line.
!  Note that Ralph I files do not have headers so this will never happen.
if(line(1:6) == '999999') then
   read(line(7:),*) header(ifile)%iver
   read(header(ifile)%iun,'(a)',end=20,err=20) line
endif

call parse(line,tokens,ntok)

if(header(ifile)%iver==1) then

   call parse(line,tokens,ntok)
   read(tokens(1),*) rupa_obs%jdate
   read(tokens(2),*) rupa_obs%jtime
   read(tokens(3),'(a)') rupa_obs%id
   read(tokens(4),*) rupa_obs%lp
   read(tokens(5),*) rupa_obs%lz
   read(tokens(6),*) rupa_obs%lat
   read(tokens(7),*) rupa_obs%lon
   read(tokens(8),*) rupa_obs%elev

   if(rupa_obs%lp.gt.max_up_levs.or. rupa_obs%lz.gt.max_up_levs) then
      print*,'Rawindsonde read error!'
      print*,'  Number of input levels greater than max_up_levs'
      print*,'  upa_lp,upa_lz,max_up_levs:',rupa_obs%lp  &
               ,rupa_obs%lz, max_up_levs
      stop 'rawin-max_up_levs'
   endif

   do n=1,rupa_obs%lp
      read(header(ifile)%iun,80,end=20,err=20)rupa_obs%p(n),rupa_obs%z(n)  &
                   ,rupa_obs%t(n),rupa_obs%r(n)  &
                 ,((rupa_obs%iqflagsp(n,i,j),j=1,3),i=1,4)
   enddo
   80 format(2f12.3,f10.2,f10.4,2x,4(':',3i1))

   do n=1,rupa_obs%lz
      read(header(ifile)%iun,85,end=20,err=20)rupa_obs%zz(n),rupa_obs%fz(n)  &
                   ,rupa_obs%dz(n)  &
                 ,((rupa_obs%iqflagsz(n,i,j),j=1,3),i=1,3)
   enddo
   85 format(f12.3,2f10.2,2x,3(':',3i1))

elseif(header(ifile)%iver==2.or.header(ifile)%iver==3) then

   read(tokens(1),*) rupa_obs%jyear
   read(tokens(2),*) rupa_obs%jmonth
   read(tokens(3),*) rupa_obs%jdate
   read(tokens(4),*) rupa_obs%jtime
   read(tokens(5),'(a)') rupa_obs%id
   read(tokens(6),*) rupa_obs%lp
   read(tokens(7),*) rupa_obs%lz
   read(tokens(8),*) rupa_obs%lat
   read(tokens(9),*) rupa_obs%lon
   read(tokens(10),*) rupa_obs%elev

   IF(rupa_obs%lp > max_up_levs.or.  &
      rupa_obs%lz > max_up_levs) then
      print*,'Rawindsonde read error!'
      print*,'  Number of input levels greater than max_up_levs'
      print*,'  upa_lp,upa_lz,max_up_levs:',rupa_obs%lp,rupa_obs%lz,max_up_levs
      stop 'rawin-max_up_levs'
   endif

!   rupa_obs%jdate=rupa_obs%jyear*10000+rupa_obs%jmonth*100+rupa_obs%jdate

   do n=1,rupa_obs%lp
      read(header(ifile)%iun,*,end=20,err=20)   &
                 rupa_obs%p(n),iqfl(1),rupa_obs%z(n),iqfl(2)  &
                ,rupa_obs%t(n),iqfl(3),rupa_obs%r(n),iqfl(4)
      do i=1,4
         rupa_obs%iqflagsp(n,i,1)=iqfl(i)/100
         rupa_obs%iqflagsp(n,i,2)=mod(iqfl(i)/10,10)
         rupa_obs%iqflagsp(n,i,3)=mod(iqfl(i),10)
      enddo
   enddo

   do n=1,rupa_obs%lz
       read(header(ifile)%iun,*,end=20,err=20)  &
                 rupa_obs%zz(n),iqfl(1),rupa_obs%fz(n),iqfl(2)  &
                ,rupa_obs%dz(n),iqfl(3)
       do i=1,3
          rupa_obs%iqflagsz(n,i,1)=iqfl(i)/100
          rupa_obs%iqflagsz(n,i,2)=mod(iqfl(i)/10,10)
          rupa_obs%iqflagsz(n,i,3)=mod(iqfl(i),10)
       enddo
   enddo

endif

! If desired, check QC flags and set appropriate values to missing.

if(qcheck=='yes') then

   do kl=1,rupa_obs%lp
      iqf=1
      do iq=1,3
         if((rupa_obs%iqflagsp(kl,1,iq).ne.5 .and.  &
             rupa_obs%iqflagsp(kl,1,iq).ne.0 .and.  &
             rupa_obs%iqflagsp(kl,1,iq).ne.8) .or.  &
             rupa_obs%p(kl).lt.-998.) iqf=0
      enddo
      if(iqf.eq.0) rupa_obs%p(kl)=-999.
      
      iqf=1
      do iq=1,3
         if((rupa_obs%iqflagsp(kl,2,iq).ne.5 .and.  &
             rupa_obs%iqflagsp(kl,2,iq).ne.0 .and.  &
             rupa_obs%iqflagsp(kl,2,iq).ne.8) .or.  &
             rupa_obs%z(kl).lt.-998.) iqf=0
      enddo
      if(iqf.eq.0) rupa_obs%z(kl)=-999.

      iqf=1
      do iq=1,3
         if((rupa_obs%iqflagsp(kl,3,iq).ne.5 .and.  &
             rupa_obs%iqflagsp(kl,3,iq).ne.0 .and.  &
             rupa_obs%iqflagsp(kl,3,iq).ne.8) .or.  &
             rupa_obs%t(kl).lt.-998.) iqf=0
      enddo
      if(iqf.eq.0) rupa_obs%t(kl)=-999.

      iqf=1
      do iq=1,3
         if((rupa_obs%iqflagsp(kl,4,iq).ne.5 .and.  &
             rupa_obs%iqflagsp(kl,4,iq).ne.0 .and.  &
             rupa_obs%iqflagsp(kl,4,iq).ne.8) .or.  &
             rupa_obs%r(kl).lt.-998.) iqf=0
      enddo
      if(iqf.eq.0) rupa_obs%r(kl)=-999.

   enddo

   do kl=1,rupa_obs%lz
      iqf=1
      do iq=1,3
         if((rupa_obs%iqflagsz(kl,1,iq).ne.5 .and.  &
             rupa_obs%iqflagsz(kl,1,iq).ne.0 .and.  &
             rupa_obs%iqflagsz(kl,1,iq).ne.8) .or.  &
             rupa_obs%zz(kl).lt.-998.) iqf=0
      enddo
      if(iqf.eq.0) rupa_obs%zz(kl)=-999.

      iqf=1
      do iq=1,3
         if((rupa_obs%iqflagsz(kl,2,iq).ne.5 .and.  &
             rupa_obs%iqflagsz(kl,2,iq).ne.0 .and.  &
             rupa_obs%iqflagsz(kl,2,iq).ne.8) .or.  &
             rupa_obs%fz(kl).lt.-998.) iqf=0
      enddo
      if(iqf.eq.0) rupa_obs%fz(kl)=-999.

      iqf=1
      do iq=1,3
         if((rupa_obs%iqflagsz(kl,3,iq).ne.5 .and.  &
             rupa_obs%iqflagsz(kl,3,iq).ne.0 .and.  &
             rupa_obs%iqflagsz(kl,3,iq).ne.8) .or.  &
             rupa_obs%dz(kl).lt.-998.) iqf=0
      enddo
      if(iqf.eq.0) rupa_obs%dz(kl)=-999.
      
   enddo
   
endif

10 continue

return

20 continue
ierr=1

return
end


!***************************************************************************

subroutine gdf_upa_get_profile (varn,nlevels,cvar,ctype)

use gdf_input

implicit none

integer :: nlevels
real :: varn(*)
character(len=*) :: cvar,ctype

real :: vv
integer :: ll,nv,k,nlev
real,external :: rs,td
real, parameter ::                    &
        rgas     = 287.               &
    ,   cp       = 1004.              &
    ,   p00      = 1.e5               &
    ,   rocp     = rgas / cp          &
    ,   p00i     = 1. / p00           

! Convert units and type of sfc input data

   if(ctype(1:1)=='z') nlevels=rupa_obs%lz
   if(ctype(1:1)=='p') nlevels=rupa_obs%lp
   ll=len_trim(cvar)
   do k=1,nlevels
      varn(k)=-999.

      if(cvar(1:ll)=='ue') then
         ! earth-relative u in m/s
         if(rupa_obs%dz(k) > -998..and.rupa_obs%fz(k)>-998.)  &
            call winduv(rupa_obs%dz(k),rupa_obs%fz(k),varn(k),vv)
      elseif(cvar(1:ll)=='ve') then  
         ! earth-relative v in m/s
         if(rupa_obs%dz(k) > -998..and.rupa_obs%fz(k)>-998.)  &
            call winduv(rupa_obs%dz(k),rupa_obs%fz(k),vv,varn(k))
      elseif(cvar(1:ll)=='zz') then  
         ! height of wind levels in m
         if(rupa_obs%zz(k) > -998.) varn(k)=rupa_obs%zz(k)
      elseif(cvar(1:ll)=='speed') then  
         ! wind speed in m/s
         if(rupa_obs%fz(k) > -998.) varn(k)=rupa_obs%fz(k)
      elseif(cvar(1:ll)=='direction') then  
         ! wind direction
         if(rupa_obs%dz(k) > -998.) varn(k)=rupa_obs%dz(k)
      elseif(cvar(1:ll)=='tempc') then  
         ! temperature in C
         if(rupa_obs%t(k)>-998.) varn(k)=rupa_obs%t(k)
      elseif(cvar(1:ll)=='tempk') then  
         ! temperature in K
         if(rupa_obs%t(k)>-998.) varn(k)=rupa_obs%t(k)+273.16
      elseif(cvar(1:ll)=='tempf') then  
         ! temperature in F
         if(rupa_obs%t(k)>-998.) varn(k)=rupa_obs%t(k)*1.8+32.
      elseif(cvar(1:ll)=='theta') then  
         ! theta in K
         if(rupa_obs%p(k)>-998..and.rupa_obs%t(k)>-998.) &
            varn(k)=(rupa_obs%t(k)+273.16)*(p00/rupa_obs%p(k))**rocp
      elseif(cvar(1:ll)=='p_pas') then  
         ! pressure in pascals
         if(rupa_obs%p(k)>-998.) varn(k)=rupa_obs%p(k)
      elseif(cvar(1:ll)=='pi') then  
         ! Exner function
         if(rupa_obs%p(k)>-998.) varn(k)=cp*(rupa_obs%p(k)*p00i)**rocp
      elseif(cvar(1:ll)=='dewptc') then  
         ! dewpoint in C
         if(rupa_obs%r(k)>-998..and.rupa_obs%t(k)>-998..and.  &
            rupa_obs%p(k)>-998.) &
            vv=min(1.,max(0.,rupa_obs%r(k)))  &
                 *rs(rupa_obs%p(k),rupa_obs%t(k)+273.16)
            varn(k)=td(rupa_obs%p(k),vv )-273.16
      elseif(cvar(1:ll)=='dewptf') then  
         ! dewpoint in F
         if(rupa_obs%r(k)>-998..and.rupa_obs%t(k)>-998..and.  &
                rupa_obs%p(k)>-998.) &
            vv=min(1.,max(0.,rupa_obs%r(k)))  &
                 *rs(rupa_obs%p(k),rupa_obs%t(k)+273.16)
            varn(k)=(td(rupa_obs%p(k),vv )-273.16)*1.8+32.
      elseif(cvar(1:ll)=='geo') then  
         ! geopotential in m
         if(rupa_obs%z(k)>-998.) varn(k)=rupa_obs%z(k)
      elseif(cvar(1:ll)=='mixrat') then  
         ! vapor in kg/kg
         if(rupa_obs%r(k)>-998..and.rupa_obs%t(k)>-998..and.  &
            rupa_obs%p(k)>-998.) &
            varn(k)=min(1.,max(0.,rupa_obs%r(k)))  &
                 *rs(rupa_obs%p(k),rupa_obs%t(k)+273.16)
       elseif(cvar(1:ll)=='relhum') then  
         ! rh in percent
         if(rupa_obs%r(k)>-998.) varn(k)=100.  &
             *min(1.,max(0.,rupa_obs%r(k)))
      elseif(cvar(1:ll)=='relfrac') then  
         ! rh in fraction
         if(rupa_obs%r(k)>-998.) varn(k)=  &
                                 min(1.,max(0.,rupa_obs%r(k)))
      else
         print*,'UNKNOWN CONVERT VARIABLE in upa_get_profile !!!!',cvar
         stop 'upa_get_profile'
      endif
   enddo

return
end


subroutine sound_interp(dat,zdat,nlev,zloc,tol,ans)

implicit none

integer :: nlev
real :: dat(nlev),zdat(nlev),zloc,tol,ans

real :: d(nlev),z(nlev) ! auto arrays
integer :: n,ngood,nn
real :: zans

ans=-999.
      print*,'nnn:',zloc

! First, filter any missing data or height levels
ngood=0
do nn=1,nlev
   if (zdat(nn) > -998. .and. dat(nn) > -998.) then
      ngood=ngood+1
      z(ngood)=zdat(nn)
      d(ngood)=dat(nn)
      print*,'nnn:',nn,z(ngood),d(ngood)
   endif
enddo

if(ngood == 0) return

if(zloc <= z(1)) then
   ans=d(1)
   zans=z(1)
   go to 100
elseif(zloc >= z(ngood)) then
   ans=d(ngood)
   zans=z(ngood)
   go to 100
endif

do n=1,ngood
   if (zloc > zdat(n) .and. zloc <= zdat(n+1) ) then
      ans = dat(n) + (dat(n+1)-dat(n))  &
                    * (zloc-zdat(n))/(zdat(n+1)-zdat(n))
      zans=zloc
      exit
   endif
enddo

100 continue

if(abs(zloc-zans) > tol) ans=-999.
      print*,'nnn:',zloc,ans

return
end
