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

subroutine gdf_read_sfc_ver (ifile)

! Reads Ralph sfc file version and header

use gdf_input

implicit none

integer :: ifile

integer :: imarker,nvar,nh,ns

header(ifile)%head_string(1:max_head_vars)=''
header(ifile)%sfc_string(1:max_sfc_vars)=''

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
endif
read(header(ifile)%iun,*) header(ifile)%nvsfc
do ns=1,header(ifile)%nvsfc
   read(header(ifile)%iun,*) header(ifile)%sfc_string(ns)
enddo
if(header(ifile)%iver.eq.1) read(header(ifile)%iun,*)

return
end

!***************************************************************************

subroutine gdf_read_sfc_obs(ifile,qcheck,ierr)

! Reads one surface obs from gdf sfc file

use gdf_input

implicit none

integer :: ifile,ierr
character(len=*) :: qcheck

integer :: ntok,iqfl(5)
character(len=80) :: var_string
character(len=16) :: cflags,tokens(100)
character(len=256) :: line
integer :: jd,nvar,nv,ic,k,iq,iqf,nh,nt

ierr=0
nvar=5
tokens(1:100)=''

read(header(ifile)%iun,'(a)',end=20,err=20) line
if(len_trim(line)==0) goto 10

! Check to see if this is a header line. If so, (re)read header.

!!!!!! Not working for v3 yet.

if(line(1:6) == '999999') then
   read(line(7:),*) header(ifile)%iver
   read(header(ifile)%iun,*) nvar
   do nv=1,nvar
      read(header(ifile)%iun,*) var_string
   enddo
   read(header(ifile)%iun,'(a)',end=20,err=20) line
endif

call parse(line,tokens,ntok)
   
if(header(ifile)%iver==1) then

   read(tokens(1),*,err=1,end=1) jd
   read(tokens(2),*,err=1,end=1) rsfc_obs%jtime
   read(tokens(3),'(a)',err=1,end=1) rsfc_obs%id
   read(tokens(4),*,err=1,end=1) rsfc_obs%lat
   read(tokens(5),*,err=1,end=1) rsfc_obs%lon
   read(tokens(6),*,err=1,end=1) rsfc_obs%elev
   rsfc_obs%hgt=0.
   rsfc_obs%ihgtflg=1
   read(tokens(7),*,err=1,end=1) rsfc_obs%ff
   read(tokens(8),*,err=1,end=1) rsfc_obs%dd
   read(tokens(9),*,err=1,end=1) rsfc_obs%t
   read(tokens(10),*,err=1,end=1) rsfc_obs%td
   read(tokens(11),*,err=1,end=1) rsfc_obs%p
   read(tokens(12),'(a)',err=1,end=1) cflags
   1 continue

   ic=1
   do nv=1,nvar
      read(cflags(ic:),'(1x,3i1)') (rsfc_obs%iqflags(nv,k),k=1,3)
      ic=ic+4
   enddo
   
   rsfc_obs%jyear=jd/10000
   rsfc_obs%jmonth=mod(jd,10000)/100
   rsfc_obs%jdate=mod(jd,100)

elseif(header(ifile)%iver==2) then

   read(tokens(1),*,err=2,end=2) rsfc_obs%jyear
   read(tokens(2),*,err=2,end=2) rsfc_obs%jmonth
   read(tokens(3),*,err=2,end=2) rsfc_obs%jdate
   read(tokens(4),*,err=2,end=2) rsfc_obs%jtime
   read(tokens(5),'(a)',err=2,end=2) rsfc_obs%id
   read(tokens(6),*,err=2,end=2) rsfc_obs%lat
   read(tokens(7),*,err=2,end=2) rsfc_obs%lon
   read(tokens(8),*,err=2,end=2) rsfc_obs%elev
   rsfc_obs%hgt=0.
   rsfc_obs%ihgtflg=1
   read(tokens(9),*,err=2,end=2) rsfc_obs%ff
   read(tokens(10),*,err=2,end=2) iqfl(1)
   read(tokens(11),*,err=2,end=2) rsfc_obs%dd
   read(tokens(12),*,err=2,end=2) iqfl(2)
   read(tokens(13),*,err=2,end=2) rsfc_obs%t
   read(tokens(14),*,err=2,end=2) iqfl(3)
   read(tokens(15),*,err=2,end=2) rsfc_obs%td
   read(tokens(16),*,err=2,end=2) iqfl(4)
   read(tokens(17),*,err=2,end=2) rsfc_obs%p
   read(tokens(18),*,err=2,end=2) iqfl(5)
   2 continue
   
   do nv=1,nvar
      rsfc_obs%iqflags(nv,1)=iqfl(nv)/100
      rsfc_obs%iqflags(nv,2)=mod(iqfl(nv)/10,10)
      rsfc_obs%iqflags(nv,3)=mod(iqfl(nv),10)
   enddo

elseif(header(ifile)%iver==3) then

   rsfc_obs%hgt=0.
   rsfc_obs%ihgtflg=1
   nt=1
   do nh=1,header(ifile)%nhead
      if(header(ifile)%head_string(nh)=='YEAR') then
         read(tokens(nt),*,err=3) rsfc_obs%jyear
         nt=nt+1
      elseif(header(ifile)%head_string(nh)=='MONTH') then
         read(tokens(nt),*,err=3) rsfc_obs%jmonth
         nt=nt+1
      elseif(header(ifile)%head_string(nh)=='DAY') then
         read(tokens(nt),*,err=3) rsfc_obs%jdate
         nt=nt+1
      elseif(header(ifile)%head_string(nh)=='HOUR') then
         read(tokens(nt),*,err=3) rsfc_obs%jtime
         nt=nt+1
      elseif(header(ifile)%head_string(nh)=='STATION_ID') then
         read(tokens(nt),'(a)',err=3) rsfc_obs%id
         nt=nt+1
      elseif(header(ifile)%head_string(nh)=='LATITUDE') then
         read(tokens(nt),*,err=3) rsfc_obs%lat
         nt=nt+1
      elseif(header(ifile)%head_string(nh)=='LONGITUDE') then
         read(tokens(nt),*,err=3) rsfc_obs%lon
         nt=nt+1
      elseif(header(ifile)%head_string(nh)=='ELEVATION') then
         read(tokens(nt),*,err=3) rsfc_obs%elev
         nt=nt+1
      elseif(header(ifile)%head_string(nh)=='HEIGHT') then
         read(tokens(nt),*,err=3) rsfc_obs%hgt
         nt=nt+1
      elseif(header(ifile)%head_string(nh)=='HEIGHT_FLAG') then
         read(tokens(nt),*,err=3) rsfc_obs%ihgtflg
         nt=nt+1
      else
         print*,'unknown header record'
      endif
   enddo
   
   read(tokens(nt),*,err=3,end=3) rsfc_obs%ff
   read(tokens(nt+1),*,err=3,end=3) iqfl(1)
   read(tokens(nt+2),*,err=3,end=3) rsfc_obs%dd
   read(tokens(nt+3),*,err=3,end=3) iqfl(2)
   read(tokens(nt+4),*,err=3,end=3) rsfc_obs%t
   read(tokens(nt+5),*,err=3,end=3) iqfl(3)
   read(tokens(nt+6),*,err=3,end=3) rsfc_obs%td
   read(tokens(nt+7),*,err=3,end=3) iqfl(4)
   read(tokens(nt+8),*,err=3,end=3) rsfc_obs%p
   read(tokens(nt+9),*,err=3,end=3) iqfl(5)
   3 continue
   
   do nv=1,nvar
      rsfc_obs%iqflags(nv,1)=iqfl(nv)/100
      rsfc_obs%iqflags(nv,2)=mod(iqfl(nv)/10,10)
      rsfc_obs%iqflags(nv,3)=mod(iqfl(nv),10)
   enddo

endif

! If desired, check QC flags and set appropriate values to missing.

if(qcheck=='yes') then
   iqf=1
   do iq=1,3
      if((rsfc_obs%iqflags(1,iq).ne.5 .and.  &
          rsfc_obs%iqflags(1,iq).ne.0 .and.  &
          rsfc_obs%iqflags(1,iq).ne.8) .or. &
          rsfc_obs%ff.lt.-998.) iqf=0
   enddo
   if(iqf.eq.0) rsfc_obs%ff=-999.

   iqf=1
   do iq=1,3
      if((rsfc_obs%iqflags(2,iq).ne.5 .and.  &
          rsfc_obs%iqflags(2,iq).ne.0 .and.  &
          rsfc_obs%iqflags(2,iq).ne.8) .or.  &
          rsfc_obs%dd.lt.-998.) iqf=0
   enddo
   if(iqf.eq.0) rsfc_obs%dd=-999.

   iqf=1
   do iq=1,3
      if((rsfc_obs%iqflags(3,iq).ne.5 .and.  &
          rsfc_obs%iqflags(3,iq).ne.0 .and.  &
          rsfc_obs%iqflags(3,iq).ne.8) .or.  &
          rsfc_obs%t.lt.-998.) iqf=0
   enddo
   if(iqf.eq.0) rsfc_obs%t=-999.

   iqf=1
   do iq=1,3
      if((rsfc_obs%iqflags(4,iq).ne.5 .and.  &
          rsfc_obs%iqflags(4,iq).ne.0 .and.  &
          rsfc_obs%iqflags(4,iq).ne.8) .or.  &
          rsfc_obs%td.lt.-998.) iqf=0
   enddo
   if(iqf.eq.0) rsfc_obs%td=-999.

   iqf=1
   do iq=1,3
      if((rsfc_obs%iqflags(5,iq).ne.5 .and.  &
          rsfc_obs%iqflags(5,iq).ne.0 .and.  &
          rsfc_obs%iqflags(5,iq).ne.8) .or.  &
          rsfc_obs%p.lt.-998.) iqf=0
   enddo
   if(iqf.eq.0) rsfc_obs%p=-999.

endif

10 continue

return

20 continue
ierr=1

return
end


!***************************************************************************

subroutine gdf_sfc_data_convert (varn,cvars,nvars)

use gdf_input

implicit none

integer :: nvars
real :: varn(nvars)
character(len=*) :: cvars(nvars)

character(len=16) :: cvar
real :: vv
integer :: ll,nv
real,external :: rs

! Convert units and type of sfc input data

do nv=1,nvars
   cvar=cvars(nv)
   ll=len_trim(cvar)
   varn(nv)=-999.
   if(cvar(1:ll)=='ue') then
      ! earth-relative u in m/s
      if(rsfc_obs%dd>-998..and.rsfc_obs%ff>-998.)  &
         call winduv(rsfc_obs%dd,rsfc_obs%ff,varn(nv),vv)
   elseif(cvar(1:ll)=='ve') then  
      ! earth-relative u in m/s
      if(rsfc_obs%dd>-998..and.rsfc_obs%ff>-998.)  &
         call winduv(rsfc_obs%dd,rsfc_obs%ff,vv,varn(nv))
   elseif(cvar(1:ll)=='speed') then  
      ! wind speed in m/s
      if(rsfc_obs%ff>-998.) varn(nv)=rsfc_obs%ff
   elseif(cvar(1:ll)=='direction') then  
      ! wind direction
      if(rsfc_obs%dd>-998.) varn(nv)=rsfc_obs%dd
   elseif(cvar(1:ll)=='tempc') then  
      ! temperature in C
      if(rsfc_obs%t>-998.) varn(nv)=rsfc_obs%t
   elseif(cvar(1:ll)=='tempf') then  
      ! temperature in F
      if(rsfc_obs%t>-998.)  varn(nv)=rsfc_obs%t*1.8+32.
   elseif(cvar(1:ll)=='dewptc') then  
      ! dewpoint in C
      if(rsfc_obs%t>-998.) varn(nv)=rsfc_obs%td
   elseif(cvar(1:ll)=='dewptf') then  
      ! dewpoint in F
      if(rsfc_obs%t>-998.) varn(nv)=rsfc_obs%td*1.8+32.
   elseif(cvar(1:ll)=='press') then  
      ! pressure in mb
      if(rsfc_obs%p>-998.) varn(nv)=rsfc_obs%p*.01
   elseif(cvar(1:ll)=='relhum') then  
      ! rh in percent
      if(rsfc_obs%t>-998. .and. rsfc_obs%td>-998. .and.  &
         rsfc_obs%p>-998.) &
      varn(nv)=100.*min(1.,max(0.  &
               ,rs(rsfc_obs%p,rsfc_obs%td+273.16)  &
               /rs(rsfc_obs%p,rsfc_obs%t+273.16)))
   else
      print*,'UNKNOWN CONVERT VARIABLE in sfc_data_convert !!!!',cvar
      stop 'sfc_data_convert'
   endif
enddo


return
end
