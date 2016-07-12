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

subroutine ralph_vars (ivok,nvar,cvar,nvr,namev,unitv,cfmt)

implicit none

integer :: ivok,nvar,nvr
character(len=*) :: cvar,namev,unitv,cfmt

if(cvar.eq.'ue') then
   nvr=nvar
   namev='U_WIND_COMPONET'
   unitv='m/s'
   cfmt='2x,f9.2,2x,i4.3'
   
elseif(cvar.eq.'ve') then
   nvr=nvar
   namev='V_WIND_COMPONET'
   unitv='m/s'
   cfmt='2x,f9.2,2x,i4.3'
   
elseif(cvar.eq.'speed') then
   nvr=nvar
   namev='WINDSPEED'
   unitv='m/s'
   cfmt='2x,f9.2,2x,i4.3'
   
elseif(cvar.eq.'direction') then
   nvr=nvar
   namev='WIND_DIRECTION'
   unitv='deg'
   cfmt='2x,f7.0,2x,i4.3'
   
elseif(cvar.eq.'tempc') then
   nvr=nvar
   namev='TEMPERATURE'
   unitv='C'
   cfmt='2x,f7.1,2x,i4.3'
   
elseif(cvar.eq.'tempf') then
   nvr=nvar
   namev='TEMPERATURE'
   unitv='F'
   cfmt='2x,f7.1,2x,i4.3'
   
elseif(cvar.eq.'dewptc') then
   nvr=nvar
   namev='DEWPOINT'
   unitv='C'
   cfmt='2x,f7.1,2x,i4.3'
   
elseif(cvar.eq.'dewptf') then
   nvr=nvar
   namev='DEWPOINT'
   unitv='F'
   cfmt='2x,f7.1,2x,i4.3'
   
elseif(cvar.eq.'dewptk') then
   nvr=nvar
   namev='DEWPOINT'
   unitv='K'
   cfmt='2x,f7.1,2x,i4.3'
   
elseif(cvar.eq.'relhum') then
   nvr=nvar
   namev='RELATIVE HUMIDITY'
   unitv='pct'
   cfmt='2x,f7.0,2x,i4.3'
   
elseif(cvar.eq.'press') then
   nvr=nvar
   namev='STN_PRES'
   unitv='Pa'
   cfmt='2x,f10.1,2x,i4.3'
   
elseif(cvar.eq.'sea_press') then
   nvr=nvar
   namev='SLP'
   unitv='Pa'
   cfmt='2x,f10.1,2x,i4.3'
   
!elseif(cvar.eq.'24hr_precip') then
!   nvr=nvar
!   namev='6-HR_PCP'
!   unitv='mm'
!   cfmt='2x,f7.1,2x,i4.3'

!elseif(cvar.eq.'6hr_precip') then
!   nvr=nvar
!   namev='24-HR_PCP'
!   unitv='mm'
!   cfmt='2x,f7.1,2x,i4.3'

!elseif(cvar.eq.'snow_depth_ps') then
!   nvr=nvar
!   namev='SNOW_DEPTH'
!   unitv='m'
!   cfmt='2x,f7.1,2x,i4.3'

elseif(cvar.eq.'cloud_frac') then
   nvr=nvar
   namev='CLOUD_COVER'
   unitv='fraction'
   cfmt='2x,f6.3,2x,i4.3'
   
else
   print*,'remove variable from namelist:',cvar
   stop 'ralph_vars: variable not in Ralph file variable list'
endif

ivok=1

return
end
