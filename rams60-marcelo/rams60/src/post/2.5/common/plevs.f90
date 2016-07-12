!
! Copyright (C) 1991-2003  ; All Rights Reserved ; ATMET, LLC
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
! 2.5.0
!###########################################################################

subroutine get_plevs(np,ips)
include 'plevs.h'
integer ips(*)

np=nplevs
do n=1,nplevs
   ips(n)=iplevs(n)
enddo

return
end

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine get_plev(np,ipl)
include 'plevs.h'

ipl=iplevs(np)

return
end

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine get_pcoor(np,ipl)
include 'plevs.h'

do np=1,nplevs
   if(ipl.eq.iplevs(np)) return
enddo
np=0

return
end

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine get_closep(ngr,ip)
include 'plevs.h'
include 'rcommons.h'

print*,'CloseP: ngr',ngr
zzz=ztn(ip,ngr)
zmin=100000000.
do np=1,nplevs
   zdif=abs(zzz-float(zplevz(np)))
   if(zdif.lt.zmin) then
      zmin=zdif
      ip=np
   endif
enddo
print*,'CloseP:',zzz,zplevz(ip),ip

return
end

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine get_closez(ngr,ip)
include 'plevs.h'
include 'rcommons.h'

zzz=float(zplevz(ip))
print*,'CloseZ:',zzz,zplevz(ip),ip
zmin=100000000.
do k=1,nnzp(ngr)
   zdif=abs(zzz-ztn(k,ngr))
   if(zdif.lt.zmin) then
      zmin=zdif
      ip=k
   endif
enddo
print*,'CloseZ:',zzz,zplevz(ip),ip

return
end
