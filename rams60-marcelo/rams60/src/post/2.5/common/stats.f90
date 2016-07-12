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

!...calculates 5 statistics (RMSE, MEAN ABSOLUTE ERROR, RELATIVE ERROR,
!...BIAS, AND RMSVE) on 6 variables (U, V, TEMP, PRESSURE, RH, AND SPECIFIC
!...HUMIDITY) on the surface and pressure level data

subroutine RAMS_STATS(fobs,pred,fpos)

include 'vcomm2.h'
dimension fobs(numtime,numpts,numfore)
dimension pred(numtime,numpts,numfore)
dimension fpos(numtime,numpts,8)
dimension iobs(nplevs+1)
real avg,stuff(5000),avg1,obar,pbar,avg2
real minlat,maxlat,minlon,maxlon
integer pm,i,j
character clevel*7

print*
print*,'in RAMS_stats'
print*

!...sort observations into pressure level groupings
call POST_GRAB_SORT(fobs,pred,fpos)

!...zero out stats array
do i=1,NUMFORE
   do j=1,9
      do k=1,11
         stat_dat(i,j,k)=-999.00
      enddo
   enddo
enddo


do k=1,nplevs+1

   ilimit2=ndxplev(k)
   if(k.eq.1) then
      ilimit1=1
      iobs(k)=ndxplev(k)
   else
      ilimit1=ndxplev(k-1)+1
      iobs(k)=ndxplev(k)-ndxplev(k-1)
   endif

   do pm=1,NUMFORE

      if(ilimit2.eq.-1) then
         do ijk = 1,9
            stat_dat(pm,ijk,k) = 0.0
         enddo
      else

!...mean of obs
      j=0
      avg=0.
      do i=ilimit1,ilimit2
         if(nint(fobs(nfndx,i,pm)).ne.-999) then
            j=j+1
            stuff(j)=fobs(nfndx,i,pm)
         endif
      enddo
      call average(stuff,j,avg)
      avg1=avg
      call reset_arr(stuff)

!...mean of predictions
      j=0
      avg=0.
      do i=ilimit1,ilimit2
         if(nint(pred(nfndx,i,pm)).ne.-999) then
            j=j+1
            stuff(j)=pred(nfndx,i,pm)
         endif
      enddo
      call average(stuff,j,avg)

      stat_dat(pm,2,k)=avg1
      stat_dat(pm,3,k)=avg
      obar=avg1
      pbar=avg

!...RMSE
      j = 0
      avg = 0.
      do i=ilimit1,ilimit2
         if(nint(fobs(nfndx,i,pm)).ne.-999.and.  &
            nint(pred(nfndx,i,pm)).ne.-999) then
            j=j+1
            stuff(j)=(pred(nfndx,i,pm)-fobs(nfndx,i,pm))**2.
         endif
      enddo
      stat_dat(pm,1,k)=j
      if(j.gt.1) then
         call average(stuff,j,avg)
         avg = avg**0.5
         stat_dat(pm,4,k)=avg
      else
         stat_dat(pm,4,k)=avg
      endif
      call reset_arr(stuff)

!...mean absolute error
      j = 0
      avg = 0.
      do i=ilimit1,ilimit2
         if(nint(fobs(nfndx,i,pm)).ne.-999.and.  &
            nint(pred(nfndx,i,pm)).ne.-999) then
            j=j+1
            stuff(j)=abs(pred(nfndx,i,pm)-fobs(nfndx,i,pm))
         endif
      enddo
      if(j.gt.1) then
         call average(stuff,j,avg)
         stat_dat(pm,5,k)=avg
      else
         stat_dat(pm,5,k)=-999.00
      endif
      call reset_arr(stuff)

!...RELATIVE ERROR
      j = 0
      avg = 0.
      do i=ilimit1,ilimit2
         if(nint(fobs(nfndx,i,pm)).ne.-999.and.  &
            nint(pred(nfndx,i,pm)).ne.-999) then
            j=j+1
            stuff(j)=pred(nfndx,i,pm)-fobs(nfndx,i,pm)
         endif
      enddo
      if(j.gt.1) then
         call average(stuff,j,avg)
         stat_dat(pm,6,k)=avg
      else
         stat_dat(pm,6,k)=-999.00
      endif
      call reset_arr(stuff)

!...BIAS
      j = 0
      avg = 0.
      do i=ilimit1,ilimit2
         if(nint(fobs(nfndx,i,pm)).ne.-999.and.  &
            nint(pred(nfndx,i,pm)).ne.-999.and.  &
            (abs(fobs(nfndx,i,pm)).gt.0.001)) then
            j=j+1
            stuff(j)=(pred(nfndx,i,pm)-fobs(nfndx,i,pm))  &
                     /fobs(nfndx,i,pm)
         endif
      enddo
      if(j.gt.1) then
         call average(stuff,j,avg)
!       stat_dat(pm,7,k)=avg
!...for grins, makings bias = relative err/mean obs
         stat_dat(pm,7,k)=stat_dat(pm,6,k)/stat_dat(pm,2,k)
      else
         stat_dat(pm,7,k)=-999.00
      endif
      call reset_arr(stuff)

!...calculate correlation coefficient, taken from SPSS
      j=0
      avg=0.0
      avg1=0.0
      avg2=0.0
      do i=ilimit1,ilimit2
         if(nint(fobs(nfndx,i,pm)).ne.-999.and.  &
            nint(pred(nfndx,i,pm)).ne.-999) then
            j=j+1
            avg =avg+((pred(nfndx,i,pm)-pbar)  &
                 *(fobs(nfndx,i,pm)-obar))
            avg1=avg1+((pred(nfndx,i,pm)-pbar)**2)
            avg2=avg2+((fobs(nfndx,i,pm)-obar)**2)
         endif
      enddo
      if(j.gt.1) then
         stat_dat(pm,9,k) = avg/((avg1*avg2)**0.5)
      else
         stat_dat(pm,9,k)=-999.00
      endif
!            print*,' '
!            print*,'index=',pm,'CORRELATION COEFFICIENT:'
!            print*,stat_dat(pm,9,k)
!            print*,'avg=',avg,' avg1=',avg1,' avg2=',avg2

      endif
   enddo

!...now do RMSVE, used only on U and V
!     print*,' '
   j=0
   avg = 0.
   do i=ilimit1,ilimit2
      if(nint(fobs(nfndx,i,4)).ne.-999.and.  &
         nint(fobs(nfndx,i,5)).ne.-999.and.  &
         nint(pred(nfndx,i,4)).ne.-999.and.  &
         nint(pred(nfndx,i,5)).ne.-999) then
         j=j+1
         stuff(j)=((pred(nfndx,i,4)-fobs(nfndx,i,4))**2.)+  &
                  ((pred(nfndx,i,5)-fobs(nfndx,i,5))**2.)
      endif
   enddo
   if(j.gt.1) then
      call average(stuff,j,avg)
      avg = avg**0.5
      stat_dat(1,8,k)=avg
      stat_dat(2,8,k)=avg
   else
   endif
   call reset_arr(stuff)

enddo

9999 return
end

!--------------------------------------------

subroutine average(stuff,numpts,avg)

real stuff(*),avg
integer numpts

j = 0
avg = 0
do i=1,numpts
   avg = avg + stuff(i)
   j = j + 1
enddo
if (j.ne.0) avg = avg/j

return
end

!-------------------------------------------------

subroutine reset_arr(stuff)

real stuff(5000)

do i=1,5000
   stuff(i) = -99.0
enddo

return
end

!-------------------------------------------------
