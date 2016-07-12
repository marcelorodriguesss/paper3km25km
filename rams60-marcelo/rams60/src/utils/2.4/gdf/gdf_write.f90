!
! Copyright (C) 1991-2005  ; All Rights Reserved ; ATMET, LLC
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

subroutine gdf_write ()
 
!use grab_coms
use gdf_input

implicit none
   
character(len=128) :: flnm,cvalfile
character(len=15)  :: cdate

integer :: iy,im,id,it,nt,iun,nval,nvar,nv,lc,ivok

integer            :: nvr(maxfore)
character(len=16)  :: namev(maxfore),unitv(maxfore)
character(len=16)  :: cfmt(maxfore)
character(len=256) :: cformat
          
call RAMS_get_cdata (0,1,flnm,nval)
iun=34
      
if(cmode(1:3)=='sfc') then

   if(multfils==0.or.multfils==1) then
   
      if(multfils==0) then
         ! filename date string is the begining of run
         call RAMS_get_cdata (1,1,cdate,nval)
      else
         ! filename date string is the first file found
         iy=gs_date(1)/10000
         im=mod(gs_date(1),10000)/100
         id=mod(gs_date(1),100)
         it=gs_time(1)/100
         write(cdate,'(i4.4,a1,i2.2,a1,i2.2,a1,i4.4)')  &
            iy,'-',im,'-',id,'-',it
      endif
      
      ! make the filename and open the file
      cvalfile=gsdir(1:len_trim(gsdir))//'dp-s'//cdate
      print*
      print*,'User gdf sfc file: ',cvalfile(1:len_trim(cvalfile))
      open(unit=iun,file=cvalfile,status='unknown')
      
      if(ioutfmt==2) then
      
         ! write the header and put together the info for the data
         write(iun,'(a)') '999999  2'
         write(iun,'(i2)') numvars
         
         cformat='(i4.4,2x,2(i2.2,2x),i4.4,2x,a12,2f10.4,2x,f7.1'
         nv=1
         do nvar=1,numvars
            lc=len_trim(cvars(nvar))
            call gdf_vars (ivok,nvar,cvars(nvar)(1:lc),nvr(nv)  &
                            ,namev(nv),unitv(nv),cfmt(nv))
            write(iun,'(3a)') namev(nv)(1:len_trim(namev(nv))),'  '  &
                             ,unitv(nv)(1:len_trim(unitv(nv)))
            cformat=cformat(1:len_trim(cformat))//','//  &
                    cfmt(nv)(1:len_trim(cfmt(nv)))
            if(ivok==1) nv=nv+1
         enddo
         cformat=cformat(1:len_trim(cformat))//')'
         
         ! get the current data and time and write the data
         do nt=1,numtimes
            iy=gs_date(nt)/10000
            im=mod(gs_date(nt),10000)/100
            id=mod(gs_date(nt),100)
            it=gs_time(nt)/100
            call write_sfc_gdf2 (iun,nt,iy,im,id,it,nv,nvr,cformat)
         enddo
         
      elseif(ioutfmt==3) then
      
         ! write the header and put together the info for the data
         write(iun,'(a)') '999999  3'
         write(iun,'(a)') '10 HEADER'
         write(iun,'(a)') 'YEAR'
         write(iun,'(a)') 'MONTH'
         write(iun,'(a)') 'DAY'
         write(iun,'(a)') 'HOUR'
         write(iun,'(a)') 'STATION_ID'
         write(iun,'(a)') 'LATITUDE'
         write(iun,'(a)') 'LONGITUDE'
         write(iun,'(a)') 'ELEVATION  m'
         write(iun,'(a)') 'HEIGHT  m'
         write(iun,'(a)') 'HEIGHT_FLAG'
         write(iun,'(i2,a)') numvars,' DATA'
      
         cformat='(i4.4,2x,2(i2.2,2x),i4.4,2x,a12,2(2x,f10.4),2(2x,f7.1),i3'
         nv=1
         do nvar=1,numvars
            lc=len_trim(cvars(nvar))
            call gdf_vars (ivok,nvar,cvars(nvar)(1:lc),nvr(nv)  &
                            ,namev(nv),unitv(nv),cfmt(nv))
            write(iun,'(3a)') namev(nv)(1:len_trim(namev(nv))),'  '  &
                             ,unitv(nv)(1:len_trim(unitv(nv)))
            cformat=cformat(1:len_trim(cformat))//','//  &
                    cfmt(nv)(1:len_trim(cfmt(nv)))
            if(ivok==1) nv=nv+1
         enddo
         cformat=cformat(1:len_trim(cformat))//')'

         ! get the current data and time and write the data
         do nt=1,numtimes
            iy=gs_date(nt)/10000
            im=mod(gs_date(nt),10000)/100
            id=mod(gs_date(nt),100)
            it=gs_time(nt)/100
            call write_sfc_gdf3 (iun,nt,iy,im,id,it,nv,nvr,cformat)
         enddo
      
      else
         print*,ioutfmt
         stop 'write_gdf_output - uncoded gdf file version'
      endif
      
      close(unit=iun)

   elseif(multfils==2) then
   
      ! new file for each time
      do nt=1,numtimes
      
         ! filename date string is the current file time
         iy=gs_date(nt)/10000
         im=mod(gs_date(nt),10000)/100
         id=mod(gs_date(nt),100)
         it=gs_time(nt)/100
         write(cdate,'(i4.4,a1,i2.2,a1,i2.2,a1,i4.4)') iy,'-',im,'-',id,'-',it
 
         ! make the filename and open the file
         cvalfile=gsdir(1:len_trim(gsdir))//'dp-s'//cdate
         print*
         print*,'gdf sfc file: ',cvalfile(1:len_trim(cvalfile))
         open(unit=iun,file=cvalfile,status='unknown')
      
         if(ioutfmt==2) then
 
            ! write the header and put together the info for the data
            write(iun,'(a)') '999999  2'
            write(iun,'(i2)') numvars
 
            cformat='(i4.4,2x,2(i2.2,2x),i4.4,2x,a12,2f10.4,2x,f7.1'
            nv=1
            do nvar=1,numvars
               lc=len_trim(cvars(nvar))
               call gdf_vars (ivok,nvar,cvars(nvar)(1:lc),nvr(nv)  &
                               ,namev(nv),unitv(nv),cfmt(nv))
               write(iun,'(3a)') namev(nv)(1:len_trim(namev(nv))),'  '  &
                                ,unitv(nv)(1:len_trim(unitv(nv)))
               cformat=cformat(1:len_trim(cformat))//','//  &
                       cfmt(nv)(1:len_trim(cfmt(nv)))
               if(ivok==1) nv=nv+1
            enddo
            cformat=cformat(1:len_trim(cformat))//')'
 
            ! get the current data and time and write the data
            call write_sfc_gdf2 (iun,nt,iy,im,id,it,nv,nvr,cformat)
 
         elseif(ioutfmt==3) then
 
            ! write the header and put together the info for the data
            write(iun,'(a)') '999999  3'
            write(iun,'(a)') '10 HEADER'
            write(iun,'(a)') 'YEAR'
            write(iun,'(a)') 'MONTH'
            write(iun,'(a)') 'DAY'
            write(iun,'(a)') 'HOUR'
            write(iun,'(a)') 'STATION_ID'
            write(iun,'(a)') 'LATITUDE'
            write(iun,'(a)') 'LONGITUDE'
            write(iun,'(a)') 'ELEVATION  m'
            write(iun,'(a)') 'HEIGHT  m'
            write(iun,'(a)') 'HEIGHT_FLAG'
            write(iun,'(i2,a)') numvars,' DATA'
 
            cformat='(i4.4,2x,2(i2.2,2x),i4.4,2x,a12,2(2x,f10.4),2(2x,f7.1),i3'
            nv=1
            do nvar=1,numvars
               lc=len_trim(cvars(nvar))
               call gdf_vars (ivok,nvar,cvars(nvar)(1:lc),nvr(nv)  &
                               ,namev(nv),unitv(nv),cfmt(nv))
               write(iun,'(3a)') namev(nv)(1:len_trim(namev(nv))),'  '  &
                                ,unitv(nv)(1:len_trim(unitv(nv)))
               cformat=cformat(1:len_trim(cformat))//','//  &
                       cfmt(nv)(1:len_trim(cfmt(nv)))
               if(ivok==1) nv=nv+1
            enddo
            cformat=cformat(1:len_trim(cformat))//')'

            ! get the current data and time and write the data
            call write_sfc_gdf3 (iun,nt,iy,im,id,it,nv,nvr,cformat)
 
         else
            print*,ioutfmt
            stop 'write_gdf_output - uncoded gdf file version'
         endif
      
         close(unit=iun)
      
      enddo
   
   else
      print*,'unknown MULTFILS option:',MULTFILS
      stop 'write_gdf_output'
   endif

elseif(cmode(1:3)=='upa' .or. cmode(1:3)=='prf') then

   if(multfils==0.or.multfils==1) then
   
      if(multfils==0) then
         ! filename date string is the begining of run
         call RAMS_get_cdata (1,1,cdate,nval)
      else
         ! filename date string is the first file found
         iy=gs_date(1)/10000
         im=mod(gs_date(1),10000)/100
         id=mod(gs_date(1),100)
         it=gs_time(1)/100
         write(cdate,'(i4.4,a1,i2.2,a1,i2.2,a1,i4.4)')  &
            iy,'-',im,'-',id,'-',it
      endif
      
      ! make the filename and open the file
      cvalfile=gsdir(1:len_trim(gsdir))//'dp-r'//cdate
      print*
      print*,'User gdf upa file: ',cvalfile(1:len_trim(cvalfile))
      open(unit=iun,file=cvalfile,status='unknown')

      ! write header information
      write(iun,'(a)') '999999  3'
      write(iun,'(a)') '10 HEADER'
      write(iun,'(a)') 'YEAR'
      write(iun,'(a)') 'MONTH'
      write(iun,'(a)') 'DAY'
      write(iun,'(a)') 'HOUR'
      write(iun,'(a)') 'STATION_ID'
      write(iun,'(a)') 'PRESSURE_LEVELS'
      write(iun,'(a)') 'HEIGHT_LEVELS'
      write(iun,'(a)') 'LATITUDE'
      write(iun,'(a)') 'LONGITUDE'
      write(iun,'(a)') 'ELEVATION  m'
      write(iun,'(a)') '4 PRESS'
      write(iun,'(a)') 'PRESSURE  Pa'
      write(iun,'(a)') 'TEMPERATURE  C'
      write(iun,'(a)') 'DEWPOINT  C'
      write(iun,'(a)') 'MOISTURE  frac'
      write(iun,'(a)') '3 HEIGHT'
      write(iun,'(a)') 'HEIGHT  m'
      write(iun,'(a)') 'WINDSPEED  m/s'
      write(iun,'(a)') 'WIND_DIRECTION  deg'
        
      ! get the current data and time and write the data
      do nt=1,numtimes
         iy=gs_date(nt)/10000
         im=mod(gs_date(nt),10000)/100
         id=mod(gs_date(nt),100)
         it=gs_time(nt)/100
         call write_upa_gdf (iun,nt,iy,im,id,it)
      enddo
      
      close(unit=iun)

   elseif(multfils==2) then
   
      ! new file for each time
      do nt=1,numtimes
      
         ! filename date string is the current file time
         iy=gs_date(nt)/10000
         im=mod(gs_date(nt),10000)/100
         id=mod(gs_date(nt),100)
         it=gs_time(nt)/100
         write(cdate,'(i4.4,a1,i2.2,a1,i2.2,a1,i4.4)') iy,'-',im,'-',id,'-',it
 
         ! make the filename and open the file
         cvalfile=gsdir(1:len_trim(gsdir))//'dp-r'//cdate
         print*
         print*,'gdf upa file: ',cvalfile(1:len_trim(cvalfile))
         open(unit=iun,file=cvalfile,status='unknown')
        
         ! write header information
         write(iun,'(a)') '999999  3'
         write(iun,'(a)') '10 HEADER'
         write(iun,'(a)') 'YEAR'
         write(iun,'(a)') 'MONTH'
         write(iun,'(a)') 'DAY'
         write(iun,'(a)') 'HOUR'
         write(iun,'(a)') 'STATION_ID'
         write(iun,'(a)') 'PRESSURE_LEVELS'
         write(iun,'(a)') 'HEIGHT_LEVELS'
         write(iun,'(a)') 'LATITUDE'
         write(iun,'(a)') 'LONGITUDE'
         write(iun,'(a)') 'ELEVATION  m'
         write(iun,'(a)') '4 PRESS'
         write(iun,'(a)') 'PRESSURE  Pa'
         write(iun,'(a)') 'TEMPERATURE  C'
         write(iun,'(a)') 'DEWPOINT  C'
         write(iun,'(a)') 'MOISTURE  fraction'
         write(iun,'(a)') '3 HEIGHT'
         write(iun,'(a)') 'HEIGHT  m'
         write(iun,'(a)') 'WINDSPEED  m/s'
         write(iun,'(a)') 'WIND_DIRECTION  deg'
        
         ! write the data
         call write_upa_gdf (iun,nt,iy,im,id,it)
      
         close(unit=iun)
      
      enddo
   
   else
      print*,'unknown MULTFILS option:',multfils
      stop 'write_gdf_output'
   endif
   
endif

return
end

