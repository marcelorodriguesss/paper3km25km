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

module grib_utils

integer, parameter :: maxtrecs = 1000, maxplevels=50, maxtrvar=200

type table
   integer :: irec,level,nx,ny,nz
   real :: swlat,swlon
   character(len=8) :: var
   character(len=2) :: proj
end type

type (table) :: grib_table(maxtrecs)

!-----------------------------------------------------------------

type g_gds
   integer :: nx,ny,icenter,nrec,igrid,mode,iscan
   character(len=20) :: projection,cpole,longdate
   real :: alat1,alon1,alov,aorient,dx,dy,reflat1,reflat2
   integer :: nplevels
   real :: plevels(maxplevels)
end type
type (g_gds) :: grib_gds

!-----------------------------------------------------------------

type g_inventory
   integer :: nrec,levtype,nlevels,nlrec(maxplevels),levels(maxplevels)
   integer :: id,dtime,nx,ny,iscan
   character(len=20) :: name 
end type
type (g_inventory) :: grib_inv(maxtrecs),master_inv
integer :: num_gentries

type g_trans
   character(len=32) :: rvar,gvar
   integer :: levtype
   real :: gdefault
end type
type(g_trans) :: gtrtable(maxtrvar)
integer :: ntrvars

!-----------------------------------------------------------------

integer :: lenout
integer, parameter :: lenbuff=10000000,maxrecs=100000
character(len=lenbuff) :: buffer
character(len=256) :: cmd
character(len=64) :: tokens(100)
integer :: ntokens
character(len=1) :: funit

character(len=128) :: lines(maxrecs)
integer :: linelen(maxrecs)

!===============================================================================

Contains

!===============================================================================

subroutine grib_inventory(flnm)

implicit none
character(len=*) :: flnm  ! grib file name

integer :: nlines,nr
character(len=32) :: ckpds
integer :: ivar,ilevf,ilev,nv,nxf,nyf,iscan

! Perform an inventory of all variables on a grib file

!!!! ASSUMES ALL RECORDS ON THE FILE ARE OF THE SAME DATE/TIME !!!!!!!!!!!

! Get nx/ny !!!! AGAIN ASSUMES FIRST RECORD IS SAME AS ALL !!!!!!!!!!

call grib_get_proj(flnm)
nxf=grib_gds%nx
nyf=grib_gds%ny
iscan=grib_gds%iscan

cmd='wgrib -v -4yr '//trim(flnm)//char(0)
call ir_popen(lenbuff,buffer,cmd,lenout)
!print*,'done ir_popen'

! Sepatate output into lines
call grib_split_out(nlines)

num_gentries = 0

! Loop through all records. Make a separate entry for every var/level-type combo
dolines: do nr = 1,nlines
   ! Parse line, get variable/level-type
   call tokenize1(lines(nr),tokens,ntokens,':')
   ckpds=tokens(6)
   read(ckpds(6:),*) ivar,ilevf,ilev
   
   ! See if a new variable
   do nv=1,num_gentries
      if (grib_inv(nv)%id == ivar) then
         ! We found the variable. See if the level type matches.
         if (ilevf == grib_inv(nv)%levtype) then
            ! It matches, therefore it must be an additional level
            grib_inv(nv)%nlevels = grib_inv(nv)%nlevels + 1
            grib_inv(nv)%levels(grib_inv(nv)%nlevels) = ilev
            read(tokens(1),*) grib_inv(nv)%nlrec(grib_inv(nv)%nlevels)
            cycle dolines
         else
            ! Doesn't match, so might be new. Continue looking
            cycle
         endif
      endif
   enddo
   
   ! If we're here, its a new combo
   num_gentries = num_gentries + 1
   grib_inv(num_gentries)%id = ivar
   grib_inv(num_gentries)%name = tokens(4)
   grib_inv(num_gentries)%levtype = ilevf
   grib_inv(num_gentries)%levels(1) = ilev
   grib_inv(num_gentries)%nlevels = 1
   grib_inv(num_gentries)%nx = nxf
   grib_inv(num_gentries)%ny = nyf
   grib_inv(num_gentries)%iscan = iscan    ! if =0, (1,1) in NW corner; flip on read
   read(tokens(1),*) grib_inv(num_gentries)%nlrec(1)
   read(tokens(3)(3:),*) grib_inv(num_gentries)%dtime

enddo dolines

do nv = 1,num_gentries
!   print*,'vars:',grib_inv(nv)%name,grib_inv(nv)%id, grib_inv(nv)%nlevels,grib_inv(nv)%levtype
!   print*,'----:',grib_inv(nv)%levels(1:grib_inv(nv)%nlevels)
!   print*,'----:',grib_inv(nv)%nlrec(1:grib_inv(nv)%nlevels)
enddo



return
end subroutine

!---------------------------------------------------------------------------

subroutine grib_info(cvar,levtype,ngrd,ndims,idims,nent,ierr)

implicit none

character(len=*) :: cvar
integer :: levtype,ngrd,ndims,idims(*),nent,ierr

integer :: nv

ierr=1

do nv = 1,num_gentries
   if (trim(cvar) == trim(grib_inv(nv)%name) .and.  &
       levtype == grib_inv(nv)%levtype) then
      
      ndims=2
      idims(1) = grib_inv(nv)%nx 
      idims(2) = grib_inv(nv)%ny
      idims(3) = 1
      if (grib_inv(nv)%nlevels > 1) then
         ndims=3
         idims(3) = grib_inv(nv)%nlevels
      endif
      ierr=0
      nent=nv
      return
   endif
enddo

ierr=1
print*, 'grib_info: did not find variable/level: ',trim(cvar),levtype

return
end subroutine

!===============================================================================

subroutine grib_irec(flnm,ndims,idims,nent,a)

implicit none

character(len=*) :: flnm  ! grib file name
integer :: ndims,idims(4),nent

real :: a(idims(1),idims(2),idims(3)), fld(idims(1),idims(2))

integer :: i,j,jj,n,nl

! Fill array with missing value

a = -9999.

print*,'grib_irec:',idims(1:3),nent

if (idims(3) > 1) then
   do n=1,idims(3)

      ! Look through the master_inv and determine which level should be filled
      do nl=1,master_inv%nlevels
         if (grib_inv(nent)%levels(n) == master_inv%levels(nl)) go to 100
      enddo
      print*,'Grib record found, but no match with master_inv level.'
      print*,'  --- ',grib_inv(nent)%name, '  level:',grib_inv(nent)%levels(n)
      100 continue
   
   
      call grib_get_rec(trim(flnm)//char(0),idims(1)*idims(2),a(1,1,nl)  &
                        ,grib_inv(nent)%nlrec(n))
   print*,'grib_irec:',n,nl,grib_inv(nent)%nlrec(n),grib_inv(nent)%iscan
   print*,'grib_irec:',n,nl,maxval(a(1:idims(1),1:idims(2),n))  &
                        ,minval(a(1:idims(1),1:idims(2),n))

   enddo
else
   call grib_get_rec(trim(flnm)//char(0),idims(1)*idims(2),a(1,1,1)  &
                    ,grib_inv(nent)%nlrec(1))

endif

! Flip fields in north-south direction if iscan= 0.
if (grib_inv(nent)%iscan == 0) then
   do n = 1,idims(3)
      do j = 1,idims(2)
         do i = 1,idims(1)
            fld(i,j)=a(i,j,n)
         enddo
      enddo
      do j = 1,idims(2)
         jj = idims(2)-j+1
         do i = 1,idims(1)
            a(i,j,n)=fld(i,jj)
         enddo
      enddo
   enddo
endif

!print*,'grib_irec:',maxval(a(1:idims(1),1:idims(2),1:idims(3)))
!print*,'grib_irec:',minval(a(1:idims(1),1:idims(2),1:idims(3)))

return
end subroutine

!===============================================================================

subroutine parse_grid(string,irec)

implicit none

integer :: irec,l1,l2,l3,l4,nx,ny

real :: lat1,lat2,latinc,lon1,lon2,loninc

character(len=256) :: string

l1=index(string,':')
if (index(string(1:l1-1),'latlon') > 0) then
   grib_table(irec)%proj='LL'
elseif (index(string(1:l1-1),'Lambert Conf') > 0) then
   grib_table(irec)%proj='LC'
else
   print*,'Unrecognized grid projection: ',string(1:l1-1)
   stop
endif

if (grib_table(irec)%proj == 'LL') then
   l1=index(string,'lat ')
   l2=index(string,'to')
   l3=index(string,'by')
   l4=index(string,'nxny')
   read(string(l1+3:l2-1),'(f)') lat1
   read(string(l2+2:l3-1),'(f)') lat2
   read(string(l3+2:l4-1),'(f)') latinc
   ny=nint((lat2-lat1)/latinc)
   ny=ny+sign(1,ny)
   string=string(l4+4:)
   l1=index(string,'long')
   l2=index(string,'to')
   l3=index(string,'by')
   l4=index(string,',')
   read(string(l1+4:l2-1),'(f)') lon1
   read(string(l2+2:l3-1),'(f)') lon2
   if (lon1 < 0.) lon1=lon1+360.
   if (lon2 < 0.) lon2=lon2+360.
   read(string(l3+2:l4-1),'(f)') loninc
   nx=nint((lon2-lon1)/loninc)
   nx=nx+sign(1,nx)
   if (ny > 0) then
      grib_table(irec)%swlat=lat1
   else
      grib_table(irec)%swlat=lat2
   endif
   grib_table(irec)%swlon=lon1
endif

grib_table(irec)%nx=nx
grib_table(irec)%ny=ny

return
end subroutine

!---------------------------------------------------

subroutine grib_get_times(filein,ctimes,ntimes)

implicit none

character(len=*) :: filein,ctimes(*)
integer :: ntimes

integer :: nl,nlines,nt,nc,idig
character(len=64) :: ctime,fhr,cout
character(len=1) :: dig
real :: xhr

! Get all the unique data times that exist on the input file

cmd='wgrib -s -4yr '//trim(filein)//char(0)
call ir_popen(lenbuff,buffer,cmd,lenout)
!print*,'done ir_popen'

! Sepatate output into lines
call grib_split_out(nlines)


! Date/time exist as second field in colon-delimited line, but also add
!   the forecast hour if any.
ntimes=0
dolines: do nl=1,nlines
   call tokenize1(lines(nl),tokens,ntokens,':')
   !print*,'nnnttt1:',ntokens, tokens(3)(3:)
   ! Regular date/time
   ctime=tokens(3)(3:)
   ! Forecast hour
   fhr=tokens(6)
   
   if (trim(fhr) /= 'anl') then
      
      ! Sometimes there are averaged fields with a range of hours. Skip these...
      if (index(fhr,'ave') /= 0) cycle dolines
   !print*,'----:',trim(ctime),':----:',trim(fhr)
      
      ! extract numeric value
      nc=index(fhr,'hr')
      if (nc <= 1) then
         print*,'grib_get_times: Unknown forecast hour string:'
         print*,'grib_get_times: ',trim(fhr)
         stop 'grib_get_times'
      endif
      
      ! Check fhr for all numeric characters. It not, skip this record.
      fhr=fhr(1:nc-1)
      do nc = 1, len_trim(fhr)
         dig = fhr(nc:nc)
         if(dig < '0' .or. dig > '9') cycle dolines
      enddo
      
      ! Since we can be reasonably sure we got a number...
      read(fhr,*) xhr
      
      ! Add forecast hour to date/time
      call date_add_to_big (trim(ctime)//'0000',xhr,'h',cout)
      !print*,'%%%%:',trim(ctime),':%%%%:',trim(fhr),':%%%%:',trim(cout)
      ctime=cout(1:10)
   endif
   
   ! List of unique times are returned
   do nt=1,ntimes
      if (trim(ctime) == trim(ctimes(nt))) cycle dolines
   enddo
   ntimes=ntimes+1
   ctimes(ntimes) = ctime
enddo dolines


return
end subroutine

!---------------------------------------------------

subroutine grib_split_out(nlines)

implicit none

integer :: nl,istart,iend,nlines

! Sepatate output into lines
istart=1
nlines=0
do while(.true.)
   iend=istart+index(buffer(istart:),char(10))-1
   if (iend > lenout .or. istart > iend) exit
   nlines=nlines+1
   if (nlines > maxrecs) then
      print*, 'Number of records in grib file exceeds MAXRECS.'
      print*, '  MAXRECS=',maxrecs
      print*, '  last record read=',lines(nlines)
      print*, '  Program stopped.'
   endif
   lines(nlines) = buffer(istart:iend)
   linelen(nlines)=iend-istart+1
   istart=iend+1
enddo

return
end subroutine

!---------------------------------------------------

subroutine grib_get_grids(filein,delx,ntimes)

implicit none

character(len=*) :: filein
real :: delx(*)
integer :: ntimes

integer :: nl,istart,iend,nlines,nt,ncx,ncy
character(len=64) :: ctime

! Get all the unique grid spacings that exist on the input file

!!!!!!!!!!!! ASSUMING: ONLY ONE PER FILE AND DX VALUE !!!!!!!!!!!!!!!!!!!!!!!

cmd='wgrib -V -d 1 -4yr '//trim(filein)//char(0)
call ir_popen(lenbuff,buffer,cmd,lenout)
!print*,'done ir_popen'

! Sepatate output into lines
!istart=1
!nlines=0
!do while(.true.)
!   iend=istart+index(buffer(istart:),char(10))-1
!   if (iend > lenout .or. istart > iend) exit
!   nlines=nlines+1
!   if (nlines > maxrecs) then
!      print*, 'Number of records in grib file exceeds MAXRECS.'
!      print*, '  MAXRECS=',maxrecs
!      print*, '  last record read=',lines(nlines)
!      print*, '  Program stopped.'
!   endif
!   lines(nlines) = buffer(istart:iend)
!   linelen(nlines)=iend-istart+1
!   istart=iend+1
!enddo

! Find Dx, then Dy. Value is between these...

ncx = index (buffer,'Dx')+2
ncy = index (buffer,'Dy')-1
!print*,'buffer:',ncx,ncy,buffer(1:lenout)

ntimes=1
read(buffer(ncx:ncy),*) delx(ntimes)
delx(ntimes) = delx(ntimes) * 1000.

!print*,'get_spacing:',delx(ntimes)

return
end subroutine

!---------------------------------------------------

subroutine grib_get_press_levels(filein,npress)

implicit none

character(len=*) :: filein
integer :: npress

integer :: nl,nlines,nv,nvv,np,ivar,ilevf,ilev
character(len=64) :: cdate,cvar,clevel,ckpds

type var
   integer :: id, levs(100),nlevs
   character(len=8) :: name
end type

type(var) :: vars(100)
integer :: nvars,nvara(100),maxlev

! Get the number of vertical levels that exist on the input file
!   This will only look at pressure level data. It will return maximum
!   levels of any variable defined on P surfaces.

!!!!!!!!!!!! ASSUMING: ONLY ONE DATE/TIME PER FILE !!!!!!!!!!!!!!!!!!!

cmd='wgrib -v -4yr '//trim(filein)//char(0)
call ir_popen(lenbuff,buffer,cmd,lenout)
!print*,'done ir_popen'

! Sepatate output into lines
call grib_split_out(nlines)

! Loop through lines, sorting by variable/date/
nvars=0
do nl=1,nlines
   call tokenize1(lines(nl),tokens,ntokens,':')
   !!!!!!print*,trim(lines(nl))
   ! 3rd token is date
   cdate=tokens(3)(3:) 
   ckpds=tokens(6)
   read(ckpds(6:),*) ivar,ilevf,ilev
   ! check if pressure level
   if(ilevf /= 100) cycle
   
   ! See if a new variable
   do nv=1,nvars
      if (vars(nv)%id == ivar) then
         nvv=nv
         go to 20
      endif
   enddo
   nvars=nvars+1
   vars(nvars)%id=ivar
   vars(nvars)%name=tokens(4)
   vars(nvars)%nlevs=0
   nvv=nvars
20 continue

   ! Now see if level exists
   do np=1,vars(nvv)%nlevs
      if (vars(nvv)%levs(np) == ilev) go to 30
   enddo
   vars(nvv)%nlevs = vars(nvv)%nlevs +1
   vars(nvv)%levs(vars(nvv)%nlevs) = ilev
30 continue
      
enddo

! Find the variable with a max number of levels

maxlev=0
do nv = 1, nvars
   if (vars(nv)%nlevs > maxlev) then
      maxlev=vars(nv)%nlevs
      nvv=nv
   endif
enddo


npress=maxval(vars(1:nvars)%nlevs)

grib_gds%nplevels = npress
grib_gds%plevels(1:npress)=float(vars(nvv)%levs(1:npress))

master_inv%nlevels=npress
master_inv%levels(1:npress)=vars(nvv)%levs(1:npress)

!do nv = 1,nvars
!   print*,'vars:',vars(nv)%name,vars(nv)%id, vars(nv)%nlevs
!enddo

!do nv = 1,npress
!   print*,'plev:',vars(nvv)%name,nv, vars(nvv)%levs(nv)
!enddo


return
end subroutine


!---------------------------------------------------

subroutine grib_get_proj(filein)

implicit none

character(len=*) :: filein
integer :: n,istart,iend,ntok

cmd='wgrib -V -d 1  -4yr '//trim(filein)//char(0)
call ir_popen(lenbuff,buffer,cmd,lenout)
!print*,'pr:',0,buffer(1:lenout)

! Take first seven lines from the output. Assuming all records have
!    same projection as first record.

istart=1
do n=1,7
   iend=istart+index(buffer(istart:),char(10))-1
   lines(n) = buffer(istart:iend)
!print*,'li:',n,buffer(istart:iend)
   linelen(n)=iend-istart+1
   istart=iend+1
enddo

call parse(lines(1),tokens,ntok)
read(tokens(3),*) grib_gds%longdate
grib_gds%longdate = trim(grib_gds%longdate)//'00'

call parse(lines(3),tokens,ntok)
read(tokens(10),*) grib_gds%nx
read(tokens(12),*) grib_gds%ny
read(tokens(15),*) grib_gds%igrid

call parse(lines(4),tokens,ntok)
read(tokens(2),*) grib_gds%icenter

call parse(lines(5),tokens,ntok)
read(tokens(1),*) grib_gds%projection




if(grib_gds%projection(1:5) == 'polar') then
   grib_gds%projection = 'PS'
   call parse(lines(5),tokens,ntok)
   read(tokens(4),*) grib_gds%alat1
   read(tokens(6),*) grib_gds%alon1
   read(tokens(8),*) grib_gds%aorient
   call parse(lines(6),tokens,ntok)
   read(tokens(1),*) grib_gds%cpole
   read(tokens(7),*) grib_gds%dx
   read(tokens(9),*) grib_gds%dy
   grib_gds%dx=grib_gds%dx*1000. ; grib_gds%dy=grib_gds%dy*1000.
   read(tokens(11),*) grib_gds%iscan
   grib_gds%mode=8
elseif(grib_gds%projection(1:5) == 'Lambe') then
   grib_gds%projection = 'LC'
   call parse(lines(5),tokens,ntok)
!do n=1,ntok
!print*,'ttttt:',n,' '//trim(tokens(n))//'*'
!enddo
   read(tokens(4),*) grib_gds%alat1
   read(tokens(6),*) grib_gds%alon1
   read(tokens(8),*) grib_gds%alov
   call parse(lines(6),tokens,ntok)
   read(tokens(2),*) grib_gds%reflat2
   read(tokens(4),*) grib_gds%reflat1
   call parse(lines(7),tokens,ntok)
   read(tokens(7),*) grib_gds%dx
   read(tokens(9),*) grib_gds%dy
   grib_gds%dx=grib_gds%dx*1000. ; grib_gds%dy=grib_gds%dy*1000.
   read(tokens(11),*) grib_gds%iscan
   read(tokens(13),*) grib_gds%mode
elseif(grib_gds%projection(1:5) == 'latlo') then
   grib_gds%projection = 'LL'
   call parse(lines(5),tokens,ntok)
   read(tokens(3),*) grib_gds%alat1
   read(tokens(7),*) grib_gds%dy
   call parse(lines(6),tokens,ntok)
   read(tokens(2),*) grib_gds%alon1
   read(tokens(6),*) grib_gds%dx
   read(tokens(11),*) grib_gds%iscan
   read(tokens(13),*) grib_gds%mode
   if (grib_gds%iscan == 0) then
      ! point (1,1) is in NW corner
      call parse(lines(5),tokens,ntok)
      read(tokens(5),*) grib_gds%alat1
   endif
      
endif


return
end subroutine


!---------------------------------------------------

subroutine grib_read_trtable(filein)

implicit none
character(len=*) :: filein

character(len=80) :: vline

open(31,file=filein,status='old')

ntrvars=0
do while (.true.)
   read(31,'(a80)',end=100) vline
   ntrvars=ntrvars+1
   call tokenize1(vline,tokens,ntokens,':')
   gtrtable(ntrvars)%rvar=tokens(1)
   gtrtable(ntrvars)%gvar=tokens(2)
   read(tokens(3),*) gtrtable(ntrvars)%levtype
   read(tokens(4),*) gtrtable(ntrvars)%gdefault
   print*,'trtable:',ntrvars,gtrtable(ntrvars)%rvar,gtrtable(ntrvars)%gvar
enddo

100 continue


return
end subroutine


!---------------------------------------------------

subroutine grib_translate(rstring,gstring,levtype,gdefault)

implicit none
character(len=*) :: rstring,gstring
integer :: levtype
real :: gdefault

integer :: nv

do nv=1,ntrvars
   if (trim(rstring) == trim(gtrtable(nv)%rvar)) then
      gstring = gtrtable(nv)%gvar
      levtype = gtrtable(nv)%levtype
      gdefault = gtrtable(nv)%gdefault
      return
   endif
enddo

print*,'grib_translate: cannot find variable: ',trim(rstring)
stop 'grib_translate'

return
end subroutine 


end module
