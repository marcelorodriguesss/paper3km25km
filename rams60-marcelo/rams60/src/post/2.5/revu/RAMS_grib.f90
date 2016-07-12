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
! 2.5.0
!###########################################################################

subroutine RAMS_grib (igunit,a,n1,n2,n3,nllat,nllon,dllat,dllon  &
                     ,swllat,swllon,nzb,nze,izstep  &
                     ,ivtime,ivvar,iztran,xt,yt,zt,ivtype  &
                     ,platn,plonn,refyear,refmonth,refday,startutc &
                     ,curtime,varname)

implicit none

integer, intent(in) :: refyear,refmonth,refday
integer, intent(in) :: izstep,ivtime,ivvar,iztran,igunit
real,    intent(in) :: startutc,curtime
integer, intent(in) :: n1,n2,n3,nllat,nllon,ivtype
real,    intent(in) :: dllat,dllon,swllat,swllon,platn,plonn
real,    intent(in) :: a(n1,n2,n3)
real,    intent(in) :: xt(*)
real,    intent(in) :: yt(*)
real,    intent(in) :: zt(*)
character(len=*)    :: varname
integer, intent(inout) :: nzb,nze

include 'plevs.h'

integer :: info(5)
integer :: s1opt(1)
integer :: sect1(36)
integer :: sect2(18)
integer :: sect3(1)
integer :: refhour,refmin
integer :: nellat,nellon,parm,dscale,nzee,k,conv,i,j
integer :: ngrlevs(100),ngrlat,ngrlon
real    :: dgrlat,dgrlon,swgrlat,swgrlon
real, allocatable :: toplev(:),botlev(:)
real, allocatable :: aa(:,:)

common/grcom/ngrlevs,ngrlat,ngrlon,dgrlat,dgrlon,swgrlat,swgrlon

call gribtable(varname,parm,dscale,conv)
if(parm .eq. -1) then
   print*,'Warning!!!!!!! Skipping ',varname, &
        ' parameter and scale not in gribtable in RAMS_grib'
   return
endif

if(allocated(aa)) deallocate(aa); allocate(aa(nllon,nllat))
if(allocated(toplev)) deallocate(toplev); allocate(toplev(n3))
if(allocated(botlev)) deallocate(botlev); allocate(botlev(n3))

refhour = int(startutc)
refmin = int((startutc-float(refhour))*60.)

ngrlat=nllat
ngrlon=nllon
dgrlat=dllat
dgrlon=dllon
swgrlat=swllat
swgrlon=swllon

! fill some of the grib arrays

info = 0
s1opt = 0
sect1 = 0
sect2 = 0
sect3 = 0

! info(1) set when buffsz known
info(2) = 0  
info(3) = 32  
info(4) = 0  
info(5) = nllat*nllon

sect1(1) = 2      
sect1(2) = 7    
sect1(3) = 10   
sect1(4) = 255 
sect1(5) = 128  
sect1(6) = parm
if(ivtype .eq. 2) then
  if (parm .eq. 2) then
    sect1(7) = 102
  else	
   sect1(7) = 1
  endif
elseif(iztran .eq. 1) then
   sect1(7) = 105
elseif(iztran .eq. 2) then
   sect1(7) = 103
elseif(iztran .eq. 3) then
   sect1(7) = 100
endif
! sect1(8-9) depend on vertical levelset in gribdat
sect1(10) = 1
sect1(11) = mod(refyear,100)  
if(sect1(11) .eq. 0) sect1(11) = 100
sect1(12) = refmonth   
sect1(13) = refday  
sect1(14) = refhour 
sect1(15) = refmin
! may want to adjust this section depending on var and time      
sect1(16) = 0
!sect1(17) = nint (curtime/60.)
!sect1(18) = 0
call mvbits(nint(curtime/60.),8,8,sect1(17),0)
call mvbits(nint(curtime/60.),0,8,sect1(18),0)
sect1(19) = 10
!
sect1(20) = 0
sect1(21) = 0 
if(refyear .gt. 2000) then 
   sect1(22) = 21
else
   sect1(22) = 20
endif
sect1(23) = 0
sect1(24) = dscale
       
sect2(1) = 0    
sect2(2) = 255  
sect2(3) = 0 
sect2(4) = nllon  
sect2(5) = nllat
if(swllat .lt. 0.0) then
   sect2(6) = 1          
   sect2(7) = nint (1000.0 * abs(swllat)) 
else
   sect2(6) = 0         
   sect2(7) = nint (1000.0 * swllat)  
endif
if(swllon .lt. 0.0) then
   sect2(8) = 1   
   sect2(9) = nint (1000.0 * abs(swllon)) 
else
   sect2(8) = 0   
   sect2(9) = nint (1000.0 * swllon) 
endif
sect2(10) = 136
nellat = (swllat+dllat*(nllat-1))
nellon = (swllon+dllon*(nllon-1))
if(nellat .lt. 0.0) then
   sect2(11) = 1   
   sect2(12) = nint (1000.0 * abs(nellat))
else
   sect2(11) = 0  
   sect2(12) = nint (1000.0 * nellat) 
endif
if(nellon .lt. 0.0) then
   sect2(13) = 1  
   sect2(14) = nint (1000.0 * abs(nellon))
else
   sect2(13) = 0   
   sect2(14) = nint (1000.0 * nellon) 
endif
sect2(15) = nint (1000.0 * dllon) 
sect2(16) = nint (1000.0 * dllat) 
sect2(17) = 64 
sect2(18) = 0  

! Write the Grib binary file.

if((iztran.eq.1.or.iztran.eq.2).and.ivtype.ne.2) then
   ! Get rid of first ZT level since below ground.
   if(nzb.eq.1) nzb=2
elseif(iztran.eq.3) then
   nze=min(nze,nplevs)
endif

if(ivtype.eq.2) then
   nzb=1
   nzee=nzb
   ngrlevs(ivvar)=1
elseif(ivtype.eq.3) then
   nzee=nze
   ngrlevs(ivvar)=(nze-nzb)/izstep+1
endif

do k=nzb,nzee,izstep
   call interp_ll(n1,n2,a(1,1,k),xt,yt  &
                 ,nllon,nllat,swllat,swllon,dllat,dllon  &
                 ,platn,plonn,aa)

   toplev = 0.0
   if(iztran .eq. 3) then
      botlev = iplevs(k)
   else
      botlev = zt(k)
   endif

   if(conv .gt. 0) then 
      aa = aa * 10**conv
   elseif(conv .lt. 0) then
      aa = aa / 10**abs(conv)
   endif

   call gribdat & 
        (igunit,info,sect1,s1opt,sect2,sect3,aa,toplev,botlev,nllon,nllat,1)
enddo

deallocate(aa)
deallocate(toplev)
deallocate(botlev)

return
end subroutine RAMS_grib

!***************************************************************************

subroutine gribtable(var,parm,dscale,conv)

! This subroutine returns the grib parameter number
! and decimal precision scale factor.  It also returns
! a factor of 10 conversion for units change between 
! RAMS hvlib units and GRIB units ie: 2 for mb -> Pa

character(len=*), intent(in)  :: var
integer,          intent(out) :: parm,dscale,conv
integer                       :: lv

lv=lastchar(var)
conv=0

if( var(1:lv) .eq. 'topo') then
   parm = 8
   dscale = 0
   return
!elseif( var(1:lv) .eq. 'u') then
!   parm = 33
!   dscale = 1
!   return
!elseif( var(1:lv) .eq. 'v') then
!   parm = 34
!   dscale = 1
!   return
elseif( var(1:lv) .eq. 'ue_avg') then
   parm = 33
   dscale = 1
   return
elseif( var(1:lv) .eq. 've_avg') then
   parm = 34
   dscale = 1
   return
elseif( var(1:lv) .eq. 'w_avg') then
   parm = 40
   dscale = 2
   return
elseif( var(1:lv) .eq. 'speed') then
   parm = 32
   dscale = 1
   return
elseif( var(1:lv) .eq. 'direction') then
   parm = 31
   dscale = 0
   return
elseif( var(1:lv) .eq. 'press') then
   parm = 1
   dscale = 0 
   conv = 2
   return
elseif( var(1:lv) .eq. 'sea_press') then
   parm = 2
   dscale = 0
   conv = 2
   return
elseif( var(1:lv) .eq. 'theta') then
   parm = 13
   dscale = 1
   return
elseif( var(1:lv) .eq. 'tempk') then
   parm = 11
   dscale = 1
   return
elseif( var(1:lv) .eq. 'theta_e') then
   parm = 14
   dscale = 1
   return
elseif( var(1:lv) .eq. 'vapor') then
   parm = 53
   dscale = 9
   conv = -3
   return
elseif( var(1:lv) .eq. 'cloud') then
   parm = 220
   dscale = 9
   conv = -3
   return
elseif( var(1:lv) .eq. 'rain') then
   parm = 222
   dscale = 9
   conv = -3
   return
elseif( var(1:lv) .eq. 'pristine') then
   parm = 221
   dscale = 9
   conv = -3
   return
elseif( var(1:lv) .eq. 'snow') then
   parm = 223
   dscale = 9
   conv = -3
   return
elseif( var(1:lv) .eq. 'graup') then
   parm = 195
   dscale = 9
   conv = -3
   return
elseif( var(1:lv) .eq. 'dewptk') then
   parm = 17
   dscale = 1
   return
elseif( var(1:lv) .eq. 'relhum') then
   parm = 52
   dscale = 1
   return
elseif( var(1:lv) .eq. 'geo') then
   parm = 7
   dscale = 1
   return
elseif( var(1:lv) .eq. 'precip') then
   parm = 61
   dscale = 1
   return
elseif( var(1:lv) .eq. 'totpcp') then
   parm = 62
   dscale = 1
   return
elseif( var(1:lv) .eq. 'acccon') then
   parm = 63
   dscale = 1
   return
elseif( var(1:lv) .eq. 'sens_flux') then
   parm = 122
   dscale = 1
   return
elseif( var(1:lv) .eq. 'lat_flux') then
   parm = 121
   dscale = 1
   return
elseif( var(1:lv) .eq. 'umom_flx') then
   parm = 124
   dscale = 1
   return
elseif( var(1:lv) .eq. 'vmom_flx') then
   parm = 125
   dscale = 1
   return
elseif( var(1:lv) .eq. 'pbl_ht') then
   parm = 67
   dscale = 0
   return
else
   parm = -1
   dscale = 0
   return
endif

end subroutine gribtable

!***************************************************************************

subroutine RAMS_gribfile (igunit,fileaction,iztran,ngd,iyear1,imonth1  &
                         ,idate1,itime1,revpref)

implicit none

integer, intent(in) :: igunit
integer, intent(in) :: ngd
integer, intent(in) :: iztran
integer, intent(in) :: iyear1
integer, intent(in) :: imonth1
integer, intent(in) :: idate1
integer, intent(in) :: itime1
character(len=*)    :: fileaction,revpref

character(len=1)    :: ftran
character(len=256)  :: flnm,flnm1
integer             :: lastslash,nval

! Open/Close the Grib binary file and keep track of write records.
!print*,'itime1,year,month,day= ',itime1,iyear1,imonth1,idate1

if(fileaction.eq.'OPEN') then

   ! binary filename
   if(iztran.eq.3) then
      ftran='P'
   elseif(iztran.eq.2) then
      ftran='C'
   elseif(iztran.eq.1) then
      ftran='S'
   endif
   call RAMS_get_cdata(0,1,flnm1,nval)
   write(flnm,'(2a,2a1,i4.4,a1,i2.2,a1,i2.2,a1,i4.4,a4,i1,a4)' )  &
       revpref(1:len_trim(revpref))  &
      ,flnm1(lastslash(flnm1)+1:len_trim(flnm1)-27),ftran,'-'  &
      ,iyear1,'-',imonth1,'-',idate1,'-',itime1,'00-g',ngd,'.grb'
   print*
   print*,'GRIB binary file: ',flnm(1:len_trim(flnm))

   open(igunit,file=flnm(1:len_trim(flnm)),form='unformatted',action='write')
        
elseif(fileaction.eq.'CLOSE') then

   close(igunit)
   
endif

return
end subroutine RAMS_gribfile

!***************************************************************************
!----------------------------------------------------------------------
!----------------------------------------------------------------------

SUBROUTINE GRIBDAT (IGRIB, INFO, SECT1, S1OPT, SECT2, SECT3, DUM3D, &
     TOPLEV, BOTLEV, IDIM, JDIM, KDIM)

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!--   
!--   NAME: GRIBDATA
!--
!--   PURPOSE
!--   =======
!--   CALL GRIB UTILITIES AND WRITE OUT THE GRIB FILE
!--
!--   VARIABLES
!--   =========...................DESCRIPTION..........................
!--   BITNUM   STARTING BIT OF WORDNO TO LOAD GRIB INFO
!--   BOTLEV   HOLDS ARRAY OF SECT1(9) VALUES
!--   BUFFSZ   DIMENSION OF GRBOUT ARRAY
!--   DUM3D    INPUT ARRAY OF DATA TO BE GRIBBED
!--   GRBOUT   ARRAY THAT HOLDS GRIBBED FIELD
!--   IDIM     I DIMENSION OF GRIBBED FIELD
!--   IGRIB    OUTPUT FILE UNIT
!--   IMIN     MINIMUM VALUE OF FIELD TO BE GRIBBED
!--   INFO     SIZES OF GRIB SECTIONS
!--   INGRID   FIELD TO BE GRIBBED AFTER SCALING AND
!--            CONVERSION TO INTEGER
!--   ISIZE    SIZE OF EACH GRIB SECTION IN BITS
!--   ISTAT    I/O STATUS
!--   ITOTBT   NUMBER OF BITS FOR ENTIRE GRIB RECORD
!--   JDIM     J DIMENSION OF GRIBBED FIELD
!--   K        LOOP INDEX
!--   KDIM     K DIMENSION OF GRIBBED FIELD
!--   NBTGRD   MINIMUM NUMBER OF BITS REQUIRED FOR FIELD BEING GRIBBED
!--   OUTBIT   ENDING BIT OF OUTWRD
!--   OUTWRD   ENDING WORD OF GRBOUT ARRAY
!--   SCALE    SCALING FACTOR FOR GRIBBED FIELD
!--   SECT1    SECTION 1 INFORMATION (SEE GRIB MANUAL FOR DETAILS)
!--   SECT2    SECTION 2 INFORMATION
!--   SECT3    SECTION 3 INFORMATION (NOT USED)
!--   S1OPT    OPTIONAL SECTION 1 INFORMATION (NOT USED)
!--   TOPLEV   HOLDS ARRAY OF SECT1(8) VALUES
!--   WORDNO   STARTING WORD OF ARRAY GRBOUT TO LOAD GRIB INFO
!--
!----------------------------------------------------------------------
!----------------------------------------------------------------------

  IMPLICIT NONE
  
  INTEGER,   INTENT(IN)        :: IDIM
  INTEGER,   INTENT(IN)        :: JDIM
  INTEGER,   INTENT(IN)        :: KDIM
  INTEGER                      :: BITNUM
  REAL,      INTENT(IN)        :: BOTLEV  (KDIM)
  INTEGER                      :: BUFFSZ
  REAL,      INTENT(IN)        :: DUM3D   (IDIM,JDIM,KDIM)
  INTEGER,   ALLOCATABLE       :: GRBOUT  ( : )
  INTEGER,   INTENT(IN)        :: IGRIB
  INTEGER                      :: IMIN
  INTEGER,   INTENT(INOUT)     :: INFO    (5)
  INTEGER                      :: INGRID  (IDIM, JDIM)
  INTEGER                      :: ISIZE   (0:5)
  INTEGER                      :: ISTAT
  INTEGER                      :: ITOTBT
  INTEGER                      :: K
  INTEGER                      :: NBTGRD
  INTEGER                      :: OUTBIT
  INTEGER                      :: OUTWRD
  INTEGER                      :: SCALE
  INTEGER,   INTENT(INOUT)     :: SECT1   (36)
  INTEGER,   INTENT(IN)        :: SECT2   (*)
  INTEGER,   INTENT(IN)        :: SECT3   (1)
  INTEGER,   INTENT(IN)        :: S1OPT   (1)
  REAL,      INTENT(IN)        :: TOPLEV  (KDIM)
  INTEGER                      :: WORDNO

  SCALE = 10 ** SECT1(24)
  DO K = 1, KDIM
     SECT1(8) = NINT(TOPLEV(K))
     SECT1(9) = NINT(BOTLEV(K))
     WORDNO   = 1
     BITNUM   = 0
     INGRID   = NINT(SCALE * DUM3D(:,:,K))
     
!----------------------------------------------------------------------
!--     CALL ROUTINE GRIBPR TO DETERMINE SIZE OF GRIB MESSAGE.
!--     THEN, ALLOCATE THE SIZE OF VARIABLE GRBOUT EXACTLY.
!----------------------------------------------------------------------

     CALL GRIBPR (WORDNO, BITNUM, INGRID, INFO, IMIN, NBTGRD, &
          ISIZE, SECT1, ITOTBT, BUFFSZ)
     INFO(1)  = BUFFSZ
     
     ! mjb - needed to delallocate this - not sure why - but core
     !       dumped on our origin 200
     if(allocated(GRBOUT)) deallocate (GRBOUT); ALLOCATE (GRBOUT(BUFFSZ))

!----------------------------------------------------------------------
!--     CALL GRIB ROUTINES TO BUILD GRIB MESSAGE. THEN WRITE IT
!--     THE GRIB FILE.
!----------------------------------------------------------------------

     CALL GRIB (GRBOUT, WORDNO, BITNUM, OUTWRD, OUTBIT, INFO, &
          SECT1, S1OPT, SECT2, SECT3, INGRID, IMIN, NBTGRD,   &
          ISIZE, ITOTBT)
     WRITE (IGRIB, IOSTAT = ISTAT, ERR = 9000)  GRBOUT(1:OUTWRD)
     DEALLOCATE ( GRBOUT )
  ENDDO
  
  RETURN

!----------------------------------------------------------------------
!--   ERROR HANDLING SECTION.
!----------------------------------------------------------------------

9000 PRINT*,' '
  PRINT*,'*********************************************'
  PRINT*,'*** SUBROUTINE: GRIBDAT'
  PRINT*,'***   ERROR WRITING GRIB FILE'
  PRINT*,'***   ISTAT IS ', ISTAT
  PRINT*,'*********************************************'
  CALL ABORT
  RETURN  
  
END SUBROUTINE GRIBDAT

!----------------------------------------------------------------------
!----------------------------------------------------------------------

SUBROUTINE GRIBPR(IWORD,IBIT,INGRID,INFO,IMIN,NBTGRD,ISIZE,   &
     SECT1,ITOTBT, BUFFSZ)

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!-- 
!-- NAME:     GRIBPR - GRIB PREPROCESSOR  (F90)   
!-- 
!-- PURPOSE:  THIS SUBROUTINE DETERMINES MAX & MIN VALUES OF GRID, ADJUSTS  
!--	      THE ARRAY SO THAT 0 IS THE MIN, DETERMINES THE NUMBER OF	
!--	      BITS REQUIRED FOR A GRID POINT, DETERMINES SIZE OF SECTIONS, 
!--	      AND DETERMINES THE SIZE OF THE ARRAY THAT WILL HOLD
!--           THE GRIB MESSAGE.	
!-- 
!-- METHOD:   LOOP  
!--		DETERMINE THE MAX AND MIN VALUES OF THE ARRAY	
!--	      IF MIN VALUE NOT EQUAL TO ZERO
!--		LOOP
!--		  RESET ALL THE VALUES OF THE ARRAY 
!--	      TAKE THE LOG TO BASE 2 OF RANGE TO DETERMINE BITS REQUIRED
!--	      DETERMINE THE SIZE OF THE SECTIONS
!--	      DETERMINE THE SIZE OF THE ARRAY THAT WILL HOLD GRIB MESSAGE
!--	      RETURN
!-- 
!----------------------------------------------------------------------
!----------------------------------------------------------------------

  IMPLICIT NONE
 
  INTEGER, INTENT(IN)    :: IWORD      ! ELEMENT OF GRBOUT TO UTILIZE 
  INTEGER, INTENT(IN)    :: IBIT       ! BIT OF IWORD TO UTILIZE	
  INTEGER, INTENT(INOUT) :: INGRID(*)  ! INTEGER ARRAY WITH GRID TO GRIB
  INTEGER, INTENT(IN)    :: INFO(5)    ! SIZES OF VARIABLE ARRAYS 
  INTEGER, INTENT(OUT)   :: IMIN       ! MIN VALUE OF PASSED GRID 
  INTEGER, INTENT(OUT)   :: NBTGRD     ! NUMBER OF BITS REQUIRED FOR EACH VAL 
  INTEGER, INTENT(OUT)   :: ISIZE(0:5) ! SIZES OF EACH SECTION IN BITS
  INTEGER, INTENT(IN)    :: SECT1(*)   ! SECTION 1 BUFFER 
  INTEGER, INTENT(OUT)   :: ITOTBT     ! TOTAL BITS AVAILABLE IN GRBOUT BUFFER
  INTEGER, INTENT(OUT)   :: BUFFSZ     ! SIZE OF ARRAY THAT WILL HOLD GRIB MSG
  REAL                   :: DIFF       ! RANGE OF PASSED GRID 
  INTEGER                :: I	       ! DO LOOP COUNTER	
  INTEGER                :: IMAX       ! MAX VALUE OF PASSED GRID 

  ISIZE(0) =  64
  ISIZE(1) = 224
  ISIZE(2) =   0
  ISIZE(3) =   0
  ISIZE(4) =  88
  ISIZE(5) =  32
  IMIN = INGRID(1)	
  IMAX = INGRID(1)	

!----------------------------------------------------------------------
!--   LOOP TO FIND THE BIGGEST AND SMALLEST VALUES  
!--   OFFSET ALL VALUES SO THAT ALL ARE POSITIVE
!----------------------------------------------------------------------
    
  DO I = 2, INFO(5) 
     IF(INGRID(I) .LT. IMIN) IMIN = INGRID(I)
     IF(INGRID(I) .GT. IMAX) IMAX = INGRID(I)
  ENDDO
  if(IMIN .NE. 0) THEN 
     DO I = 1, INFO(5)	
        INGRID(I) = INGRID(I) - IMIN	
     ENDDO
  ENDIF

!----------------------------------------------------------------------
!--   DETERMINE THE NUMBER OF BITS REQUIRED FOR EACH GRID POINT 
!--	 UNUSAL "ROUNDING" DUE TO TRUNCATION POTENTIAL OF LOG	
!----------------------------------------------------------------------
    
  DIFF = IMAX - IMIN
  if(DIFF .LE. 1.01) THEN	
     NBTGRD = 1  
  ELSE  
     NBTGRD = LOG(DIFF + 1.0) / LOG(2.0) + 0.99999998
  ENDIF

!----------------------------------------------------------------------
!--   DETERMINE SIZE OF EACH INDIVIDUAL SECTION AND TOTAL   
!----------------------------------------------------------------------
    
  DO I = 25,36 
     if(SECT1(I) .GT. 0) ISIZE(1) = 320
  ENDDO
  if(INFO(2) .NE. 0) ISIZE(1) = 320 + 8 * INFO(2)	
  if(INFO(3) .NE. 0) ISIZE(2) = 8 * INFO(3)  
  if(INFO(4) .NE. 0) ISIZE(3) = 8 * INFO(4)  
  ISIZE(4) = ISIZE(4) + ( (NBTGRD * INFO(5) + 7) / 8 * 8)	
    
!----------------------------------------------------------------------
!--   VERIFY THAT THE NUMBER OF OCTETS WILL BE EVEN
!----------------------------------------------------------------------

  ITOTBT  =	 ISIZE(0) + ISIZE(1) + ISIZE(2) + ISIZE(3) + &
       ISIZE(4) + ISIZE(5) 

  if(MOD(ITOTBT,16) .NE. 0) THEN	
     ISIZE(4) = ISIZE(4) + 8
     ITOTBT   = ITOTBT   + 8
  endif

!----------------------------------------------------------------------
!--   CALCULATE THE SIZE OF THE ARRAY THAT WILL HOLD THE GRIB
!--   MESSAGE.
!----------------------------------------------------------------------
    
  BUFFSZ = ((ITOTBT + 32*(IWORD-1) + IBIT) / 32) + 1
    
  RETURN
END SUBROUTINE GRIBPR

!----------------------------------------------------------------------
!----------------------------------------------------------------------

SUBROUTINE GRIB(GRBOUT,WORDNO,BITNUM,OUTWRD,OUTBIT,INFO,SECT1, &
     S1OPT,SECT2,SECT3,INGRID,IMIN,NBTGRD,ISIZE,ITOTBT)

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!-- 
!--   NAME:    GRIB - DRIVER SUBROUTINE TO GRIB INFO  (F90) 
!-- 
!--   PURPOSE: THE PURPOSE OF THIS SUBROUTINE IS TO GRIB A BUFFER.
!-- 
!--   METHOD:  CALL GRIB0 TO SET UP INFO FOR SECTION 0
!--            CALL GRIB1 TO SET UP INFO FOR SECTION 1
!--            IF THERE IS A SECTION 2 OR 3	
!--              CALL GRIB23 TO SET UP INFO FOR SECTION 2 AND 3  
!--              CALL GRIB4 TO SET UP INFO FOR SECTION 4
!--              CALL GRIB5 TO SET UP INFO FOR SECTION 5
!-- 
!--	COMMON BLOCKS:	
!--	    NONE
!-- 
!--	SUBROUTINES CALLED: 
!--  
!--	  CALL GRIB0(GRBOUT,IWORD,IBIT,IVERSN,ITOTBT)	
!--	  CALL GRIB1(GRBOUT,IWORD,IBIT,SECT1,S1OPT,INFO,ISIZE)	
!--	  CALL GRIB23(GRBOUT,IWORD,IBIT,SECT2,SECT3,INFO,ISIZE) 
!--	  CALL GRIB4(GRBOUT,IWORD,IBIT,INGRID,INFO,NBTGRD,ISIZE,IMIN)	
!--	  CALL GRIB5(GRBOUT,IWORD,IBIT) 
!----------------------------------------------------------------------
!----------------------------------------------------------------------
    
!----------------------------------------------------------------------
!--   DECLARE ARGUMENTS	 (ORDER OF CALL)
!----------------------------------------------------------------------
    
  INTEGER, INTENT(INOUT) :: GRBOUT(*)  ! OUTPUT BUFFER
  INTEGER, INTENT(IN)    :: WORDNO     ! BEGIN WORD OF GRIB DATA	
  INTEGER, INTENT(IN)    :: BITNUM     ! BEGIN BIT OF GRIB DATA	
  INTEGER, INTENT(OUT)   :: OUTWRD     ! END WORD OF GRIB DATA
  INTEGER, INTENT(OUT)   :: OUTBIT     ! END BIT OF GRIB DATA 
  INTEGER, INTENT(IN)    :: INFO(5)    ! SIZES OF GRIB SECTIONS
  INTEGER, INTENT(IN)    :: SECT1(36)  ! INFO FOR SECTION 1   
  INTEGER, INTENT(IN)    :: S1OPT(*)   ! ABNORMAL INFO FOR SECTION 1 (OPT)
  INTEGER, INTENT(IN)    :: SECT2(*)   ! INFO FOR SECTION 2 (OPTIONAL)
  INTEGER, INTENT(IN)    :: SECT3(*)   ! INFO FOR SECTION 3 (OPTIONAL)
  INTEGER, INTENT(IN)    :: INGRID(*)  ! GRID DATA TO GRIB
  INTEGER, INTENT(IN)    :: IMIN       ! MINIMUM VALUE OF INGRID  
  INTEGER, INTENT(IN)    :: NBTGRD     ! NUMBER OF BITS REQ FOR EACH GRID POINT
  INTEGER, INTENT(IN)    :: ISIZE(0:5) ! SIZE OF EACH SECTION (IN BITS)   
  INTEGER, INTENT(IN)    :: ITOTBT     ! TOTAL NUMBER OF BYTES IN REPORT  
  INTEGER                :: IBIT       ! CURRENT BIT POINTER	
  INTEGER                :: IVERSN     ! VERSION NUMBER FOR GRID  
  INTEGER                :: IWORD      ! CURRENT WORD COUNTER 

  IWORD  = WORDNO	
  IBIT   = BITNUM	
  IVERSN = 1

  CALL GRIB0(GRBOUT,IWORD,IBIT,IVERSN,ITOTBT)
  CALL GRIB1(GRBOUT,IWORD,IBIT,SECT1,S1OPT,INFO,ISIZE)
  
  if(SECT1(5) .NE. 0) THEN   
     CALL GRIB23(GRBOUT,IWORD,IBIT,SECT2,SECT3,INFO,ISIZE) 
  endif
  
  CALL GRIB4(GRBOUT,IWORD,IBIT,INGRID,INFO,NBTGRD,ISIZE,IMIN) 
  CALL GRIB5(GRBOUT,IWORD,IBIT)	

  if(IBIT .EQ. 1) THEN	
     OUTWRD = IWORD - 1
     OUTBIT = 32
  ELSE
     OUTWRD = IWORD
     OUTBIT = IBIT - 1 
  ENDIF
    
  RETURN
END SUBROUTINE GRIB

!----------------------------------------------------------------------
!----------------------------------------------------------------------

SUBROUTINE GRIB0 (GRBOUT,IWORD,IBIT,IVERSN,ITOTBT)

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!-- 
!-- NAME:     GRIB0 - GRIB SECTION ZERO (0)   (F90)	
!-- 
!-- PURPOSE:  THIS SUBROUTINE FILLS IN THE 'GRIB' AND VERSION IN SECTION 0  
!-- 
!-- METHOD:   CALL PACKER TO PUT IN THE TOTAL NUMBER OF BYTES IN GRBOUT 
!--	      FOR EACH CHARACTER IN G-R-I-B 
!--		PUT THE CORRESPONDING CHARACTER IN THE GRIB ARRAY   
!--	      CALL PACKER TO PUT THE FOUR CHARACTERS 'GRIB' IN GRBOUT	
!--	      CALL PACKER TO PUT THE VERSION NUMBER IN GRBOUT	
!--	      RETURN
!-- 
!--   COMMON BLOCKS USED:   
!--	 NONE	
!-- 
!--   CALLED BY:
!--	 GRIB	
!-- 
!--   FILES ACCESSED:	
!--	 NONE	
!-- 
!--   SUBROUTINES CALLED:   
!--	CALL PACKER(IARY,INUM,NUMBIT,IBUF,IWORD,IBIT)	
!-- 
!----------------------------------------------------------------------
!----------------------------------------------------------------------
  
  IMPLICIT NONE

  INTEGER, INTENT(INOUT) :: GRBOUT(*) ! PASSED ARRAY TO PUT GRIBBED REPORT   
  INTEGER, INTENT(INOUT) :: IWORD     ! ELEMENT # IN GRBOUT  
  INTEGER, INTENT(INOUT) :: IBIT      ! BIT LOCATION IN IWORD
  INTEGER, INTENT(IN)    :: IVERSN    ! VERSION OF GRID	
  INTEGER, INTENT(IN)    :: ITOTBT    ! TOTAL NUM OF BITS IN GRIBBED REPORT
  INTEGER                :: IGRIB(4)  ! EACH CHARACTER OF 'GRIB' 
  INTEGER                :: INUM      ! # OF ELEMENTS TO GRIB FOR PACKER 
  INTEGER                :: NUMBIT    ! # OF BITS REQUIRED FOR EACH VALUE
  INTEGER                :: NUMOCT    ! TOTAL NUMBER # "BYTES" IN REPORT 

  INUM   = 4
  NUMBIT = 8
  IGRIB(1) = ICHAR('G') 
  IGRIB(2) = ICHAR('R') 
  IGRIB(3) = ICHAR('I') 
  IGRIB(4) = ICHAR('B') 
    
  CALL PACKER(IGRIB(1),INUM,NUMBIT,GRBOUT,IWORD,IBIT)	
  NUMOCT = (ITOTBT+7)/8 
  INUM   =	1   
  NUMBIT = 24
  CALL PACKER(NUMOCT,INUM,NUMBIT,GRBOUT,IWORD,IBIT)
  NUMBIT =	8   
  CALL PACKER(IVERSN,INUM,NUMBIT,GRBOUT,IWORD,IBIT)
    
  RETURN
END SUBROUTINE GRIB0

!----------------------------------------------------------------------
!----------------------------------------------------------------------

SUBROUTINE GRIB1(GRBOUT,IWORD,IBIT,SECT1,S1OPT,INFO,ISIZE)

!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!-- 
!-- NAME:     GRIB1 - GRIB SECTION ONE (1)  
!-- 
!-- PURPOSE:  THIS SUBROUTINE FILLS IN SECTION 1
!-- 
!-- METHOD:   LOAD "DEFAULT" CHARACTERS IN WORK ARRAY	
!--	      LOAD ALL PASSED VALUE IN WORK ARRAY EXCEPT FOR OCTETS 11&12   
!--	      IF OCTETS 11 AND 12 DISTINCT  
!--		 LOAD SEPERATELY
!--	      ELSE  
!--		 LOAD COMBINED VALUE
!--	      PUT WORK ARRAY INTO GRBOUT
!--	      RETURN
!-- 
!--   SUBROUTINES CALLED:   
!--	 CALL PACKER(IARY,INUM,NUMBIT,IBUF,IWORD,IBIT)	
!-- 
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

  IMPLICIT NONE

  INTEGER, INTENT(INOUT) :: GRBOUT(*)  ! ARRAY TO PUT GRIB PRODUCT  
  INTEGER, INTENT(INOUT) :: IWORD      ! ELEMENT NUMBER IN GRBOUT   
  INTEGER, INTENT(INOUT) :: IBIT       ! BIT LOCATION IN IWORD	
  INTEGER, INTENT(IN)    :: SECT1(36)  ! ARRAY WITH SEC 1 TO BE INSERTED 
  INTEGER, INTENT(IN)    :: S1OPT(*)   ! OPTIONAL ARRAY WITH MORE SEC1 INFO 
  INTEGER, INTENT(IN)    :: INFO(5)    ! ARRAY WITH SIZE OF S1OPT (+ OTHERS)
  INTEGER, INTENT(IN)    :: ISIZE(0:5) ! ARRAY WITH SIZES OF EACH SECTION   
  INTEGER                :: INUM       ! # OF ELEMENTS FOR CALL TO PACKER   
  INTEGER                :: ISCALE     ! SCALE FACTOR (POWER OF 10) 
  INTEGER                :: ISIGN      ! SIGN OF SCALE FACTOR	
  INTEGER                :: NUMBIT     ! # OF BITS FOR EACH ELEMENT FOR PACKER
  INTEGER                :: NUMOCT     ! # OF OCTETS FOR THIS SECTION	

  INUM   =	1   
  NUMBIT = 24   
  NUMOCT = ISIZE(1) / 8 
  CALL PACKER(NUMOCT,INUM,NUMBIT,GRBOUT,IWORD,IBIT) 
  INUM   =	7   
  NUMBIT =	8   
  CALL PACKER(SECT1(1),INUM,NUMBIT,GRBOUT,IWORD,IBIT)       
  if(SECT1(10) .EQ. 0) THEN
     INUM   =  1 
     NUMBIT =  8 
     CALL PACKER(SECT1(8),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
     INUM   =  1 
     NUMBIT =  8 
     CALL PACKER(SECT1(9),INUM,NUMBIT,GRBOUT,IWORD,IBIT) 
  ELSE  
     INUM   =  1 
     NUMBIT = 16 
     CALL PACKER(SECT1(9),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
  ENDIF
  INUM   =	9   
  NUMBIT =	8   
  CALL PACKER(SECT1(11),INUM,NUMBIT,GRBOUT,IWORD,IBIT)  
  INUM   =	1   
  NUMBIT = 16   
  CALL PACKER(SECT1(20),INUM,NUMBIT,GRBOUT,IWORD,IBIT)  
  INUM   =	3   
  NUMBIT =	8   
  CALL PACKER(SECT1(21),INUM,NUMBIT,GRBOUT,IWORD,IBIT)      
  INUM   =	1   
  ISCALE = SECT1(24)
  if(ISCALE .LT. 0) THEN	
     ISCALE = IABS(ISCALE)	
     NUMBIT = 1  
     ISIGN  = 1  
     CALL PACKER(ISIGN,INUM,NUMBIT,GRBOUT,IWORD,IBIT)
     NUMBIT = 15 
  ELSE  
     NUMBIT = 16 
  ENDIF
  CALL PACKER(ISCALE,INUM,NUMBIT,GRBOUT,IWORD,IBIT) 
  if(ISIZE(1) .GT. 224) THEN   
     INUM   = 12 
     NUMBIT =  8 
     CALL PACKER(SECT1(25),INUM,NUMBIT,GRBOUT,IWORD,IBIT)    
     if(INFO(2) .GT. 0) THEN
        INUM   =  INFO(2) 
        NUMBIT =  8	
        CALL PACKER(S1OPT(1),INUM,NUMBIT,GRBOUT,IWORD,IBIT)	
     ENDIF
  ENDIF

  RETURN
END SUBROUTINE GRIB1

!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

SUBROUTINE GRIB23 (GRBOUT,IWORD,IBIT,SECT2,SECT3,INFO,ISIZE)  

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!-- 
!-- NAME:     GRIB23 - GRIB SECTIONS 2 AND 3   (FTN77)	
!-- 
!-- PURPOSE:  THIS SUBROUTINE SHOULD SET UP SECTIONS 2 & 3 FOR GRIB.
!-- 
!-- METHOD:   IF THERE IS EITHER SECTION 2 OR 3 
!--	      IF THERE IS A SECTION 2 
!--		FILL IN SECTION 2 
!--	      IF THERE IS A SECTION 3 
!--		FILL IN SECTION 3 
!--	    RETURN
!-- 
!-- 
!--* CALLING VARIABLES:	
!--* GRBOUT  IO   INTEGER ARRAY TO RETURN COMPLETE GRIB PRODUCT
!--* IWORD   IO   THE CURRENT ELEMENT OF GRBOUT
!--* IBIT    IO   THE CURRENT BIT LOCATION OF IWORD
!--* SECT2   I   ARRAY FOR FILLING IN GRID IDENTIFIER (SECT 2)
!--* SECT3   I   ARRAY FOR FILLING IN GRID IDENTIFIER (SECT 3)
!--* INFO    I   ARRAY WITH SIZES OF SECT2 AND SECT3	
!--* ISIZE   I   ARRAY WITH SIZES OF ALL SECTIONS 
!-- 
!-- SUBROUTINES CALLED:   
!--    CALL PACKER(IARY,INUM,NUMBIT,IBUF,IWORD,IBIT)	
!----------------------------------------------------------------------
!----------------------------------------------------------------------
 
  IMPLICIT NONE

  INTEGER, INTENT(INOUT) :: GRBOUT(*)  ! ARRAY TO PUT GRIB PRODUCT  
  INTEGER, INTENT(INOUT) :: IWORD      ! ELEMENT NUMBER IN GRBOUT   
  INTEGER, INTENT(INOUT) :: IBIT       ! BIT LOCATION IN IWORD	
  INTEGER, INTENT(IN)    :: SECT2(*)   ! ARRAY WITH SEC 2 TO BE INSERTED 
  INTEGER, INTENT(IN)    :: SECT3(*)   ! OPTIONAL ARRAY WITH SEC 3 INFO
  INTEGER, INTENT(IN)    :: INFO(5)    ! ARRAY WITH SIZE OF SECT (+ OTHERS)
  INTEGER, INTENT(IN)    :: ISIZE(0:5) ! ARRAY WITH SIZES OF EACH SECTION    
  INTEGER                :: INUM       ! # OF ELEMENTS TO GRIB FOR PACKER 
  INTEGER                :: NUMBIT     ! # OF BITS REQUIRED FOR EACH VALUE
  INTEGER                :: NUMOCT     ! TOTAL NUMBER # "BYTES" IN REPORT 

  if(INFO(3) .GT. 2  .OR. INFO(4) .GT. 2) THEN
     if(INFO(3) .GT. 2) THEN
        INUM   =  1	
        NUMBIT = 24	
        NUMOCT = ISIZE(2) / 8 
        CALL PACKER(NUMOCT,INUM,NUMBIT,GRBOUT,IWORD,IBIT) 
        INUM   =  1  
        NUMBIT =  8	
        CALL PACKER(SECT2(1),INUM,NUMBIT,GRBOUT,IWORD,IBIT)		
        INUM   =  1  
        NUMBIT =  8	
        CALL PACKER(SECT2(2),INUM,NUMBIT,GRBOUT,IWORD,IBIT)	
        INUM   =  1
        NUMBIT =  8	
        CALL PACKER(SECT2(3),INUM,NUMBIT,GRBOUT,IWORD,IBIT)	
        INUM   =  1  
        NUMBIT = 16	
        CALL PACKER(SECT2(4),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
        INUM   =  1
        NUMBIT = 16	
        CALL PACKER(SECT2(5),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
        if(SECT2(6) .EQ. 1) THEN
           INUM   = 1
           NUMBIT = 1  
           CALL PACKER(SECT2(6),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           INUM   =  1
           NUMBIT = 23
           CALL PACKER(SECT2(7),INUM,NUMBIT,GRBOUT,IWORD,IBIT)            
        ELSE
           INUM   =  1
           NUMBIT = 24
           CALL PACKER(SECT2(7),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
        ENDIF
        if(SECT2(8) .EQ. 1) THEN
           INUM   = 1
           NUMBIT = 1  
           CALL PACKER(SECT2(8),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           INUM   =  1
           NUMBIT = 23
           CALL PACKER(SECT2(9),INUM,NUMBIT,GRBOUT,IWORD,IBIT)            
        ELSE
           INUM   =  1
           NUMBIT = 24
           CALL PACKER(SECT2(9),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
        ENDIF
        INUM   = 1
        NUMBIT = 8	
        CALL PACKER(SECT2(10),INUM,NUMBIT,GRBOUT,IWORD,IBIT)

!----------------------------------------------------------------------
!--	  IF GRID TYPE=5 THEN BUILD GDS FOR POLAR STEREOGRAPHIC
!----------------------------------------------------------------------

        if(SECT2(3) .EQ. 5) THEN
           if(SECT2(11) .GT. 0) THEN
              INUM   = 1
              NUMBIT = 1  
              CALL PACKER(SECT2(11),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
              INUM   =  1
              NUMBIT = 23
              CALL PACKER(SECT2(12),INUM,NUMBIT,GRBOUT,IWORD,IBIT)            
           ELSE
              INUM   =  1
              NUMBIT = 24
              CALL PACKER(SECT2(12),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           ENDIF
           INUM   =  1
           NUMBIT = 24
           CALL PACKER(SECT2(13),INUM,NUMBIT,GRBOUT,IWORD,IBIT) 
           INUM   =  1
           NUMBIT = 24
           CALL PACKER(SECT2(14),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           INUM   = 1
           NUMBIT = 8
           CALL PACKER(SECT2(15),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           INUM   = 1
           NUMBIT = 8
           CALL PACKER(SECT2(16),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           INUM   =  1
           NUMBIT = 32
           CALL PACKER(SECT2(17),INUM,NUMBIT,GRBOUT,IWORD,IBIT)

!----------------------------------------------------------------------
!--	  elseif TYPE=1 THEN BUILD GDS FOR MERCATOR
!----------------------------------------------------------------------

        elseif(SECT2(3) .EQ. 1) THEN
           if(SECT2(11) .GT. 0) THEN
              INUM   = 1
              NUMBIT = 1  
              CALL PACKER(SECT2(11),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
              INUM   =  1
              NUMBIT = 23
              CALL PACKER(SECT2(12),INUM,NUMBIT,GRBOUT,IWORD,IBIT)            
           ELSE
              INUM   =  1
              NUMBIT = 24
              CALL PACKER(SECT2(12),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           ENDIF
           if(SECT2(13) .GT. 0) THEN
              INUM   = 1
              NUMBIT = 1  
              CALL PACKER(SECT2(13),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
              INUM   =  1
              NUMBIT = 23
              CALL PACKER(SECT2(14),INUM,NUMBIT,GRBOUT,IWORD,IBIT)            
           ELSE
              INUM   =  1
              NUMBIT = 24
              CALL PACKER(SECT2(14),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           ENDIF
           INUM   =  1
           NUMBIT = 24
           CALL PACKER(SECT2(15),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           INUM   = 1
           NUMBIT = 8
           CALL PACKER(SECT2(16),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           INUM   = 1
           NUMBIT = 8
           CALL PACKER(SECT2(17),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           INUM   =  1
           NUMBIT = 24
           CALL PACKER(SECT2(18),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           INUM   =  1
           NUMBIT = 24
           CALL PACKER(SECT2(19),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           INUM   =  1
           NUMBIT = 64
           CALL PACKER(SECT2(20),INUM,NUMBIT,GRBOUT,IWORD,IBIT)

!----------------------------------------------------------------------
!--	  elseif TYPE=3 THEN BUILD GDS FOR LAMBERT CONFORMAL
!----------------------------------------------------------------------

        ELSEif(SECT2(3) .EQ. 3) THEN
           if(SECT2(11) .GT. 0) THEN
              INUM   = 1
              NUMBIT = 1  
              CALL PACKER(SECT2(11),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
              INUM   =  1
              NUMBIT = 23
              CALL PACKER(SECT2(12),INUM,NUMBIT,GRBOUT,IWORD,IBIT)            
           ELSE
              INUM   =  1
              NUMBIT = 24
              CALL PACKER(SECT2(12),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           ENDIF
           INUM   =  1
           NUMBIT = 24
           CALL PACKER(SECT2(13),INUM,NUMBIT,GRBOUT,IWORD,IBIT) 
           INUM   =  1
           NUMBIT = 24
           CALL PACKER(SECT2(14),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           INUM   = 1
           NUMBIT = 8
           CALL PACKER(SECT2(15),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           INUM   = 1
           NUMBIT = 8
           CALL PACKER(SECT2(16),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           if(SECT2(17) .GT. 0) THEN
              INUM   = 1
              NUMBIT = 1  
              CALL PACKER(SECT2(17),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
              INUM   =  1
              NUMBIT = 23
              CALL PACKER(SECT2(18),INUM,NUMBIT,GRBOUT,IWORD,IBIT)            
           ELSE
              INUM   =  1
              NUMBIT = 24
              CALL PACKER(SECT2(18),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           ENDIF
           if(SECT2(19) .GT. 0) THEN
              INUM   = 1
              NUMBIT = 1  
              CALL PACKER(SECT2(19),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
              INUM   =  1
              NUMBIT = 23
              CALL PACKER(SECT2(20),INUM,NUMBIT,GRBOUT,IWORD,IBIT)            
           ELSE
              INUM   =  1
              NUMBIT = 24
              CALL PACKER(SECT2(20),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           ENDIF
           INUM   = 1
           NUMBIT = 1  
           CALL PACKER(SECT2(21),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           INUM   =  1
           NUMBIT = 23
           CALL PACKER(SECT2(22),INUM,NUMBIT,GRBOUT,IWORD,IBIT)            
           if(SECT2(23) .GT. 0) THEN
              INUM   = 1
              NUMBIT = 1  
              CALL PACKER(SECT2(23),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
              INUM   =  1
              NUMBIT = 23
              CALL PACKER(SECT2(24),INUM,NUMBIT,GRBOUT,IWORD,IBIT)            
           ELSE
              INUM   =  1
              NUMBIT = 24
              CALL PACKER(SECT2(24),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           ENDIF
           INUM   = 1
           NUMBIT = 16
           CALL PACKER(SECT2(25),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           
!----------------------------------------------------------------------
!--	  elseif TYPE=0 THEN BUILD GDS FOR LATITUDE/LONGITUDE GRID
!----------------------------------------------------------------------

        ELSEif(SECT2(3) .EQ. 0) THEN
           if( SECT2(11) .GT. 0 ) THEN
              INUM = 1
              NUMBIT  = 1
              CALL PACKER (SECT2(11),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
              INUM = 1
              NUMBIT  = 23
              CALL PACKER (SECT2(12),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           ELSE
              INUM = 1
              NUMBIT  = 24
              CALL PACKER (SECT2(12),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           endif
           if( SECT2(13) .GT. 0 ) THEN
              INUM = 1
              NUMBIT  = 1
              CALL PACKER (SECT2(13),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
              INUM = 1
              NUMBIT  = 23
              CALL PACKER (SECT2(14),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           ELSE
              INUM = 1
              NUMBIT  = 24
              CALL PACKER (SECT2(14),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           endif
           INUM = 1
           NUMBIT  = 16
           CALL PACKER (SECT2(15),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           INUM = 1
           NUMBIT  = 16
           CALL PACKER (SECT2(16),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           INUM = 1
           NUMBIT  = 8
           CALL PACKER (SECT2(17),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
           INUM = 1
           NUMBIT  = 32
           CALL PACKER (SECT2(18),INUM,NUMBIT,GRBOUT,IWORD,IBIT)
        endif
     ENDIF

!----------------------------------------------------------------------
!--	SET UP SECTION 3
!----------------------------------------------------------------------
    
     if(INFO(4) .GT. 2) THEN
        INUM   =  1	
        NUMBIT = 24	
        NUMOCT = ISIZE(3) / 8 
        CALL PACKER(NUMOCT,INUM,NUMBIT,GRBOUT,IWORD,IBIT) 
        INUM   = INFO(3)  
        NUMBIT =  8	
        CALL PACKER(SECT3(1),INUM,NUMBIT,GRBOUT,IWORD,IBIT)	
     ENDIF
     
  ENDIF
  
  RETURN
END SUBROUTINE GRIB23

!----------------------------------------------------------------------
!----------------------------------------------------------------------

SUBROUTINE GRIB4(GRBOUT,IWORD,IBIT,INGRID,INFO,NBTGRD,ISIZE,IMIN) 

!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!-- 
!-- NAME:     GRIB4 - GRIB SECTION FOUR (4)   (FTN77)	
!-- 
!-- PURPOSE:  THIS SUBROUTINE FILLS THE GUTS OF THE GRIB INFO (SECTION 4)   
!-- 
!-- METHOD:   CALL PACKER TO PUT IN 
!--		   THE TOTAL NUMBER OF BYTES IN SECTION 4   
!--		   THE NUMBER OF EXTRA BITS AT THE END OF THE SECTION	
!--		   THE SCALING FACTOR (CURRENTLY ZERO)	
!--		   THE NUMBER OF BITS REQUIRED FOR EACH GRID POINT  
!--	      CALL CNVRTI TO PUT THE MINIMUM VALUE IN IBM FORMAT
!--	      CALL PACKER TO PUT IN 
!--		   ALL THE 'NORMALIZED' GRID VALUES 
!--		   IF REQUIRED, THE 'FILL' BITS 
!--	      RETURN
!-- 
!--   SUBROUTINES CALLED:   
!-- 
!--	 CALL PACKER(IARY,INUM,NUMBIT,IBUF,IWORD,IBIT)	
!--	 CALL CNVRTI(IMIN,ISCALE)   
!-- 
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

  IMPLICIT NONE    
    
  INTEGER, INTENT(INOUT) :: GRBOUT(*)  ! PASSED ARRAY TO PUT GRIBBED REPORT
  INTEGER, INTENT(INOUT) :: IWORD      ! ELEMENT # IN GRBOUT	
  INTEGER, INTENT(INOUT) :: IBIT       ! BIT LOCATION IN IWORD 
  INTEGER, INTENT(IN)    :: INGRID(*)  ! 'NORMALIZED' GRIDDED VALUE
  INTEGER, INTENT(IN)    :: INFO(5)    ! ARRAY WITH NUMBER OF OF GRID POINTS
  INTEGER, INTENT(IN)    :: NBTGRD     ! # OF BITS REQUIRED FOR EACH GRID VALUE
  INTEGER, INTENT(IN)    :: ISIZE(0:5) ! ARRAY WITH THE SIZE OF INGRID 
  INTEGER, INTENT(IN)    :: IMIN       ! VALUE OF MINIMUM GRID   
  INTEGER                :: IFILL      ! # OF FILL BITS
  INTEGER                :: INUM       ! # OF ELEMENTS TO GRIB FOR PACKER  
  INTEGER                :: ISCALE     ! SCALE FACTOR FOR DATA 
  INTEGER                :: NUMBIT     ! # OF BITS REQUIRED FOR EACH VALUE 
  INTEGER                :: NUMGRD     ! # OF BITS PER GRID POINT  
  INTEGER                :: NUMOCT     ! TOTAL NUMBER # "BYTES" IN REPORT  
  INTEGER                :: NUMXTR     ! # OF EXTRA BITS IN LAST OCTET 

  INUM   =	1   
  NUMBIT = 24   
  NUMOCT = ISIZE(4) / 8 
  CALL PACKER(NUMOCT,INUM,NUMBIT,GRBOUT,IWORD,IBIT) 
  INUM   =	1   
  NUMBIT =	8   
  NUMXTR = ISIZE(4) - (11*8 + NBTGRD*INFO(5))   
  CALL PACKER(NUMXTR,INUM,NUMBIT,GRBOUT,IWORD,IBIT) 
  INUM   =	1   
  NUMBIT = 16   
  ISCALE =	0   
  CALL PACKER(ISCALE,INUM,NUMBIT,GRBOUT,IWORD,IBIT) 
  INUM   =	1   
  NUMBIT = 32
  CALL CNVRTI(IMIN,ISCALE)
  CALL PACKER(ISCALE,INUM,NUMBIT,GRBOUT,IWORD,IBIT) 
  INUM   =	1   
  NUMBIT =	8   
  NUMGRD = NBTGRD	
  CALL PACKER(NUMGRD,INUM,NUMBIT,GRBOUT,IWORD,IBIT) 
  INUM   =	INFO(5) 
  NUMBIT =	NBTGRD	
  CALL PACKER(INGRID(1),INUM,NUMBIT,GRBOUT,IWORD,IBIT)  
  if(NUMXTR .NE. 0) THEN	
     IFILL  =  0 
     INUM   =  1 
     NUMBIT =  NUMXTR
     CALL PACKER(IFILL,INUM,NUMBIT,GRBOUT,IWORD,IBIT)
  ENDIF
  
  RETURN
END SUBROUTINE GRIB4

!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

SUBROUTINE GRIB5(GRBOUT,IWORD,IBIT)   

!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!-- 
!-- NAME:     GRIB5 - GRIB SECTION FIVE (5)   (FTN77)	
!-- 
!-- PURPOSE:  THIS SUBROUTINE FILLS IN THE '7777' IN SECTION 5	AND UPDATES 
!--		     THE WORD COUNT IN SECTION 0
!-- 
!-- METHOD:   PUT CHARACTER '7', RIGHT JUSTIFIED, IN ISEVEN 
!--	      CALL PACKER TO PUT THE FOUR CHARACTERS '7777' IN GRBOUT	
!--	      RETURN
!-- 
!--   SUBROUTINES CALLED:   
!--	 CALL PACKER(IARY,INUM,NUMBIT,IBUF,IWORD,IBIT)	
!-- 
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

  IMPLICIT NONE    
    
  INTEGER, INTENT(INOUT) :: GRBOUT(*) ! BUFFER TO RETURN COMPLETE GRIB PRODUCT
  INTEGER, INTENT(INOUT) :: IWORD     ! CURRENT ELEMENT IN GRBOUT	
  INTEGER, INTENT(INOUT) :: IBIT      ! LOCATION IN IWORD	
  INTEGER                :: I         ! DO LOOP COUNTER 
  INTEGER                :: INUM      ! NUMBER OF WORDS 
  INTEGER                :: ISEVEN    ! VARIABLE TO PUT RIGHT JUSTIFIED '7' 
  INTEGER                :: NUMBIT    ! NUMBER OF BITS  

  INUM   = 1
  NUMBIT = 8
  ISEVEN = ICHAR('7')   
  DO I = 1, 4	
     CALL PACKER(ISEVEN,INUM,NUMBIT,GRBOUT,IWORD,IBIT)  
  ENDDO
  
  RETURN
END SUBROUTINE GRIB5

!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

SUBROUTINE PACKER(IARY,INUM,NUMBIT,GRBOUT,IWORD,IBIT) 

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!-- 
!-- NAME:     PACKER - PACK THE DATA INTO BINARY
!-- 
!-- PURPOSE:  THIS SUBROUTINE PACKS THE DATA INTO THE GRIB BUFFER   
!-- 
!-- METHOD:   FOR EACH VALUE TO BE GRIBBED DO	
!--		IF THERE IS ENOUGH ROOM TO PUT ALL THE BITS 
!--		  PUT THE DATA INTO THE BUFFER	
!--		  UPDATE THE BIT LOCATION   
!--		ELSE
!--		  DETERMINE HOW MANY BITS TO EACH WORD	
!--		  COMPLETE FIRST WORD	
!--		  PUT REST OF BITS IN NEXT WORD 
!--			NOTE: MAX OF 32 BITS MAY BE USED
!--		  UPDATE THE BIT LOCATION   
!--		ENDIF	
!--		IF BITS GO BEYOND A WORD
!--		  UPDATE BIT AND WORD COUNTER	
!--		ENDIF	
!--	      ENDDO 
!--	      RETURN
!-- 
!----------------------------------------------------------------------
!----------------------------------------------------------------------

  IMPLICIT NONE    

  INTEGER, INTENT(IN)    :: IARY(*)   ! INPUT BUFFER
  INTEGER, INTENT(IN)    :: INUM      ! NUMBER OF VALUES TO PACK 
  INTEGER, INTENT(IN)    :: NUMBIT    ! NUMBER OF BITS FOR EACH VALUE
  INTEGER, INTENT(INOUT) :: GRBOUT(*) ! OUTPUT BUFFER
  INTEGER, INTENT(INOUT) :: IWORD     ! WORD OF GRBOUT TO PACK (UPDATED) 
  INTEGER, INTENT(INOUT) :: IBIT      ! BIT OF IWORDTO PACK (UPDATED)
  INTEGER                :: I	      ! DO LOOP COUNTER  
  INTEGER                :: IEND      ! ENDING BIT FOR VALUE 
  INTEGER                :: ILEFT     ! BITS IN LEFT HALF FOR SPLIT VALUE
  INTEGER                :: IRIGHT    ! BITS IN RIGHT HALF FOR SPLIT VALUE
  INTEGER                :: ISTART    ! COMPLEMENT OF IBIT VALUE

  DO I = 1, INUM
     ISTART = (32 - IBIT) - NUMBIT
     IEND = NUMBIT + IBIT
     if(IEND .LE. 32) THEN
        if(NUMBIT .EQ. 32 .AND. ISTART .EQ. 0) THEN
           GRBOUT(IWORD) = IBITS(IARY(I),0,NUMBIT)
        ELSE
           CALL MVBITS(IARY(I), 0, NUMBIT, GRBOUT(IWORD), ISTART)
        ENDIF
        IBIT   = IEND
     ELSE
        IRIGHT = IEND - 32
        ILEFT  = NUMBIT - IRIGHT
        CALL MVBITS(IARY(I), IRIGHT, ILEFT, GRBOUT(IWORD), 0)
        IWORD= IWORD + 1
        CALL MVBITS(IARY(I), 0, IRIGHT,GRBOUT(IWORD), ISTART+32)
        IBIT = IRIGHT
     ENDIF
     if(IBIT .GT. 31) THEN	
        IWORD= IWORD + 1   
        IBIT = IBIT  - 32  
     ENDIF
  ENDDO
  
  RETURN
END SUBROUTINE PACKER

!----------------------------------------------------------------------
!----------------------------------------------------------------------

SUBROUTINE CNVRTI(IMINIMUM,ISCALE)

!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!-- 
!-- NAME:     CNVRTI - CONVERT TO IBM REAL FORMAT    (FTN77)
!-- 
!-- PURPOSE:  THIS SUBROUTINE CONVERTS THE MINIMUM VALUE TO AN 'IBM' 
!--	      REAL FORMAT
!-- 
!-- METHOD:   DETERMINE SIGN AND COMPLIMENT THE REAL VALUE (IF REQUIRED)
!--	      DETERMINE POWER OF 16 
!--	      DETERMINE SIGNIFICANT DIGITS  
!--	      PUT VALUE IN SCALED NUMBER
!--	      RETURN
!-- 
!--   CALLING VARIABLES:
!--	 SUBROUTINE CNVRTI(IMIN,ISCALE) 
!--	      IMIN     I    MINIMUM (OFFSET) VALUE  
!--	      ISCALE	O   SCALED VALUE IN 'IBM' REAL FORMAT	
!-- 
!--      PORTED FROM UNISYS 36 BIT ARCHITECTURE TO SUN 32 BIT 
!--      WORKSTATION ARCHITECTURE. THE FOLLOWING INFORMATION CAN BE
!--      USED TO UNDERSTAND THE DIFFERENCES BETWEEN THE AFLASH UNISYS
!--      GRIB CNVRTI ROUTINE AND THE SUN CNVRTI ROUTINE
!--
!--      SOURCE SINGLE PRECISSION REAL FORMAT
!--      ------------------------------------
!--                        111111111122222222223333333
!--      UNISYS  >123456789012345678901234567890123456< 36 BIT WORD
!--               ||______||_________________________|
!--               S  EXP          MANTISSA
!--
!--                   3322222222221111111111
!--      SUN         >10987654321098765432109876543210< 32 BIT WORD
!--                   ||______||_____________________|
!--                   S  EXP      FRACTION
!--
!--      TARGET SINGLE PRECISSION REAL FORMAT
!--      ------------------------------------
!--
!--                   3322222222221111111111
!--      IBM         >10987654321098765432109876543210< 32 BIT WORD
!--                   ||_____||______________________|
!--                   S  EXP     MANTISSA
!--
!--      S = SIGN BIT; SET TO 1 FOR NEGATIVE VALUES
!--      UNISYS EXP REPRESENTS 2**MANTISSA_EXPONENT  + 128 BIAS
!--      SUN    EXP REPRESENTS 2**EXPONENT           + 127 BIAS
!--      IBM    EXP REPRESENTS 16**MANTISSA_EXPONENT +  64 BIAS
!--
!--      SUN REAL NUMBERS ARE REPRESENTED BY THE FOLLOWING FORM:
!--
!--           (-1)**SIGN !--2**(EXPONENT - BIAS) * 1.FRACTION
!-- 
!--      ISCALE BECOMES THE REFERENCE VALUE 'R' SEE GRIB MANUAL,
!--      EDITION 1. R=THE MINIMUM VALUE OF THE (POSSIBLY DECIMALLY
!--      SCALED DATA THAT IS BEING ENCODED. 
!--
!--      IBM REAL NUMBERS ARE REPRESENTED BY THE FOLLOWING FORM:
!--
!--           (-1)**SIGN * 2**(-24) * MANTISSA * 16(EXPONENT - BIAS) 
!--
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

  IMPLICIT NONE    
  
  INTEGER, INTENT(IN)  :: IMINIMUM   ! MINIMUM (OFFSET) VALUE OF ARRAY   
  INTEGER, INTENT(OUT) :: ISCALE ! VALUE IN 'IBM' REAL FORMAT
  INTEGER              :: IBTNUM ! BIT NUMBER
  INTEGER              :: IMIN   ! MINIMUM (OFFSET) VALUE OF ARRAY
  INTEGER              :: IMOVE  ! NUMBER OF SPACES TO MOVE MANTISSA 
  INTEGER              :: IPOWER ! POWER OF 2 IN SPERRY FORMAT	
  INTEGER              :: IPWR16 ! POWER OF 16 IN 'IBM' FORMAT
  INTEGER*4            :: IRMIN  ! INTEGER EQUIVALENT OF RMIN
  INTEGER              :: ISIGBT ! SIGNIFICANT BITS IN IBM FORMAT	  
  INTEGER              :: ISIGSZ ! SIGNIFICANT BITS FIELD SIZE
  INTEGER              :: ISIGST ! START OF SIGNIFICANT BITS (IBM)
  REAL                 :: RMIN   ! 'REAL' VALUE OF IMIN	
  
  EQUIVALENCE (RMIN, IRMIN) 

  IMIN = IMINIMUM
  IBTNUM = 31
  IMOVE  = 0
  IPOWER = 0
  IPWR16 = 0
  ISIGBT = 0
  ISIGSZ = 0
  ISIGST = 0
  RMIN   = 0.0
  
!---------------------------------------------------------------------- 
!--   SET UP SIGN OF IBM NUMBER 
!---------------------------------------------------------------------- 
       
  if(IMIN .LT. 0) THEN 
     ISCALE = IBSET (ISCALE, IBTNUM)
     IMIN   = -IMIN
  ELSE
     ISCALE = IBCLR (ISCALE, IBTNUM)
  endif
  
!---------------------------------------------------------------------- 
!--   HANDLE EXPONENT AND SIGNIFICANT BITS SEPARATELY.  
!--   THE FOLLOWING BLOCKS OF CODE MODIFIES THE REPRESENTATION OF A SUN
!--   SINGLE PRECISSION(REAL) FLOATING POINT NUMBER TO AN IBM
!--   REPRESENTATION FOR A SINGLE PRECISSION FLOATING POINT NUMBER
!----------------------------------------------------------------------

  if(IMIN .NE. 0) THEN 
     RMIN = FLOAT(IMIN)

!----------------------------------------------------------------------
!--     EXTRACT SUN REAL NUMBER EXPONENT
!--     USE INTEGER EQUIVALENT OF RMIN IN CALL TO MVBITS.  MVBITS
!--     REQUIRES AN INTEGER ARGUMENT.
!----------------------------------------------------------------------

     CALL MVBITS (IRMIN, 23, 8, IPOWER, 0)
     IPOWER = IPOWER - 126

!----------------------------------------------------------------------
!--     CONVERT EXPONENT FROM BASE 2 TO BASE 16 FOR IBM MANTISSA EXP.
!----------------------------------------------------------------------

     IPWR16 = (IPOWER + 3)/4 
     IMOVE  = IPWR16*4 - IPOWER  
     IPWR16 = IPWR16 + 64
     
!----------------------------------------------------------------------
!--     DETERMINE SIGNIFICANT BITS FIELD SIZE FOR THE IBM MANTISSA
!----------------------------------------------------------------------

     ISIGSZ = IPOWER + 1
     
!----------------------------------------------------------------------
!--     WALK BACKWARD FROM BIT 23(IBM) THE LENGHT OF THE SIGNIFICANT 
!--     BITS FIELD. ISIGST REPRESENTS THE POSITION OF THE LEAST 
!--     SIGNIFICANT BIT THAT HOLDS A MANTISSA VALUE 
!----------------------------------------------------------------------

     ISIGST = 24 - IPOWER
                       
!----------------------------------------------------------------------
!--     BUILD IBM MANTISSA
!---------------------------------------------------------------------- 

     CALL MVBITS (IMIN, 0, ISIGSZ, ISIGBT, ISIGST)
     ISIGBT = NINT(ISIGBT/2.0**IMOVE) 

!----------------------------------------------------------------------
!--     BUILD IBM REAL NUMBER
!---------------------------------------------------------------------- 

     CALL MVBITS (IPWR16, 0, 7, ISCALE, 24)
     CALL MVBITS (ISIGBT, 0, 24, ISCALE, 0)
  endif
   
  RETURN
END SUBROUTINE CNVRTI
