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

program revu

include 'vcomm2.h'
include 'frame.h'

character cargv*2,cargs(0:20)*81,cargx*81,revuin*80

external nvfilla

null=char(0)

! argument defaults
revuin='REVU_IN'

! read arguments
numarg=iargc()
do n=0,numarg
   call ugetarg(n,cargx)
   cargs(n)=cargx(1:len_trim(cargx))
enddo
print*
print*,'Starting: ', cargs(0)(1:len_trim(cargs(0)))
print*
if(mod(numarg,2)==1.or.numarg.gt.2) call usage
do n=1,numarg,2
   if(cargs(n)(1:1)=='-') then 
      write(*,'(a5,i2,6a)') 'Args ',nint(float(n+1)/2.)  &
                            ,': ',cargs(n)(1:len_trim(cargs(n)))  &
                            ,' ',cargs(n+1)(1:len_trim(cargs(n+1)))
      if(cargs(n)(1:2)=='-f') then
         if(len_trim(cargs(n+1)).gt.80) then
            print*,'max filename length = 80 characters'
            call usage
         endif 
         revuin=cargs(n+1)(1:len_trim(cargs(n+1)))
      else
         call usage
      endif
   else
      call usage
   endif
enddo
print*

! give some values to namelist parameters that may be missing
head1=''
head2=''
revpref='./'
ipltinfo=1
ipanel=1
igridll=1
ibackgnd=1
do i=1,maxfore
   tvar(i)=''
   zvar(i)=''
   xvar(i)=''
   yvar(i)=''
   cframe_a(i)=''
   cframe_b(i)=''
   cframe_c(i)=''
   landmark(i)=''
   colors(i)=''
enddo

! read in the namelist
open(1,status='old',FILE=revuin)
call namein_revu(1,'$CONTROL',0)
call namein_revu(1,'$GRAB',0)
call namein_revu(1,'$STATS',0)
call namein_revu(1,'$GLL',0)
close(1)

! reset IBACKGND=0
if(ibackgnd==0) ibackgnd=1

! read the contour.defaults file - needs to be in runtime dir
print*
call var_defaults_read ('contour.defaults')

! fill the rams arrays
call RAMS_anal_init (nfl,anpref)

! This supports new version where all grids may not be available at all times.
!   Do grid inventory by looking at file names
call ra_grid_inv()

! set up the background configuration from input strings
if(len_trim(TVAR(1)).eq.0.or.len_trim(ZVAR(1)).eq.0.or.  &
   len_trim(XVAR(1)).eq.0.or.len_trim(YVAR(1)).eq.0) then
   print*,'TVAR: ',TVAR(1),' ZVAR: ',ZVAR(1)
   print*,'XVAR: ',XVAR(1),' YVAR: ',YVAR(1)
   stop 'plotspc: frame not defined'
endif
call backset(1)

! do the stuff
print*
write(*,'(2a)') 'mode: ',anatype

if(anatype.eq.'SPACE'.or.anatype.eq.'STATS') then

   call opngks
   call gopwk(2,10,3)
   call plotspc(nfl,anatype)
   call gclwk(2)
   call clsgks

elseif(anatype.eq.'DUMP'.or.anatype.eq.'MEDOC'.or.  &
       anatype.eq.'V5D'.or. anatype.eq.'GRAB'.or.  &
       anatype.eq.'GRADS'.or.anatype.eq.'GRIB') then

   call plotspc (nfl,anatype)

else

   print*,'No action for: ',anatype
   print*,'  options= SPACE, STATS, GRAB, V5D, GRIB, GRADS, MEDOC or DUMP'

endif

print*
write(*,'(2a)') cargs(0)(1:len_trim(cargs(0))),' finished normally'
print*

end

!***************************************************************************
subroutine ra_grid_inv()

implicit none

include 'vcomm2.h'

character(len=256) :: hfiles(maxtims),fpref,gfiles(maxgrds),suff
integer :: nhfiles,n,nc,ngfiles,ng,ngr

fpref=trim(anpref)//'*-head.txt'
call RAMS_filelist(hfiles,fpref,nhfiles)

do n=1,nhfiles
   nc=len_trim(hfiles(n))-9
   fpref=hfiles(n)(1:nc)//'-g*'
   call RAMS_filelist(gfiles,fpref,ngfiles)
   print*,'ngfiles:',ngfiles
   gridinv(n,1:maxgrds)=.false.
   do ng=1,ngfiles
      print*,'gfiles:',trim(gfiles(ng))
      ! Find last "." in file name. Grid number is character before it.
      do nc=len_trim(gfiles(ng)),1,-1
         if (gfiles(ng)(nc:nc) == '.') exit
      enddo
      nc=nc-1
      read(gfiles(ng)(nc:nc),*) ngr
      gridinv(n,ngr)=.true.
   enddo
   print*,'nhhhhhh:',n,gridinv(n,1:3)
enddo
   
return
end
!***************************************************************************

subroutine usage ()

implicit none

print*
print*,'USAGE: revu [-f REVU_IN-filename]'
print*
stop

return
end
      
!***************************************************************************

SUBROUTINE NVFILLA (GROUP,VA,ISUB1,ISUB2,IN,FV,CH,NVA)

! Namelist reading routine, called by NAMEIN.

CHARACTER*(*) GROUP,VA,CH
COMMON/NREAD/NRFLAG,NFATAL
include 'vcomm2.h'

PARAMETER (NVCONTRL=28,NVGRAB=2,NVSTATS=9,NVGLL=7)
DIMENSION ICONTROL(NVCONTRL),IGRAB(NVGRAB),ISTATS(NVSTATS)
DIMENSION IGLL(NVGLL)
CHARACTER*8 CONTROL(NVCONTRL),GRAB(NVGRAB),STATS(NVSTATS)
CHARACTER*8 GLL(NVGLL)
DATA ICONTROL/NVCONTRL*0/,IGRAB/NVGRAB*0/,ISTATS/NVSTATS*0/
DATA IGLL/NVGLL*0/

DATA CONTROL/'ANATYPE','ANPREF','REVPREF'  &
            ,'XVAR','YVAR','ZVAR','TVAR'  &
            ,'IGRID','MAPFILL','IBACKGND','IPLTINFO','IPANEL'  &
            ,'XAXLAB','YAXLAB','LABFMTX','LABFMTY','NUMLABX'  &
            ,'NUMLABY','HEAD1','HEAD2'  &
            ,'JGRID','IZTRAN','CFRAME_A','CFRAME_B','CFRAME_C'  &
            ,'IPLEVEL','LANDMARK','COLORS'/
DATA GRAB/'GRABIN','IGRABFMT'/
DATA STATS/'CMODE','SFCPREF','RWNPREF','NOQ'  &
          ,'IPDIFF','IPVALS','IPWIND','IPHIST','IPCONT'/
DATA GLL/'IGRIDLL','GLLDLLAT','GLLDLLON'  &
        ,'GLLWLON','GLLELON','GLLSLAT','GLLNLAT'/

IS1=ISUB1
IS2 = ISUB2
IF(ISUB1.EQ.0)IS1=NVA

if(GROUP.eq.'$CONTROL') then
  CALL VARCHK(VA,GROUP,CONTROL,ICONTROL,NVCONTRL,INRFLG)
  IF(VA.EQ.'ANPREF')   CALL VARSETC(VA,ANPREF,IS1,1,CH,0,256)
  IF(VA.EQ.'REVPREF')  CALL VARSETC(VA,REVPREF,IS1,1,CH,0,256)
  IF(VA.EQ.'ANATYPE')  CALL VARSETC(VA,ANATYPE,IS1,1,CH,0,8)
  IF(VA.EQ.'XVAR')     CALL VARSETC(VA,XVAR(IS1),IS1,MAXFORE,CH,0,20)
  IF(VA.EQ.'YVAR')     CALL VARSETC(VA,YVAR(IS1),IS1,MAXFORE,CH,0,20)
  IF(VA.EQ.'ZVAR')     CALL VARSETC(VA,ZVAR(IS1),IS1,MAXFORE,CH,0,20)
  IF(VA.EQ.'TVAR')     CALL VARSETC(VA,TVAR(IS1),IS1,MAXFORE,CH,0,20)
  IF(VA.EQ.'IGRID')    CALL VARSETI(VA,IGRID(IS1),IS1,MAXFORE,IN,0,10)
  IF(VA.EQ.'MAPFILL')  CALL VARSETI(VA,MAPFILL,IS1,1,IN,-3,3)
  IF(VA.EQ.'IBACKGND') CALL VARSETI(VA,IBACKGND,IS1,1,IN,-3,3)
  IF(VA.EQ.'IPLTINFO') CALL VARSETI(VA,IPLTINFO,IS1,1,IN,0,3)
  IF(VA.EQ.'IPANEL')   CALL VARSETI(VA,IPANEL,IS1,1,IN,0,4)
  IF(VA.EQ.'IZTRAN')   CALL VARSETI(VA,IZTRAN(IS1),IS1,MAXFORE,IN,1,3)
  IF(VA.EQ.'IPLEVEL')  CALL VARSETI(VA,IPLEVEL(IS1),IS1,MAXFORE,IN,0,2000)
  IF(VA.EQ.'LANDMARK') CALL VARSETC(VA,LANDMARK(IS1),IS1,MAXFORE,CH,0,128)
  IF(VA.EQ.'COLORS')   CALL VARSETC(VA,COLORS(IS1),IS1,MAXFORE,CH,0,128)
  IF(VA.EQ.'XAXLAB')   CALL VARSETC(VA,XAXLAB,IS1,1,CH,0,8)
  IF(VA.EQ.'YAXLAB')   CALL VARSETC(VA,YAXLAB,IS1,1,CH,0,8)
  IF(VA.EQ.'LABFMTX')  CALL VARSETC(VA,LABFMTX,IS1,1,CH,0,8)
  IF(VA.EQ.'LABFMTY')  CALL VARSETC(VA,LABFMTY,IS1,1,CH,0,8)
  IF(VA.EQ.'NUMLABX')  CALL VARSETI(VA,NUMLABX,IS1,1,IN,1,100)
  IF(VA.EQ.'NUMLABY')  CALL VARSETI(VA,NUMLABY,IS1,1,IN,1,100)
  IF(VA.EQ.'CFRAME_A') CALL VARSETC(VA,CFRAME_A(IS1),IS1,MAXFORE,CH,0,128)
  IF(VA.EQ.'CFRAME_B') CALL VARSETC(VA,CFRAME_B(IS1),IS1,MAXFORE,CH,0,128)
  IF(VA.EQ.'CFRAME_C') CALL VARSETC(VA,CFRAME_C(IS1),IS1,MAXFORE,CH,0,128)
  IF(VA.EQ.'HEAD1')    CALL VARSETC(VA,HEAD1,IS1,1,CH,0,40)
  IF(VA.EQ.'HEAD2')    CALL VARSETC(VA,HEAD2,IS1,1,CH,0,40)
  IF(VA.EQ.'JGRID')    CALL VARSETI(VA,JGRID,IS1,1,IN,0,180)
endif

IF(GROUP.EQ.'$GRAB')THEN
  CALL VARCHK(VA,GROUP,GRAB,IGRAB,NVGRAB,INRFLG)
  IF(VA.EQ.'GRABIN')   CALL VARSETC(VA,GRABIN,IS1,1,CH,0,128)
  IF(VA.EQ.'IGRABFMT') CALL VARSETI(VA,IGRABFMT,IS1,1,IN,1,2)
ENDIF

IF(GROUP.eq.'$STATS') then
  CALL VARCHK(VA,GROUP,STATS,ISTATS,NVSTATS,INRFLG)
  IF(VA.EQ.'CMODE')   CALL VARSETC(VA,CMODE,IS1,1,CH,0,5)
  IF(VA.EQ.'SFCPREF') CALL VARSETC(VA,SFCPREF,IS1,1,CH,0,128)
  IF(VA.EQ.'RWNPREF') CALL VARSETC(VA,RWNPREF,IS1,1,CH,0,128)
  IF(VA.EQ.'NOQ')     CALL VARSETC(VA,NOQ,IS1,1,CH,0,4)
  IF(VA.EQ.'IPDIFF')  CALL VARSETI(VA,IPDIFF,IS1,1,IN,0,1)
  IF(VA.EQ.'IPVALS')  CALL VARSETI(VA,IPVALS,IS1,1,IN,0,1)
  IF(VA.EQ.'IPWIND')  CALL VARSETI(VA,IPWIND,IS1,1,IN,0,1)
  IF(VA.EQ.'IPHIST')  CALL VARSETI(VA,IPHIST,IS1,1,IN,0,1)
  IF(VA.EQ.'IPCONT')  CALL VARSETI(VA,IPCONT,IS1,1,IN,0,1)
ENDIF

IF(GROUP.eq.'$GLL') then
  CALL VARCHK(VA,GROUP,GLL,IGLL,NVGLL,INRFLG)
  IF(VA.EQ.'IGRIDLL')  CALL VARSETI(VA,IGRIDLL,IS1,1,IN,0,2)
  IF(VA.EQ.'GLLDLLAT') CALL VARSETF(VA,GLLDLLAT,IS1,1,FV,0.00001,100.)
  IF(VA.EQ.'GLLDLLON') CALL VARSETF(VA,GLLDLLON,IS1,1,FV,0.00001,100.)
  IF(VA.EQ.'GLLWLON')  CALL VARSETF(VA,GLLWLON,IS1,1,FV,-180.,180.)
  IF(VA.EQ.'GLLELON')  CALL VARSETF(VA,GLLELON,IS1,1,FV,-180.,180.)
  IF(VA.EQ.'GLLSLAT')  CALL VARSETF(VA,GLLSLAT,IS1,1,FV,-90.,90.)
  IF(VA.EQ.'GLLNLAT')  CALL VARSETF(VA,GLLNLAT,IS1,1,FV,-90.,90.)
ENDIF

RETURN
END

!***************************************************************************

SUBROUTINE NAMEIN_revu (IUNIT,GROUP,IFATAL)

! This routine is called by routines INITLZ, CONSTAT, and ARRSND
! to input the values for the NAMELIST specified by the character stri
! GROUP from input unit IUNIT.

PARAMETER (MAXREC=1000,LINVAR=10,MAXVAR=100,MAXVAL=300)
CHARACTER*(*) GROUP
CHARACTER*20 VARN
CHARACTER*256 LINE,LINEW,VALUE(MAXVAL),TOKENS(50)
COMMON/NREAD/NRFLAG,NFATAL

REWIND IUNIT
NFATAL=IFATAL
NRFLAG=0
NVALUE=0
NVARN=0
CALL FINDGR(IUNIT,GROUP,MAXREC)
IF(NRFLAG.EQ.1.AND.IFATAL.EQ.1) STOP 'NAMEIN'
DO NR=1,MAXREC
   READ(IUNIT,'(A256)',END=100,ERR=100) LINE
   CALL STRIP(LINE,LINEW,NCW)
   NCW=MAX(NCW,1)
   CALL TOKNZE(LINEW,NCW,TOKENS,NTOK)
   IF(LINEW(1:NCW).EQ.'$END') THEN
      GOTO 100
   ENDIF

   NT=1
   20 CONTINUE
      IF(NT.GT.NTOK) GOTO 10
      IF(LETTER(TOKENS(NT)).EQ.1) THEN
         IF(NVARN.GT.0)  &
            CALL NVTRAN_revu(GROUP,VARN,ISUB1,ISUB2,VALUE,NVALUE)
         NVALUE=0
         ISUB1=0
         ISUB2=0
         VARN=TOKENS(NT)
         NVARN=NVARN+1
         NT=NT+1
         IF(TOKENS(NT).EQ.'(')THEN
            NT=NT+1
            CALL CH2INT(TOKENS(NT),ISUB1)
            NT=NT+1
            IF(TOKENS(NT).EQ.',') THEN
               NT=NT+1
               CALL CH2INT(TOKENS(NT),ISUB2)
               NT=NT+1
            ENDIF
         ENDIF
      ELSEIF(NUMBER(TOKENS(NT)).EQ.1.OR.LETQUO(TOKENS(NT)).EQ.1) THEN
         NVALUE=NVALUE+1
         VALUE(NVALUE)=TOKENS(NT)
      ENDIF
      NT=NT+1
   GOTO 20
   10 CONTINUE
ENDDO

100 CONTINUE
CALL NVTRAN_revu(GROUP,VARN,ISUB1,ISUB2,VALUE,NVALUE)
VARN='$END'
CALL NVFILLa(GROUP,VARN,ISUB1,ISUB2,INT,FNUM,LINEW(1:NCW),NR)

RETURN
END

!***************************************************************************

SUBROUTINE NVTRAN_revu (GROUP,VARN,ISUB1,ISUB2,VALUE,NVALUE)

! This routine converts a variable value(s) (VALUE) read in as a
! character string(s) to the proper type (integer, real, or character)
! and assigns it (them) to the storage location(s) corresponding to
! the NAMELIST variable VARN(ISUB1,ISUB2).  GROUP is the NAMELIST
! group name.  NVALUE is the number of values to be assigned.

CHARACTER*(*) GROUP,VARN,VALUE(*),CHVAL*80

IF(LETINT(VARN).EQ.1) THEN
   DO NV=1,NVALUE
      IF(LETQUO(VALUE(NV)).EQ.0) THEN
         CALL CH2INT(VALUE(NV),INT)
         CALL NVFILLa(GROUP,VARN,ISUB1,ISUB2,INT,FNUM,CHVAL(1:NCW),NV)
      ELSE
         CALL CH2CH(VALUE(NV),CHVAL,NCW)
         CALL NVFILLa(GROUP,VARN,ISUB1,ISUB2,INT,FNUM,CHVAL(1:NCW),NV)
      ENDIF
   ENDDO
ELSE
   DO NV=1,NVALUE
      IF(LETQUO(VALUE(NV)).EQ.0) THEN
         CALL CH2REAL(VALUE(NV),FNUM)
         CALL NVFILLa(GROUP,VARN,ISUB1,ISUB2,INT,FNUM,CHVAL(1:NCW),NV)
      ELSE
         CALL CH2CH(VALUE(NV),CHVAL,NCW)
         CALL NVFILLa(GROUP,VARN,ISUB1,ISUB2,INT,FNUM,CHVAL(1:NCW),NV)
      ENDIF
   ENDDO
ENDIF

RETURN
END

