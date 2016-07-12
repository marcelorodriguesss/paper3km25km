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

! Namelist reading routines for ISAN

subroutine nvfills (group,v,isub1,isub2,iv,fv,cv,nva)

! This routine places values read in for a particular NAMELIST
! variable in the storage location(s) for that variable
!
! GROUP   - NAMELIST group name
! VR      - NAMELIST variable name
! ISUB1   - value of first subscript
! ISUB2   - value of second subscript
! II      - vector of integer values
! FF      - vector of real values
! CC      - vector of character strings
! NV      - number of values to be inserted

use isan_coms

implicit none

character(len=*) :: group,v,cv
integer :: isub1,isub2,iv,nva
real :: fv

integer, parameter :: nvcont=13,nvinst3=23
integer :: icontrol(nvcont),iinst3(nvinst3)
character(len=16) :: control(nvcont),inst3(nvinst3)
data icontrol/nvcont*0/,iinst3/nvinst3*0/
data control/'IASRFCE','GUESS1ST'  &
     ,'ISAN_INC','I1ST_FLG','IAPR','IARAWI'  &
     ,'VARPFX','IOFLGISZ','IOFLGVAR'  &
     ,'IUPA_FLG','ISFC_FLG','ISZSTAGE','IVRSTAGE'/
data inst3/'NOTSTA','NOTID','IOBSWIN','WVLNTH','RESPON'  &
     ,'SWVLNTH','STASEP','IGRIDFL','GRIDWT'  &
     ,'GOBSEP','GOBRAD','MAXSTA','MAXSFC'  &
     ,'NISN','LEVTH','NIGRIDS','SIGZWT','TOPSIGZ'  &
     ,'HYBBOT','HYBTOP','SFCINF','NFEEDVAR','USED_FILE'/

integer :: nrflag,nfatal,is1,is2,inrflg
common/nread/nrflag,nfatal

IS1=ISUB1
IS2 = ISUB2
IF(ISUB1.EQ.0)IS1=NVA

IF(GROUP.EQ.'$ISAN_CONTROL') THEN
   CALL VARCHK(V,GROUP,CONTROL,ICONTROL,NVCONT,INRFLG)
   IF(V.EQ.'ISZSTAGE') CALL VARSETI(V,ISZSTAGE,IS1,1,IV,0,1)
   IF(V.EQ.'IVRSTAGE') CALL VARSETI(V,IVRSTAGE,IS1,1,IV,0,1)
   IF(V.EQ.'ISAN_INC') CALL VARSETI(V,ISAN_INC,IS1,1,IV,0,9999)
   IF(V.EQ.'I1ST_FLG') CALL VARSETI(V,I1ST_FLG,IS1,1,IV,1,10)
   IF(V.EQ.'IUPA_FLG') CALL VARSETI(V,IUPA_FLG,IS1,1,IV,1,10)
   IF(V.EQ.'ISFC_FLG') CALL VARSETI(V,ISFC_FLG,IS1,1,IV,1,10)
   IF(V.EQ.'GUESS1ST') CALL VARSETC(V,guess1st,IS1,1,CV,0,80)
   IF(V.EQ.'IAPR')     CALL VARSETC(V,IAPR,IS1,1,CV,0,80)
   IF(V.EQ.'IARAWI')   CALL VARSETC(V,IARAWI,IS1,1,CV,0,80)
   IF(V.EQ.'IASRFCE')  CALL VARSETC(V,IASRFCE,IS1,1,CV,0,80)
   IF(V.EQ.'VARPFX')   CALL VARSETC(V,VARPFX,IS1,1,CV,0,80)
   IF(V.EQ.'IOFLGISZ') CALL VARSETI(V,IOFLGISZ,IS1,1,IV,0,1)
   IF(V.EQ.'IOFLGVAR') CALL VARSETI(V,IOFLGVAR,IS1,1,IV,0,1)
ENDIF

IF(GROUP.EQ.'$ISAN_ISENTROPIC') THEN
   CALL VARCHK(V,GROUP,INST3,IINST3,NVINST3,INRFLG)
   IF(V.EQ.'NIGRIDS')  CALL VARSETI(V,NIGRIDS,IS1,1,IV,1,10)
   IF(V.EQ.'NFEEDVAR') CALL VARSETI(V,NFEEDVAR,IS1,1,IV,0,1)
   IF(V.EQ.'SIGZWT')   CALL VARSETF(V,SIGZWT,IS1,1,FV,0.,1.)
   IF(V.EQ.'NISN')     CALL VARSETI(V,NISN,IS1,1,IV,0,MAXISN)
   IF(V.EQ.'LEVTH')    CALL VARSETI(V,LEVTH(IS1),IS1,MAXISN,IV,200,800)
   IF(V.EQ.'TOPSIGZ')  CALL VARSETF(V,TOPSIGZ,IS1,1,FV,0.,100000.)
   IF(V.EQ.'HYBBOT')   CALL VARSETF(V,HYBBOT,IS1,1,FV,0.,100000.)
   IF(V.EQ.'HYBTOP')   CALL VARSETF(V,HYBTOP,IS1,1,FV,0.,100000.)
   IF(V.EQ.'SFCINF')   CALL VARSETF(V,SFCINF,IS1,1,FV,0.,100000.)
   IF(V.EQ.'MAXSTA')   CALL VARSETI(V,MAXSTA,IS1,1,IV,0,100000)
   IF(V.EQ.'MAXSFC')   CALL VARSETI(V,MAXSFC,IS1,1,IV,0,100000)
   IF(V.EQ.'GOBSEP')   CALL VARSETF(V,GOBSEP,IS1,1,FV,0.,100.)
   IF(V.EQ.'GOBRAD')   CALL VARSETF(V,GOBRAD,IS1,1,FV,0.,100.)
   IF(V.EQ.'WVLNTH')   CALL VARSETF(V,WVLNTH(IS1),IS1,maxagrds,FV,0.,1e5)
   IF(V.EQ.'SWVLNTH')  CALL VARSETF(V,SWVLNTH(IS1),IS1,maxagrds,FV,0.,1E5)
   IF(V.EQ.'RESPON')   CALL VARSETF(V,RESPON(IS1),IS1,maxagrds,FV,0.,1.)
   IF(V.EQ.'STASEP')   CALL VARSETF(V,STASEP,IS1,1,FV,0.,100.)
   IF(V.EQ.'IGRIDFL')  CALL VARSETI(V,IGRIDFL,IS1,1,IV,0,4)
   IF(V.EQ.'GRIDWT')   CALL VARSETF(V,GRIDWT(IS1),IS1,maxagrds,FV,0.,1.)
   IF(V.EQ.'NOTSTA')   CALL VARSETI(V,NOTSTA,IS1,1,IV,0,50)
   IF(V.EQ.'NOTID')    CALL VARSETC(V,NOTID(IS1),IS1,50,CV,1,8)
   IF(V.EQ.'USED_FILE')CALL VARSETC(V,USED_FILE,IS1,1,CV,0,128)
   IF(V.EQ.'IOBSWIN')  CALL VARSETI(V,IOBSWIN,IS1,1,IV,-21600,21600)
ENDIF

RETURN
END

!***************************************************************************

SUBROUTINE NAMEIN_isan(IUNIT,GROUP,IFATAL)

! This routine is called by routines INITLZ, CONSTAT, and ARRSND
! to input the values for the NAMELIST specified by the character stri
! GROUP from input unit IUNIT.
implicit none
character(len=*) :: group
integer :: ifatal,iunit

integer, parameter :: maxrec=1000,linvar=10,maxvar=100,maxval=300
character(len=20) :: varn
character(len=80) :: line,linew,value(maxval),tokens(50)
integer :: nrflag,nfatal
common/nread/nrflag,nfatal

integer :: nvalue,nvarn,nr,ncw,nt,ntok,isub1,isub2,int
real :: fnum
integer, external :: letter,number,letquo

rewind iunit

NFATAL=IFATAL
NRFLAG=0
NVALUE=0
NVARN=0
CALL FINDGR(IUNIT,GROUP,MAXREC)
IF(NRFLAG.EQ.1.AND.IFATAL.EQ.1) STOP 'NAMEIN'
DO 10 NR=1,MAXREC
  READ(IUNIT,'(A80)',END=100)LINE
  CALL STRIP(LINE,LINEW,NCW)
  NCW=MAX(NCW,1)
  CALL TOKNZE(LINEW,NCW,TOKENS,NTOK)
  IF(LINEW(1:NCW).EQ.'$END') GOTO 100

  NT=1
  20 CONTINUE
    IF(NT.GT.NTOK) GOTO 10
    IF(LETTER(TOKENS(NT)).EQ.1) THEN
      IF(NVARN.GT.0) CALL NVTRAN_isan(GROUP,VARN,ISUB1,ISUB2,VALUE,NVALUE)
      NVALUE=0
      ISUB1=0
      ISUB2=0
      VARN=TOKENS(NT)
      NVARN=NVARN+1
      NT=NT+1
      IF(TOKENS(NT).EQ.'(') THEN
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

100 CONTINUE
CALL NVTRAN_isan(GROUP,VARN,ISUB1,ISUB2,VALUE,NVALUE)
VARN='$END'
CALL NVFILLs(GROUP,VARN,ISUB1,ISUB2,INT,FNUM,LINEW(1:NCW),NR)

RETURN
END

!***************************************************************************

SUBROUTINE NVTRAN_isan(GROUP,VARN,ISUB1,ISUB2,VALUE,NVALUE)

! This routine converts a variable value(s) (VALUE) read in as a
! character string(s) to the proper type (integer, real, or character)
! and assigns it (them) to the storage location(s) corresponding to
! the NAMELIST variable VARN(ISUB1,ISUB2).  GROUP is the NAMELIST
! group name.  NVALUE is the number of values to be assigned.
implicit none
integer :: ISUB1,ISUB2,NVALUE
CHARACTER(len=*) :: GROUP,VARN,VALUE(*)
character(len=80) :: CHVAL

integer :: nv,int,ncw
real :: fnum
integer, external :: letint,letquo

ncw=1

IF(LETINT(VARN).EQ.1) THEN
  DO NV=1,NVALUE
    IF(LETQUO(VALUE(NV)).EQ.0) THEN
      CALL CH2INT(VALUE(NV),INT)
      CALL NVFILLs(GROUP,VARN,ISUB1,ISUB2,INT,FNUM,CHVAL(1:NCW),NV)
    ELSE
      CALL CH2CH(VALUE(NV),CHVAL,NCW)
      CALL NVFILLs(GROUP,VARN,ISUB1,ISUB2,INT,FNUM,CHVAL(1:NCW),NV)
    ENDIF
  ENDDO
ELSE
  DO NV=1,NVALUE
    IF(LETQUO(VALUE(NV)).EQ.0) THEN
      CALL CH2REAL(VALUE(NV),FNUM)
      CALL NVFILLs(GROUP,VARN,ISUB1,ISUB2,INT,FNUM,CHVAL(1:NCW),NV)
    ELSE
      CALL CH2CH(VALUE(NV),CHVAL,NCW)
      CALL NVFILLs(GROUP,VARN,ISUB1,ISUB2,INT,FNUM,CHVAL(1:NCW),NV)
    ENDIF
  ENDDO
ENDIF

RETURN
END

