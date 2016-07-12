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

!------------------------------------------------------------------
!
!      Namelist reading routines
!
!------------------------------------------------------------------

subroutine varsetf (varn,var,is1,maxsub,fnum,fmin,fmax)
implicit none
integer :: is1,maxsub
character(len=*) :: varn
real :: var,fnum,fmin,fmax

! Assign real value read in (FNUM) to the corresponding real variable
! in the NAMELIST and do a bounds check

if(is1.le.maxsub) then
  var=fnum
else
  print 9,trim(varn),is1,maxsub,fnum
  9 format(' -- ERROR --   Input variable - ',A,' - attempting'  &
        ,' to read extra values, values ignored.',/,  &
         ' Subscript, max dimension, value ',2I6,F20.6)
endif

if(fnum.lt.fmin.or.fnum.gt.fmax) then
  print 10,trim(varn),fnum,fmin,fmax
  10 format(' -- ERROR --   Input variable - ',A,' - set to ',F18.5  &
      ,/,'                 allowable range ',F15.5,' to ',F15.5)
endif

return
end

!***************************************************************************

subroutine varsetf2 (varn,var,is1,max1,is2,max2,fnum,fmin,fmax)
implicit none
integer :: is1,max1,is2,max2
character(len=*) :: varn
real :: var,fnum,fmin,fmax

! Assign real value read in (FNUM) to the corresponding real variable
! in the NAMELIST and do a bounds check

if(is1.le.max1.and.is2.le.max2) then
  var = fnum
else
  print 9,trim(varn),is1,max1,is2,max2,fnum
  9 format(' -- ERROR --   Input variable - ',A,' - attempting'  &
        ,' to read extra values, values ignored.',/,  &
         ' Subscript',I6,', max dimension',I6,/,  &
         ' Subscript',I6,', max dimension',I6,', value ',F20.6)
endif

if(fnum.lt.fmin.or.fnum.gt.fmax) then
  print 10,trim(varn),fnum,fmin,fmax
  10 format(' -- ERROR --   Input variable - ',A,' - set to ',F18.5  &
      ,/,'                 allowable range ',F15.5,' to ',F15.5)
endif

return
end

!***************************************************************************

subroutine varseti (varn,ivar,is1,maxsub,inum,imin,imax)
implicit none
integer :: ivar,is1,maxsub,inum,imin,imax
character(len=*) :: varn

! Assign integer value read in (INUM) to the corresponding integer var
! (IVAR) in the NAMELIST and do a bounds check

if(is1.le.maxsub) then
  ivar=inum
else
  print 9,trim(varn),is1,maxsub,inum
  9 format(' -- ERROR --   Input variable - ',A,' - attempting'  &
        ,' to read extra values, values ignored.',/,  &
         ' Subscript, max dimension, value ',2I6,I20)
endif

if(inum.lt.imin.or.inum.gt.imax) then
  print 10,trim(varn),inum,imin,imax
  10 format(' -- ERROR --   Input variable - ',A,' - set to ',I10  &
      ,/,'                 allowable range ',I10,' to ',I10)
endif

return
end

!***************************************************************************

subroutine varseti2 (varn,ivar,is1,max1,is2,max2,inum,imin,imax)
implicit none
integer :: ivar,is1,max1,is2,max2,inum,imin,imax
character(len=*) :: varn

! Assign integer value read in (INUM) to the corresponding integer var
! (IVAR) in the NAMELIST and do a bounds check

if(is1.le.max2.and.is2.le.max2) then
  ivar = inum
else
  print 9,trim(varn),is1,max1,is2,max2,inum
  9 format(' -- ERROR --   Input variable - ',A,' - attempting'  &
        ,' to read extra values, values ignored.',/,  &
         ' Subscript',I6,', max dimension',I6,/,  &
         ' Subscript',I6,', max dimension',I6,', value ',I10)
ENDIF

if(inum.lt.imin.or.inum.gt.imax) then
  print 10,trim(varn),inum,imin,imax
  10 format(' -- ERROR --   Input variable - ',A,' - set to ',I10  &
      ,/,'                 allowable range ',I10,' to ',I10)
endif

return
end

!***************************************************************************

subroutine varsetc (varn,var,is1,maxsub,ch,imin,imax)
implicit none
integer :: is1,maxsub,imin,imax
character(len=*) :: varn,ch,var

integer :: lch

! Assign character value read in (CH) to the corresponding character
! variable (VAR) in the NAMELIST and do a bounds check

if(is1.le.maxsub) then
  var=ch
else
  print 9,trim(varn),is1,maxsub,trim(ch)
  9 FORMAT(' -- ERROR --   Input variable - ',A,' - attempting'  &
        ,' to read extra values, values ignored.',/,  &
         ' Subscript, max dimension, value ',2I6,A)
endif

lch=len(ch)

if(lch.lt.imin.or.lch.gt.imax) then
  print 10,trim(varn),trim(ch),imin,imax
  10 FORMAT(' -- ERROR --   Input variable - ',A,' - set to ',A  &
      ,/,'                 allowable length ',I10,' to ',I10)
endif

return
end

!***************************************************************************

subroutine varsetc2 (varn,var,is1,max1,is2,max2,ch,imin,imax)
implicit none
integer :: is1,max1,is2,max2,imin,imax
character(len=*) :: varn,ch,var

integer :: lch

! Assign character value read in (CH) to the corresponding character
! variable (VAR) in the NAMELIST and do a bounds check

IF(IS1.LE.MAX1.AND.IS2.LE.MAX2) THEN
  VAR = CH
ELSE
  PRINT 9,VARN(1:len_trim(VARN)),IS1,MAX1,IS2,MAX2  &
       ,CH(1:len_trim(CH))
  9 FORMAT(' -- ERROR --   Input variable - ',A,' - attempting'  &
        ,' to read extra values, values ignored.',/,  &
         ' Subscript',I6,', max dimension',I6,/,  &
         ' Subscript',I6,', max dimension',I6,', value ',A)
ENDIF

LCH=LEN(CH)

IF(LCH.LT.IMIN.OR.LCH.GT.IMAX) THEN
  PRINT 10,VARN(1:len_trim(VARN)),CH(1:len_trim(CH)),IMIN,IMAX
  10 FORMAT(' -- ERROR --   Input variable - ',A,' - set to ',A  &
      ,/,'                 allowable length ',I10,' to ',I10)
ENDIF

RETURN
END

!***************************************************************************

subroutine varchk (varn,group,namelst,iname,nname,inrflg)
implicit none
integer :: nname,inrflg
integer :: iname(nname)
character(len=*) :: varn,group,namelst(nname)
integer :: nrflag,nfatal,nn,inoset
common/nread/nrflag,nfatal

! This routine checks that all member variables of the NAMELIST
! specified by GROUP have been assigned values.
!
! GROUP   - name of the NAMELIST being checked
! INNAME  - storage vector for counting number of times each member
!           of the NAMELIST has been assigned a value
! NAMELST - character vector containing names of members of NAMELIST
! NNAME   - number of variables in NAMELIST
! VARN    - name of NAMELIST variable being checked

INRFLG=0

IF(VARN.NE.'$END')THEN

  ! Update INAME value corresponding to the NAMELIST variable which has
  ! been assigned a value in the call to NVFILL.

  DO NN=1,NNAME
    IF(VARN.EQ.NAMELST(NN)) THEN
      INAME(NN)=1
      RETURN
    ENDIF
  enddo

  INRFLG=1
  PRINT 20,GROUP(1:len_trim(GROUP)),VARN(1:len_trim(VARN))
  20 FORMAT(' Extra variable in namelist -- ',A,'-- variable name -',A)

ELSE

  ! End of NAMELIST input has been reached - check that all variables ha
  ! been given values

  INRFLG=1
  IF(NFATAL.EQ.0) RETURN
  INOSET=0
  DO NN=1,NNAME
    IF(INAME(NN).EQ.0) THEN
      IF(INOSET.EQ.0) THEN
        PRINT 30,GROUP(1:len_trim(GROUP))
      ENDIF
      INOSET=1
      PRINT 31,NAMELST(NN)(1:len_trim(NAMELST(NN)))
    ENDIF
  enddo

  30 FORMAT(' The following variables have not been set in the -- ',A  &
           ,' -- namelist:')
  31 FORMAT('     variable name: ',A)

ENDIF

RETURN
END

!***************************************************************************

subroutine ch2int (str,int)
implicit none
integer :: int
character(len=*) :: str

character(len=8) :: form
integer :: nc

! Read integer value INT from character string STR
nc=len_trim(str)
write(form,90)nc
90 format('(i',i2,')')
read(str,form)int
  
return
end

!***************************************************************************

subroutine ch2real (str,fnum)
implicit none
real :: fnum
character(len=*) :: str

character(len=8) :: form
integer :: nc

! Read real value FNUM from character string STR

nc=len_trim(str)
write(form,90)nc
90 format('(f',i2,'.0)')
read(str,form)fnum
  
return
end

!***************************************************************************

subroutine ch2ch (str,chval,ncw)
implicit none
character(len=*) :: str,chval

integer :: nc,ncw,ncstr

! Remove trailing blanks from character string STR and store remaining
! NCW characters in character string CHVAL

ncstr=len(str)
do 10 nc=ncstr,1,-1
  if(str(nc:nc).eq.' ') goto 10
  chval=str(2:nc)
  ncw=nc-2
  return
10 continue

return
end


!***************************************************************************

subroutine findgr (iunit,group,maxrec)
implicit none
integer :: maxrec,iunit
character(len=*) :: group

character(len=80) :: line
integer :: nrflag,nfatal
common/nread/nrflag,nfatal

integer :: nr,ind

! This routine checks to see if any of the first MAXREC lines on input
! unit IUNIT contains the character string GROUP

do nr=1,maxrec
  read(iunit,'(a80)',end=100) line
  ind=index(line,group)
  if(ind.ne.0) return
enddo
100 continue
if(nfatal.eq.1) then
print *,' Namelist read error -- group not found -- ',group
endif

return
end

!***************************************************************************

subroutine strip (lin1,lin2,nc2)
implicit none
integer :: nc2
character(len=*) :: lin1,lin2

integer :: iquote,nc,nl

! This routine strips blank characters from character string LIN1
! (as well as comments beginning with an '!') and stores the stripped-
! down remainder in character string LIN1.  NC2 is the number of
! characters in LIN2.

nl=len(lin1)

nc2=0
iquote=0
do nc=1,nl
  if(iquote.eq.0) then
    if(lin1(nc:nc).ne.' ') then
      if(lin1(nc:nc).eq.'!') return
      nc2=nc2+1
      lin2(nc2:nc2)=lin1(nc:nc)
    endif
  else
    nc2=nc2+1
    lin2(nc2:nc2)=lin1(nc:nc)
  endif
  if(lin1(nc:nc).eq.'''') then
    if(iquote.eq.0) then
      iquote=1
    else
      iquote=0
    endif
  endif
enddo

return
end

!***************************************************************************

subroutine toknze (str,nch,tokens,ntok)
implicit none
integer :: nch,ntok
character(len=*) :: str, tokens(*)

integer, parameter :: nsep=4
character(len=1) :: toksep(nsep)
data toksep/'=',',','(',')'/

integer :: nc,ns,npt

! This routine "parses" character string STR into different pieces
! or tokens by looking for one of four possible token separators (TOKS
! STR contains NCH characters.  The number of tokens identified is NTO
! the character string tokens are stored in TOKENS.


ntok=0
npt=1
do 10 nc=1,nch
  do 5 ns=1,nsep
    if(str(nc:nc).eq.toksep(ns))then
      if(nc-npt.ge.1)then
        ntok=ntok+1
        tokens(ntok)=str(npt:nc-1)
      endif
      ntok=ntok+1
      tokens(ntok)=str(nc:nc)
      npt=nc+1
      goto 10
    endif
  5 continue
10 continue

return
end

!***************************************************************************

!------------------------------------------------------------------
!
!      NAMELIST output routines
!
!------------------------------------------------------------------

subroutine fillpg (varname,irow,icol,ivalue,rvalue,cvalue,vartype,nvalue)
implicit none
! his routine builds an output page for a listing of NAMELIST
! ariable values by inserting NAMELIST variable names and values
! nto a CHARACTER array called PAGE.  One call to this routine
! s required for each variable to be output.
!
!
! VARNAME - name of variable to be inserted on page (character stri
! IROW    - row number of page on which variable name and value are
!              to be inserted
! ICOL    - column of page in which name and value are to be insert
! IVALUE  - vector of values if variable is an integer
! RVALUE  - vector of values if variable is a real
! CVALUE  - vector of values if variable is a character string
! VARTYPE - type of variable to be written (choice of 'I', 'R', or
!              'C')
! NVALUE  - number of values in value vector

character(len=*)  ::cvalue(1)
character(len=132)::  page(80)
character(len=64) ::  cdup, form
character(len=8 ) ::  varname
character(len=4 ) ::  numd
character(len=1 ) ::  vartype, apstrph
integer        :: ivalue(1), idup, nvalue, row, col, colp7, colp9
integer        :: icol, irow, leftcol, numdup, oldnum,length,nv
real           :: rvalue(1), rdup

common /pageout/ page

data  apstrph/''''/

row = irow
col = icol
colp7 = col + 7
colp9 = col + 9
page(row)(col:colp7) = varname
page(row)(colp9:colp9) = '='
leftcol = col + 11
numdup = 1

if(vartype .eq. 'i') then

   ! Integer variable

   IDUP = IVALUE(1)
   IF(NVALUE .EQ. 1) THEN

      ! Single variable value

      WRITE(FORM,600) IDUP
      LENGTH = 8
      CALL WRITEVL(ROW, COL, LEFTCOL, FORM, LENGTH)
   ELSE

      ! More than one variable value

      DO NV = 2, NVALUE, 1
         OLDNUM = NUMDUP
         IF(IDUP .EQ. IVALUE(NV)) NUMDUP = NUMDUP + 1
         IF(NUMDUP .EQ. OLDNUM) THEN
            IF(NUMDUP .EQ. 1) THEN
               WRITE(FORM,600) IDUP
               LENGTH = 10
            ELSE
               WRITE(FORM,650) NUMDUP, IDUP
               LENGTH = 13
            ENDIF
            CALL WRITEVL(ROW, COL, LEFTCOL, FORM, LENGTH)
            IDUP = IVALUE(NV)
            NUMDUP = 1
         ENDIF
      enddo
      WRITE(FORM,650) NUMDUP, IDUP
      LENGTH = 11
      CALL WRITEVL(ROW, COL, LEFTCOL, FORM, LENGTH)
   ENDIF

ELSEIF(VARTYPE .EQ. 'R') THEN

   ! Real variable

   RDUP = RVALUE(1)
   IF(NVALUE .EQ. 1) THEN

      ! Single variable value

      WRITE(FORM,610) RDUP
      LENGTH = 10
      CALL WRITEVL(ROW, COL, LEFTCOL, FORM, LENGTH)
   ELSE

      ! More than one variable value

      DO NV = 2, NVALUE, 1
         OLDNUM = NUMDUP
         IF(RDUP .EQ. RVALUE(NV)) NUMDUP = NUMDUP + 1
         IF(NUMDUP .EQ. OLDNUM) THEN
            IF(NUMDUP .EQ. 1) THEN
               WRITE(FORM,610) RDUP
               LENGTH = 10
            ELSE
               WRITE(FORM,660) NUMDUP, RDUP
               LENGTH = 13
            ENDIF
            CALL WRITEVL(ROW, COL, LEFTCOL, FORM, LENGTH)
            RDUP = RVALUE(NV)
            NUMDUP = 1
         ENDIF
      enddo
      WRITE(FORM,660) NUMDUP, RDUP
      LENGTH = 13
      CALL WRITEVL(ROW, COL, LEFTCOL, FORM, LENGTH)
   ENDIF

ELSE

   ! Character variable

   CDUP = CVALUE(1)
   IF(NVALUE .EQ. 1) THEN

      ! Single variable value

      LENGTH = len_trim(CDUP)
      !LENGTH = INDEX(CDUP, '        ')
      FORM = APSTRPH//CDUP(1:LENGTH)//APSTRPH
      LENGTH = LENGTH + 2
      CALL WRITEVL(ROW, COL, LEFTCOL, FORM, LENGTH)
      
   ELSE

      ! More than one variable value

      DO NV = 2, NVALUE, 1
         OLDNUM = NUMDUP
         IF(CDUP .EQ. CVALUE(NV)) NUMDUP = NUMDUP + 1
         IF(NUMDUP .EQ. OLDNUM) THEN
            LENGTH = len_trim(CDUP)
            !LENGTH = INDEX(CDUP, '        ')
            IF(CDUP(1:1) .EQ. ' ') LENGTH = 8
            IF(NUMDUP .EQ. 1) THEN
               FORM = APSTRPH//CDUP(1:LENGTH)//APSTRPH
               LENGTH = LENGTH + 2
            ELSE
               WRITE(NUMD,670) NUMDUP
               FORM = NUMD//CDUP(1:LENGTH)//APSTRPH
               LENGTH = LENGTH + 5
            ENDIF
            CALL WRITEVL(ROW, COL, LEFTCOL, FORM, LENGTH)
            CDUP = CVALUE(NV)
            NUMDUP = 1
         ENDIF
      enddo
      LENGTH = len_trim(CDUP)
      !LENGTH = INDEX(CDUP, '        ')
      IF(CDUP(1:1) .EQ. ' ') LENGTH = 8
      WRITE(NUMD,670) NUMDUP
      FORM = NUMD//CDUP(1:LENGTH)//APSTRPH
      LENGTH = LENGTH + 5
      CALL WRITEVL(ROW, COL, LEFTCOL, FORM, LENGTH)
   ENDIF

ENDIF

RETURN

600 FORMAT(I8)
610 FORMAT(G10.3)
!  650 FORMAT(I2,1H*,I8)
!  660 FORMAT(I2,1H*,G10.3)
!  670 FORMAT(I2,2H*')
650 FORMAT(I2,'*',I8)
660 FORMAT(I2,'*',G10.3)
670 FORMAT(I2,'*')

END

!***************************************************************************

SUBROUTINE WRITEVL (ROW, COL, LEFTCOL, FORM, LENGTH)
implicit none
! This routine inserts one NAMELIST variable value into the
! character array PAGE.  ROW and COL refer the the location in
! PAGE of the first value of the variable being output.  LEFTCOL
! gives the first column for any one of the values of the variable
! being inserted in PAGE.

CHARACTER*132 PAGE(80)
CHARACTER*64  FORM
CHARACTER*1   COMMA
INTEGER       COL, LEFTCOL, LENGTH, RGHTCOL, ROW

COMMON /PAGEOUT/ PAGE

DATA  COMMA/','/

RGHTCOL = LEFTCOL + LENGTH - 1
IF(ROW .LT. 1  .OR.  ROW .GT. 80) STOP 'WRITEVL'
PAGE(ROW)(LEFTCOL:RGHTCOL) = FORM(1:LENGTH)
LEFTCOL = RGHTCOL + 2
PAGE(ROW)(LEFTCOL:LEFTCOL) = COMMA
LEFTCOL = LEFTCOL + 5
IF((LEFTCOL+LENGTH) .GT. 90) THEN
   LEFTCOL = COL + 11
   ROW = ROW + 1
ENDIF

RETURN
END

!***************************************************************************

SUBROUTINE BLANKPG
implicit none
! This routine sets the output page character variable to all blanks

CHARACTER*132 PAGE(80)
CHARACTER*61 BLANKLN
integer :: i
COMMON /PAGEOUT/ PAGE

DATA BLANKLN/' '/

DO I=1,80
   PAGE(I)=BLANKLN
ENDDO

RETURN
END

!***************************************************************************

SUBROUTINE WRITEPG (GROUP,NUMROW)
implicit none
! This routine writes out the character array containing the list of
! variable names and values for a specified NAMELIST given by  GROUP

CHARACTER*132 PAGE(80)
CHARACTER*(*) GROUP
CHARACTER*9 FMT
INTEGER NR,NUMROW,nch

COMMON  /PAGEOUT/ PAGE

WRITE(6,600) GROUP
DO NR = 1, NUMROW
   DO NCH=132,1,-1
     IF(PAGE(NR)(NCH:NCH).NE.' ') GOTO 110
   ENDDO
   NCH=1
   110 CONTINUE
   WRITE(FMT,630) NCH
   630 FORMAT('(1X,A',I3,')')
   WRITE(6,FMT) PAGE(NR)(1:NCH)
ENDDO
WRITE(6,620)

600 FORMAT(///// ' NAMELIST  ', A7 / 1X, 120('-') /)
620 FORMAT(1X, 120('-') )

RETURN
END
