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

! This routine computes nice round intervals to make ticks at
! given a starting and ending value of the variable and a rough
! number of intervals desired.

SUBROUTINE NICEINC_mrc(X1,X2,NX,XINC,XLABST)

! This routine chooses from 4 to 10 intervals

  XDIF=(X2-X1)
  B=INT(ALOG10(XDIF))
  IF(XDIF.LT.1.0)THEN
     D=XDIF*10.**(-INT(B)+1.0)
  ELSE
     D=XDIF*10.**(-INT(B))
  ENDIF
  IF(D.LT.2.0)THEN
     XINC=0.2*XDIF/D
  ELSEIF(D.LT.4.5)THEN
     XINC=0.5*XDIF/D
  ELSEIF(D.LT.9.)THEN
     XINC=1.0*XDIF/D
  ELSE
     XINC=2.0*XDIF/D
  ENDIF
  XLABST=INT(X1/XINC)*XINC
  IF(X1.GT.0.)XLABST=XLABST+XINC
  XLABRT=INT(X2/XINC)*XINC
  IF(X2.LT.0.)XLABRT=XLABRT-XINC
  NX=(XLABRT-XLABST)/XINC+1.0001
  
RETURN
END

!***************************************************************************

SUBROUTINE NICEINC6_mrc(X1,X2,NX,XINC,XLABST)

! This routine chooses from 6 to 15 intervals

  XDIF=(X2-X1)
  B=INT(ALOG10(XDIF))
  IF(XDIF.LT.1.0)THEN
     D=XDIF*10.**(-INT(B)+1.0)
  ELSE
     D=XDIF*10.**(-INT(B))
  ENDIF
  IF(D.LT.1.5)THEN
     XINC=0.1*XDIF/D
  ELSEIF(D.LT.3.0)THEN
     XINC=0.2*XDIF/D
  ELSEIF(D.LT.7.5)THEN
     XINC=0.5*XDIF/D
  ELSE
     XINC=1.0*XDIF/D
  ENDIF
  XLABST=INT(X1/XINC)*XINC
  IF(X1.GT.0.)XLABST=XLABST+XINC
  XLABRT=INT(X2/XINC)*XINC
  IF(X2.LT.0.)XLABRT=XLABRT-XINC
  NX=(XLABRT-XLABST)/XINC+1.0001
RETURN
END

!***************************************************************************

FUNCTION FX(X,Y)

! This function is called by the NCAR plotting routines.
! It deals with the two possible kinds of transformations
! you can have in the horizontal direction.  If ITRANS is
! 1, a map projection set up by MAPPLOT is used.  If ITRANS
! is 0 then no change in the input is made.

COMMON/TRANS/ITRANS,M,N,XLONL,XLONR,YLATB,YLATT,YANS,MYANS

IF(ITRANS.EQ.1)THEN
  XLON=XLONL+(XLONR-XLONL)*(X-1.)/(M-1.)
  YLAT=YLATB+(YLATT-YLATB)*(Y-1.)/(N-1.)
  CALL MAPTRN(YLAT,XLON,XANS,YANS)
  FX=XANS
ELSE
  FX=X
ENDIF
RETURN
END

!***************************************************************************

FUNCTION FY(X,Y)

! This function produces 4 kinds of vertical transformation.
! If IVTRAN = 1 and IZSTRAN = 0 then the input Y is indexed
! into the array of heights which is Z.  This is good for
! irregularly spaced grid points in Z.
! If IVTRAN = 1 and IZSTRAN = 1 then the vertical direction
! is not only converted to Z, but also scaled to fit in only
! the area above the topography, which is found in the array
! ZS.
! If IVTRAN = 0 and ITRANS = 1 then the transformation is
! a mapping projection of some sort which was set up in the
! MAPPLOT subroutine and calculated in the FX function.
! If IVTRAN = 0 and ITRANS = 0 then the input Y is simply
! returned as the output FY.

COMMON/TRANS/ITRANS,M,N,XLONL,XLONR,YLATB,YLATT,YANS,MYANS
COMMON/TRANS2/IVTRAN,Z(1000),IZSTRAN,ZS(1000),ZMODTOP,ZFACTOR

IF(IVTRAN.EQ.1)THEN
  K=INT(Y)
  FCTZ=Y-K
  FY=Z(K)+(Z(K+1)-Z(K))*FCTZ
  IF(IZSTRAN.EQ.1)THEN
    I=INT(X)
    FCTX=X-I
    ZSX=ZS(I)+(ZS(I+1)-ZS(I))*FCTX
    RTG=1.-ZSX/ZMODTOP
    FY=FY*RTG+ZSX
  ENDIF
ELSE
  IF(ITRANS.EQ.1)THEN
    XLON = XLONL + (XLONR-XLONL)*(X-1.)/(M-1.)
    YLAT = YLATB + (YLATT-YLATB)*(Y-1.)/(N-1.)
    CALL MAPTRN(YLAT,XLON,XANS,YANS)
    FY=YANS
  ELSE
    FY=Y
  ENDIF
ENDIF

RETURN
END

!***************************************************************************

FUNCTION MXF(X,Y,U,V,SFX,SFY,MX,MY)

! This transformation routine is used by the NCAR vector
! routine to determine how to draw the arrows.  If ITRANS =1
! then the vectors are being projected onto a map and the U and
! V velocities need to be used along with the projection to get
! the right effect.  This only is done when the model has been
! run on a lat-lon grid so LATLON = 1.

COMMON/TRANS/ITRANS,M,N,XLONL,XLONR,YLATB,YLATT,YANS,MYANS

IF(ITRANS.EQ.1)THEN
  XLON=XLONL+(XLONR-XLONL)*(X-1.)/(M-1.)
  YLAT=YLATB+(YLATT-YLATB)*(Y-1.)/(N-1.)
  CALL MAPTRN(YLAT,XLON,XANS,YANS)
  CFCT=COS(YLAT*.0174533)
  CALL MAPTRN(YLAT+1.E-4*V,XLON+1.E-4*U/CFCT,XANS2,YANS2)
  DIS=SQRT((XANS2-XANS)**2+(YANS2-YANS)**2)
  IF(DIS.GT.0.) THEN
    UU=(XANS2-XANS)/DIS*SQRT(U**2+V**2)
    VV=(YANS2-YANS)/DIS*SQRT(U**2+V**2)
  ELSE
    UU=U
    VV=V
  ENDIF
  MXF = MX+SFX*UU*.9
  MYANS = MY+SFY*VV*.9
ELSE
  MXF = MX + U*SFX*.9
ENDIF

RETURN
END

!***************************************************************************

FUNCTION MYF(X,Y,U,V,SFX,SFY,MX,MY)

COMMON/TRANS/ITRANS,M,N,XLONL,XLONR,YLATB,YLATT,YANS,MYANS
COMMON/TRANS2/IVTRAN,Z(1000),IZSTRAN,ZS(1000),ZMODTOP,ZFACTOR

  IF(ITRANS.EQ.1)THEN
    MYF=MYANS
  ELSE
    MYF=MY + V*SFY*.8
  ENDIF

RETURN
END

FUNCTION FU(U,V)
   FU = U
RETURN
END

FUNCTION FV(U,V)
COMMON/TRANS2/IVTRAN,Z(1000),IZSTRAN,ZS(1000),ZMODTOP,ZFACTOR

FV = V*ZFACTOR

RETURN
END

!***************************************************************************

SUBROUTINE CONREC_mrc(Z,L,M,N,FLO,HI,FINC,NSET,NHI,NDOT)

! +-----------------------------------------------------------------+
! |                                                                 |
! |                Copyright (C) 1987 by UCAR                       |
! |        University Corporation for Atmospheric Research          |
! |                    All Rights Reserved                          |
! |                                                                 |
! |                 NCARGRAPHICS  Version 2.00                      |
! |                                                                 |
! +-----------------------------------------------------------------+
!
!
!
!     DIMENSION OF           Z(L,N)
!     ARGUMENTS
!
!     LATEST REVISION        June, 1987
!
!     PURPOSE                CONREC draws a contour map from data stored
!                            in a rectangular array, labeling the lines.
!
!     USAGE                  If the following assumptions are met, use
!
!                              CALL EZCNTR (Z,M,N)
!
!                              ASSUMPTIONS:
!                                  --All of the array is to be contoured.
!                                  --Contour levels are picked
!                                    internally.
!                                  --Contouring routine picks scale
!                                    factors.
!                                  --Highs and lows are marked.
!                                  --Negative lines are drawn with a
!                                    dashed line pattern.
!                                  --EZCNTR calls FRAME after drawing the
!                                    contour map.
!
!                            If these assumptions are not met, use
!
!                              CALL CONREC (Z,L,M,N,FLO,HI,FINC,NSET,
!                                           NHI,NDOT)
!
!     ARGUMENTS
!
!     ON INPUT               Z
!     FOR EZCNTR               M by N array to be contoured.
!
!                            M
!                              First dimension of Z.
!
!                            N
!                              Second dimension of Z.
!
!     ON OUTPUT              All arguments are unchanged.
!     FOR EZCNTR
!
!     ON INPUT               Z
!     FOR CONREC               The (origin of the) array to be
!                              contoured.  Z is dimensioned L by N.
!
!                            L
!                              The first dimension of Z in the calling
!                              program.
!
!                            M
!                              The number of data values to be contoured
!                              in the X-direction (the first subscript
!                              direction).  When plotting an entire
!                              array, L = M.
!
!                            N
!                              The number of data values to be contoured
!                              in the Y-direction (the second subscript
!                              direction).
!
!                            FLO
!                              The value of the lowest contour level.
!                              If FLO = HI = 0., a value rounded up from
!                              the minimum Z is generated by CONREC.
!
!                            HI
!                              The value of the highest contour level.
!                              If HI = FLO = 0., a value rounded down
!                              from the maximum Z is generated by
!                              CONREC.
!
!                            FINC
!                              > 0  Increment between contour levels.
!                              = 0  A value, which produces between 10
!                                   and 30 contour levels at nice values,
!                                   is generated by CONREC.
!                              < 0  The number of levels generated by
!                                   CONREC is ABS(FINC).
!
!                            NSET
!                              Flag to control scaling.
!                              = 0  CONREC automatically sets the
!                                   window and viewport to properly
!                                   scale the frame to the standard
!                                   configuration.
!                                   The GRIDAL entry PERIM is
!                                   called and tick marks are placed
!                                   corresponding to the data points.
!                              > 0  CONREC assumes that the user
!                                   has set the window and viewport
!                                   in such a way as to properly
!                                   scale the plotting
!                                   instructions generated by CONREC.
!                                   PERIM is not called.
!                              < 0  CONREC generates coordinates so as
!                                   to place the (untransformed) contour
!                                   plot within the limits of the
!                                   user's current window and
!                                   viewport.  PERIM is not called.
!
!                            NHI
!                              Flag to control extra information on the
!                              contour plot.
!                              = 0  Highs and lows are marked with an H
!                                   or L as appropriate, and the value
!                                   of the high or low is plotted under
!                                   the symbol.
!                              > 0  The data values are plotted at
!                                   each Z point, with the center of
!                                   the string indicating the data
!                                   point location.
!                              < 0  Neither of the above.
!
!                            NDOT
!                              A 10-bit constant designating the desired
!                              dashed line pattern.
!                              If ABS(NDOT) = 0, 1, or 1023, solid lines
!                              are drawn.
!                              > 0 NDOT pattern is used for all lines.
!                              < 0 ABS(NDOT) pattern is used for nega-
!                                tive-valued contour lines, and solid is
!                                used for positive-valued contours.
!                                CONREC converts NDOT
!                                to a 16-bit pattern and DASHDB is used.
!                                See DASHDB comments in the DASHLINE
!                                documentation for details.
!
!
!
!     ON OUTPUT              All arguments are unchanged.
!     FOR CONREC
!
!
!     ENTRY POINTS           CONREC, CLGEN, REORD, STLINE, DRLINE,
!                            MINMAX, PNTVAL, CALCNT, EZCNTR, CONBD
!
!     COMMON BLOCKS          INTPR, RECINT, CONRE1, CONRE2, CONRE3,
!                            CONRE4,CONRE5
!
!     REQUIRED LIBRARY       Standard version: DASHCHAR, which at
!     ROUTINES               NCAR is loaded by default.
!                            Smooth version: DASHSMTH which must be
!                            requested at NCAR.
!                            Both versions require  GRIDAL, the
!                            ERPRT77 package, and the SPPS.
!
!     REQUIRED GKS LEVEL     0A
!
!     I/O                    Plots contour map.
!
!     PRECISION              Single
!
!     LANGUAGE               FORTRAN 77
!
!     HISTORY                Replaces old contouring package called
!                            CALCNT at NCAR.
!
!     ALGORITHM              Each line is followed to completion.  Points
!                            along a line are found on boundaries of the
!                            (rectangular) cells. These points are
!                            connected by line segments using the
!                            software dashed line package, DASHCHAR.
!                            DASHCHAR is also used to label the
!                            lines.
!
!     NOTE                   To draw non-uniform contour levels, see
!                            the comments in CLGEN.  To make special
!                            modifications for specific needs see the
!                            explanation of the internal parameters
!                            below.
!
!     TIMING                 Varies widely with size and smoothness of
!                            Z.
!
!     INTERNAL PARAMETERS    NAME   DEFAULT         FUNCTION
!                            ----   -------         --------
!
!                            ISIZEL   1      Size of line labels,
!                                            as per the size definitions
!                                            given in the SPPS
!                                            documentation for WTSTR.
!
!                            ISIZEM   2      size of labels for minimums
!                                            and maximums,
!                                            as per the size definitions
!                                            given in the SPPS
!                                            documentation for WTSTR.
!
!                            ISIZEP   0      Size of labels for data
!                                            point values as per the size
!                                            definitions given in the SPPS
!                                            documentation for WTSTR.
!
!                            NLA      16     Approximate number of
!                                            contour levels when
!                                            internally generated.
!
!                            NLM      40     Maximum number of contour
!                                            levels.  If this is to be
!                                            increased, the dimensions
!                                            of CL and RWORK in CONREC
!                                            must be increased by the
!                                            same amount.
!
!                            XLT      .05    Left hand edge of the plot
!                                            (0.0 is the left edge of
!                                            the frame and 1.0 is the
!                                            right edge of the frame.)
!
!                            YBT      .05    Bottom edge of the plot
!                                            (0.0 is the bottom of the
!                                            frame and 1.0 is the top
!                                            of the frame.)
!
!                            SIDE     0.9    Length of longer edge of
!                                            plot (see also EXT).
!
!                            NREP     6      Number of repetitions of
!                                            the dash pattern between
!                                            line labels.
!
!                            NCRT     4      Number of CRT units per
!                                            element (bit) in the dash
!                                            pattern.
!
!                            ILAB     1      Flag to control the drawing
!                                            of line labels.
!                                            . ILAB non-zero means label
!                                              the lines.
!                                            . ILAB = 0 means do not
!                                              label the lines.
!
!                            NULBLL   3      Number of unlabeled lines
!                                            between labeled lines.  For
!                                            example, when NULBLL = 3,
!                                            every fourth level is
!                                            labeled.
!
!                            IOFFD    0      Flag to control
!                                            normalization of label
!                                            numbers.
!                                            . IOFFD = 0 means include
!                                              decimal point when
!                                              possible (do not
!                                              normalize unless
!                                              required).
!                                            . IOFFD non-zero means
!                                              normalize all label
!                                              numbers and output a
!                                              scale factor in the
!                                              message below the graph.
!
!                            EXT      .25    Lengths of the sides of the
!                                            plot are proportional to M
!                                            and N (when CONREC sets
!                                            the window and viewport).
!                                            In extreme cases, when
!                                            MIN(M,N)/MAX(M,N) is less
!                                            than EXT, CONREC
!                                            produces a square plot.
!
!                            IOFFP      0    Flag to control special
!                                            value feature.
!                                            . IOFFP = 0 means special
!                                              value feature not in use.
!                                            . IOFFP non-zero means
!                                              special value feature in
!                                              use.  (SPVAL is set to the
!                                              special value.)  Contour
!                                              lines will then be
!                                              omitted from any cell
!                                              with any corner equal to
!                                              the special value.
!
!                            SPVAL    0.     Contains the special value
!                                            when IOFFP is non-zero.
!
!                            IOFFM    0      Flag to control the message
!                                            below the plot.
!                                            . IOFFM = 0  if the message
!                                              is to be plotted.
!                                            . IOFFM non-zero if the
!                                              message is to be omitted.
!
!                            ISOLID   1023   Dash pattern for
!                                            non-negative contour lines.
!
!
!mjb      EXTERNAL        CONBD
!
SAVE
CHARACTER*1     IGAP       ,ISOL       ,RCHAR
CHARACTER       ENCSCR*22  ,IWORK*126
DIMENSION       LNGTHS(5)  ,HOLD(5)    ,WNDW(4)    ,VWPRT(4)
DIMENSION       Z(L,N)     ,CL(40)     ,RWORK(40)  ,LASF(13)
COMMON /ANACON/ GL,HA,GP,ASH
COMMON /INTPR/ PAD1, FPART, PAD(8)
COMMON /CONRE1/ IOFFP      ,SPVAL
COMMON /CONRE3/ IXBITS     ,IYBITS
COMMON /CONRE4/ ISIZEL     ,ISIZEM     ,ISIZEP     ,NREP       ,  &
                NCRT       ,ILAB       ,NULBLL     ,IOFFD      ,  &
                EXT        ,IOFFM      ,ISOLID     ,NLA        ,  &
                NLM        ,XLT        ,YBT        ,SIDE
COMMON /CONRE5/ SCLY
COMMON /RECINT/ IRECMJ     ,IRECMN     ,IRECTX
DATA  LNGTHS(1),LNGTHS(2),LNGTHS(3),LNGTHS(4),LNGTHS(5)  &
      /  12,       3,        20,       9,        17       /
DATA  ISOL, IGAP /'$', ''''/

! ISOL AND IGAP (DOLLAR-SIGN AND APOSTROPHE) ARE USED TO CONSTRUCT PAT-
! TERNS PASSED TO ROUTINE DASHDC IN THE SOFTWARE DASHED-LINE PACKAGE.

! THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR

CALL Q8QST4 ('GRAPHX','CONREC','CONREC','VERSION 01')

! NONSMOOTHING VERSION

!  CALL RESET FOR COMPATIBILITY WITH ALL DASH ROUTINES(EXCEPT DASHLINE)

CALL RESET

!  GET NUMBER OF BITS IN INTEGER ARITHMETIC

IARTH = I1MACH(8)
IXBITS = 0
DO I=1,IARTH
   IF (M .LE. (2**I-1)) GOTO 102
   IXBITS = I+1
enddo
102 continue
IYBITS = 0
DO I=1,IARTH
   IF (N .LE. (2**I-1)) GOTO 104
   IYBITS = I+1
enddo
104 continue
IF ((IXBITS*IYBITS).GT.0 .AND. (IXBITS+IYBITS).LE.24) GOTO 105

! REPORT ERROR NUMBER ONE

IWORK =    'CONREC  - DIMENSION ERROR - M*N .GT. (2**IARTH)    M =  N = '
WRITE (IWORK(56:62),'(I6)') M
WRITE (IWORK(73:79),'(I6)') N
CALL SETER( IWORK, 1, 1 )
RETURN
105 CONTINUE

! INQUIRE CURRENT TEXT AND LINE COLOR INDEX

CALL GQTXCI ( IERR, ITXCI )
CALL GQPLCI ( IERR, IPLCI )

! Set requested text color.

CALL GSTXCI(IRECTX)

! SET LINE AND TEXT ASF TO INDIVIDUAL

CALL GQASF ( IERR, LASF )
LSV3  = LASF(3)
LSV10 = LASF(10)
LASF(3)  = 1
LASF(10) = 1
CALL GSASF ( LASF )

GL = FLO
HA = HI
GP = FINC
MX = L
NX = M
NY = N
IDASH = NDOT
NEGPOS = ISIGN(1,IDASH)
IDASH = IABS(IDASH)
IF (IDASH.EQ.0 .OR. IDASH.EQ.1) IDASH = ISOLID

! SET CONTOUR LEVELS.

CALL CLGEN (Z,MX,NX,NY,GL,HA,GP,NLA,NLM,CL,NCL,ICNST)

! FIND MAJOR AND MINOR LINES

IF (ILAB .NE. 0) CALL REORD (CL,NCL,RWORK,NML,NULBLL+1)
IF (ILAB .EQ. 0) NML = 0

! SAVE CURRENT NORMALIZATION TRANS NUMBER NTORIG AND LOG SCALING FLAG

CALL GQCNTN ( IERR, NTORIG )
CALL GETUSV ('LS',IOLLS)

! SET UP SCALING

CALL GETUSV ( 'YF' , IYVAL )
SCLY = 1.0 / ISHFT ( 1, 15 - IYVAL )

IF (NSET.lt.0) GOTO 106
IF (NSET.eq.0) GOTO 107
IF (NSET.gt.0) GOTO 111
106 continue
CALL GQNT ( NTORIG,IERR,WNDW,VWPRT )
X1 = VWPRT(1)
X2 = VWPRT(2)
Y1 = VWPRT(3)
Y2 = VWPRT(4)

! SAVE NORMALIZATION TRANS 1

CALL GQNT (1,IERR,WNDW,VWPRT)

! DEFINE NORMALIZATION TRANS AND LOG SCALING

CALL SET(X1, X2, Y1, Y2, 1.0, FLOAT(NX), 1.0, FLOAT(NY), 1)
GOTO 111
107 CONTINUE
X1 = XLT
X2 = XLT+SIDE
Y1 = YBT
Y2 = YBT+SIDE
X3 = NX
Y3 = NY
IF (AMIN1(X3,Y3)/AMAX1(X3,Y3) .LT. EXT) GO TO 110
IF (NX.lt.NY) GOTO 108
IF (NX.eq.NY) GOTO 110
IF (NX.gt.NY) GOTO 109
108 continue
X2 = SIDE*X3/Y3+XLT
GOTO 110
109 continue
Y2 = SIDE*Y3/X3+YBT

! SAVE NORMALIZATION TRANS 1

110 continue
CALL GQNT ( 1, IERR, WNDW, VWPRT )

! DEFINE NORMALIZATION TRANS 1 AND LOG SCALING

CALL SET(X1,X2,Y1,Y2,1.0,X3,1.0,Y3,1)

! DRAW PERIMETER

CALL PERIM (NX-1,1,NY-1,1)
111 continue
IF (ICNST .NE. 0) GOTO 124

! SET UP LABEL SCALING

IOFFDT = IOFFD
IF (GL.NE.0.0 .AND. (ABS(GL).LT.0.1 .OR. ABS(GL).GE.1.E5)) IOFFDT = 1
IF (HA.NE.0.0 .AND. (ABS(HA).LT.0.1 .OR. ABS(HA).GE.1.E5)) IOFFDT = 1
ASH = 10.**(3-IFIX(ALOG10(AMAX1(ABS(GL),ABS(HA),ABS(GP)))-5000.)-5000)
IF (IOFFDT .EQ. 0) ASH = 1.

! CT change
!IF (IOFFM .NE. 0) GOTO 115
!goto 115
!IWORK ='CONTOUR FROM              TO              CONTOUR INTERVAL OF              PT(3,3)=              LABELS SCALED BY'
!HOLD(1) = GL
!HOLD(2) = HA
!HOLD(3) = GP
!HOLD(4) = Z(3,3)
!HOLD(5) = ASH
!NCHAR = 0
!DO I=1,5
!   WRITE ( ENCSCR, '(G13.5)' ) HOLD(I)
!   NCHAR = NCHAR+LNGTHS(I)
!   DO J=1,13
!      NCHAR = NCHAR+1
!      IWORK(NCHAR:NCHAR) = ENCSCR(J:J)
!   enddo
!enddo
!IF (ASH .EQ. 1.) NCHAR = NCHAR-13-LNGTHS(5)

! WRITE TITLE USING NORMALIZATION TRANS NUMBER 0

!CALL GETUSV('LS',LSO)
!CALL SETUSV('LS',1)
!CALL GSELNT (0)
!CALL WTSTR ( 0.5, 0.015625, IWORK(1:NCHAR), 0, 0, 0 )
!CALL SETUSV('LS',LSO)
!CALL GSELNT (1)

!                       * * * * * * * * * *

! PROCESS EACH LEVEL

115 FPART = .5

DO I=1,NCL
   CALL PLOTIT(0,0,0)
   CALL GSPLCI ( IRECMJ )
   CONTR = CL(I)
   NDASH = IDASH
   IF (NEGPOS.LT.0 .AND. CONTR.GE.0.) NDASH = ISOLID

!  CHANGE 10 BIT PATTERN TO 10 CHARACTER PATTERN.

   DO J=1,10
      IBIT = IAND(ISHFT(NDASH,(J-10)),1)
      RCHAR = IGAP
      IF (IBIT .NE. 0) RCHAR = ISOL
      IWORK(J:J) = RCHAR
   enddo
   IF (I .GT. NML) GOTO 121

!  SET UP MAJOR LINE (LABELED)

!  NREP REPITITIONS OF PATTERN PER LABEL.

   NCHAR = 10
   IF (NREP .LT. 2) GOTO 119
   DO J=1,10
      NCHAR = J
      RCHAR = IWORK(J:J)
      DO K=2,NREP
         NCHAR = NCHAR+10
         IWORK(NCHAR:NCHAR) = RCHAR
      enddo
   enddo
   119 CONTINUE

!  PUT IN LABEL.

   CALL ENCD (CONTR,ASH,ENCSCR,NCUSED,IOFFDT)
   DO J=1,NCUSED
      NCHAR = NCHAR+1
      IWORK(NCHAR:NCHAR) = ENCSCR(J:J)
   ENDDO
   GOTO 122
 
!  SET UP MINOR LINE (UNLABELED).

   121 CONTINUE

!  SET LINE INTENSITY TO LOW

   CALL GSPLCI ( IRECMN )
   NCHAR = 10
   122 continue
   CALL DASHDC ( IWORK(1:NCHAR),NCRT, ISIZEL )

!  DRAW ALL LINES AT THIS LEVEL.

   CALL STLINE (Z,MX,NX,NY,CONTR)

enddo
CALL GSPLCI(IRECMJ)

! FIND RELATIVE MINIMUMS AND MAXIMUMS IF WANTED, AND MARK VALUES IF WANTED.

IF (NHI .EQ. 0) CALL MINMAX (Z,MX,NX,NY,ISIZEM,ASH,IOFFDT)
IF (NHI .GT. 0) CALL MINMAX (Z,MX,NX,NY,ISIZEP,-ASH,IOFFDT)
FPART = 1.
GOTO 127
124 CONTINUE
IWORK = 'CONSTANT FIELD'
WRITE( ENCSCR, '(G22.14)' ) GL
DO I=1,22
   IWORK(I+14:I+14) = ENCSCR(I:I)
enddo

! WRITE TITLE USING NORMALIZATION TRNS 0

CALL GETUSV('LS',LSO)
CALL SETUSV('LS',1)
CALL GSELNT (0)
CALL WTSTR ( 0.09765, 0.48825, IWORK(1:36), 3, 0, -1 )

! RESTORE NORMALIZATION TRANS 1, LINE AND TEXT INTENSITY TO ORIGINAL

127 continue
IF (NSET.LE.0) THEN
    CALL SET(VWPRT(1),VWPRT(2),VWPRT(3),VWPRT(4),  &
             WNDW(1),WNDW(2),WNDW(3),WNDW(4),IOLLS)
END IF
CALL GSPLCI ( IPLCI )
CALL GSTXCI ( ITXCI )

! SELECT ORIGINAL NORMALIZATION TRANS NUMBER NTORIG, AND RESTORE ASF

CALL GSELNT ( NTORIG )
LASF(3)  = LSV3
LASF(10) = LSV10
CALL GSASF ( LASF )

RETURN

END

!***************************************************************************

SUBROUTINE STRMLN_mrc(Ux,Vx,U,V,WORK,IMAX,IPTSX,JPTSY,NSET,IER)
DIMENSION       Ux(IMAX+1,JPTSY+1)    ,Vx(IMAX+1,JPTSY+1)      ,  &
                U(IMAX,JPTSY)         ,V(IMAX,JPTSY)           ,  &
                WORK(1)
DIMENSION       WIND(4)               ,VIEW(4)
!
COMMON /STR01/  IS         ,IEND      ,JS        ,JEND  &
             ,  IEND1      ,JEND1     ,I         ,J  &
             ,  X          ,Y         ,DELX      ,DELY  &
             ,  ICYC1      ,IMSG1     ,IGFL1
COMMON /STR02/  EXT , SIDE , XLT , YBT
COMMON /STR03/  INITA , INITB , AROWL , ITERP , ITERC , IGFLG  &
             ,  IMSG , UVMSG , ICYC , DISPL , DISPC , CSTOP

SAVE

! fix to give a nii by njj array going into nii by njj (not nii+1 by njj+1)
do i=1,IMAX
  do j=1,JPTSY
      U(i,j)=Ux(i,j)
      V(i,j)=Vx(i,j)
   enddo
enddo

EXT       = 0.25
SIDE      = 0.90
XLT       = 0.05
YBT       = 0.05

!INITA     = 2
!INITB     = 2
AROWL     = 0.33
ITERP     = 35
ITERC     = -99
IGFLG     = 0
ICYC      = 0
IMSG      = 0
UVMSG     = 1.E+36
DISPL     = 0.33
DISPC     = 0.67
CSTOP     = 0.50

! THE FOLLOWING CALL IS FOR MONITORING LIBRARY USE AT NCAR

CALL Q8QST4 ( 'GRAPHX', 'STRMLN', 'STRMLN', 'VERSION 01')

IER = 0

! LOAD THE COMMUNICATION COMMON BLOCK WITH PARAMETERS

IS = 1
IEND = IPTSX
JS = 1
JEND = JPTSY
IEND1 = IEND-1
JEND1 = JEND-1
IEND2 = IEND-2
JEND2 = JEND-2
XNX = FLOAT(IEND-IS+1)
XNY = FLOAT(JEND-JS+1)
ICYC1 = ICYC
IGFL1 = IGFLG
IMSG1 = 0

! IF ICYC .NE. 0 THEN CHECK TO MAKE SURE THE CYCLIC CONDITION EXISTS.

IF (ICYC1.NE.0) CALL CHKCYC  (U,V,IMAX,JPTSY,IER)

! Save original SET call.

CALL GETSET (VIEW(1),VIEW(2),VIEW(3),VIEW(4),  &
             WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)

IF (NSET.lt.0) GOTO 10
IF (NSET.eq.0) GOTO 20
IF (NSET.gt.0) GOTO 60

10 X1 = VIEW(1)
X2 = VIEW(2)
Y1 = VIEW(3)
Y2 = VIEW(4)
X3 = IS
X4 = IEND
Y3 = JS
Y4 = JEND
GOTO  55

20 ITYPE = 1
X1 = XLT
X2 = (XLT+SIDE)
Y1 = YBT
Y2 = (YBT+SIDE)
X3 = IS
X4 = IEND
Y3 = JS
Y4 = JEND
IF (AMIN1(XNX,XNY)/AMAX1(XNX,XNY).LT.EXT) GO TO  50
IF (XNX.lt.XNY) GOTO 30
IF (XNX.eq.XNY) GOTO 50
IF (XNX.gt.XNY) GOTO 40
30 X2 = (SIDE*(XNX/XNY) + XLT)
GOTO  50
40 Y2 = (SIDE*(XNY/XNX) + YBT)
50 CONTINUE

! CENTER THE PLOT

DX = 0.25*( 1. - (X2-X1) )
DY = 0.25*( 1. - (Y2-Y1) )
X1 = (XLT+DX)
X2 = (X2+DX )
Y1 = (YBT+DY)
Y2 = (Y2+DY )

55 CONTINUE

! DEFINE AND SELECT NORMALIZATION TRANS, SET LOG SCALING

CALL SET(X1,X2,Y1,Y2,X3,X4,Y3,Y4,ITYPE)

IF (NSET.EQ.0) CALL PERIM (1,0,1,0)

60 CONTINUE

! DRAW THE STREAMLINES
! .   BREAK THE WORK ARRAY INTO TWO PARTS.  SEE DRWSTR FOR FURTHER
! .   COMMENTS ON THIS.


CALL DRWSTR (U,V,WORK(1),WORK(IMAX*JPTSY+1),IMAX,JPTSY)

! Restore SET call.

CALL SET (VIEW(1),VIEW(2),VIEW(3),VIEW(4),  &
          WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)

RETURN
END
