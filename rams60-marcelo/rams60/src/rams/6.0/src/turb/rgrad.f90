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

subroutine grad(m1,m2,m3,ia,iz,ja,jz  &
     ,vc3da,vc3db,dir,gpnt)
implicit none
integer :: m1,m2,m3,ia,iz,ja,jz
real :: vc3da(m1,m2,m3),vc3db(m1,m2,m3)
character(len=*) :: dir,gpnt
character(len=6) :: optyp

optyp='GRADNT'

call rams_grad(m1,m2,m3,ia,iz,ja,jz,VC3DA,VC3DB,DIR,GPNT,optyp)

return
end

subroutine divcart(m1,m2,m3,ia,iz,ja,jz,vc3da,vc3db,dir,gpnt)
implicit none
integer :: m1,m2,m3,ia,iz,ja,jz
real :: vc3da(m1,m2,m3),vc3db(m1,m2,m3)
character(len=*) :: dir,gpnt
character(len=6) :: optyp

optyp='DIVCRT'

call rams_grad(m1,m2,m3,ia,iz,ja,jz,VC3DA,VC3DB,DIR,GPNT,optyp)

return
end

subroutine divstar(m1,m2,m3,ia,iz,ja,jz,vc3da,vc3db,dir,gpnt)
implicit none
integer :: m1,m2,m3,ia,iz,ja,jz
real :: vc3da(m1,m2,m3),vc3db(m1,m2,m3)
character(len=*) :: dir,gpnt
character(len=6) :: optyp

optyp='DIVSTR'

call rams_grad(m1,m2,m3,ia,iz,ja,jz,VC3DA,VC3DB,DIR,GPNT,optyp)

return
end


subroutine rams_grad(m1,m2,m3,ia,iz,ja,jz,vc3da,vc3db,dir,gpnt,optyp)

use mem_grid
use mem_scratch

implicit none
integer :: m1,m2,m3,ia,iz,ja,jz

real :: vc3da(m1,m2,m3),vc3db(m1,m2,m3)
character(len=*) :: dir,gpnt
character(len=6) :: optyp

integer :: jaa,jzz

jaa=ja
jzz=jz
if(jdim.eq.0) then
   jaa=1
   jzz=1
endif

IF(DIR.EQ.'XDIR')THEN
   IF(GPNT.EQ.'UPNT')THEN
      CALL GRADXU(m1,m2,m3,ia,iz,jaa,jzz  &
           ,OPTYP,VC3DA,VC3DB,VCTR1,GRID_G(NGRID)%RTGU(1,1)  &
           ,GRID_G(NGRID)%RTGT(1,1),GRID_G(NGRID)%DXT(1,1),DZT  &
           ,GRID_G(NGRID)%FMAPUI(1,1),GRID_G(NGRID)%FMAPT(1,1)  &
           ,GRID_G(NGRID)%F13T(1,1)  &
           ,HW,VCTR2,'T',JDIM)
   ELSEIF(GPNT.EQ.'VPNT')THEN
      CALL GRADXT(m1,m2,m3,ia,iz,jaa,jzz  &
           ,OPTYP,VC3DA,VC3DB,VCTR1,GRID_G(NGRID)%RTGV(1,1)  &
           ,GRID_G(NGRID)%RTGM(1,1),GRID_G(NGRID)%DXM(1,1),DZT  &
           ,GRID_G(NGRID)%FMAPVI(1,1),GRID_G(NGRID)%FMAPM(1,1)  &
           ,GRID_G(NGRID)%F13M(1,1)  &
           ,HW,VCTR2,'T',JDIM)
   ELSEIF(GPNT.EQ.'WPNT')THEN
      CALL GRADXT(m1,m2,m3,ia,iz,jaa,jzz  &
           ,OPTYP,VC3DA,VC3DB,VCTR1,GRID_G(NGRID)%RTGT(1,1)  &
           ,GRID_G(NGRID)%RTGU(1,1),GRID_G(NGRID)%DXU(1,1),DZM  &
           ,GRID_G(NGRID)%FMAPTI(1,1),GRID_G(NGRID)%FMAPU(1,1)  &
           ,GRID_G(NGRID)%F13U(1,1)  &
           ,HT,VCTR2,'W',JDIM)
   ELSEIF(GPNT.EQ.'TPNT')THEN
      CALL GRADXT(m1,m2,m3,ia,iz,jaa,jzz  &
           ,OPTYP,VC3DA,VC3DB,VCTR1,GRID_G(NGRID)%RTGT(1,1)  &
           ,GRID_G(NGRID)%RTGU(1,1),GRID_G(NGRID)%DXU(1,1),DZT  &
           ,GRID_G(NGRID)%FMAPTI(1,1),GRID_G(NGRID)%FMAPU(1,1)  &
           ,GRID_G(NGRID)%F13U(1,1)  &
           ,HW,VCTR2,'T',JDIM)
   ELSEIF(GPNT.EQ.'NPNT')THEN
      CALL GRADXT(m1,m2,m3,ia,iz,jaa,jzz  &
           ,OPTYP,VC3DA,VC3DB,VCTR1,GRID_G(NGRID)%RTGV(1,1)  &
           ,GRID_G(NGRID)%RTGM(1,1),GRID_G(NGRID)%DXM(1,1),DZM  &
           ,GRID_G(NGRID)%FMAPVI(1,1),GRID_G(NGRID)%FMAPM(1,1)  &
           ,GRID_G(NGRID)%F13M(1,1)  &
           ,HT,VCTR2,'W',JDIM)
   ELSEIF(GPNT.EQ.'OPNT')THEN
      CALL GRADXU(m1,m2,m3,ia,iz,jaa,jzz  &
           ,OPTYP,VC3DA,VC3DB,VCTR1,GRID_G(NGRID)%RTGU(1,1)  &
           ,GRID_G(NGRID)%RTGT(1,1),GRID_G(NGRID)%DXT(1,1),DZM  &
           ,GRID_G(NGRID)%FMAPUI(1,1),GRID_G(NGRID)%FMAPT(1,1)  &
           ,GRID_G(NGRID)%F13T(1,1)  &
           ,HT,VCTR2,'W',JDIM)
   ELSEIF(GPNT.EQ.'PPNT')THEN
      CALL GRADXU(m1,m2,m3,ia,iz,jaa,jzz  &
           ,OPTYP,VC3DA,VC3DB,VCTR1,GRID_G(NGRID)%RTGM(1,1)  &
           ,GRID_G(NGRID)%RTGV(1,1),GRID_G(NGRID)%DXV(1,1),DZT  &
           ,GRID_G(NGRID)%FMAPMI(1,1),GRID_G(NGRID)%FMAPV(1,1)  &
           ,GRID_G(NGRID)%F13V(1,1)  &
           ,HW,VCTR2,'T',JDIM)
   ELSEIF(GPNT.EQ.'MPNT')THEN
      CALL GRADXU(m1,m2,m3,ia,iz,jaa,jzz  &
           ,OPTYP,VC3DA,VC3DB,VCTR1,GRID_G(NGRID)%RTGM(1,1)  &
           ,GRID_G(NGRID)%RTGV(1,1),GRID_G(NGRID)%DXV(1,1),DZM  &
           ,GRID_G(NGRID)%FMAPMI(1,1),GRID_G(NGRID)%FMAPV(1,1)  &
           ,GRID_G(NGRID)%F13V(1,1)  &
           ,HT,VCTR2,'W',JDIM)
   ENDIF
ELSEIF(DIR.EQ.'YDIR')THEN
   IF(GPNT.EQ.'UPNT')THEN
      CALL GRADYT(m1,m2,m3,ia,iz,jaa,jzz  &
           ,OPTYP,VC3DA,VC3DB,VCTR1,GRID_G(NGRID)%RTGU(1,1)  &
           ,GRID_G(NGRID)%RTGM(1,1),GRID_G(NGRID)%DYM(1,1),DZT  &
           ,GRID_G(NGRID)%FMAPUI(1,1),GRID_G(NGRID)%FMAPM(1,1)  &
           ,GRID_G(NGRID)%F23M(1,1)  &
           ,HW,VCTR2,'T',JDIM)
   ELSEIF(GPNT.EQ.'VPNT')THEN
      CALL GRADYV(m1,m2,m3,ia,iz,jaa,jzz  &
           ,OPTYP,VC3DA,VC3DB,VCTR1,GRID_G(NGRID)%RTGV(1,1)  &
           ,GRID_G(NGRID)%RTGT(1,1),GRID_G(NGRID)%DYT(1,1),DZT  &
           ,GRID_G(NGRID)%FMAPVI(1,1),GRID_G(NGRID)%FMAPT(1,1)  &
           ,GRID_G(NGRID)%F23T(1,1)  &
           ,HW,VCTR2,'T',JDIM)
   ELSEIF(GPNT.EQ.'WPNT')THEN
      CALL GRADYT(m1,m2,m3,ia,iz,jaa,jzz  &
           ,OPTYP,VC3DA,VC3DB,VCTR1,GRID_G(NGRID)%RTGT(1,1)  &
           ,GRID_G(NGRID)%RTGV(1,1),GRID_G(NGRID)%DYV(1,1),DZM  &
           ,GRID_G(NGRID)%FMAPTI(1,1),GRID_G(NGRID)%FMAPV(1,1)  &
           ,GRID_G(NGRID)%F23V(1,1)  &
           ,HT,VCTR2,'W',JDIM)
   ELSEIF(GPNT.EQ.'TPNT')THEN
      CALL GRADYT(m1,m2,m3,ia,iz,jaa,jzz  &
           ,OPTYP,VC3DA,VC3DB,VCTR1,GRID_G(NGRID)%RTGT(1,1)  &
           ,GRID_G(NGRID)%RTGV(1,1),GRID_G(NGRID)%DYV(1,1),DZT  &
           ,GRID_G(NGRID)%FMAPTI(1,1),GRID_G(NGRID)%FMAPV(1,1)  &
           ,GRID_G(NGRID)%F23V(1,1)  &
           ,HW,VCTR2,'T',JDIM)
   ELSEIF(GPNT.EQ.'NPNT')THEN
      CALL GRADYV(m1,m2,m3,ia,iz,jaa,jzz  &
           ,OPTYP,VC3DA,VC3DB,VCTR1,GRID_G(NGRID)%RTGV(1,1)  &
           ,GRID_G(NGRID)%RTGT(1,1),GRID_G(NGRID)%DYT(1,1),DZM  &
           ,GRID_G(NGRID)%FMAPVI(1,1),GRID_G(NGRID)%FMAPT(1,1)  &
           ,GRID_G(NGRID)%F23T(1,1)  &
           ,HT,VCTR2,'W',JDIM)
   ELSEIF(GPNT.EQ.'OPNT')THEN
      CALL GRADYT(m1,m2,m3,ia,iz,jaa,jzz  &
           ,OPTYP,VC3DA,VC3DB,VCTR1,GRID_G(NGRID)%RTGU(1,1)  &
           ,GRID_G(NGRID)%RTGM(1,1),GRID_G(NGRID)%DYM(1,1),DZM  &
           ,GRID_G(NGRID)%FMAPUI(1,1),GRID_G(NGRID)%FMAPM(1,1)  &
           ,GRID_G(NGRID)%F23M(1,1)  &
           ,HT,VCTR2,'W',JDIM)
   ELSEIF(GPNT.EQ.'PPNT')THEN
      CALL GRADYV(m1,m2,m3,ia,iz,jaa,jzz  &
           ,OPTYP,VC3DA,VC3DB,VCTR1,GRID_G(NGRID)%RTGM(1,1)  &
           ,GRID_G(NGRID)%RTGU(1,1),GRID_G(NGRID)%DYU(1,1),DZT  &
           ,GRID_G(NGRID)%FMAPMI(1,1),GRID_G(NGRID)%FMAPU(1,1)  &
           ,GRID_G(NGRID)%F23U(1,1)  &
           ,HW,VCTR2,'T',JDIM)
   ELSEIF(GPNT.EQ.'MPNT')THEN
      CALL GRADYV(m1,m2,m3,ia,iz,jaa,jzz  &
           ,OPTYP,VC3DA,VC3DB,VCTR1,GRID_G(NGRID)%RTGM(1,1)  &
           ,GRID_G(NGRID)%RTGU(1,1),GRID_G(NGRID)%DYU(1,1),DZM  &
           ,GRID_G(NGRID)%FMAPMI(1,1),GRID_G(NGRID)%FMAPU(1,1)  &
           ,GRID_G(NGRID)%F23U(1,1)  &
           ,HT,VCTR2,'W',JDIM)
   ENDIF
ELSEIF(DIR.EQ.'ZDIR')THEN
   IF(GPNT.EQ.'UPNT')THEN
      CALL GRADZT(m1,m2,m3,ia,iz,jaa,jzz,VC3DA,VC3DB  &
         ,GRID_G(NGRID)%RTGU(1,1),DZM)
   ELSEIF(GPNT.EQ.'VPNT')THEN
      CALL GRADZT(m1,m2,m3,ia,iz,jaa,jzz,VC3DA,VC3DB  &
         ,GRID_G(NGRID)%RTGV(1,1),DZM)
   ELSEIF(GPNT.EQ.'WPNT')THEN
      CALL GRADZW(m1,m2,m3,ia,iz,jaa,jzz,VC3DA,VC3DB  &
         ,GRID_G(NGRID)%RTGT(1,1),DZT)
   ELSEIF(GPNT.EQ.'TPNT')THEN
      CALL GRADZT(m1,m2,m3,ia,iz,jaa,jzz,VC3DA,VC3DB  &
         ,GRID_G(NGRID)%RTGT(1,1),DZM)
   ELSEIF(GPNT.EQ.'NPNT')THEN
      CALL GRADZW(m1,m2,m3,ia,iz,jaa,jzz,VC3DA,VC3DB  &
         ,GRID_G(NGRID)%RTGV(1,1),DZT)
   ELSEIF(GPNT.EQ.'OPNT')THEN
      CALL GRADZW(m1,m2,m3,ia,iz,jaa,jzz,VC3DA,VC3DB  &
         ,GRID_G(NGRID)%RTGU(1,1),DZT)
   ELSEIF(GPNT.EQ.'PPNT')THEN
      CALL GRADZT(m1,m2,m3,ia,iz,jaa,jzz,VC3DA,VC3DB  &
         ,GRID_G(NGRID)%RTGM(1,1),DZM)
   ELSEIF(GPNT.EQ.'MPNT')THEN
      CALL GRADZW(m1,m2,m3,ia,iz,jaa,jzz,VC3DA,VC3DB  &
         ,GRID_G(NGRID)%RTGM(1,1),DZT)
   ENDIF
ENDIF

RETURN
END

!     ******************************************************************
!
!     This is a general subroutine which computes any component of the
!     gradient or divergence of VC3DA and stores it in VC3DB.

subroutine gradxu(m1,m2,m3,ia,iz,ja,jz  &
     ,optyp,vc3da,vc3db,vc1da,rtge,rtgc  &
     ,dx,dz,fmapi,fmap,fq,hq,hq4,lev,jd)
implicit none
integer :: m1,m2,m3,ia,iz,ja,jz,jd
real :: vc3da(m1,m2,m3),vc3db(m1,m2,m3),vc1da(*)  &
  ,rtge(m2,m3),rtgc(m2,m3),dx(m2,m3)  &
  ,fmap(m2,m3),fmapi(m2,m3),dz(*),fq(m2,m3),hq(*),hq4(*)
character(len=*) :: optyp,lev

integer :: i,j,k

IF(OPTYP.EQ.'GRADNT')THEN
   DO J=ja,jz
      DO I=ia,iz
         DO K=1,m1
            VC3DB(K,I,J)=(VC3DA(K,I,J)*RTGE(I,J)  &
                 -VC3DA(K,I-1,J)*RTGE(I-1,J))  &
                 *DX(I,J)/RTGC(I,J)
         ENDDO
      ENDDO
   ENDDO
ELSE
   DO J=ja,jz
      DO I=ia,iz
         DO K=1,m1
            VC3DB(K,I,J)=(VC3DA(K,I,J)*RTGE(I,J)  &
                 *FMAPI(I,J)  &
                 -VC3DA(K,I-1,J)*RTGE(I-1,J)  &
                 *FMAPI(I-1,J))  &
                 *DX(I,J)/RTGC(I,J)*FMAP(I,J)
         ENDDO
      ENDDO
   ENDDO
ENDIF

IF(OPTYP.NE.'DIVSTR')THEN
   IF(LEV.EQ.'W')THEN
      DO K=1,m1
         HQ4(K)=0.25*HQ(K)
      ENDDO
   ELSE
      DO K=2,m1
         HQ4(K)=0.25*HQ(K-1)
      ENDDO
   ENDIF

   DO J=ja,jz
      DO I=ia,iz
         DO K=2,m1
            VC1DA(K)=HQ4(K)*(VC3DA(K,I,J)+VC3DA(K-1,I,J)  &
                 +VC3DA(K,I-1,J)+VC3DA(K-1,I-1,J))
         ENDDO
         IF(OPTYP .NE. 'GRADNT')   VC1DA(2) = 0.
         
         DO K=2,m1-1
            VC3DB(K,I,J)=VC3DB(K,I,J)  &
                 +FQ(I,J)*DZ(K)*(VC1DA(K+1)-VC1DA(K))
         ENDDO
         VC3DB(1,I,J)=VC3DB(2,I,J)
         IF(LEV.EQ.'W')VC3DB(m1-1,I,J)=VC3DB(m1-2,I,J)
         IF(LEV.EQ.'T')VC3DB(m1,I,J)=VC3DB(m1-1,I,J)
      ENDDO
   ENDDO
ENDIF

RETURN
END

SUBROUTINE GRADXT(m1,m2,m3,ia,iz,ja,jz  &
     ,OPTYP,VC3DA,VC3DB,VC1DA,RTGE,RTGC  &
     ,DX,DZ,FMAPI,FMAP,FQ,HQ,HQ4,LEV,JD)
implicit none
integer :: m1,m2,m3,ia,iz,ja,jz,jd
real :: VC3DA(m1,m2,m3),VC3DB(m1,m2,m3),VC1DA(*)  &
  ,RTGE(m2,m3),RTGC(m2,m3),DX(m2,m3)  &
  ,FMAP(m2,m3),FMAPI(m2,m3),DZ(*),FQ(m2,m3),HQ(*),HQ4(*)
character(len=*) :: OPTYP,LEV

integer :: i,j,k

IF(OPTYP.EQ.'GRADNT')THEN
   DO J=ja,jz
      DO I=ia,iz
         DO K=1,m1
            VC3DB(K,I,J)=(VC3DA(K,I+1,J)*RTGE(I+1,J)  &
                 -VC3DA(K,I,J)*RTGE(I,J))  &
                 *DX(I,J)/RTGC(I,J)
         ENDDO
      ENDDO
   ENDDO
ELSE
   DO J=ja,jz
      DO I=ia,iz
         DO K=1,m1
            VC3DB(K,I,J)=(VC3DA(K,I+1,J)*RTGE(I+1,J)  &
                 *FMAPI(I+1,J)  &
                 -VC3DA(K,I,J)*RTGE(I,J)  &
                 *FMAPI(I,J))  &
                 *DX(I,J)/RTGC(I,J)*FMAP(I,J)
         ENDDO
      ENDDO
   ENDDO
ENDIF

IF(OPTYP.NE.'DIVSTR')THEN
   IF(LEV.EQ.'W')THEN
      DO K=1,m1
         HQ4(K)=0.25*HQ(K)
      ENDDO
   ELSE
      DO K=2,m1
         HQ4(K)=0.25*HQ(K-1)
      ENDDO
   ENDIF

   DO J=ja,jz
      DO I=ia,iz
         DO K=2,m1
            VC1DA(K)=HQ4(K)*(VC3DA(K,I,J)+VC3DA(K-1,I,J)  &
                 +VC3DA(K,I+1,J)+VC3DA(K-1,I+1,J))
         ENDDO
         IF(OPTYP .NE. 'GRADNT')   VC1DA(2) = 0.

         DO K=2,m1-1
            VC3DB(K,I,J)=VC3DB(K,I,J)  &
                 +FQ(I,J)*DZ(K)*(VC1DA(K+1)-VC1DA(K))
         ENDDO
         VC3DB(1,I,J)=VC3DB(2,I,J)
         IF(LEV.EQ.'W')VC3DB(m1-1,I,J)=VC3DB(m1-2,I,J)
         IF(LEV.EQ.'T')VC3DB(m1,I,J)=VC3DB(m1-1,I,J)
      ENDDO
   ENDDO
ENDIF

RETURN
END
!
SUBROUTINE GRADYV(m1,m2,m3,ia,iz,ja,jz  &
     ,OPTYP,VC3DA,VC3DB,VC1DA,RTGE,RTGC  &
     ,DY,DZ,FMAPI,FMAP,FQ,HQ,HQ4,LEV,JD)
implicit none
integer :: m1,m2,m3,ia,iz,ja,jz,jd
real :: VC3DA(m1,m2,m3),VC3DB(m1,m2,m3),VC1DA(*)  &
  ,RTGE(m2,m3),RTGC(m2,m3),DY(m2,m3)  &
  ,FMAP(m2,m3),FMAPI(m2,m3),DZ(*),FQ(m2,m3),HQ(*),HQ4(*)
character(len=*) :: OPTYP,LEV

integer :: i,j,k

IF(OPTYP.EQ.'GRADNT')THEN
   DO J=ja,jz
      DO I=ia,iz
         DO K=1,m1
            VC3DB(K,I,J)=(VC3DA(K,I,J)*RTGE(I,J)  &
                 -VC3DA(K,I,J-jd)*RTGE(I,J-jd))  &
                 *DY(I,J)/RTGC(I,J)
         ENDDO
      ENDDO
   ENDDO
ELSE
   DO J=ja,jz
      DO I=ia,iz
         DO K=1,m1
            VC3DB(K,I,J)=(VC3DA(K,I,J)*RTGE(I,J)  &
                 *FMAPI(I,J)  &
                 -VC3DA(K,I,J-jd)*RTGE(I,J-jd)  &
                 *FMAPI(I,J-jd))  &
                 *DY(I,J)/RTGC(I,J)*FMAP(I,J)
         ENDDO
      ENDDO
   ENDDO
ENDIF

IF(OPTYP.NE.'DIVSTR')THEN
   IF(LEV.EQ.'W')THEN
      DO K=1,m1
         HQ4(K)=0.25*HQ(K)
      ENDDO
   ELSE
      DO K=2,m1
         HQ4(K)=0.25*HQ(K-1)
      ENDDO
   ENDIF

   DO J=ja,jz
      DO I=ia,iz
         DO K=2,m1
            VC1DA(K)=HQ4(K)*(VC3DA(K,I,J)+VC3DA(K-1,I,J)  &
                 +VC3DA(K,I,J-jd)+VC3DA(K-1,I,J-jd))
         ENDDO
         IF(OPTYP .NE. 'GRADNT')   VC1DA(2) = 0.

         DO K=2,m1-1
            VC3DB(K,I,J)=VC3DB(K,I,J)  &
                 +FQ(I,J)*DZ(K)*(VC1DA(K+1)-VC1DA(K))
         ENDDO
         VC3DB(1,I,J)=VC3DB(2,I,J)
         IF(LEV.EQ.'W')VC3DB(m1-1,I,J)=VC3DB(m1-2,I,J)
         IF(LEV.EQ.'T')VC3DB(m1,I,J)=VC3DB(m1-1,I,J)
      ENDDO
   ENDDO
ENDIF

RETURN
END

SUBROUTINE GRADYT(m1,m2,m3,ia,iz,ja,jz  &
     ,OPTYP,VC3DA,VC3DB,VC1DA,RTGE,RTGC  &
     ,DY,DZ,FMAPI,FMAP,FQ,HQ,HQ4,LEV,JD)
implicit none
integer :: m1,m2,m3,ia,iz,ja,jz,jd
real :: VC3DA(m1,m2,m3),VC3DB(m1,m2,m3),VC1DA(*)  &
     ,RTGE(m2,m3),RTGC(m2,m3),DY(m2,m3)  &
     ,FMAP(m2,m3),FMAPI(m2,m3),DZ(*),FQ(m2,m3),HQ(*),HQ4(*)
character(len=*) :: OPTYP,LEV

integer :: i,j,k

IF(OPTYP.EQ.'GRADNT')THEN
   DO J=ja,jz
      DO I=ia,iz
         DO K=1,m1
            VC3DB(K,I,J)=(VC3DA(K,I,J+jd)*RTGE(I,J+jd)  &
                 -VC3DA(K,I,J)*RTGE(I,J))  &
                 *DY(I,J)/RTGC(I,J)
         ENDDO
      ENDDO
   ENDDO
ELSE
   DO J=ja,jz
      DO I=ia,iz
         DO K=1,m1
            VC3DB(K,I,J)=(VC3DA(K,I,J+jd)*RTGE(I,J+jd)  &
                 *FMAPI(I,J+jd)  &
                 -VC3DA(K,I,J)*RTGE(I,J)  &
                 *FMAPI(I,J))  &
                 *DY(I,J)/RTGC(I,J)*FMAP(I,J)
         ENDDO
      ENDDO
   ENDDO
ENDIF

IF(OPTYP.NE.'DIVSTR')THEN
   IF(LEV.EQ.'W')THEN
      DO K=1,m1
         HQ4(K)=0.25*HQ(K)
      ENDDO
   ELSE
      DO K=2,m1
         HQ4(K)=0.25*HQ(K-1)
      ENDDO
   ENDIF

   DO J=ja,jz
      DO I=ia,iz
         DO K=2,m1
            VC1DA(K)=HQ4(K)*(VC3DA(K,I,J)+VC3DA(K-1,I,J)  &
                 +VC3DA(K,I,J+jd)+VC3DA(K-1,I,J+jd))
         ENDDO
         IF(OPTYP .NE. 'GRADNT')   VC1DA(2) = 0.

         DO K=2,m1-1
            VC3DB(K,I,J)=VC3DB(K,I,J)  &
                 +FQ(I,J)*DZ(K)*(VC1DA(K+1)-VC1DA(K))
         ENDDO
         VC3DB(1,I,J)=VC3DB(2,I,J)
         IF(LEV.EQ.'W')VC3DB(m1-1,I,J)=VC3DB(m1-2,I,J)
         IF(LEV.EQ.'T')VC3DB(m1,I,J)=VC3DB(m1-1,I,J)
      ENDDO
   ENDDO
ENDIF

RETURN
END

SUBROUTINE GRADZW(m1,m2,m3,ia,iz,ja,jz,VC3DA,VC3DB,RTGC,DZ)
implicit none
integer :: m1,m2,m3,ia,iz,ja,jz
real :: VC3DA(m1,m2,m3),VC3DB(m1,m2,m3),RTGC(m2,m3) ,DZ(*)

integer :: i,j,k

DO J=ja,jz
   DO I=ia,iz
      DO K=2,m1
         VC3DB(K,I,J)=(VC3DA(K,I,J)-VC3DA(K-1,I,J))*DZ(K)  &
              /RTGC(I,J)
      ENDDO
   ENDDO
ENDDO
RETURN
END

SUBROUTINE GRADZT(m1,m2,m3,ia,iz,ja,jz,VC3DA,VC3DB,RTGC,DZ)
implicit none
integer :: m1,m2,m3,ia,iz,ja,jz
real :: VC3DA(m1,m2,m3),VC3DB(m1,m2,m3),RTGC(m2,m3) ,DZ(*)

integer :: i,j,k

DO J=ja,jz
   DO I=ia,iz
      DO K=1,m1-1
         VC3DB(K,I,J)=(VC3DA(K+1,I,J)-VC3DA(K,I,J))*DZ(K)  &
              /RTGC(I,J)
      ENDDO
   ENDDO
ENDDO
RETURN
END
