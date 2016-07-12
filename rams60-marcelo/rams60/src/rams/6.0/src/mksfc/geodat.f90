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

subroutine geodat(n2,n3,datr,hfn,ofn,vt2da,vt2db,ngr,vnam)

use mem_grid
use io_params

implicit none
integer :: n2,n3,ngr
real ::  vt2da(*),vt2db(*),datr(n2,n3)
character(len=80) :: hfn,ofn,title
character(len=3) :: vnam
character(len=64) h5name

integer :: lb,iblksizo,no,isbego,iwbego,iodim,mof,niq,njq,np
real :: offlat,offlon,deltallo,deltaxq,deltayq,deltaxp,deltayp,erad

real,allocatable:: dato(:)

print*,'====================================================='
if(VNAM(1:2).eq.'TO') then
   print*,'starting topography on grid:',NGR
elseif(VNAM(1:2).eq.'ZO') then
   print*,'starting surface roughness on grid:',NGR
else
   print*,'starting '//vnam//' data on grid:',NGR
endif

LB=len_trim(HFN)
if(LB.le.0) then
   print*,'==================================================='
   print*,'|  Problem in GEODAT, Input data prefix incorrect !'
   print*,'|  Grid :',ngrid
   print*,'|  File prefix:',HFN
   print*,'==================================================='
   stop 'GEODAT-file'
endif

if((vnam(1:2).eq.'TO'.or.vnam(1:2).eq.'ZO').and.  &
   (ITOPSFLG(NGR).eq.1.and.TOPTENH(NGR).gt.1.)) then
   print*,'==================================================='
   print*,'|  Problem in GEODAT, Silhouette weight too high !'
   print*,'|  Grid :',NGR
   print*,'|  Weight (range TOPTENH=0-1):',TOPTENH(NGR)
   print*,'==================================================='
   stop 'GEODAT'
endif

!     configure grid specs for raw data to rams grid (R) transfer

!     raw data grid (O)
TITLE=HFN(1:LB)//'HEADER'
LB=len_trim(TITLE)
call rams_f_open(29,title(1:lb),'FORMATTED','OLD','READ',0)
READ(29,*,end=1)IBLKSIZO,NO,ISBEGO,IWBEGO,offlat,offlon,h5name
1 continue
CLOSE(29)
DELTALLO=FLOAT(IBLKSIZO)/FLOAT(NO-1)

iodim=max(100000,4*no*no)
MOF=IODIM/(NO*NO)

   allocate(dato(iodim+mof+mof))

!     temp grid (Q) - smoothing only applied to topo
if(vnam(1:2).eq.'TO') then
      DELTAXQ=0.5*TOPTWVL(NGR)*DELTAXN(NGR)
   DELTAYQ=0.5*TOPTWVL(NGR)*DELTAYN(NGR)
else
      DELTAXQ=DELTAXN(NGR)
   DELTAYQ=DELTAYN(NGR)
endif
NIQ=INT(FLOAT(NNXP(NGR)-1)*DELTAXN(NGR)/DELTAXQ)+4
NJQ=INT(FLOAT(NNYP(NGR)-1)*DELTAYN(NGR)/DELTAYQ)+4

!     interpollated raw data grid (P)
NP=MIN(10,MAX(1,INT(DELTAXQ/(DELTALLO*111000.))))
DELTAXP=DELTAXQ/FLOAT(NP)
DELTAYP=DELTAYQ/FLOAT(NP)

CALL SFCOPQR(NO,MOF,NP,NIQ,NJQ,N2,N3,XTN(1,NGR),YTN(1,NGR)  &
     ,platn(ngr),plonn(ngr)  &
     ,ERAD,DELTALLO,DELTAXP,DELTAYP,DELTAXQ,DELTAYQ,IBLKSIZO  &
     ,ISBEGO,IWBEGO,DATO(1),VT2DA,VT2DB,DATR  &
     ,OFN,offlat,offlon,VNAM,NGR,itopsflg(ngr),iz0flg(ngr),h5name)

deallocate(dato)

RETURN
END

!     ******************************************************************

subroutine sfcopqr(no,mof,np,niq,njq,n2,n3,xt,yt,platn,plonn  &
     ,erad,deltallo,deltaxp,deltayp,deltaxq,deltayq,iblksizo  &
     ,isbego,iwbego,dato,datp,datq,datr  &
     ,ofn,offlat,offlon,vnam,ngr,itopsflg,iz0flg,h5name)

use hdf5_utils

implicit none

integer :: no,mof,np,niq,njq,n2,n3,iblksizo,isbego,iwbego,ngr  &
          ,itopsflg,iz0flg
real :: dato(no,no,mof),datp(np,np),datq(niq,njq),datr(n2,n3)  &
         ,xt(n2),yt(n3)
real :: erad,deltallo,deltaxp,deltayp,deltaxq,deltayq,offlat,offlon  
character(len=80) :: ofn,title3
character(len=3) :: title1,vnam
character(len=4) :: title2
character(len=*) :: h5name
logical :: l1,l2,h5
integer,parameter :: maxmiss=1000
character(len=80) :: fnmiss(maxmiss)
real, allocatable :: sdq(:,:),shaq(:,:),sdr(:,:),datre(:,:)
real, allocatable :: iso(:),iwo(:)

real :: platn,plonn,xcentr,ycentr,glatp,glonp,rio_full,rjo_full  &
       ,xq,yq,xp,yp,wio1,wio2,wjo1,wjo2,sha,rha,rh2,sh,rh &
       ,xq1,yq1,xr,yr,rval,diff,difflcl
integer :: nmiss,nono,nofr,iof,iq,jq,ip,jp,iwoc,isoc,io1,io2,jo1,jo2 &
          ,lb,nn,isocpt,isocpo,iwocpo,iwocph,iwocpt,io_full,jo_full &
          ,iofr,jofr,ir,jr,is,js,i,j

integer :: ndims,idims(4)


allocate (sdq(niq,njq),shaq(niq,njq),sdr(n2,n3),datre(n2,n3))
allocate (iso(mof),iwo(mof))

nmiss=0

nono=no*no
XCENTR=0.5*(XT(1)+XT(N2))
YCENTR=0.5*(YT(1)+YT(N3))
NOFR=0
DO IOF=1,MOF
   ISO(IOF)=0
   IWO(IOF)=0
ENDDO
DO JQ=1,NJQ
   DO IQ=1,NIQ
      XQ=(FLOAT(IQ)-0.5*FLOAT(NIQ+1))*DELTAXQ+XCENTR
      YQ=(FLOAT(JQ)-0.5*FLOAT(NJQ+1))*DELTAYQ+YCENTR
      DO JP=1,NP
         DO IP=1,NP
            XP=XQ+(FLOAT(IP)-0.5*FLOAT(NP+1))*DELTAXP
            YP=YQ+(FLOAT(JP)-0.5*FLOAT(NP+1))*DELTAYP

            call xy_ll(GLATP,GLONP,platn,plonn,xp,yp)

            glatp = max(-89.9999,min(89.9999,glatp - offlat))
            glonp = max(-179.999,min(179.999,glonp - offlon))

            if (glonp >=  180.) glonp = glonp - 360.
            if (glonp <= -180.) glonp = glonp + 360.

            rio_full = (glonp - float(iwbego)) / deltallo
            rjo_full = (glatp - float(isbego)) / deltallo

            io_full = int(rio_full)
            jo_full = int(rjo_full)

            iwoc = (io_full / (no-1)) * iblksizo + iwbego
            isoc = (jo_full / (no-1)) * iblksizo + isbego

            wio2 = rio_full - float(io_full)
            wjo2 = rjo_full - float(jo_full)
           
            wio1 = 1. - wio2
            wjo1 = 1. - wjo2

            io1 = mod(io_full,no-1) + 1
            jo1 = mod(jo_full,no-1) + 1

            io2 = io1 + 1
            jo2 = jo1 + 1
            
            DO IOFR=1,NOFR
               JOFR=IOFR
               IF(ISO(IOFR).EQ.ISOC.AND.IWO(IOFR).EQ.IWOC)GO TO 10
            ENDDO

            ISOCPT=ABS(ISOC)/10
            ISOCPO=ABS(ISOC)-ISOCPT*10
            IWOCPH=ABS(IWOC)/100
            IWOCPT=(ABS(IWOC)-IWOCPH*100)/10
            IWOCPO=ABS(IWOC)-IWOCPH*100-IWOCPT*10
            IF(ISOC.GE.0) THEN
               WRITE(TITLE1,'(2I1,A1)')ISOCPT,ISOCPO,'N'
            ELSE
               WRITE(TITLE1,'(2I1,A1)')ISOCPT,ISOCPO,'S'
            ENDIF
            IF(IWOC.GE.0) THEN
               WRITE(TITLE2,'(3I1,A1)')IWOCPH,IWOCPT,IWOCPO,'E'
            ELSE
               WRITE(TITLE2,'(3I1,A1)')IWOCPH,IWOCPT,IWOCPO,'W'
            ENDIF
            LB=len_trim(OFN)
            TITLE3=OFN(1:LB)//TITLE1//TITLE2
            LB=len_trim(TITLE3)
            INQUIRE(FILE=TITLE3(1:LB),EXIST=L1,OPENED=L2)

            h5=.false.
            if (.not. l1) then
               title3=title3(1:lb)//'.h5'
               inquire(file=trim(title3),exist=l1,opened=l2)
               h5=.true.
            endif

            IF(.NOT.L1)THEN
               do nn=1,nmiss
                  if(trim(TITLE3).eq.fnmiss(nn)) goto 302
               enddo
               nmiss=nmiss+1
               fnmiss(nmiss)=trim(TITLE3)
302                 continue
               DATP(IP,JP)=0.
               GOTO 20
            ENDIF

            IF(NOFR.GE.MOF) THEN
               DO IOF=1,MOF
                  ISO(IOF)=0
                  IWO(IOF)=0
               ENDDO
               NOFR=0
            ENDIF
            NOFR=NOFR+1
            JOFR=NOFR

            if (h5) then
               call shdf5_open(title3,'R')
               ndims=2 ; idims(1)=no ; idims(2)=no
               call shdf5_irec(trim(h5name),rvara=dato(1,1,nofr))
               call shdf5_close()
            else
               call rams_f_open  &
                    (29,TITLE3(1:LB),'FORMATTED','OLD','READ',0)
               CALL VFIREC(29,DATO(1,1,NOFR),NONO,'LIN')
               CLOSE(29)
            endif

            ISO(NOFR)=ISOC
            IWO(NOFR)=IWOC

10          CONTINUE

            datp(ip,jp)=wio1*(wjo1*dato(io1,jo1,jofr)   &
                             +wjo2*dato(io1,jo2,jofr))  &
                       +wio2*(wjo1*dato(io2,jo1,jofr)   &
                             +wjo2*dato(io2,jo2,jofr))
                                                          
20          CONTINUE
         ENDDO
      ENDDO

!           std dev for envelope orog and topo based zo schemes
      SHA=0.
      RHA=0.
      RH2=0.
      DO JP=1,NP
         SH=0.
         RH=0.
         DO IP=1,NP
            SH=MAX(SH,DATP(IP,JP))
            RH=RH+DATP(IP,JP)
            RH2=RH2+DATP(IP,JP)**2
         ENDDO
         SHA=SHA+SH/(2.*FLOAT(NP))
         RHA=RHA+RH
      ENDDO
      DATQ(IQ,JQ)=RHA/FLOAT(NP*NP)
      SDQ(IQ,JQ)=SQRT(max(0.,RH2/NP**2-DATQ(IQ,JQ)**2))
      DO IP=1,NP
         SH=0.
         DO JP=1,NP
            SH=MAX(SH,DATP(IP,JP))
         ENDDO
         SHA=SHA+SH/(2.*FLOAT(NP))
      ENDDO
      SHAQ(IQ,JQ)=SHA
      
   ENDDO
!         print*,'finished sfcopqr row jq = ',jq
ENDDO

!     envelope and zo schemes

if((vnam.eq.'TOP').and.  &
   (ITOPSFLG.eq.2).and.  &
   NP*NP.lt.8) print*,'Warning - '  &
   ,'trying to calc a std dev for: ',NP*NP,' points'
if((vnam.eq.'ZOT').and.  &
   IZ0FLG.eq.1.and.NP*NP.lt.8) print*,'Warning - '  &
   ,'trying to calc a std dev for: ',NP*NP,' points'


!           envelope orog and zo schemes
if(vnam.eq.'TOP')  &
   CALL TOPOQ(NIQ,NJQ,DELTAXQ,DELTAYQ,DATQ,SDQ,SHAQ,DATRE  &
           ,NGR,N2,N3)
if(vnam.eq.'ZOT')  &
   CALL ZOQ(NIQ,NJQ,DATQ,SDQ,NGR)

XQ1=(1.-0.5*FLOAT(NIQ+1))*DELTAXQ+XCENTR
YQ1=(1.-0.5*FLOAT(NJQ+1))*DELTAYQ+YCENTR
DO JR=1,N3
   DO IR=1,N2
      XR=(XT(IR)-XQ1)/DELTAXQ+1.
      YR=(YT(JR)-YQ1)/DELTAYQ+1.
      CALL GDTOST2(DATQ,NIQ,NJQ,XR,YR,RVAL)

      if(vnam.eq.'TOP') then
         DATR(IR,JR)=RVAL
      elseif(vnam.eq.'ZOT') then
         DATR(IR,JR)=MAX(0.,RVAL)
      else
         DATR(IR,JR)=MAX(0.,RVAL)
      endif
      
      CALL GDTOST(SDQ,NIQ,NJQ,XR,YR,RVAL)
      SDR(IR,JR)=MAX(0.,RVAL)
   ENDDO
ENDDO

if(nmiss.gt.0) then
   print*,'-----------------------------------------------------'
   print*,'Input physiographical data file processing:'
   print*,'-----------------------------------------------------'
   print*,'  Input data blocks not found '  &
        ,' (data assumed to be zero):'
   do nn=1,nmiss
      print*,fnmiss(nn)
   enddo
   print*,'-----------------------------------------------------'
endif

!     check to find the largest change in topo height

if(vnam.eq.'TOP') then
   diff=0.
   difflcl=0.
   is=-999
   js=-999
   do j=2,n3-1
      do i=2,n2-1
         difflcl=max(difflcl,abs(datr(i,j)-datr(i-1,j)))
         difflcl=max(difflcl,abs(datr(i,j)-datr(i+1,j)))
         difflcl=max(difflcl,abs(datr(i,j)-datr(i,j-1)))
         difflcl=max(difflcl,abs(datr(i,j)-datr(i,j+1)))
         if(abs(diff-difflcl).gt.1.) then
            is=i
            js=j
         endif
         diff=max(diff,difflcl)
      enddo
   enddo
   write(6,100) ' Max d(topo) on grid @i,j=',ngr,is,js,diff
100     format(a,3i4,f8.1)
endif

deallocate(SDQ,SHAQ,SDR,DATRE)
deallocate(ISO,IWO)

RETURN
END

!**********************************************************************

subroutine topoq(niq,njq,deltaxq,deltayq,datq,sdq,shaq,datre  &
                ,ngr,n2,n3)
                
use io_params
                
implicit none
integer :: niq,njq,ngr,n2,n3
real :: deltaxq,deltayq
real :: datq(niq,njq),sdq(niq,njq),shaq(niq,njq),datre(n2,n3)

integer :: iq,jq,jmin,imin,ire,jre,imax,jmax
real :: rad,count,total,remax,remin,average

!     orographic schemes

if(ITOPSFLG(ngr).lt.0) then                         ! No orography
   do jq=1,njq
      do iq=1,niq
         datq(iq,jq)=0.
!            print*,'None',iq,jq,datq(iq,jq)
      enddo
   enddo
   print *,'No orography'

elseif(ITOPSFLG(ngr).gt.0) then                      ! Average
   print *,'No orography enhancement applied'

   elseif(ITOPSFLG(ngr).eq.1) then                   ! Silhouette
      do jq=1,njq
         do iq=1,niq
            datq(iq,jq)=SHAQ(IQ,JQ)*toptenh(ngr)  &
                       +DATQ(IQ,JQ)*(1.-toptenh(ngr))
!                  print*,'Silhouette',iq,jq,datq(iq,jq)
         enddo
      enddo
      print *,'Silhouette Orography applied with'
      print *,'weighting = ',toptenh(ngr)

   elseif(ITOPSFLG(ngr).eq.2) then                   ! Envelope
      do jq=1,njq
         do iq=1,niq
            datq(iq,jq)=datq(iq,jq)+toptenh(ngr)*sdq(iq,jq)
!                  print*,'EO',iq,jq,datq(iq,jq)
         enddo
      enddo
      print *,'Envelope Orography applied with'
      print *,'enhancement = ',toptenh(ngr),' x std dev'

   else if(ITOPSFLG(ngr).ge.3) then                  ! Reflected Envelope

!        the radius we want to search for the current pts relative
!        height should correspond well to half the filtering wavelength
!        used on the topo (toptwvl)

         Rad=toptwvl(ngr)/2
      do jq=1,njq
         do iq=1,niq
            datre(iq,jq)=datq(iq,jq)
         enddo
      enddo
      do jq=1,njq
         do iq=1,niq
            count=0.
            total=0.
            remax=datre(iq,jq)
            remin=datre(iq,jq)
            jmin=jq-nint(Rad)
            imin=iq-nint(Rad)
            jmax=jq+nint(Rad)
            imax=iq+nint(Rad)
            do jre=max(1,jmin),min(njq,jmax)
               do ire=max(1,imin),min(niq,imax)
                  if((float((iq-ire)))**2  &
                    +(float((jq-jre)))**2.le.Rad**2) then
                   count=count+1.
                  total=total+datre(ire,jre)
                  remax=max(remax,datre(ire,jre))
                  remin=min(remin,datre(ire,jre))
               endif
            enddo
            enddo
         average=total/count
         if(remax.ne.remin)  &
                  datq(iq,jq)=datre(iq,jq)+(datre(iq,jq)-average)/  &
            ((remax-remin)/2)*toptenh(ngr)*sdq(iq,jq)
!               print*,'REO',iq,jq,datre(iq,jq),sdq(iq,jq),datq(iq,jq)
!               print*,'avg,n',average,count,remax,remin
      enddo
   enddo
   print *,'Reflected Envelope Orography applied with'
   print *,'enhancement = ',toptenh(ngr),' x std dev'
   print *,'and search radius (grid points) = ',Rad
endif

RETURN
END

!**********************************************************************

SUBROUTINE ZOQ(NIQ,NJQ,DATQ,SDQ,NGR)

use io_params
use mem_leaf

implicit none
integer :: NIQ,NJQ,NGR
real :: DATQ(NIQ,NJQ),SDQ(NIQ,NJQ)

integer :: iq,jq

!     topo base roughness length.


do jq=1,njq
   do iq=1,niq
      if(ITOPSFLG(ngr).lt.0) then  ! No orography
         datq(iq,jq)=zrough
      elseif(iz0flg(ngr).eq.1) then
         datq(iq,jq)=min(z0fact*sdq(iq,jq),z0max(NGR))
      else
         datq(iq,jq)=zrough
      endif
!            print*,'z0',iq,jq,datq(iq,jq)
   enddo
enddo
if(ITOPSFLG(ngr).lt.0) then  ! No orography
   print *,'No orography'
else
   print *,'Subgrid terrain roughness applied with'
   print *,'factor  = ',z0fact,' x std dev'
   print *,'maximum = ',z0max(NGR)
endif

RETURN
END

