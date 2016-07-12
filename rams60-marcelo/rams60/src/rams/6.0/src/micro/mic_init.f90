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

subroutine micro_master

use micphys
use rconstants

implicit none

integer :: lhcat,khcat,lcat,nd1,nd2,nip,ilcat,ilhcat,idum

real, dimension(7,15) :: dstprms
real, dimension(15,15) :: jpairr,jpairc
character*80 dataline,cname

data dstprms/ &
!----------------------------------------------------------------------
! shape      cfmas   pwmas      cfvt    pwvt     dmb0      dmb1
!---------------------------------------------------------------------- 
    .5,      524.,     3.,    3173.,     2.,   2.e-6,   40.e-6,  & !cloud
    .5,      524.,     3.,     149.,     .5,   .1e-3,    5.e-3,  & !rain
  .179,     110.8,   2.91,  5.769e5,   1.88,  15.e-6,  125.e-6,  & !pris col
  .179,  2.739e-3,   1.74,  188.146,   .933,   .1e-3,   10.e-3,  & !snow col
    .5,      .496,    2.4,    3.084,     .2,   .1e-3,   10.e-3,  & !aggreg
    .5,      157.,     3.,     93.3,     .5,   .1e-3,    5.e-3,  & !graup
    .5,      471.,     3.,     161.,     .5,   .8e-3,   10.e-3,  & !hail 
 .0429,     .8854,    2.5,     316.,   1.01,      00,       00,  & !pris hex
 .3183,   .377e-2,     2.,     316.,   1.01,      00,       00,  & !pris den
 .1803,   1.23e-3,    1.8,  5.769e5,   1.88,      00,       00,  & !pris ndl
    .5,     .1001,  2.256,   3.19e4,   1.66,      00,       00,  & !pris ros
 .0429,     .8854,    2.5,    4.836,    .25,      00,       00,  & !snow hex
 .3183,   .377e-2,     2.,    4.836,    .25,      00,       00,  & !snow den
 .1803,   1.23e-3,    1.8,  188.146,   .933,      00,       00,  & !snow ndl
    .5,     .1001,  2.256,  1348.38,  1.241,      00,       00/    !snow ros

data jpairr/  &
     0,  0,  0,  1,  2,  3,  4,  0,  0,  0,  0,  5,  6,  7,  8,  &
     0,  0,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,  &
     0, 22, 23, 24,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
    25, 26, 27, 28,  0,  0,  0, 29, 30, 31, 32,  0,  0,  0,  0,  &
    33, 34, 35, 36,  0,  0,  0, 37, 38, 39, 40, 41, 42, 43, 44,  &
    45, 46, 47, 48, 49,  0,  0, 50, 51, 52, 53, 54, 55, 56, 57,  &
    58, 59, 60, 61, 62, 63,  0, 64, 65, 66, 67, 68, 69, 70, 71,  &
     0, 72,  0, 73,  0,  0,  0, 74,  0,  0,  0, 75, 76, 77, 78,  &
     0, 79,  0, 80,  0,  0,  0,  0, 81,  0,  0, 82, 83, 84, 85,  &
     0, 86,  0, 87,  0,  0,  0,  0,  0, 88,  0, 89, 90, 91, 92,  &
     0, 93,  0, 94,  0,  0,  0,  0,  0,  0, 95, 96, 97, 98, 99,  &
   100,101,102,  0,  0,  0,  0,103,104,105,106,107,  0,  0,  0,  &
   108,109,110,  0,  0,  0,  0,111,112,113,114,  0,115,  0,  0,  &
   116,117,118,  0,  0,  0,  0,119,120,121,122,  0,  0,123,  0,  &
   124,125,126,  0,  0,  0,  0,127,128,129,130,  0,  0,  0,131/

data jpairc/  &
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
     0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
     0,  2,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  &
     4,  5,  6,  7,  0,  0,  0,  8,  9, 10, 11,  0,  0,  0,  0,  &
    12, 13, 14, 15, 16,  0,  0, 17, 18, 19, 20, 21, 22, 23, 24,  &
    25, 26, 27, 28, 29, 30,  0, 31, 32, 33, 34, 35, 36, 37, 38,  &
    39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53,  &
     0, 54,  0,  0,  0,  0,  0, 55,  0,  0,  0,  0,  0,  0,  0,  &
     0, 56,  0,  0,  0,  0,  0,  0, 57,  0,  0,  0,  0,  0,  0,  &
     0, 58,  0,  0,  0,  0,  0,  0,  0, 59,  0,  0,  0,  0,  0,  &
     0, 60,  0,  0,  0,  0,  0,  0,  0,  0, 61,  0,  0,  0,  0,  &
    62, 63, 64,  0,  0,  0,  0, 65, 66, 67, 68, 69,  0,  0,  0,  &
    70, 71, 72,  0,  0,  0,  0, 73, 74, 75, 76,  0, 77,  0,  0,  &
    78, 79, 80,  0,  0,  0,  0, 81, 82, 83, 84,  0,  0, 85,  0,  &
    86, 87, 88,  0,  0,  0,  0, 89, 90, 91, 92,  0,  0,  0, 93/

!  Define several parameters from above data list

do lhcat=1,nhcat
   shape(lhcat) = dstprms(1,lhcat)
   cfmas(lhcat) = dstprms(2,lhcat)
   pwmas(lhcat) = dstprms(3,lhcat)
   cfvt (lhcat) = dstprms(4,lhcat)
   pwvt (lhcat) = dstprms(5,lhcat)

   do khcat=1,nhcat
      ipairc(lhcat,khcat) = jpairc(lhcat,khcat)
      ipairr(lhcat,khcat) = jpairr(lhcat,khcat)
   enddo
enddo

do lcat=1,ncat
   emb0 (lcat) = cfmas(lcat) * dstprms(6,lcat) ** pwmas(lcat)
   emb1 (lcat) = cfmas(lcat) * dstprms(7,lcat) ** pwmas(lcat)
enddo

if (level .ne. 3) return

if(mkcoltab.lt.0.or.mkcoltab.gt.1)then
   print*, 'mkcoltab set to ',mkcoltab, 'which is out of bounds'
   stop 'mkcoltab'
endif

cname=coltabfn(1:len_trim(coltabfn))

if(mkcoltab.eq.1)then

! Make collection table and write to file

   call mkcoltb
   open(91,file=cname,form='formatted',status='unknown')
   rewind(91)
   write(91,181)
   do lcat = 1,ncat
      write(91,182)lcat,gnu(lcat),emb0(lcat),emb1(lcat)
   enddo
   write(91,180)
   write(91,183)
   do lhcat = 1,nhcat
      write(91,182)lhcat,cfmas(lhcat),pwmas(lhcat)  &
         ,cfvt(lhcat),pwvt(lhcat)
   enddo
   write(91,180)
   do nip=1,npairc
      write(91,186)nip
      write(91,184)(nd2,(coltabc(nd1,nd2,nip)  &
         ,nd1=1,nembc),nd2=1,nembc)
   enddo
   write(91,180)
   do nip=1,npairr
      write(91,187)nip
      write(91,184)(nd2,(coltabr(nd1,nd2,nip)  &
         ,nd1=1,nembc),nd2=1,nembc)
   enddo

else

!  Read collection table

   open(91,file=cname,form='formatted',status='old')
   read(91,185)dataline
   do ilcat = 1,ncat
      read(91,182)lcat,gnu(lcat),emb0(lcat),emb1(lcat)
   enddo
   read(91,185)dataline
   read(91,185)dataline
   do ilhcat = 1,nhcat
      read(91,182)lhcat,cfmas(lhcat),pwmas(lhcat)  &
         ,cfvt(lhcat),pwvt(lhcat)
   enddo
   read(91,185)dataline
   do nip=1,npairc
      read(91,185)dataline
      read(91,184)(idum,(coltabc(nd1,nd2,nip)  &
         ,nd1=1,nembc),nd2=1,nembc)
   enddo
   read(91,185)dataline
   do nip=1,npairr
      read(91,185)dataline
      read(91,184)(idum,(coltabr(nd1,nd2,nip)  &
         ,nd1=1,nembc),nd2=1,nembc)
   enddo
   
endif

180  format(' ')
181  format(' lcat    gnu        emb0       emb1    ')
182  format(i4,7e11.4)
183  format(' lhcat  cfmas      pwmas       cfvt       pwvt')
184  format(i3,20f6.2)
185  format(a80)
186  format('ipairc',i4)
1186 format(6x,i4)
187  format('ipairr',i4)

close(91)

return
end

!******************************************************************************

subroutine initqin(n1,n2,n3,q2,q6,q7,pi0,pp,theta,dn0)

use micphys
use rconstants

implicit none

integer :: n1,n2,n3,i,j,k
real, dimension(n1,n2,n3) :: q2,q6,q7,pi0,pp,theta,dn0

! Initialize Q2, Q6, Q7, CCN, IFN.

do j = 1,n3
   do i = 1,n2
      do k = 1,n1
         pitot(k) = pi0(k,i,j) + pp(k,i,j)
         tair(k) = theta(k,i,j) * pitot(k) / cp

         if(irain .ge. 1) q2(k,i,j) = tair(k) - 193.16
         if(igraup .ge. 1) q6(k,i,j) = 0.5 * min(0.,tair(k) - 273.16)
         if(ihail .ge. 1) q7(k,i,j) = 0.5 * min(0.,tair(k) - 273.16)

      enddo
   enddo
enddo
return
end
subroutine initqin2(n1,n2,n3,cccnp)

use micphys
use rconstants

implicit none

integer :: n1,n2,n3,i,j,k
real, dimension(n1,n2,n3) :: cccnp

! Initialize  CCN

do j = 1,n3
   do i = 1,n2
      do k = 1,n1
!        if (icloud .eq. 7) cccnp(k,i,j) = ???
      enddo
   enddo
enddo
return
end
subroutine initqin3(n1,n2,n3,cifnp,dn0)

use micphys
use rconstants

implicit none

integer :: n1,n2,n3,i,j,k
real, dimension(n1,n2,n3) :: cifnp,dn0

! Initialize  IFN

do j = 1,n3
   do i = 1,n2
      do k = 1,n1
         if (ipris .eq. 7) cifnp(k,i,j) = 1.e5 * dn0(k,i,j) ** 5.4
      enddo
   enddo
enddo
return
end

!******************************************************************************

subroutine jnmbinit()

use micphys

implicit none

if (level /= 3) then

   if (level <= 1) then
      jnmb(1) = 0
   else
      jnmb(1) = 4
   endif

   jnmb(2) = 0
   jnmb(3) = 0
   jnmb(4) = 0
   jnmb(5) = 0
   jnmb(6) = 0
   jnmb(7) = 0

else

   jnmb(1) = icloud
   jnmb(2) = irain
   jnmb(3) = ipris
   jnmb(4) = isnow
   jnmb(5) = iaggr
   jnmb(6) = igraup
   jnmb(7) = ihail

   if (icloud .eq. 1) jnmb(1) = 4
   if (irain  .eq. 1) jnmb(2) = 2
   if (ipris  .ge. 1) jnmb(3) = 5
   if (isnow  .eq. 1) jnmb(4) = 2
   if (iaggr  .eq. 1) jnmb(5) = 2
   if (igraup .eq. 1) jnmb(6) = 2
   if (ihail  .eq. 1) jnmb(7) = 2

   if (irain == 5 .or. isnow == 5 .or. iaggr == 5 .or.  &
      igraup == 5 .or. ihail == 5) then

      if (irain  >= 1) jnmb(2) = 5
      if (isnow  >= 1) jnmb(4) = 5
      if (iaggr  >= 1) jnmb(5) = 5
      if (igraup >= 1) jnmb(6) = 5
      if (ihail  >= 1) jnmb(7) = 5

   endif

endif
return
end

!******************************************************************************

subroutine micinit()

use micphys

implicit none

integer :: lhcat,lcat,ia
real :: cfmasi,c1,glg,glg1,glg2,glgm,glgc,glgmv,gym,flngi,dpsi,embsip,dnsip
real :: gammln,gammp,gammq

! Initialize arrays based on microphysics namelist parameters

parm(1) = cparm
parm(2) = rparm
!     parm(3) = pparm   [obsolete]
parm(4) = sparm
parm(5) = aparm
parm(6) = gparm
parm(7) = hparm

if (icloud .le. 1) parm(1) = .3e9
if (irain  .eq. 1) parm(2) = .1e-2
!     if (ipris  .eq. 1) parm(3) = .1e4     [obsolete]
if (isnow  .eq. 1) parm(4) = .1e-2
if (iaggr  .eq. 1) parm(5) = .1e-2
if (igraup .eq. 1) parm(6) = .1e-2
if (ihail  .eq. 1) parm(7) = .3e-2

dps = 125.e-6
dps2 = dps ** 2
rictmin = 1.0001
rictmax = 0.9999 * float(nembc)

do lhcat = 1,nhcat
   lcat = lhcat + (3 - lhcat) * (lhcat / 8) + lhcat / 12

   cfden(lhcat) = cfmas(lhcat) * 6.0 / 3.14159
   pwden(lhcat) = pwmas(lhcat) - 3.
   emb0log(lcat) = log(emb0(lcat))
   emb1log(lcat) = log(emb1(lcat))

! Define coefficients [vtfac, frefac1, frefac2] used for terminal velocity
! and Reynolds number

   cfmasi = 1. / cfmas(lhcat)
   pwmasi(lhcat) = 1. / pwmas(lhcat)
   pwen0(lhcat) = 1. / (pwmas(lhcat) + 1.)
   pwemb0(lhcat) = pwmas(lhcat) / (pwmas(lhcat) + 1.)
   c1 = 1.5 + .5 * pwvt(lhcat)

   glg = gammln(gnu(lcat))
   glg1 = gammln(gnu(lcat) + 1.)
   glg2 = gammln(gnu(lcat) + 2.)
   glgm = gammln(gnu(lcat) + pwmas(lhcat))
   glgc = gammln(gnu(lcat) + c1)
   glgmv = gammln(gnu(lcat) + pwmas(lhcat) + pwvt(lhcat))

   if (jnmb(lcat) .eq. 3) then
      cfemb0(lhcat) = cfmas(lhcat) * exp(glgm - glg)  &
         ** pwen0(lhcat) * (1. / parm(lcat)) ** pwemb0(lhcat)
      cfen0(lhcat) = parm(lcat) * (exp(glg - glgm) / parm(lcat))  &
         ** pwen0(lhcat)
   endif

   dnfac(lhcat) = (cfmasi * exp(glg - glgm)) ** pwmasi(lhcat)

   vtfac(lhcat) = cfvt(lhcat) * exp(glgmv - glgm)  &
      * (cfmasi * exp(glg - glgm)) ** (pwvt(lhcat) *pwmasi(lhcat))

   frefac1(lhcat) = shape(lhcat) * exp(glg1 - glg)  &
      * (cfmasi * exp(glg - glgm)) ** pwmasi(lhcat)

   frefac2(lhcat) = shape(lhcat) * 0.229 * sqrt(cfvt(lcat))  &
      * (cfmasi * exp(glg - glgm)) ** (pwmasi(lhcat) * c1)  &
      * exp(glgc - glg)

   sipfac(lhcat) = .785 * exp(glg2 - glg)  &
      * (cfmasi * exp(glg - glgm)) ** (2. * pwmasi(lhcat))

   cfmasft(lhcat) = cfmas(lhcat) * exp(gammln  &
      (gnu(lcat) + pwmas(lhcat)) - gammln(gnu(lcat)))

   dict(lcat) = float(nembc-1) / (emb1log(lcat) - emb0log(lcat))

   dpsmi(lhcat) = 1. / (cfmas(lhcat) * dps ** pwmas(lhcat))
   if (lhcat .le. 4) gamm(lhcat) = exp(glg)
   if (lhcat .le. 4) gamn1(lhcat) = exp(glg1)

! gam1   :  the integral of the pristine distribution from dps to infty
! gam2   :  the integral of the snow dist. from 0 to dps
! gam3   :  values of the exponential exp(-dps/dn)

enddo

flngi = 1. / float(ngam)
do ia=1,ngam
   dpsi = dps * 1.e6 / float(ia)

   gam(ia,1) = gammq(gnu(3) + 1., dpsi)
   gam(ia,2) = gammp(gnu(4) + 1., dpsi)
   gam(ia,3) = exp(-dpsi)

   GAMINC(IA,1)=GAMMQ(GNU(3),dpsi)
   GAMINC(IA,2)=GAMMP(GNU(4),dpsi)

   embsip = emb1(1) * float(ia) * flngi
   dnsip = dnfac(1) * embsip ** pwmasi(1)
   gamsip13(ia) = gammp(gnu(1),13.e-6/dnsip)
   gamsip24(ia) = gammq(gnu(1),24.e-6/dnsip)
enddo

return
end

