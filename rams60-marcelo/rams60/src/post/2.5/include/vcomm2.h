!f90
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

!---------------------------------------------------------------------------
!     RAMS POST commons
!---------------------------------------------------------------------------
include 'rcommons.h'
include 'plevs.h'
include 'window.h'
!---------------------------------------------------------------------------
integer, parameter :: &
         maxfore=30   & ! maximum number of foregrounds on any background.
       , maxtims=200  & ! maximum number of times to use in a time plot
       , maxfils=200  & ! maximum number of input files
       , maxsiz=max(maxdim,maxtims)  & ! maximum size of any grid in x,y,z,t
       , maxloc=2000  & ! maximum number of observation locations
       , maxlev=500      ! maximun levels in a profile
                         ! (set the same as in ascoms.h)
!---------------------------------------------------------------------------
logical :: xst_sfc,xst_rwn
common/logvars/xst_sfc,xst_rwn
!---------------------------------------------------------------------------
integer :: ifile,ifiltot,nplot,nback
common/contrl/ ifile,ifiltot,nplot,nback
!---------------------------------------------------------------------------
integer :: ioffl,ioffr,joffb,jofft,numlabx,numlaby,jgrid
real    :: xmin,xmax,ymin,ymax
common/analys/ioffl,ioffr,joffb,jofft,numlabx,numlaby  &
             ,jgrid,xmin,xmax,ymin,ymax
!---------------------------------------------------------------------------
integer :: itbeg,itstep,itend,ixbeg,ixstep,ixend  &
          ,iybeg,iystep,iyend,izbeg,izstep,izend  &
          ,ihbeg,ihstep,ihend,ivbeg,ivstep,ivend  &
          ,inbeg,instep,inend,igrid(maxfore),nfile  &
          ,mapfill,ibackgnd,ipltinfo,ipanel  &
          ,iplevel(maxfore),iztran(maxfore)
real    :: times(maxtims),xplot(maxsiz)
common/anasis/itbeg, itstep, itend, ixbeg, ixstep, ixend  &
             ,iybeg, iystep, iyend, izbeg, izstep, izend  &
             ,ihbeg, ihstep, ihend, ivbeg, ivstep, ivend  &
             ,inbeg, instep, inend, igrid, nfile  &
             ,mapfill,ibackgnd,ipltinfo,ipanel  &
             ,iplevel,iztran,times,xplot
!---------------------------------------------------------------------------
integer :: mfx,nfx,myans,itrans,ixs,ilftw,ibotw
real    :: xlonl,xlonr,ylatb,ylatt,yans,ztbot,zttop  &
          ,xtrans(nxpmax),ytrans(nypmax),ztrans(nzpmax)
common/trans/itrans,mfx,nfx,xlonl,xlonr,ylatb,ylatt,yans,myans  &
            ,xtrans,ytrans,ztrans,ixs,ilftw,ibotw,ztbot,zttop
!---------------------------------------------------------------------------
integer :: ivtran,izstran
real    :: ztr(1000),zs(1000),zmodtop,zfactor
common/trans2/ivtran,ztr,izstran,zs,zmodtop,zfactor
!---------------------------------------------------------------------------
real    :: plm1(2),plm2(2),plm3(2),plm4(2)
!---------------------------------------------------------------------------
character(len=  8) :: dirfile,backtype,infile,fldex,anatype
character(len=  8) :: labfmtx,labfmty,xaxlab,yaxlab
character(len= 30) :: head1,head2
character(len=  1) :: horiz,vert,fix1,fix2,fix3,linv,linh,norm,null
character(len=256) :: anpref,revpref
character(len=128) :: dfname,exname,ename,hname
character(len= 20), dimension(maxfore) :: xvar,yvar,zvar,tvar
character(len=128), dimension(maxfore) :: cframe_a,cframe_b,cframe_c
character(len=128), dimension(maxfore) :: landmark,colors
logical :: gridinv(maxtims,maxgrds)
common/logicals/gridinv
common/strings/dfname,labfmtx,labfmty,xaxlab  &
              ,yaxlab,horiz,vert,head1,head2,fix1,fix2,fix3  &
              ,linv,linh,norm,infile,null  &
              ,ename,hname,anatype,anpref,revpref,backtype  &
              ,xvar,yvar,zvar,tvar,cframe_a,cframe_b,cframe_c  &
              ,landmark,colors
!---------------------------------------------------------------------------
real, dimension(maxloc) :: fxloc,fyloc,fzloc,ftloc
real    :: fdata(maxloc,maxfore,maxtims)
common/fdgcom/fxloc,fyloc,fzloc,ftloc,fdata
!---------------------------------------------------------------------------
integer :: igrabfmt
character(len=128) :: grabin
common/grab/igrabfmt,grabin
!---------------------------------------------------------------------------
integer :: igrabgrd(maxgrds),ifobs,ipred,istnloc(maxloc),ndxplev(nplevs+1) &
          ,numpts,numtime,numfore,nfndx,ipdiff,ipvals,ipwind  &
          ,iphist,ipcont  
real    :: stat_dat(maxfore,9,nplevs+1),fmnlat,fmxlat,fmnlon,fmxlon
character(len=128) :: sfcpref,rwnpref
character(len=  5) :: cmode
character(len= 16) :: stchar(maxfore)
character(len=  4) :: noq
common/stats/stat_dat,igrabgrd,ifobs,ipred,istnloc,ndxplev  &
            ,fmnlat,numpts,numtime  &
            ,numfore,nfndx,fmxlat,fmnlon,fmxlon,ipdiff,ipvals,ipwind  &
            ,iphist,ipcont,sfcpref,rwnpref,noq,cmode,stchar
!---------------------------------------------------------------------------
integer :: igridll
real    :: glldllat,glldllon,gllwlon,gllelon,gllslat,gllnlat
common/gll/igridll,glldllat,glldllon,gllwlon,gllelon,gllslat,gllnlat
!---------------------------------------------------------------------------

