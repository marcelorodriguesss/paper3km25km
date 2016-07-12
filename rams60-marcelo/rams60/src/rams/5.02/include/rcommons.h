!############################# Change Log ##################################
! 4.3.0.0
!
! 000823 MJB none ##
!            Added change log. ##
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000 - All Rights Reserved
!  Regional Atmospheric Modelling System - RAMS
!  Mission Research Corporation / *ASTER Division
!###########################################################################

!-------------------------------------------------------------------------------                                                                     *
!     Common block include file for the   R A M S   model
!------------------------------------------------------------------------------- 
!
include 'rconfig.h'
include 'micphys.h'

!-------------------------------------------------------------------------------
!        Parameters for some array dimensions

integer, parameter :: nxyzpm=nzpmax*nxpmax*nypmax  &
                     ,maxdimp=maxdim+2,nstyp=12,nvtyp=20  &
                     ,nkeep=90,nke=nkeep,nintgm=12,maxsndg=200  &
                     ,maxvarf=2000,maxsstf=2000,maxndvif=2000

!-------------------------------------------------------------------------------
!        COMMON block includes

!-------------------------------------------------------------------------------
character(len=64) :: expnme
common /allch/ expnme
!-------------------------------------------------------------------------------

integer :: if_adap
common /vert_coord/ if_adap
!-------------------------------------------------------------------------------
integer                     :: itopo,initial,impl,iadvl,iadvf,lonrad,ngrids  &
                              ,lsflg,ibnd,jbnd,icorflg,ilwrtyp,iswrtyp,iref  &
                              ,jref,ihtran,nfpt,nsndg,ideltat,nacoust,iflag  &
                              ,ntopsmth,izflat,iyear1,imonth1,idate1,ihour1  &
                              ,itime1,isfcl,ihorgrad
integer, dimension(maxgrds) :: idiffk
common /all/ itopo,initial,impl,iadvl,iadvf,lonrad,ngrids  &
            ,lsflg,ibnd,jbnd,icorflg,ilwrtyp,iswrtyp,iref,jref  &
            ,ihtran,nfpt,nsndg,ideltat,nacoust,iflag,ntopsmth  &
            ,izflat,iyear1,imonth1,idate1,ihour1,itime1  &
            ,idiffk,isfcl,ihorgrad
!-------------------------------------------------------------------------------
integer                     :: naddsc,nestz1,nestz2,nzg,nzs,iversion,npatch  &
                              ,nvegpat
integer, dimension(maxgrds) :: nnqparm,nndtrat,nstratx,nstraty  &
                              ,ngbegun,nnacoust,nxtnest,nnsttop,nnstbot  &
                              ,ninest,njnest,nknest
integer, target, dimension(maxgrds) :: nnxp,nnyp,nnzp                              
integer, dimension(nzpmax)  :: nstratz1,nstratz2
integer, dimension(nvtyp)   :: kroot
common /all/ nnqparm,nndtrat,nstratx,nstraty,nstratz1,nstratz2  &
            ,ngbegun,nnacoust,nxtnest,nnsttop,nnstbot  &
            ,nnxp,nnyp,nnzp,ninest,njnest,nknest  &
            ,naddsc,nestz1,nestz2,nzg,nzs,iversion,npatch  &
            ,nvegpat,kroot
!-------------------------------------------------------------------------------
integer, parameter                     :: maxsched=2000,maxschent=5
integer                                :: nsubs
integer, dimension(maxsched,maxschent) :: isched
common/schedule/nsubs,isched
!-------------------------------------------------------------------------------
real                     :: brunt,wcldbs,drtcon,rmin,radfrq,distim,seatmp  &
                           ,confrq,ubmin,eps,albedo,dthcon,rmax  &
                           ,cphas,dtlong,topref,sspct,polelat,polelon
real, dimension(maxgrds) :: platn,plonn,centlat,centlon  &
                           ,zkhkm,xkhkm,cflxy,cflz,csz,csx,akmin
integer                  :: nhemgrd2
common /all/ brunt,wcldbs,drtcon,rmin,radfrq,distim,seatmp  &
            ,confrq,cflxy,cflz,csz,csx,rmax,akmin  &
            ,ubmin,eps,albedo,xkhkm,zkhkm,dthcon  &
            ,centlat,centlon,cphas,dtlong,topref,sspct  &
            ,polelat,polelon,platn,plonn,nhemgrd2
!-------------------------------------------------------------------------------
integer                     :: nhemt,nhemu,nhemv  
integer, dimension(4,maxhp) :: ihem1tt,jhem1tt,ihem1uu,jhem1uu  &
                              ,ihem1uv,jhem1uv,ihem1vu,jhem1vu  &
                              ,ihem1vv,jhem1vv
integer, dimension(maxhp)   :: ihem2tt,jhem2tt,ihem2uu,jhem2uu  &
                              ,ihem2uv,jhem2uv,ihem2vu,jhem2vu  &
                              ,ihem2vv,jhem2vv
real, dimension(maxhp)      :: hlatt,hlatu,hlatv,hlont,hlonu,hlonv
real, dimension(4,maxhp)    :: whem1tt,whem1uu,whem1uv,whem1vu,whem1vv
common /hemisphere/ nhemt,nhemu,nhemv  &
                   ,ihem1tt,jhem1tt,whem1tt  &
                   ,ihem1uu,jhem1uu,whem1uu  &
                   ,ihem1uv,jhem1uv,whem1uv  &
                   ,ihem1vu,jhem1vu,whem1vu  &
                   ,ihem1vv,jhem1vv,whem1vv  &
                   ,ihem2tt,jhem2tt,ihem2uu,jhem2uu  &
                   ,ihem2uv,jhem2uv,ihem2vu,jhem2vu,ihem2vv,jhem2vv  &
                   ,hlatt,hlatu,hlatv,hlont,hlonu,hlonv
!-------------------------------------------------------------------------------
real, dimension(nzpmax,maxgrds) :: u01dn,v01dn,pi01dn,th01dn,dn01dn,rt01dn
common /reference1d/ u01dn,v01dn,pi01dn,th01dn,dn01dn,rt01dn
!-------------------------------------------------------------------------------
integer              :: ipsflg,itsflg,irtsflg,iusflg
real, dimension(maxsndg) :: us,vs,ts,thds,ps,hs,rts
common /init_sounding/ ipsflg,itsflg,irtsflg,iusflg,us,vs,ts,thds,ps,hs,rts
!-------------------------------------------------------------------------------
real, dimension(nstyp)        :: slden,slcpd,slbs,slcond  &
                                ,slcons,slmsts,slpots,ssand,sclay  &
                                ,sorgan,sporo,soilcp,slfc,emisg
real, dimension(nvtyp)        :: albv_green,albv_brown,emisv,sr_max,tai_max  &
                                ,sai,veg_clump,veg_frac,veg_ht,glai_max  &
                                ,dead_frac,rcmin
real                          :: cmin,corg,cwat,cair,cka,ckw
real, dimension(nzgmax)       :: slz
real, dimension(nzgmax,nvtyp) :: root
common /soil_veg/ slden,slcpd,slbs,slcond,slcons,slz,slmsts  &
                 ,slpots,ssand,sclay,sorgan,sporo,soilcp,slfc,emisg  &
                 ,albv_green,albv_brown,emisv,sr_max,tai_max,sai  &
                 ,veg_clump,veg_frac,veg_ht,glai_max,dead_frac,rcmin  &
                 ,root,cmin,corg,cwat,cair,cka,ckw
!-------------------------------------------------------------------------------
real                     :: time,ztop,dzrat,dzmax
real, dimension(maxgrds) :: deltazn,deltaxn,deltayn,dtlongn,dimove,djmove  &
                           ,gridu,gridv
real, dimension(nzpmax)  :: zz
common /time_grid / time,deltazn,deltaxn,deltayn,ztop,zz,dzrat,dzmax  &
                   ,dtlongn,dimove,djmove,gridu,gridv
!-------------------------------------------------------------------------------
character(len=8), dimension(50) :: plfmt,pltit
character(len=16), dimension(50) :: iplfld
common /prtchr/plfmt,pltit,iplfld
!-------------------------------------------------------------------------------
integer                :: nplt
integer, dimension(50) :: ixsctn,iplvect,isbval,iaa,iab,joa,job,naavg,noavg
real                   :: frqprt
real, dimension(50)    :: plconlo,plconhi,plconin
common /prtcom/ nplt,ixsctn,iplvect,isbval,iaa,iab,joa,job,plconin  &
               ,naavg,noavg,plconlo,plconhi,frqprt
!-------------------------------------------------------------------------------
character (len=16) :: runtype
character(len=1)   :: timeunit

integer, parameter :: maxlite=50

character (len=32) :: lite_vars(maxlite)
character(len=80)  :: hfilin,afilin,hfilout,afilout,sfcfiles,pastfn
character(len=20)  :: xlite,ylite,zlite
common /filchr/ hfilin,hfilout,afilin,afilout  &
               ,runtype,timeunit,sfcfiles,lite_vars,xlite,ylite,zlite,pastfn
integer :: ipastin
common/past_init/ ipastin
!-------------------------------------------------------------------------------
integer :: ioutput,iinput,iopunt,kwrite,ihistdel,iclobber,nlite_vars
real    :: frqhis,frqanl,timstr,avgtim,frqlite,frqmean,frqboth  
common /files/ frqhis,frqanl,ioutput,iinput,timstr,iopunt,kwrite  &
              ,ihistdel,avgtim,frqlite,frqmean,frqboth,iclobber,nlite_vars
!-------------------------------------------------------------------------------

integer, dimension(maxgrds) :: itoptflg,isstflg,ivegtflg,isoilflg  &
                              ,ndviflg,nofilflg,itopsflg,iz0flg
real                        :: z0fact
real, dimension(maxgrds)    :: z0max,toptenh,toptwvl
common /topocom/ itoptflg,isstflg,ivegtflg,isoilflg,ndviflg,nofilflg  &
                ,toptenh,toptwvl,itopsflg,iz0flg,z0max,z0fact
!-------------------------------------------------------------------------------
character(len=80), dimension(maxgrds) :: itoptfn,isstfn,ivegtfn,isoilfn  &
                                        ,ndvifn
common /topccom/ itoptfn,isstfn,ivegtfn,isoilfn,ndvifn
!-------------------------------------------------------------------------------
integer :: ngridsh
common /hisgrd/ ngridsh
!-------------------------------------------------------------------------------
integer :: level,nqparm,itopbc
common /option/ level,nqparm,itopbc
!-------------------------------------------------------------------------------
integer, dimension(maxgrds) :: nnx,nnx1,nnx2,nny,nny1,nny2,nnz,nnz1  &
                              ,nnxyzp,nnxysp,nnxyp
common /grpnts/ nnx,nnx1,nnx2,nny,nny1,nny2,nnz,nnz1  &
               ,nnxyzp,nnxysp,nnxyp
!-------------------------------------------------------------------------------
real, dimension(nzpmax,maxgrds) :: htn,ht2n,ht4n,hwn,hw2n,hw4n
common /sigmaz/ htn,ht2n,ht4n,hwn,hw2n,hw4n
!-------------------------------------------------------------------------------
real, dimension(nzpmax,maxgrds) :: dztn,dzmn,dzt2n,dzm2n,ztn,zmn
real, dimension(nxpmax,maxgrds) :: xtn,xmn
real, dimension(nypmax,maxgrds) :: ytn,ymn
common /spacing/ dztn,dzmn,dzt2n,dzm2n,xtn,xmn,ytn,ymn,ztn,zmn
!-------------------------------------------------------------------------------
real, dimension(nxpmax,6,6,2,maxgrds) :: advwtx
real, dimension(nypmax,6,6,2,maxgrds) :: advwty
real, dimension(nzpmax,6,6,2,maxgrds) :: advwtz
common /advctn/ advwtx,advwty,advwtz
!-------------------------------------------------------------------------------
integer                 :: nslcon,nvgcon
real                    :: zrough,pctlcon
real, dimension(nzgmax) :: stgoff,slmstr
common /soil_veg2/ stgoff,slmstr,nslcon,zrough,pctlcon,nvgcon
!-------------------------------------------------------------------------------
integer :: nxp,nx,nx1,nx2,nyp,ny,ny1,ny2,nzp,nzpp,nz,nz1  &
          ,nxyzp,nxyp,nxysp,nscl,nsttop,nstbot,ndtrat,jdim
common /dompts/ nxp,nx,nx1,nx2,nyp,ny,ny1,ny2,nzp,nzpp,nz,nz1  &
               ,nxyzp,nxyp,nxysp,nscl,nsttop,nstbot,ndtrat,jdim
!-------------------------------------------------------------------------------
real                    :: deltax,deltay,deltaz
real, dimension(nzpmax) :: ht,ht2,ht4,hw,hw2,hw4,zt,zm,dzt,dzm,dzt2,dzm2
real, dimension(nxpmax) :: xt,xm
real, dimension(nypmax) :: yt,ym
common /grid/ deltax,deltay,deltaz,ht,ht2,ht4,hw,hw2,hw4  &
             ,xt,xm,yt,ym,zt,zm,dzt,dzm,dzt2,dzm2
!-------------------------------------------------------------------------------
real, dimension(nzpmax,nypmax,4) :: cphx
real, dimension(nzpmax,nxpmax,4) :: cphy
common /bndcon/cphx,cphy
!-------------------------------------------------------------------------------

character(len=80)                     :: varfil1,varfil2,varfpfx
character(len=80), dimension(maxvarf) :: varfil
common /varchr/ varfil1,varfil2,varfil,varfpfx
!-------------------------------------------------------------------------------
real                    :: vtime1,vtime2,vwait1,vwaittot
real,dimension(maxvarf) :: vtime
integer                 :: nvarf
common /var2/ vtime1,vtime2,vtime,nvarf,vwait1,vwaittot
!-------------------------------------------------------------------------------
character(len=80)                             :: sstfpfx
character(len=80), dimension(maxgrds)         :: sstfil1,sstfil2
character(len=80), dimension(maxsstf,maxgrds) :: vsstfil,sstfil
common /sstchr/ vsstfil,sstfil,sstfil1,sstfil2,sstfpfx
!-------------------------------------------------------------------------------
integer                             :: iupdsst
integer, dimension(maxgrds)         :: nvsstf,nsstf,isstf,isstrecy
integer, dimension(maxsstf,maxgrds) :: iyearvs,imonthvs,idatevs,ihourvs
real ,dimension(maxgrds)            :: ssttime1,ssttime2
real ,dimension(maxsstf,maxgrds)    :: ssttime
common /sst2/ iyearvs,imonthvs,idatevs,ihourvs  &
             ,ssttime,ssttime1,ssttime2  &
             ,nvsstf,nsstf,isstf,iupdsst,isstrecy
!-------------------------------------------------------------------------------
character(len=80)                             :: ndvifpfx
character(len=80), dimension(maxgrds)         :: ndvifil1,ndvifil2
character(len=80), dimension(maxsstf,maxgrds) :: vndvifil,ndvifil
common /ndvichr/ vndvifil,ndvifil,ndvifil1,ndvifil2,ndvifpfx
!-------------------------------------------------------------------------------
integer                             :: iupdndvi
integer, dimension(maxgrds)         :: nvndvif,nndvif,indvif,indvirecy
integer, dimension(maxsstf,maxgrds) :: iyearvn,imonthvn,idatevn,ihourvn
real ,dimension(maxgrds)            :: time1_ndvi,time2_ndvi
real ,dimension(maxsstf,maxgrds)    :: time_ndvi
common /ndvi2/ iyearvn,imonthvn,idatevn,ihourvn  &
             ,time_ndvi,time1_ndvi,time2_ndvi  &
             ,nvndvif,nndvif,indvif,iupdndvi,indvirecy
!-------------------------------------------------------------------------------
real, dimension(maxdimp)    :: vctr1 ,vctr2 ,vctr3 ,vctr4 ,vctr5 ,vctr6  &
                              ,vctr7 ,vctr8 ,vctr9 ,vctr10,vctr11,vctr12  &
                              ,vctr13,vctr14,vctr15,vctr16,vctr17,vctr18  &
                              ,vctr19,vctr20,vctr21,vctr22,vctr23,vctr24  &
                              ,vctr25,vctr26,vctr27,vctr28,vctr29,vctr30  &
                              ,vctr31,vctr32,vctr33,vctr34,vctr35,vctr36  &
                              ,vctr37,vctr38,vctr39,vctr40,vctr41
integer, dimension(maxdimp) :: ivctr 
common /vctemp/ vctr1 ,vctr2 ,vctr3 ,vctr4 ,vctr5 ,vctr6  &
               ,vctr7 ,vctr8 ,vctr9 ,vctr10,vctr11,vctr12  &
               ,vctr13,vctr14,vctr15,vctr16,vctr17,vctr18  &
               ,vctr19,vctr20,vctr21,vctr22,vctr23,vctr24  &
               ,vctr25,vctr26,vctr27,vctr28,vctr29,vctr30  &
               ,vctr31,vctr32,vctr33,vctr34,vctr35,vctr36  &
               ,vctr37,vctr38,vctr39,vctr40,vctr41,ivctr
!-------------------------------------------------------------------------------
integer :: isstp,istp,initfld
real    :: timmax,dts,dtlt,dtlv
common /cntrlr/ timmax,isstp,istp,dts,dtlt,dtlv,initfld
!-------------------------------------------------------------------------------
real, dimension(nzpmax) :: pi,p0,temp,prt,rc,thet,rvls
real                    :: pfactr,tnudlat,tnudcent,tnudtop,znudtop
integer                 :: nudlat
common /stuff/ pi,p0,temp,prt,rc,thet,rvls,pfactr  &
              ,nudlat,tnudlat,tnudcent,tnudtop,znudtop

!-------------------------------------------------------------------------------
!      I/O table commons and information

!-------------------------------------------------------------------------------
integer :: ngrid,ngridc,ngrido,iscr1,iscr2
common /ioinfo/ ngrid,ngridc,ngrido,iscr1,iscr2
!-------------------------------------------------------------------------------
integer                    :: memsize,iounit,maxpro,memscr,memind  &
                             ,iogrid,maxpts,maxnzp,maxnxp,maxnyp,i2dvar
integer,dimension(maxgrds) :: memgrd
common/ioparm/memsize,iounit,maxpro,memscr,memind  &
             ,iogrid,memgrd,maxpts,maxnzp,maxnxp,maxnyp,i2dvar




