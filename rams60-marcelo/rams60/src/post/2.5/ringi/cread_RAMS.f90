!############################# Change Log ##################################
! 2.3.0.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000 - All Rights Reserved
!  Regional Atmospheric Modelling System - RAMS
!  Mission Research Corporation / *ASTER Division
!###########################################################################

subroutine cread_RAMS (islab_r,icoor_r,cgrid_r,cvar_r,ctime_r  &
                      ,itrans_r,ctype_r,ccont_r,cwinds_r  &
                      ,iwoffl_r,iwoffr_r,iwoffb_r,iwofft_r  &
                      ,conrinc_r,conrlo_r,conrhi_r  &
                      ,wcx1_r,wcx2_r,wcy1_r,wcy2_r  &
                      ,intwind_r,ibscale_r,ifill_r,iplotinfo_r  &
                      ,vartype_r,overvar_r,overtype_r &
                      ,ovmin_r,ovmax_r,ovinc_r  &
                      ,colorbar_r,title_r  &
                      ,wxmin,wxmax,wymin,wymax)
                      
implicit none

integer :: islab_r,icoor_r,itrans_r,iwoffl_r,iwoffr_r,iwoffb_r,iwofft_r  &
          ,intwind_r,ibscale_r,ifill_r,iplotinfo_r
real :: conrinc_r,conrlo_r,conrhi_r,wcx1_r,wcx2_r,wcy1_r,wcy2_r  &
       ,ovmin_r,ovmax_r,ovinc_r  &
       ,wxmin,wxmax,wymin,wymax
character(len=*) :: cgrid_r,cvar_r,ctime_r,ctype_r,ccont_r  &
     ,cwinds_r,vartype_r,overvar_r,overtype_r,colorbar_r,title_r

include 'frame.h'
include 'window.h'

real :: fobs(1),pred(1),fpos(1)

real :: xmin,xmax,ymin,ymax
common/mapcnr/xmin,xmax,ymin,ymax

integer :: nplot,ivvar,ivtime,maxfore,igunit,irefg,jrefg, nzb,nze &
          ,izstep,igridll,nnboff,nneoff
real  :: glats,glatn,glonw,glone,xref,yref,glldllat,glldllon  &
        ,gllwlon,gllelon,gllslat,gllnlat

character(len=1)  :: cframe_a(1),revpref
character(len=15) :: dategrads
character(len=4)  :: cincgrads
         
integer, save :: ncall=0,ibackgnd=1
character(len=24), save :: cdname1=' ',cdunits1=' '


if(ncall.eq.0) then
   ncall=1
   iwk=1
   call gks_colors(iwk,ibackgnd)
endif

print *
print *,  "Calling read_RAMS with..."
print 11, "  title                 ",title_r
print 10, "  islab                 ",islab_r
print 10, "  icoor                 ",icoor_r
print 11, "  cgrid   (Grid Number) ",cgrid_r
print 11, "  ctime   (File Number) ",ctime_r
print 10, "  itrans                ",itrans_r
print 11, "  ctype   (Var Type)    ",ctype_r
print 11, "  cvar    (Variable)    ",cvar_r
print 11, "  vartype  (Plot Type)  ",vartype_r
print 11, "  over var (Variable)   ",overvar_r
print 11, "  overtype (Plot Type)  ",overtype_r
print 11, "  ccont   (Contour?)    ",ccont_r
print 11, "  cwinds  (Winds?)      ",cwinds_r,intwind_r,ibscale_r
print 10, "  iwof?   (Offsets)     ",iwoffl_r,iwoffr_r,iwoffb_r,iwofft_r
print 12, "  contour (inc,lo,hi)   ",conrinc_r,conrlo_r,conrhi_r
print 12, "  over cont (inc,lo,hi) ",ovinc_r,ovmin_r,ovmax_r
print 11, "  colorbar              ",colorbar_r
print 10, "  iplotinfo             ",iplotinfo_r
print 10, "  map fill              ",ifill_r
print 13, "  coords1               ",iwoffl_r,iwoffr_r,iwoffb_r,iwofft_r
print 14, "  coords1               ",wcx1_r,wcx2_r,wcy1_r,wcy2_r
print *

10  format(a24,':',i12,1x,i12,1x,i12,1x,i12)
11  format(a24,':',a12,2i5)
12  format(a24,':',f12.4,f12.4,f12.4)
13  format(a24,':',4i6)
14  format(a24,':',4f12.3)

print*,'Window in: ',xmin,xmax,ymin,ymax

!     set defaults for contour and window variables, then
!     replace those that are passed in from ringi

call continit()

fillcols(1,1)='cyan'
fillcols(2,1)='yellow'
ichigh(1:2)=-1

cvar(1)=cvar_r
cvar(2)=overvar_r
conttyp(1)=vartype_r(1:1)
conttyp(2)=overtype_r(1:1)
if(cvar_r(1:4).ne.'none') then
   colorbar(1)=colorbar_r(1:1)
else
   colorbar(1)='n'
endif
if(overvar_r(1:4).ne.'none') then
   colorbar(2)=colorbar_r(1:1)
else
   colorbar(2)='n'
endif
cwinds=cwinds_r
ipinfo=iplotinfo_r

conrlo(1)=conrlo_r
conrhi(1)=conrhi_r
conrinc(1)=conrinc_r

ibgnd=ibackgnd
mfill=ifill_r
mfill=3

islab=islab_r
icoor=icoor_r

ipanl=1
           
nplot=1
ivvar=1
ivtime=1
cframe_a(1)=' '
maxfore=30
fobs(1)=1.
pred(1)=1.
fpos(1)=1.
igunit=0
glats=-1.
glatn=-1.
glonw=-1.
glone=-1.
irefg=0
jrefg=0
xref=-1.
yref=-1.
dategrads=' '
cincgrads=' '
!cdname=' '  ! done in data statement
!cdunits=' '
igridll=0
glldllat=0.
glldllon=0.
gllwlon=0.
gllelon=0.
gllslat=0.
gllnlat=0.
revpref=' '

nnboff=0
nneoff=0

! set CFRAME_B defaults
intwindi=intwind_r
intwindj=intwind_r
ibscale=ibscale_r
stemleng=-1.
!stemleng=.8
velomax=0.
headleng=-1.
headleng0=-1.
headang=-1.

! set LANDMARK defaults
ilandmk=1
bufflandmk=0.02
itypelandmk=2
collandmk='lightgray'
sizelandmk=1.0
ilandlab=1
collandlab='lightgray'
sizelandlab=0.008

! temp fix preventing cart plots in the vert
! should be done in the ringi gui
if(islab.ne.3) itrans_r=1

call read_RAMS ('ringi','SPACE',ivvar,ivtime  &
               ,cgrid_r,ctime_r,itrans_r,ccont_r  &
               ,iwoffl_r,iwoffr_r,iwoffb_r,iwofft_r,nnboff, nneoff  &
               ,title_r,wcx1_r,wcx2_r,wcy1_r,wcy2_r  &
               ,fobs(1),pred(1),fpos(1)  &
               ,cframe_a,maxfore,igunit  &
               ,glats,glatn,glonw,glone  &
               ,irefg,jrefg,xref,yref,dategrads,cincgrads  &
               ,igridll,glldllat,glldllon  &
               ,gllwlon,gllelon,gllslat,gllnlat  &
               ,cdname1,cdunits1,revpref)
               

print*,'Window out: ',xmin,xmax,ymin,ymax
!print*,' ======= finished frame =========='

wxmin=xmin
wxmax=xmax
wymin=ymin
wymax=ymax

return
end
